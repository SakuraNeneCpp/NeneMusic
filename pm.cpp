// pm.cpp — Programmable Music (.pm) single‑file compiler/player
// --------------------------------------------------------------
// Implements a tiny DSL for music ("pm: programmable music").
// Features
//  - Global bpm and meter
//  - sheet <name>(<instrument>, <keysig>) = { ... } with bars [ ... ] and repeats *N
//  - Notes: c4, d#4 (via key signature only; inline #/b not yet), rests: _
//  - Chords: c4+e4+g4 (parenthesize if you apply *N to the whole chord)
//  - Group duration: 8( ... ) means each token inside is an 8th‑note relative to the meter
//  - Multipliers: (c4+e4+g4)*2  or  _*0.5
//  - Orchestration block `music start { ... } end`
//      • Parallel:  a+b  (start simultaneously; end when the longer ends)
//      • Sequence:  lines separated by ';' are concatenated in time
//      • Repeats:   piano1*2 or (piano1+piano2)*3
//  - Synth: simple physically‑inspired piano (multi‑partial + decays)
//  - makewav: writes 16‑bit PCM WAV; play: tries to invoke OS player
//
// Usage:
//   pm makewav song.pm   → creates song.wav in the same directory
//   pm play    song.pm   → renders and opens the wav with the default player
//
// Notes & limits:
//  - Key signatures are written like: 0+-, f+, be-  (letters cdefgab followed by + or -).
//    Example: f+  (F# major key sig pattern),  be- (Bb and Eb),  0+- (no accidentals).
//  - Inline accidentals (#/b) inside notes are not supported in this first version.
//  - Beat = the meter’s denominator note. In 4/4 the beat is a quarter; in 3/8 the beat is an eighth.
//    8( ... ) assigns each item one 8th‑note (= D/N beats, D=denominator, N=8 here).
//  - Bars auto‑pad with rest to the bar length if they’re short; over‑filled bars are accepted but warned.
//  - Mono output (44100 Hz). Easy to extend to stereo.

#define _USE_MATH_DEFINES
#include <algorithm>
#include <array>
#include <cctype>
#include <cmath>
#include <cstdint>
#include <cstdlib>
#include <exception>
#include <filesystem>
#include <fstream>
#include <functional>
#include <iomanip>
#include <iostream>
#include <map>
#include <optional>
#include <sstream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>

constexpr double PI = 3.141592653589793238462643383279502884;

using std::string; using std::string_view; using std::vector; using std::cout; using std::cerr; using std::endl;

// ----------------------------- Utilities -----------------------------
static inline std::string trim_copy(std::string s){
    auto notspace = [](int ch){ return !std::isspace(ch); };
    s.erase(s.begin(), std::find_if(s.begin(), s.end(), notspace));
    s.erase(std::find_if(s.rbegin(), s.rend(), notspace).base(), s.end());
    return s;
}

static inline char to_lower_ascii(char ch){
    return static_cast<char>(std::tolower(static_cast<unsigned char>(ch)));
}

[[noreturn]] static void fail(const std::string& msg){ throw std::runtime_error(msg); }

// ----------------------------- Tokenizer -----------------------------------
// We use a simple tokenizer with comment skipping (// ... endline).

struct Tok{ enum Kind{ ID, NUM, SYM, END } kind; std::string text; int line; };

struct Lexer{
    string src; size_t i{0}; int line{1};
    explicit Lexer(string s):src(std::move(s)){}

    void skip_ws(){
        while(i<src.size()){
            if(src[i]=='/' && i+1<src.size() && src[i+1]=='/'){
                i+=2; while(i<src.size() && src[i]!='\n') ++i; // skip comment
            } else if(std::isspace((unsigned char)src[i])){
                if(src[i]=='\n') ++line; ++i;
            } else break;
        }
    }

    Tok next(){
        skip_ws();
        if(i>=src.size()) return {Tok::END, "", line};
        char c=src[i];
        // Symbols single‑char set
        const string syms = "=;(),{}[]+*-/_"; // '_' as a symbol too
        if(syms.find(c)!=string::npos){
            ++i; return {Tok::SYM, string(1,c), line};
        }
        if(std::isalpha((unsigned char)c) || c=='_'){
            size_t j=i+1; while(j<src.size() && (std::isalnum((unsigned char)src[j]) || src[j]=='_' )) ++j;
            string t = src.substr(i,j-i); i=j; return Tok{Tok::ID, t, line};
        }
        if(std::isdigit((unsigned char)c)){
            size_t j=i+1; while(j<src.size() && std::isdigit((unsigned char)src[j])) ++j;
            string t = src.substr(i,j-i); i=j; return Tok{Tok::NUM, t, line};
        }
        // Special case for key signature token like "f+" or "be-" or "0+-":
        // We allow a run of letters/digits followed by +/- chars until delimiter.
        if(c=='0' || c=='b' || c=='e' || c=='f'){
            size_t j=i; while(j<src.size()){
                char ch=src[j];
                if(std::isalnum((unsigned char)ch) || ch=='+' || ch=='-') { ++j; }
                else break;
            }
            // Heuristic: only treat as ID if followed by ')' or ',' nearby in sheet() header.
            string t = src.substr(i, j-i);
            i=j; return Tok{Tok::ID, t, line};
        }
        fail("Lexer: Unexpected character at line "+std::to_string(line)+": '"+string(1,c)+"'");
    }
};

// ----------------------------- Music model ---------------------------------

struct KeySig{
    // 0: C D E F G A B  (indices 0..6)
    bool sharp[7]{}; bool flat[7]{};
    static int note_index_from_char(char ch){
        switch(std::tolower((unsigned char)ch)){
            case 'c': return 0; case 'd': return 1; case 'e': return 2;
            case 'f': return 3; case 'g': return 4; case 'a': return 5; case 'b': return 6;
            default: return -1;
        }
    }
    static int semitone_from_letter(int idx){ // C=0,D=2,E=4,F=5,G=7,A=9,B=11
        static int base[7]={0,2,4,5,7,9,11}; return base[idx];
    }
    int accidental_for(int letter_idx) const{
        return (sharp[letter_idx]? +1:0) + (flat[letter_idx]? -1:0);
    }
};

struct Global{
    int bpm{120}; int meter_num{4}; int meter_den{4};
    double seconds_per_beat() const{ return 60.0 / std::max(1, bpm); }
    double bar_seconds() const{ return seconds_per_beat() * meter_num; }
    // Convert an N‑th note to seconds: beats = D/N, seconds = beats * spb.
    double nth_note_seconds(int N) const{ return seconds_per_beat() * (double)meter_den / (double)N; }
};

struct Event{ // one timed musical event (single note, chord, or rest)
    double start_sec{}; double dur_sec{}; // duration in seconds
    std::vector<int> midi; // empty => rest
};

struct Sheet{
    string name; string instrument; KeySig keysig; vector<Event> events; double length_sec{0.0};
};

// ----------------------------- Parser --------------------------------------

struct Parser{
    Lexer L; Tok t{Tok::END, "", 1}; Global G; std::unordered_map<string, Sheet> sheets; 

    explicit Parser(string s):L(std::move(s)){ t=L.next(); }

    bool accept(Tok::Kind k, string_view s=""){ if(t.kind!=k) return false; if(s.size() && t.text!=s) return false; t=L.next(); return true; }
    void expect(Tok::Kind k, string_view s=""){ if(!accept(k,s)) fail(here("Expected '"+string(s.empty()?"<tok>":string(s))+"'")); }
    std::string here(const std::string& msg){ return msg+" at line "+std::to_string(t.line); }

    static KeySig parse_keysig_token(const std::string& token){
        KeySig k{}; // token like "0+-", "f+", "be-" etc.
        // Any contiguous letters belong to the set being modified; trailing + or - applies to the *preceding* run.
        // e.g., "be-" => letters {b,e} get flat,  "f+" => {f} sharp.
        // "0+-" => empty (no accidentals).
        if(token=="0+-" || token=="0") return k;
        // Split into (letters run)+(sign)
        std::string letters; char sign='\0';
        for(char ch: token){ if(std::isalpha((unsigned char)ch)) letters.push_back(to_lower_ascii(ch)); else if(ch=='+'||ch=='-') sign=ch; }
        if(sign=='\0' || letters.empty()) return k; // ignore malformed → treat as C major
        for(char ch: letters){ int idx=KeySig::note_index_from_char(ch); if(idx>=0){ if(sign=='+') k.sharp[idx]=true; else if(sign=='-') k.flat[idx]=true; } }
        return k;
    }

    int parse_int(const char* what){ if(t.kind!=Tok::NUM) fail(here(string("Expected number for ")+what)); int v=std::stoi(t.text); t=L.next(); return v; }
    string parse_id(const char* what){ if(t.kind!=Tok::ID) fail(here(string("Expected identifier for ")+what)); string v=t.text; t=L.next(); return v; }

    void parse_program(){
        while(t.kind!=Tok::END){
            if(t.kind==Tok::ID && t.text=="bpm"){ t=L.next(); expect(Tok::SYM,"="); G.bpm=parse_int("bpm"); expect(Tok::SYM,";"); }
            else if(t.kind==Tok::ID && t.text=="meter"){ t=L.next(); expect(Tok::SYM,"="); int a=parse_int("meter num"); expect(Tok::SYM,"/"); int b=parse_int("meter den"); G.meter_num=a; G.meter_den=b; expect(Tok::SYM,";"); }
            else if(t.kind==Tok::ID && t.text=="sheet"){ parse_sheet(); }
            else if(t.kind==Tok::ID && t.text=="music"){ parse_music_block(); }
            else{ fail(here("Unexpected token: "+t.text)); }
        }
    }

    // ------------ Sheet parsing -------------
    // sheet name (instrument, keysig) = { bars } ;
    void parse_sheet(){
        expect(Tok::ID, "sheet"); string name = parse_id("sheet name");
        expect(Tok::SYM, "("); string instrument = parse_id("instrument"); expect(Tok::SYM, ",");
        // keysig token can be like 0+-, f+, be-
        if(t.kind!=Tok::ID && t.kind!=Tok::NUM) fail(here("Expected keysig token like 0+-, f+, be-"));
        std::string ktoken = t.text;
        t = L.next();
        while (t.kind == Tok::SYM && (t.text == "+" || t.text == "-")) {
            ktoken += t.text;
            t = L.next();
        }
        expect(Tok::SYM, ")"); expect(Tok::SYM, "="); expect(Tok::SYM, "{");
        Sheet sh{}; sh.name=name; sh.instrument=instrument; sh.keysig=parse_keysig_token(ktoken);
        current_sheet_keysig = sh.keysig;
        parse_bars_into(sh);
        expect(Tok::SYM, "}"); expect(Tok::SYM, ";");
        sheets[sh.name]=std::move(sh);
    }

    struct RelEvent{ double start; double dur; std::vector<int> midi; }; // relative inside a bar

    void parse_bars_into(Sheet& sh){
        const double bar_len = G.bar_seconds();
        while(!(t.kind==Tok::SYM && t.text=="}")){
            if(accept(Tok::SYM,"[")){
                // Parse a single bar content
                vector<RelEvent> rel; double cursor=0.0; // seconds from bar start
                parse_bar_items(rel, cursor);
                expect(Tok::SYM, "]");
                // Optional *N repeater for the bar
                int times=1; if(accept(Tok::SYM,"*")){ times = parse_int("bar repeat count"); }
                // Commit events into absolute sheet timeline for 'times' repeats
                for(int r=0;r<times;++r){
                    // Over/under filling management
                    if(cursor < bar_len){ // pad rest
                        rel.push_back(RelEvent{cursor, bar_len-cursor, {}});
                    } else if(cursor > bar_len){
                        cerr << "[warn] bar overfilled by " << (cursor-bar_len) << " s at line " << t.line << "\n";
                    }
                    for(auto &rv: rel){ Event e; e.start_sec = sh.length_sec + rv.start; e.dur_sec = rv.dur; e.midi = rv.midi; sh.events.push_back(std::move(e)); }
                    sh.length_sec += bar_len; // advance to next bar position
                }
            } else {
                // allow stray semicolons or whitespace
                if(accept(Tok::SYM,";")) continue; else break;
            }
        }
    }

    // Parse inside a [ ... ] bar until ']'
    void parse_bar_items(vector<RelEvent>& out, double& cursor){
        // default unit = 1 beat (meter_den‑th note)
        const double unit_default = G.seconds_per_beat();
        while(!(t.kind==Tok::SYM && t.text=="]")){
            if(t.kind==Tok::NUM){ // group N( ... )
                int denom = parse_int("group note length"); expect(Tok::SYM,"(");
                double unit = G.nth_note_seconds(denom);
                // parse tokens until ')'
                while(!(t.kind==Tok::SYM && t.text==")")){
                    parse_one_token_as_event_in_unit(out, cursor, unit);
                }
                expect(Tok::SYM,")");
            } else if(accept(Tok::SYM,"(")){
                // Parenthesized chord possibly with *factor
                vector<int> chord = parse_chord_body(); expect(Tok::SYM,")");
                double dur = unit_default; // default 1 beat
                if(accept(Tok::SYM,"*")){
                    // factor could be integer or x/y as NUM '/' NUM; we accept integer or float via NUM(.NUM)? but lexer is integer only, accept NUM[/NUM]
                    int mul = parse_int("multiplier"); dur *= mul;
                }
                out.push_back(RelEvent{cursor, dur, chord}); cursor += dur;
            } else if(t.kind==Tok::ID || (t.kind==Tok::SYM && t.text=="_")){
                parse_one_token_as_event_in_unit(out, cursor, unit_default);
            } else if(accept(Tok::SYM,";")){
                // separator inside bar (optional)
            } else {
                break; // let caller handle unexpected
            }
        }
    }

    // Parse a single note/rest/chord token, using provided unit duration (before *factor)
    void parse_one_token_as_event_in_unit(vector<RelEvent>& out, double& cursor, double unit){
        if(accept(Tok::SYM,"_")){
            double dur = unit; if(accept(Tok::SYM,"*")){ int mul=parse_int("rest multiplier"); dur*=mul; }
            out.push_back(RelEvent{cursor, dur, {}}); cursor += dur; return;
        }
        if(t.kind==Tok::ID){
            // Could be a run like c4+e4+g4 (unparenthesized chord)
            vector<int> chord;
            chord.push_back(parse_pitch());
            while(accept(Tok::SYM,"+")){
                chord.push_back(parse_pitch());
            }
            double dur = unit; if(accept(Tok::SYM,"*")){ int mul=parse_int("note multiplier"); dur*=mul; }
            out.push_back(RelEvent{cursor, dur, chord}); cursor += dur; return;
        }
        fail(here("Expected note/rest/chord token"));
    }

    // Parses chord content: c4+e4+g4 inside '( ... )'
    vector<int> parse_chord_body(){
        vector<int> chord;
        chord.push_back(parse_pitch());
        while(accept(Tok::SYM,"+")) chord.push_back(parse_pitch());
        return chord;
    }

    int parse_pitch(){ // from token like c4
        string id = parse_id("pitch");
        if(id.size()<2) fail(here("Bad pitch token: "+id));
        char note = to_lower_ascii(id[0]);
        int letter = KeySig::note_index_from_char(note);
        if(letter<0) fail(here("Unknown note letter: "+id));
        // parse trailing integer octave
        int oct = 0; size_t k=1; bool any=false; while(k<id.size() && std::isdigit((unsigned char)id[k])){ any=true; oct = oct*10 + (id[k]-'0'); ++k; }
        if(!any) fail(here("Missing octave in pitch: "+id));
        int semitone = KeySig::semitone_from_letter(letter);
        semitone += sheets_current_keysig().accidental_for(letter);
        int midi = (oct + 1)*12 + semitone; // MIDI note number (C4=60)
        return midi;
    }

    // helper: in pitch parsing we need current sheet’s keysig, but we parse sheet sequentially.
    KeySig current_sheet_keysig; // set by parse_sheet
    const KeySig& sheets_current_keysig() const{ return current_sheet_keysig; }

    // ---------------- Music block parsing ----------------
    struct MusicNode{ // AST for orchestration
        struct Ref{ string name; };
        using Ptr = std::shared_ptr<MusicNode>;
        enum Type{ REF, PAR, SEQ, REP } type;
        // PAR: left+right;  SEQ: left;right  (we'll build SEQ from lines, not AST), REP: (child)*k
        Ptr left, right, child; int times{1}; string ref_name;
        static Ptr make_ref(string n){ auto p=std::make_shared<MusicNode>(); p->type=REF; p->ref_name=std::move(n); return p; }
        static Ptr make_par(Ptr a, Ptr b){ auto p=std::make_shared<MusicNode>(); p->type=PAR; p->left=a; p->right=b; return p; }
        static Ptr make_rep(Ptr c, int k){ auto p=std::make_shared<MusicNode>(); p->type=REP; p->child=c; p->times=k; return p; }
    };

    vector<MusicNode::Ptr> music_lines; // each line is a sequence element

    void parse_music_block(){
        expect(Tok::ID, "music"); expect(Tok::ID, "start"); expect(Tok::SYM, "{");
        // Parse zero or more expressions separated by ';'
        while(!(t.kind==Tok::SYM && t.text=="}")){
            if(t.kind==Tok::SYM && t.text==";") { t=L.next(); continue; }
            auto expr = parse_music_expr();
            music_lines.push_back(expr);
            if(accept(Tok::SYM, ";")) continue; else break;
        }
        expect(Tok::SYM, "}"); expect(Tok::ID, "end");
    }

    // expr := term ( "+" term )*
    MusicNode::Ptr parse_music_expr(){
        auto lhs = parse_music_term();
        while(accept(Tok::SYM, "+")){
            auto rhs = parse_music_term();
            lhs = MusicNode::make_par(lhs, rhs);
        }
        return lhs;
    }

    // term := factor ["*" NUM]
    // factor := ID | "(" expr ")"
    MusicNode::Ptr parse_music_term(){
        MusicNode::Ptr base;
        if(accept(Tok::SYM, "(")){
            base = parse_music_expr(); expect(Tok::SYM, ")");
        } else {
            string nm = parse_id("sheet ref"); base = MusicNode::make_ref(nm);
        }
        if(accept(Tok::SYM, "*")){
            int k = parse_int("repeat count"); base = MusicNode::make_rep(base, k);
        }
        return base;
    }

    // ----------------- Rendering orchestrations -----------------

    // Retrieve sheet by name or fail.
    const Sheet& get_sheet(const string& nm) const{
        auto it=sheets.find(nm); if(it==sheets.end()) fail("Undefined sheet: "+nm); return it->second; }

    // Render one MusicNode into an audio buffer; sample rate given.
    using Audio = vector<float>;
    static void mix_into(Audio& dst, const Audio& src, size_t offset){
        if(dst.size() < offset+src.size()) dst.resize(offset+src.size(), 0.0f);
        for(size_t i=0;i<src.size();++i) dst[offset+i] += src[i];
    }

    Audio render_music(const MusicNode::Ptr& node, int sr) const{
        switch(node->type){
            case MusicNode::REF: {
                const Sheet& sh = get_sheet(node->ref_name);
                return render_sheet(sh, sr);
            }
            case MusicNode::PAR: {
                Audio a = render_music(node->left, sr);
                Audio b = render_music(node->right, sr);
                if(a.size()<b.size()) a.resize(b.size(), 0.0f);
                for(size_t i=0;i<b.size();++i) a[i]+=b[i];
                return a;
            }
            case MusicNode::REP: {
                Audio unit = render_music(node->child, sr); Audio out; out.reserve(unit.size()*node->times);
                for(int i=0;i<node->times;++i){ size_t off = out.size(); out.resize(off + unit.size(), 0.0f); for(size_t j=0;j<unit.size();++j) out[off+j] = unit[j]; }
                return out;
            }
            default: return {}; // SEQ not used here
        }
    }

    // Render full music (sequence of lines) → concatenated audio.
    Audio render_full_music(int sr) const{
        Audio master; for(const auto& line : music_lines){ Audio seg = render_music(line, sr); mix_into(master, seg, master.size()); }
        return master;
    }

    // ----------------- Synthesizer -----------------

    struct Instrument{
        virtual ~Instrument() = default;
        virtual void render_note(vector<float>& buf, int sr, double t0, double dur, int midi, float vel=1.0f) const = 0;
    };

    struct Piano : Instrument{
        void render_note(vector<float>& buf, int sr, double t0, double dur, int midi, float vel) const override{
            double f = 440.0 * std::pow(2.0, (midi - 69) / 12.0);
            size_t start = (size_t)std::llround(t0 * sr);
            size_t n = (size_t)std::llround(dur * sr);
            if(buf.size() < start+n+1) buf.resize(start+n+1, 0.0f);
            // Simple piano model: multi‑partial sum with double exponential decay, quick attack.
            const int partials = 8;
            double amp0 = 0.18 * vel; // global amplitude
            std::array<double, partials> w{};
            for(int k=0;k<partials;++k) w[k] = amp0 * std::pow(0.6, k); // geometric falloff
            // detune slight for chorus feel
            std::array<double, partials> det{}; for(int k=0;k<partials;++k) det[k] = (k%2? 1.0+0.002*k : 1.0-0.0015*k);
            double tau_fast = 0.12, tau_slow = 1.20; // seconds
            for(size_t i=0;i<n;++i){
                double time_s = static_cast<double>(i) / sr;
                // attack curve (~5ms)
                double a = std::min(1.0, (time_s*200.0));
                double env = a * (0.9*std::exp(-time_s/tau_fast) + 0.35*std::exp(-time_s/tau_slow));
                double s=0.0;
                for(int k=0;k<partials;++k){
                    double fk = f * (k+1) * det[k];
                    s += w[k] * std::sin(2*PI * fk * time_s);;
                }
                buf[start+i] += (float)(env * s);
            }
            // soft release tail (5 ms) to avoid click when next note starts later
            size_t tail = std::min((size_t) (0.005*sr), (size_t)1024);
            for(size_t i=0;i<tail && start+n+i<buf.size(); ++i){
                double g = 1.0 - (double)i / tail; buf[start+n+i] += (float)(0.0 * g);
            }
        }
    };

    // Registry for instruments
    std::unordered_map<string, std::unique_ptr<Instrument>> instrument_bank;

    void ensure_instruments(){ if(instrument_bank.empty()){ instrument_bank["piano"] = std::make_unique<Piano>(); } }

    // Render a sheet to audio
    Audio render_sheet(const Sheet& sh, int sr) const{
        auto it = instrument_bank.find(sh.instrument);
        if(it==instrument_bank.end()) fail("Unknown instrument: "+sh.instrument);
        const Instrument& inst = *it->second;
        Audio buf; // mono
        for(const auto& e : sh.events){
            if(e.midi.empty()) continue; // rest
            // Render chord by summing each pitch
            for(int m : e.midi){ inst.render_note(buf, sr, e.start_sec, e.dur_sec, m, 1.0f); }
        }
        return buf;
    }
};

// ----------------------------- WAV writer ----------------------------------

struct WavWriter{
    static void write_wav_16(const std::string& path, const std::vector<float>& mono, int sample_rate=44100){
        // Normalize softly if peaks exceed 1.0
        float peak = 0.f; for(float x : mono) peak = std::max(peak, std::abs(x));
        float g = (peak>0.999f)? (0.999f/peak) : 1.0f;
        std::ofstream f(path, std::ios::binary); if(!f) fail("Cannot open output file: "+path);
        auto write_u32=[&](uint32_t v){ f.put((char)(v&0xFF)); f.put((char)((v>>8)&0xFF)); f.put((char)((v>>16)&0xFF)); f.put((char)((v>>24)&0xFF)); };
        auto write_u16=[&](uint16_t v){ f.put(static_cast<char>(v & 0xFF)); f.put(static_cast<char>((v >> 8) & 0xFF)); };
        auto write_str=[&](const char* s){ f.write(s, 4); };
        int channels=1; int bits=16; int byte_rate = sample_rate * channels * bits/8; int block_align = channels * bits/8;
        int data_bytes = (int)mono.size() * block_align;
        // RIFF header
        write_str("RIFF"); write_u32(36 + data_bytes); write_str("WAVE");
        // fmt chunk
        write_str("fmt "); write_u32(16); write_u16(static_cast<uint16_t>(1)); write_u16(static_cast<uint16_t>(channels)); write_u32(sample_rate); write_u32(byte_rate); write_u16(static_cast<uint16_t>(block_align)); write_u16(static_cast<uint16_t>(bits));
        // data chunk
        write_str("data"); write_u32(data_bytes);
        for(float x : mono){
            int16_t s = static_cast<int16_t>(std::lrintf(x * g * 32767.0f));
            write_u16(static_cast<uint16_t>(s));
        }
    }
};

// ----------------------------- Main ----------------------------------------

static std::string slurp_file(const std::string& path){ std::ifstream ifs(path); if(!ifs) fail("Cannot open: "+path); std::ostringstream ss; ss<<ifs.rdbuf(); return ss.str(); }

static std::string replace_ext(const std::string& path, const std::string& newext){
    std::filesystem::path p(path); p.replace_extension(newext); return p.string(); }

static void try_play(const std::string& wavpath){
#if defined(_WIN32)
    std::string cmd = "start \"\" \"" + wavpath + "\""; std::system(cmd.c_str());
#elif defined(__APPLE__)
    std::string cmd = "afplay \"" + wavpath + "\" &"; std::system(cmd.c_str());
#else
    // Linux/Unix: try xdg-open or aplay
    std::string cmd = "(command -v xdg-open >/dev/null && xdg-open \""+wavpath+"\") || (command -v aplay >/dev/null && aplay \""+wavpath+"\") &"; std::system(cmd.c_str());
#endif
}

int main(int argc, char** argv){
    try{
        if(argc<3){
            cout << "pm — programmable music\n";
            cout << "Usage: pm makewav <file.pm>\n";
            cout << "       pm play    <file.pm>\n";
            return 0;
        }
        std::string cmd = argv[1]; std::string pmfile = argv[2];
        std::string src = slurp_file(pmfile);
        Parser P(src);
        // Prepare instrument bank before pitch parse (for errors later)
        P.ensure_instruments();
        // Set current keysig dynamically while parsing sheets
        // (We expose a trick: set Parser::current_sheet_keysig before bar parsing)
        // Patch: we slightly refactor parse_sheet to set current_sheet_keysig — already done.
        // Parse program
        P.parse_program();
        // Render audio
        int sr=44100;
        auto audio = P.render_full_music(sr);
        std::string wav = replace_ext(pmfile, ".wav");
        WavWriter::write_wav_16(wav, audio, sr);
        if(cmd=="makewav"){ cout << "Wrote: " << wav << "\n"; }
        else if(cmd=="play"){ cout << "Playing: " << wav << "\n"; try_play(wav); }
        else { fail("Unknown command: "+cmd); }
    } catch(const std::exception& e){ cerr << "Error: " << e.what() << "\n"; return 1; }
    return 0;
}
