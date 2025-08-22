# NeneMusic
楽譜DSLとそのコンパイラ(DSL→WAV). ABC記譜法の進化版を作ろう.

```
// かえるの歌
filename="kero"

include<mp3> flog = "flog.mp3";
include<sf2> piano = "piano.sf2";

bpm=120; beat=4/4; bpl=4;

// 右手の譜面
sheet right(piano){
    1| c4 d4 e4 f4 | e4 d4 c4 _ | e4 d4 f4 g4 | f4 d4 e4 |;
    2| c4 _ c4 _ | c4 _ c4 _ | 8(c4 c4 d4 d4 e4 e4 f4 f4) | e4 d4 c4 |;
}

// 左手の譜面
sheet left(piano){
    1| _ _ _ _ | _ _ _ _ | c3 d3 e3 f3 | e3 d3 c3 _ |;
    2| e3 d3 f3 g3 | f3 d3 e3 | c3 _ c3 _ | c3 _ c3 _ |; 
    3| 8(c3 c3 d3 d3 e3 e3 f3 f3) | e3 d3 c3 |;
}

sheet main(){
    [1] right+left;
}

// : コメントアウト
bpm: bpm
m/n: n分のm拍子
bpl: bar par line
*| : 行番号, 番号が同じ行は同時に演奏される
|  : 小節線
;    : 明示的な行の終わり. 書かなくても行のbpl小節目以降は無視される.
c4, d4, e4, f4, g4, a4, b4 : ドからシのn分音符(数字はオクターブ). 空白によって区切られる
c4# : c4の半音上げ
_ : n分休符
c4+d4 : 和音
c4*n, _*n : 音価, 休符をn倍に増加
c4/n, _/n : 音価, 休符をn分の1に減少


- 音符に対する関数 -
n(): 括弧内のすべての音符をn分音符として解釈せよ
play(): mp3を再生
stacc(): スタッカート
ten(): テヌート
f(), ff(), fff(): フォルテ
p(), pp(), ppp(); ピアノ
arp(*+*): アルペジオ(和音を順に押す)
:タイ
:スラー
auf(): アウフタクト(不完全小節)

- 楽譜に対する関数 -
*+*: 合成(同じ番号の行は同時に演奏)
*;*; : 結合
mute(n) : n小節分の無音
octave_up(n), octave_down(n): 楽譜全体のオクターブをnだけ上下させる
```
