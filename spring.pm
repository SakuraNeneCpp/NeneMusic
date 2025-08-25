bpm = 120;
meter = 4/4;

sheet piano1(piano, fcgd+) = {
    [ c4 d4 e4 f4 ][ e4 d4 c4 _ ][ e4 f4 g4 a4 ][ g4 f4 e4 _ ]
    [ c4 _ c4 _ ][ c4 _ c4 _ ][ 8(c4 c4 d4 d4 e4 e4 f4 f4) ][ e4 d4 c4 _ ]
};

music start{
    piano1+piano2;
}end