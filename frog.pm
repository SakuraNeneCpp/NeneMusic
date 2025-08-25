// frog.pm

bpm = 120;
meter = 4/4;

sheet piano1(piano, 0+-) = {
    [ c4 d4 e4 f4 ][ e4 d4 c4 _ ][ e4 f4 g4 a4 ][ g4 f4 e4 _ ]
    [ c4 _ c4 _ ][ c4 _ c4 _ ][ 8(c4 c4 d4 d4 e4 e4 f4 f4) ][ e4 d4 c4 _ ]
};

sheet piano2(piano, 0+-) = {
    [ _ _ _ _ ][ _ _ _ _]
    [ c3 d3 e3 f3 ][ e3 d3 c3 _ ][ e3 f3 g3 a3 ][ g3 f3 e3 _ ]
    [ c3 _ c3 _ ][ c3 _ c3 _ ][ 8(c3 c3 d3 d3 e3 e3 f3 f3) ][ e3 d3 c3 _ ]
};


sheet piano3(piano, 0+-) = {
    [ (c3+e3+g3)*2 (c3+e3+g3)*2 ]*4
    [ _ c3+e3+g3 _ c3+e3+g3 ]*2[ (c3+e3+g3)*2 (c3+e3+g3)*2 ][ c3+e3+g3 c3+e3+g3 c3+e3+g3 _ ]
};

music start{
    piano1+piano2;
}end