# pm言語の文法

## 基本
```
// frog.pm

bpm = 120;
meter = 4/4;

sheet piano1(piano, 0+-) = {
    [ c4 d4 e4 f4 ][ e4 d4 c4 _ ][ e4 d4 f4 g4 ][ f4 d4 e4 _ ]
    [ c4 _ c4 _ ][ c4 _ c4 _ ][ 8(c4 c4 d4 d4 e4 e4 f4 f4) ][ e4 d4 c4 _ ]
};


sheet piano2(piano, 0+-) = {
    [ (c3+e3+g3)*2 (c3+e3+g3)*2 ]*4
    [ _ c3+e3+g3 _ c3+e3+g3 ]*2[ (c3+e3+g3)*2 (c3+e3+g3)*2 ][ c3+e3+g3 c3+e3+g3 c3+e3+g3 _ ]
};

music start{
    piano1+piano2;
}end

```

|記号|意味|
|---|---|
|`//`|コメントアウト. ルールはc++と同じ.|
|`;`|行の終わり|
|`bpm`|beats par second|
|`meter`|拍子|
|`sheet`|譜面クラス|
|`piano1()`|譜面の名前|
|`piano`|楽器|
|`0+-`|調号. 0+-はハ長調/イ短調を表す. ト長調/ホ短調はf+, 変ロ長調/ト短調はbe-.|
|`[]`|小節|
|`c4`|ドの音. 他も同様|
|`_`|休符|
|`8(c4 c4 d4 d4 e4 e4 f4 f4)`|括弧内を一連の8分音符と解釈|
|`c3+e3+g3`|和音|
|`(c3+e3+g3)*2`|音価を2倍. 単音, 休符に対しても適用可能|
|`[]*4`|小節の繰り返し(この場合は4回)|
|`music start{}end`|エントリポイント|

## オーケストレーション
### 合成
```
music start{
    piano1+piano2;
}end
```
と書けば, piano1とpiano2が同時に演奏を開始する. 合成を構成しているもっとも長い譜面が演奏を終了した時点を, 合成された演奏の終了とする.
### 結合
```
music start{
    piano1;
    piano2;
}end
```
と書けば, piano1の演奏後にpiano2が演奏される.
### 繰り返し
```
music start{
    piano1*2;
}end
```
と書けば, piano1が2回繰り返される.
