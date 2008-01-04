//
// Timer：タイマーチャネルと選択実行のサンプル
//   --- 一定時間入力が無い場合にはタイムアウトする
//
proc Timer() = 
      stdin?msg -> stdout!"mesg:"^msg
    | timer!(1,0) -> stdout!"timeout\n"; Timer()

proc Main() = Timer()
