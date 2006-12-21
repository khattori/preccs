//
// Timer：タイマーチャネルと選択実行のサンプル
//   --- 一定時間入力が無い場合にはタイムアウトする
//
proc Timer() = 
    stdin?msg -> stdout!("message: " ^ msg)
  | timer!3  -> stdout!"timeout\n"

proc Main() = Timer()