import GHC.JS.Prim

foreign import javascript "((x) => { console.log(x); })"
  log_js_int :: JSVal -> IO ()

foreign import javascript "(() => { return 3; })"
  an_int :: JSVal

main :: IO ()
main = do
  log_js_int (toJSInt 0)
  log_js_int (toJSInt 1)
  log_js_int (toJSInt 2)
  log_js_int an_int
  print (fromJSInt an_int)
  print (fromJSInt $ toJSInt 4)
