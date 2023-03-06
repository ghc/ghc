import GHC.JS.Prim

foreign import javascript "((xs) => { console.log(xs) })"
  log_js :: JSVal -> IO ()

foreign import javascript "((xs,i) => { return xs[i]; })"
  js_index :: JSVal -> JSVal -> JSVal

foreign import javascript "(() => { return ['t','e','s','t']; })"
  an_array :: JSVal

main :: IO ()
main = do
  log_js =<< toJSArray []
  log_js =<< toJSArray [jsNull, toJSInt 0, toJSString "", toJSInt 1, toJSString "test", toJSInt 2]
  xs <- toJSArray $ map toJSInt [1..10]
  log_js $ js_index xs (toJSInt 3)
  mapM_ log_js =<< fromJSArray an_array
