import GHC.JS.Prim

foreign import javascript "((x) => { console.log(x); })"
  log_js_string :: JSVal -> IO ()

foreign import javascript "(() => { return 'a string'; })"
  a_string :: JSVal

main :: IO ()
main = do
  log_js_string (toJSString "test")
  putStrLn (fromJSString a_string)
  putStrLn (fromJSString $ toJSString "test")
