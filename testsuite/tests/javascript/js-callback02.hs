import GHC.JS.Prim
import GHC.JS.Foreign.Callback

foreign import javascript "(() => { return 1; })"
  plus_one0 :: IO JSVal

foreign import javascript "((x) => { return x + 1; })"
  plus_one1 :: JSVal -> IO JSVal

foreign import javascript "((x,y) => { return x + y + 1; })"
  plus_one2 :: JSVal -> JSVal -> IO JSVal

foreign import javascript "((x,y,z) => { return x + y + z + 1; })"
  plus_one3 :: JSVal -> JSVal -> JSVal -> IO JSVal

foreign import javascript "((f) => { return f(); })"
  js_apply0 :: Callback (IO JSVal) -> IO JSVal

foreign import javascript "((f,x) => { return f(x); })"
  js_apply1 :: Callback (JSVal -> IO JSVal) -> JSVal -> IO JSVal

foreign import javascript "((f,x,y) => { return f(x,y); })"
  js_apply2 :: Callback (JSVal -> JSVal -> IO JSVal) -> JSVal -> JSVal -> IO JSVal

foreign import javascript "((f,x,y,z) => { return f(x,y,z); })"
  js_apply3 :: Callback (JSVal -> JSVal -> JSVal -> IO JSVal) -> JSVal -> JSVal -> JSVal -> IO JSVal

logJSInt :: JSVal -> IO ()
logJSInt = print . fromJSInt

main :: IO ()
main = do
  plusOne0 <- syncCallback'  plus_one0
  plusOne1 <- syncCallback1' plus_one1
  plusOne2 <- syncCallback2' plus_one2
  plusOne3 <- syncCallback3' plus_one3

  logJSInt =<< js_apply0 plusOne0
  logJSInt =<< js_apply1 plusOne1 (toJSInt 2)
  logJSInt =<< js_apply2 plusOne2 (toJSInt 2) (toJSInt 3)
  logJSInt =<< js_apply3 plusOne3 (toJSInt 2) (toJSInt 3) (toJSInt 4)
  
