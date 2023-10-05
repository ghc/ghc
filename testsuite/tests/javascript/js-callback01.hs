import GHC.JS.Prim
import GHC.JS.Foreign.Callback
import Control.Concurrent

foreign import javascript "(() => { console.log('test'); })"
  js_log0 :: IO ()

foreign import javascript "((x) => { console.log(x); })"
  js_log1 :: JSVal -> IO ()

foreign import javascript "((x,y) => { console.log(x); console.log(y); })"
  js_log2 :: JSVal -> JSVal -> IO ()

foreign import javascript "((x,y,z) => { console.log(x); console.log(y); console.log(z); })"
  js_log3 :: JSVal -> JSVal -> JSVal -> IO ()

foreign import javascript "((f) => { f(); })"
  js_apply0_ :: Callback (IO ()) -> IO ()

foreign import javascript "((f,x) => { f(x); })"
  js_apply1_ :: Callback (JSVal -> IO ()) -> JSVal -> IO ()

foreign import javascript "((f,x,y) => { f(x,y); })"
  js_apply2_ :: Callback (JSVal -> JSVal -> IO ()) -> JSVal -> JSVal -> IO ()

foreign import javascript "((f,x,y,z) => { f(x,y,z); })"
  js_apply3_ :: Callback (JSVal -> JSVal -> JSVal -> IO ()) -> JSVal -> JSVal -> JSVal -> IO ()

main :: IO ()
main = do
  log0  <- syncCallback  ThrowWouldBlock js_log0
  log1  <- syncCallback1 ThrowWouldBlock js_log1
  log2  <- syncCallback2 ThrowWouldBlock js_log2
  log3  <- syncCallback3 ThrowWouldBlock js_log3

  js_apply0_ log0
  js_apply1_ log1  (toJSString "test1x")
  js_apply2_ log2  (toJSString "test2x") (toJSString "test2y")
  js_apply3_ log3  (toJSString "test3x") (toJSString "test3y") (toJSString "test3z")

  log0' <- asyncCallback  js_log0
  log1' <- asyncCallback1 js_log1
  log2' <- asyncCallback2 js_log2
  log3' <- asyncCallback3 js_log3

  js_apply0_ log0'
  js_apply1_ log1' (toJSString "test")
  js_apply2_ log2' (toJSString "test") (toJSString "test")
  js_apply3_ log3' (toJSString "test") (toJSString "test") (toJSString "test")

  threadDelay 1000000 -- Wait long enough for the async actions to complete
