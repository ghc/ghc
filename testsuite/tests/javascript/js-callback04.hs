import GHC.JS.Prim
import GHC.JS.Foreign.Callback

foreign import javascript "(() => { console.log('javascript'); })"
  js_log :: IO ()

foreign import javascript "((f) => { f(); })"
  js_apply0_ :: Callback (IO ()) -> IO ()

main :: IO ()
main = do
  logH <- syncCallback ThrowWouldBlock (putStrLn "haskell")
  logJ <- syncCallback ThrowWouldBlock js_log

  js_apply0_ logH
  js_apply0_ logJ
