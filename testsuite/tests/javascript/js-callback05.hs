import GHC.JS.Prim
import GHC.JS.Foreign.Callback
import System.IO

foreign import javascript "((f) => { f(); })"
  js_apply0_ :: Callback (IO ()) -> IO ()

main :: IO ()
main = do
  log <- syncCallback ThrowWouldBlock (putStrLn "test" >> hFlush stdout)
  js_apply0_ log
  js_apply0_ log
  
  log <- syncCallback ThrowWouldBlock (putStrLn "test1" >> hFlush stdout)
  log <- syncCallback ThrowWouldBlock (putStrLn "test2" >> hFlush stdout)
  log <- syncCallback ThrowWouldBlock (putStrLn "test3" >> hFlush stdout)
  js_apply0_ log1
  js_apply0_ log2
  js_apply0_ log3
