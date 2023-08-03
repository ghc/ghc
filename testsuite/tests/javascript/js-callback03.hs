import GHC.JS.Prim
import GHC.JS.Foreign.Callback

foreign import javascript "((f) => { globalF = f; })"
  setF :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript "((x) => { globalF(x); })"
  callF :: JSVal -> IO ()

foreign import javascript "((x,y) => { return x + y })"
  js_plus :: JSVal -> JSVal -> IO JSVal

foreign import javascript "((g) => { globalThis.globalG = g; })"
  setG :: Callback (JSVal -> JSVal -> IO JSVal) -> IO ()

foreign import javascript "((x,y) => { return globalG(x,y); })"
  callG :: JSVal -> JSVal -> IO JSVal

main :: IO ()
main = do
  -- Set functions globally on the JavaScript side, to be accessed in regular JavaScript code
  f <- syncCallback1 ThrowWouldBlock (\x -> if isNull x then putStrLn "isNull" else putStrLn "isNotNull")
  g <- syncCallback2' js_plus
  setF f
  setG g

  -- Do other things before using the globally-set functions
  putStrLn "test"

  -- Use the globally-set functions
  callF jsNull
  callF $ toJSString ""
  print . fromJSInt =<< callG (toJSInt 1) (toJSInt 2)
