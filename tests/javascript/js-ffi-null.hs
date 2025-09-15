import GHC.JS.Prim

foreign import javascript "((x) => { console.log(x); })"
  log_null :: JSVal -> IO ()

main :: IO ()
main = log_null jsNull
