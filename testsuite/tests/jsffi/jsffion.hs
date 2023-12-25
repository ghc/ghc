import GHC.Wasm.Prim

foreign export javascript "main"
  main :: IO ()

main :: IO ()
main = print isJSFFIUsed
