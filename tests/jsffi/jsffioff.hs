import GHC.TopHandler
import GHC.Wasm.Prim

foreign export ccall "main"
  main :: IO ()

main :: IO ()
main = do
  print isJSFFIUsed
  flushStdHandles
