import GHC.Internal.Control.Exception

main :: IO ()
main =
  throw $ ErrorCall "throw error"
