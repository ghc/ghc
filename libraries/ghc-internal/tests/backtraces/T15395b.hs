import GHC.Internal.Control.Exception

main :: IO ()
main =
  throwIO $ ErrorCall "throwIO error"
