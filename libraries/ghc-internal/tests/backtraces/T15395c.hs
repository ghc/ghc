
import GHC.Internal.STM
import GHC.Internal.Control.Exception

main :: IO ()
main =
  atomically $ do
    throwSTM $ ErrorCall "STM error"
