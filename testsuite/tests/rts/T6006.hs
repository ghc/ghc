module F where
import System.Environment

foreign export ccall f :: IO ()
f = do
  getProgName >>= print
  getArgs >>= print
