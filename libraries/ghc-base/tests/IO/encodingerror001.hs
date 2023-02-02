import System.IO
import System.IO.Error
import Text.Printf
import Control.Monad

main = do
  hSetEncoding stdout latin1
  forM [NoBuffering,
        LineBuffering,
        BlockBuffering Nothing,
        BlockBuffering (Just 3),
        BlockBuffering (Just 9),
        BlockBuffering (Just 32)] $ \b -> do
     hSetBuffering stdout b
     checkedPutStr "test 1\n"
     checkedPutStr "ě\n" -- nothing gets written
     checkedPutStr "test 2\n"
     checkedPutStr "Hέllo\n" -- we should write at least the 'H'
     checkedPutStr "test 3\n"
     checkedPutStr "Hello αβγ\n" -- we should write at least the "Hello "

checkedPutStr str = do
  r <- tryIOError $ putStr str
  case r of
    Right _ -> return ()
    Left  e -> printf "Caught %s while trying to write %s\n"
                  (show e) (show str)
