import Control.Concurrent
import System.Posix.Signals

main :: IO ()
main = do
    _ <- flip (installHandler sig) Nothing $ Catch $
        putStrLn $ "Received my signal"
    raiseSignal sig
    threadDelay 100000
  where
    sig = sigUSR2
