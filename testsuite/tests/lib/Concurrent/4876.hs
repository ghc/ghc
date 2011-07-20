import System.Random
import Control.Concurrent.SampleVar
import Control.Concurrent
import Control.Monad

produce, consume :: SampleVar Int -> IO ()
produce svar = do
   b <- isEmptySampleVar svar
   if b then writeSampleVar svar 3 else return ()

consume svar = readSampleVar svar >>= print

main = do
   svar <- newEmptySampleVar
   m <- newEmptyMVar
   forkIO $ consume svar >> putMVar m ()
   threadDelay 100000     -- 100 ms
   produce svar
   takeMVar m -- deadlocked before the fix in #4876
