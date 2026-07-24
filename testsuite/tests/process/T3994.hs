module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad
import System.IO
import System.IO.Error
import System.Process

main :: IO ()
main = do (_,Just hout,_,p) <- createProcess (proc "./T3994app" ["start", "10000"])
                            { std_out = CreatePipe, create_group = True }
          start <- hGetLine hout
          putStrLn start

          -- On FreeBSD if we're _really_ unlucky with scheduling, then the
          -- call to interruptProcessGroupOf can fail due to the process
          -- having already terminated (despite it running for at least 10ms!)
          -- If so, we just skip doing anything rather than fail the test,
          -- since this isn't our fault and is rare and scheduling dependent.
          -- See #27512 and https://reviews.freebsd.org/D58393
          handleJust (guard . isDoesNotExistError) (\_ -> return ()) $ do
            interruptProcessGroupOf p
            t <- myThreadId
                 -- timeout
            forkIO $ do
              threadDelay 5000000
              putStrLn "Interrupting a Running Process Failed"
              hFlush stdout
              killThread t
            waitForProcess p
            return ()

          putStrLn "end"
          return ()
