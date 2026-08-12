{-# LANGUAGE ScopedTypeVariables #-}

-- An async exception delivered while a catchSTM handler runs must abort the
-- transaction, not be swallowed by a restart of the invalidated one.

import Control.Concurrent.MVar
import Control.Exception
import GHC.Conc

waitParked :: ThreadId -> IO ()
waitParked t = do
  s <- threadStatus t
  case s of
    ThreadBlocked BlockedOnMVar -> pure ()
    _                           -> threadDelay 1000 >> waitParked t

main :: IO ()
main = do
  tv     <- newTVarIO (0 :: Int)
  park   <- newEmptyMVar
  result <- newEmptyMVar
  t <- forkIO $ do
    r <- try $ atomically $ do
      v <- readTVar tv
      catchSTM (throwSTM (ErrorCall "boom"))
               (\(_ :: SomeException) ->
                  if v == 0
                    then do unsafeIOToSTM (takeMVar park)
                            pure "handler resumed"
                    else pure "transaction restarted, exception dropped")
    putMVar result (r :: Either SomeException String)
  -- parked in the handler, so t cannot revalidate its trec before delivery
  waitParked t
  atomically (writeTVar tv 1)
  killThread t
  r <- takeMVar result
  putStrLn $ case r of
    Left e | Just ThreadKilled <- fromException e -> "T27657b: killThread delivered"
           | otherwise -> "T27657b: unexpected exception: " ++ displayException e
    Right s -> "T27657b: FAILED, " ++ s
