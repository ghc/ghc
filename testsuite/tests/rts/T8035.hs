{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception
import Control.Monad
import GHC.Conc

main = join $ atomically $ do
  catchSTM
    (throwSTM ThreadKilled `orElse` return (putStrLn "wtf"))
    (\(e::SomeException) -> return (putStrLn "ok"))
