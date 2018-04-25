{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Monad
import Control.Monad.Fix
import Data.IORef
import Prelude hiding (until)

data Phase a = Ready a | Updated a a

delay :: IO Int               -- ^ the signal to delay
      -> IO (IO (), IO (), IO Int)   -- ^ the delayed signal
delay s = do
  ref <- newIORef (Ready 0)
  let
      upd = do v <- readIORef ref
               case v of
                 Ready x -> do putStrLn "upd: Ready"; x' <- s; putStrLn (show x'); writeIORef ref (Updated x' x)
                 _       -> return ()

      fin = do v <- readIORef ref
               case v of
                 Updated x _ -> do putStrLn "fin: Updated"; writeIORef ref $! Ready x
                 _           -> error "Signal not updated!"

      sig = do v <- readIORef ref
               case v of
                 Ready x     -> do putStrLn "sig: Ready"; return x
                 Updated _ x -> do putStrLn "sig: Updated"; return x

  return (upd,fin,sig)

main = do
    (upd,fin,_) <- mfix $ \ ~(_,_,sig) -> delay (fmap (1+) sig)
    upd
    fin
    upd
