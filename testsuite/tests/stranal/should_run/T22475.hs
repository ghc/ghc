{-# OPTIONS_GHC -O -fforce-recomp #-}
{-# OPTIONS_GHC -fmax-worker-args=0 #-}
{-# LANGUAGE StrictData #-}

import Control.Monad
import Control.Exception
import GHC.Conc
import System.Timeout

twiceIO :: (Int -> IO ()) -> IO ()
twiceIO f = f 0 >> f 1
{-# NOINLINE twiceIO #-}

data Config
  = Cfg
  { a :: Integer
  , b :: Integer
  , c :: Integer
  , params :: (Maybe Int)
  , e :: Integer
  , f :: Integer
  , g :: Integer
  , h :: Integer
  , i :: Integer
  , j :: Integer
  , k :: Integer
  }

rnf :: Config -> ()
rnf (Cfg a b c _ e f g h i j k) = a + b + c + e + f + g + h + i + j + k `seq` ()

handshakeServer' :: Config -> Int -> IO ()
handshakeServer' cfg 0 = rnf cfg `seq` return ()
handshakeServer' _   _ = return ()
{-# NOINLINE handshakeServer' #-}

run :: Config -> Int -> IO ()
run conf n = do
  tv <- rnf conf `seq` params conf `seq` newTVarIO 0
  forever $ do
    acc <- rnf conf `seq` readTVarIO tv
    let conf' = conf{params=Just acc}
    forkIO $ twiceIO (\eta -> handshakeServer' conf' (eta+acc))
{-# NOINLINE run #-}

-- The crash should happen instantly, within the first 10ms. 100ms is a safe bet
main = timeout 100 $ run (Cfg 0 1 2 (Just 3) 4 5 6 7 8 9 10) 13
