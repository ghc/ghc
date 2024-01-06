{-# OPTIONS_GHC -O -fforce-recomp #-}
{-# OPTIONS_GHC -fmax-worker-args=0 #-}

data Config
  = Cfg
  { a      :: Integer
  , params :: !(Maybe Int)
  }

use :: Bool -> Config -> Int
use True cfg = a cfg `seq` 42
use _    _   = 0
{-# NOINLINE use #-}

run :: Config -> Int -> Int
run conf n =
  let !conf' = conf{params=Just n}
  in use True conf' + use False conf'
{-# NOINLINE run #-}

main = print $ run (Cfg 0 (Just 1)) 13
