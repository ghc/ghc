{-# LANGUAGE TypeFamilies, UndecidableInstances #-}

module T9554 where

import Data.Proxy

type family F a where
  F a = F (F a)

foo :: Proxy (F Bool) -> Proxy (F Int)
foo x = x

main = case foo Proxy of Proxy -> putStrLn "Made it!"
