{-# LANGUAGE TypeFamilies, NoMonoLocalBinds #-}
  -- The NoMonoLocalBinds is needed because of #21023
  -- If you are here after #21023 is fixed, consider removing the NoMonoLocalBinds.

module T20668 where

type family F a

inject :: a -> F a
inject = undefined

x = [5]

f y = [inject y, x]
