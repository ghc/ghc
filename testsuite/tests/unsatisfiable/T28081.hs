{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GHC2024 #-}
module T28081 where

import GHC.TypeError

type family F a where
  F Int = Int

class F a ~ a => C a

instance Unsatisfiable ( Text "no C for Bool" ) => C Bool
