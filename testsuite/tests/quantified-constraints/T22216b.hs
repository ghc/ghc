{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module T22216b where

import Data.Functor.Classes

newtype T f a = MkT (f a)
  deriving ( Eq, Ord, Eq1
           , Ord1
           )
