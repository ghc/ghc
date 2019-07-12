{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
module DerivingViaFail5 where

import Data.Functor.Identity

newtype Foo4 a = Foo4 a
deriving via (Identity b)
  instance Show (Foo4 a)
