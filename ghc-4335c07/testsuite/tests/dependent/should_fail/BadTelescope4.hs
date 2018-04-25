{-# LANGUAGE ExistentialQuantification, TypeInType #-}
module BadTelescope4 where

import Data.Proxy
import Data.Kind

data SameKind :: k -> k -> *

data Bad a (c :: Proxy b) (d :: Proxy a) (x :: SameKind b d)

data Borked a (b :: k) = forall (c :: k). B (Proxy c)
  -- this last one is OK. But there was a bug involving renaming
  -- that failed here, so the test case remains.
