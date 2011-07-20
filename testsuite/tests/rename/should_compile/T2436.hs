{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fwarn-duplicate-exports #-}

module T2436( C(..), T(..), module T2436a, S(..) ) where

import T2436a

class C a where
  data T a

instance C Int where
  data T Int = TInt Int

data instance S Int = SInt