{-# language DataKinds #-}
{-# language DerivingStrategies #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language KindSignatures #-}
{-# language MagicHash #-}
{-# language PolyKinds #-}
{-# language UnliftedNewtypes #-}

module UnliftedNewtypesGnd where

import GHC.Exts (Int#,TYPE,RuntimeRep(IntRep),isTrue#,(==#))

class LevityEq (a :: TYPE 'IntRep) where
  levityEq :: a -> a -> Bool

instance LevityEq Int# where
  levityEq x y = isTrue# (x ==# y)

newtype Foo = Foo Int#
  deriving newtype (LevityEq)
