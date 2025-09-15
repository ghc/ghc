{-# LANGUAGE GADTs, MagicHash, ScopedTypeVariables, UnliftedDatatypes #-}

module LevPolyDataToTag where

import GHC.Exts
import Type.Reflection

type LevPoly :: forall (lev :: Levity) -> TYPE (BoxedRep lev)
data LevPoly lev = MkLevPoly Int

ok1 :: LevPoly Lifted -> Int#
ok1 = dataToTag#

ok2 :: LevPoly Unlifted -> Int#
ok2 = dataToTag#

ok3 :: DataToTag (LevPoly lev) => LevPoly lev -> Int#
ok3 = dataToTag#

ok4 :: forall lev. Typeable lev => LevPoly lev -> Int#
ok4 | Just HRefl <- typeRep @lev `eqTypeRep` typeRep @Lifted
                  = dataToTag#
    | Just HRefl <- typeRep @lev `eqTypeRep` typeRep @Unlifted
                  = dataToTag#
    | otherwise = error "unexpected levity"
