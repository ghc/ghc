{-# LANGUAGE MagicHash, UnliftedDatatypes #-}

module LevPolyDataToTag where

import GHC.Exts

type LevPoly :: forall (lev :: Levity) -> TYPE (BoxedRep lev)
data LevPoly lev = MkLevPoly Int

notOK :: LevPoly lev -> Int#
-- should produce a nice 'no DataToTag instance' error
-- and not panic or fail core lint
notOK = dataToTag#
