{-# LANGUAGE MagicHash, UnboxedTuples #-}
import GHC.Prim
import GHC.Types

main = do
    let local = ()
    let null = 0## :: Word#
    let triple = (# local, null, null #)
    IO (\s -> case mkWeakNoFinalizer# triple () s of (# s, r #) -> (# s, () #))
