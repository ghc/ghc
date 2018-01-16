{-# OPTIONS_GHC -fvectorise #-}

module Data.Array.Parallel.Prelude.Ordering
        ( Ordering
        , isLT, isEQ, isGT)
where
import Data.Array.Parallel.Prim                         ()      
import Data.Array.Parallel.Prelude.Base                 ()
import Data.Array.Parallel.PArray.PData
import Data.Array.Parallel.PArray.PData.Word8
import Data.Array.Parallel.PArray.PRepr
import Data.Array.Parallel.Lifted                       ((:->)(..))
import qualified Data.Array.Parallel.Lifted             as L
import qualified Data.Array.Parallel.PArray.Scalar      as SC


isLT, isEQ, isGT :: Ordering -> Bool

isLT _  = False
{-# NOINLINE  isLT #-}
{-# VECTORISE isLT = isLtPP #-}

isEQ _  = False
{-# NOINLINE  isEQ #-}
{-# VECTORISE isEQ = isEqPP #-}

isGT _  = False
{-# NOINLINE  isGT #-}
{-# VECTORISE isGT = isGtPP #-}


isLtPP, isEqPP, isGtPP :: Ordering :-> Bool
isLtPP  = L.closure1' (== LT) (isOrdering LT)
{-# INLINE isLtPP #-}
{-# NOVECTORISE isLtPP #-}

isEqPP  = L.closure1' (== EQ) (isOrdering EQ)
{-# INLINE isEqPP #-}
{-# NOVECTORISE isEqPP #-}

isGtPP  = L.closure1' (== GT) (isOrdering GT)
{-# INLINE isGtPP #-}
{-# NOVECTORISE isGtPP #-}

isOrdering :: Ordering -> PArray Ordering -> PArray Bool
isOrdering o (PArray n pdata)
 = case pdata of
    POrdering w8s
     -> SC.map (== (toPRepr o)) (PArray n $ PWord8 w8s)
{-# INLINE      isOrdering #-}
{-# NOVECTORISE isOrdering #-}
