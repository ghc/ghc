module T26642 ( saveClobberedTemps ) where

import Prelude   ( IO, Bool(..), Int, (>>=), (==), return )
import Data.Word ( Word64 )

-------------------------------------------------------------------------------

data Word64Map a
  = Bin (Word64Map a) (Word64Map a)
  | Tip a
  | Nil

{-# NOINLINE myFoldr #-}
myFoldr :: (a -> b -> b) -> b -> Word64Map a -> b
myFoldr f = go
  where
    {-# NOINLINE go #-}
    go z' Nil       = z'
    go z' (Tip x)   = f x z'
    go z' (Bin l r) = go (go z' r) l

{-# NOINLINE nonDetFold #-}
nonDetFold :: (b -> elt -> IO b) -> b -> Word64Map elt -> IO b
nonDetFold f z0 xs = myFoldr c return xs z0
  where
    {-# NOINLINE c #-}
    c x k z = f z x >>= k

{-# NOINLINE myFalse #-}
myFalse :: Bool
myFalse = False

type RealReg = Int
data Loc = InReg RealReg | InMem

saveClobberedTemps :: forall instr. [RealReg] -> IO [instr]
saveClobberedTemps clobbered = nonDetFold maybe_spill [] Nil
  where
    {-# NOINLINE maybe_spill #-}
    maybe_spill :: [instr] -> Loc -> IO [instr]
    maybe_spill instrs !loc =
      case loc of
        InReg reg
          | myFalse
          -> return []
        _ -> return instrs
