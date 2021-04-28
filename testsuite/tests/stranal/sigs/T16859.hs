module T16859 where

import GHC.Types.Name (OccName)
import GHC.Types.SrcLoc
import GHC.Types.Unique

data NameSort = Internal | External

data Name = Name {
                n_sort :: NameSort,     -- What sort of name it is
                n_occ  :: !OccName,     -- Its occurrence name
                n_uniq :: {-# UNPACK #-} !Unique,
                n_loc  :: !SrcSpan      -- Definition site
            }

{-# NOINLINE mkInternalName #-}
mkInternalName :: Unique -> OccName -> SrcSpan -> Name
mkInternalName uniq occ loc = Name { n_uniq = uniq
                                   , n_sort = Internal
                                   , n_occ = occ
                                   , n_loc = loc }

-- | Should not unbox `x`.
foo :: Int -> Int -> (Int, Int)
foo x y = x `seq` (x, y)
{-# NOINLINE foo #-}

-- | Should unbox `x`.
bar :: Int -> Int -> (Int, Int)
bar x y = x `seq` (y, y)
{-# NOINLINE bar #-}

-- | Should not unbox `x`.
baz :: Int -> Int -> (Int -> Int) -> Int
baz x y f = x `seq` (f x + y)
{-# NOINLINE baz #-}

-- | Should unbox `p`.
buz :: (Int, Int) -> (Int, Int)
buz p@(x,y) = (y,x)
{-# NOINLINE buz #-}
