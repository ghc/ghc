{-# LANGUAGE MonoLocalBinds, ScopedTypeVariables, RankNTypes #-}

module T18802 where

-- Check that we handle higher-rank types properly.
data R b = MkR { f :: (forall a. a -> a) -> (Int,b), c :: Int }

foo r = r { f = \ k -> (k 3, k 'x') }


-- Check that we handle existentials properly.
class C a where

data D
  = forall ty. C ty
  => MkD { fld1 :: !ty
         , fld2 :: Bool
         }

g :: D -> D
g d = d { fld2 = False }
