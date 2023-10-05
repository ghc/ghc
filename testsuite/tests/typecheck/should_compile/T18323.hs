{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

-- Tests SPECIAL CASE 2 in GHC.Tc.Gen.Bind.tcMonoBinds
-- See Note [Special case for non-recursive pattern bindings]
--
-- Doesn't have any useful effect until we have
-- ImpredicativeTypes, but does no harm either

module T18323 where

ids :: [forall a. a -> a]
ids = [id]

combine :: (forall a . [a] -> a)
        -> [forall a. a -> a]
        -> ((forall a . [a] -> a), [forall a. a -> a])
combine x y = (x,y)

-- This works
works = let t = combine head ids
        in (fst t) (snd t) True

-- But this does not typecheck, and it could
breaks = let (x,y) = combine head ids
         in x y True

-- And nor does this, but it could too
breaks2 = let (t) = combine head ids
          in (fst t) (snd t) True
