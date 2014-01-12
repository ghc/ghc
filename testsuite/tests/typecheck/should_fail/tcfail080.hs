{-# LANGUAGE MultiParamTypeClasses #-}

-- !!! Multi-param type classes test: ambiguity bug

-- GHC actually accepts this program because
--	q :: Collection c a => a -> Bool
-- and there is no a priori reason to suppose that
-- q would be ambiguous in every context. For example,
-- it could be fine if we had
--	instance c Int where ...
-- Of course, it'd be hard to fill in the "..." in this particular
-- case, but that relies on observations about the form of the types
-- of the class methods, surely beyond what a compiler should do.
--
-- Still, the type is ambiguous because there's nothing to fix 'c'


module ShouldFail where

class Collection c a where
    empty :: c a
    add :: a -> c a -> c a
    isempty :: c a -> Bool

singleton x = add x empty

q x = isempty (singleton x)


