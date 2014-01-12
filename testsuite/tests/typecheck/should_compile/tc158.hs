{-# LANGUAGE RankNTypes #-}

-- Types should be checked for well-formedness only after
-- expanding type synonyms.  GHC 5.03 fails this

module ShouldCompile where

type All u = forall x. x->u
type All' u = u -> All u

all1 :: All u -> (u -> All u) -> All u
all1 = undefined
