{-# LANGUAGE MagicHash #-}

-- It used to be that (->) would have a very restrictive kind when used in
-- prefix position. This restriction was lifted after the levity polymorphism
-- work in 2016.

module ShouldSucceed where
import GHC.Base

type T = (->) Int#

-- Here's the old comment from TypeRep:
--
-- funTyCon = mkFunTyCon funTyConName
--              (mkArrowKinds [liftedTypeKind, liftedTypeKind]
--                              liftedTypeKind)
-- You might think that (->) should have type (? -> ? -> *), and you'd be right
-- But if we do that we get kind errors when saying
--      instance Control.Arrow (->)
-- because the expected kind is (*->*->*).  The trouble is that the
-- expected/actual stuff in the unifier does not go contra-variant, whereas
-- the kind sub-typing does.  Sigh.  It really only matters if you use (->) in
-- a prefix way, thus:  (->) Int# Int#.  And this is unusual.
