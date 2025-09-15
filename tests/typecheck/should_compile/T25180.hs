{-# LANGUAGE MagicHash, UnboxedTuples, UnboxedSums #-}

module T25180 where

import Prelude.Experimental
import Data.Tuple
import GHC.Types

-- Valid hole fits include
--   () :: () (bound at <wired into compiler>)
tup0 :: Unit
tup0 = _

-- Valid hole fits include
--   MkSolo :: forall a. a -> Solo a
--     with MkSolo @a
--     (imported from ‘Data.Tuple’ and originally defined in ‘GHC.Tuple’)
tup1 :: a -> Solo a
tup1 = _

-- Valid hole fits include
--   (,) :: forall a b. a -> b -> (a, b)
--     with (,) @a @b
--     (bound at <wired into compiler>)
tup2 :: a -> b -> Tuple2 a b
tup2 = _

-- Valid hole fits include
--   (,,) :: forall a b c. a -> b -> c -> (a, b, c)
--     with (,,) @a @b @c
--     (bound at <wired into compiler>)
tup3 :: a -> b -> c -> Tuple3 a b c
tup3 = _

-- Valid hole fits include
--   (##) :: (# #) (bound at <wired into compiler>)
utup0 :: () -> Unit#
utup0 _ = _

-- Valid hole fits include
--   MkSolo# :: forall a. a -> (# a #)
--     with MkSolo# @LiftedRep @a
--     (imported from ‘GHC.Types’)
utup1 :: a -> Solo# a
utup1 = _

-- Valid hole fits include
--   (#,#) :: forall a b. a -> b -> (# a, b #)
--     with (#,#) @LiftedRep @LiftedRep @a @b
--     (bound at <wired into compiler>)
utup2 :: a -> b -> (# a, b #)
utup2 = _

-- Valid hole fits include
--   (#,,#) :: forall a b c. a -> b -> c -> (# a, b, c #)
--     with (#,,#) @LiftedRep @LiftedRep @LiftedRep @a @b @c
--     (bound at <wired into compiler>)
utup3 :: a -> b -> c -> (# a, b, c #)
utup3 = _

-- Valid hole fits include
--   (# _| #) :: forall a b. a -> (# a | b #)
--     with (# _| #) @LiftedRep @LiftedRep @a @b
--     (bound at <wired into compiler>)
usum2_1 :: a -> (# a | b #)
usum2_1 = _

-- Valid hole fits include
--   (# |_ #) :: forall a b. b -> (# a | b #)
--     with (# |_ #) @LiftedRep @LiftedRep @a @b
--     (bound at <wired into compiler>)
usum2_2 :: b -> (# a | b #)
usum2_2 = _

-- Valid hole fits include
--   (# _| | #) :: forall a b c. a -> (# a | b | c #)
--     with (# _| | #) @LiftedRep @LiftedRep @LiftedRep @a @b @c
--     (bound at <wired into compiler>)
usum3_1 :: a -> (# a | b | c #)
usum3_1 = _

-- Valid hole fits include
--   (# |_| #) :: forall a b c. b -> (# a | b | c #)
--     with (# |_| #) @LiftedRep @LiftedRep @LiftedRep @a @b @c
--     (bound at <wired into compiler>)
usum3_2 :: b -> (# a | b | c #)
usum3_2 = _

-- Valid hole fits include
--   (# | |_ #) :: forall a b c. c -> (# a | b | c #)
--     with (# | |_ #) @LiftedRep @LiftedRep @LiftedRep @a @b @c
--     (bound at <wired into compiler>)
usum3_3 :: c -> (# a | b | c #)
usum3_3 = _