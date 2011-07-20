{-# LANGUAGE GADTs, ScopedTypeVariables #-}

-- Tests for scoped type variables and GADTs

module GADT where

data C x y where
     C :: a -> C a a

data D x y where
     D :: C b c -> D a c

-------  All these should be ok

-- Rejected!
g1 :: forall x y . C x y -> ()
-- C (..) :: C x y
-- Inside match on C, x=y
g1 (C (p :: y)) = ()

-- OK!
g2 :: forall x y . C x y -> ()
-- C (..) :: C x y
-- Inside match on C, x=y
g2 (C (p :: x)) = ()

-- Rejected!
g3 :: forall x y . D x y -> ()
-- D (..)  :: D x y
-- C (..)  :: C sk y
--	sk = y
-- p :: sk
g3 (D (C (p :: y))) = ()
