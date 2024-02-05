{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}

module DsSpecPragmas where

import Control.Monad.ST
  ( ST )
import Data.Kind
  ( Constraint, Type )
import Data.Proxy
  ( Proxy(..) )

-- Some specialise pragmas that are tricky to generate the correct RULE for.

--------------------------------------------------------------------------------

f1 :: ( Num a, Eq b ) => a -> b -> Int
f1 _ _ = 111
-- Make sure we don't generate a rule with an LHS of the form
--
--  forall @e (d :: Eq e). f1 @[e] ($fEqList d) = ...
--
--     but rather
--
--  forall @e (d :: Eq [e]). f1 @[e] d = ...
{-# SPECIALISE f1 :: Eq [e] => Word -> [e] -> Int #-}

f1_qc :: ( forall x. Eq x => Eq ( f x ), Eq a, Num b ) => Proxy f -> a -> b -> Char
f1_qc _ _ _ = 'q'

-- Like 'f1', but with a local instance (quantified constraint).
{-# SPECIALISE f1_qc :: ( forall y. Eq y => Eq ( g y ), Eq ( g e ) ) => Proxy g -> g e -> Word -> Char #-}

--------------------------------------------------------------------------------

f2 :: ( Eq a, Eq b, Num c ) => a -> b -> c -> Int
f2 _ _ _ = 2

-- Make sure the rule LHS is of the form
--
--   f2 @c @c d1 d2     and not    f2 @c @c d d
{-# SPECIALISE f2 :: Eq c => c -> c -> Word -> Int #-}

--------------------------------------------------------------------------------

f3 :: ( Eq a, forall x. Eq x => Eq ( f x ) ) => f a -> Bool
f3 z = z == z

-- Discharge the quantified constraint but keep the 'Eq' constraint
{-# SPECIALISE f3 :: Eq c => [ c ] -> Bool #-}

-- Discharge the 'Eq' constraint but keep the quantified constraint
{-# SPECIALISE f3 :: ( forall y. Eq y => Eq ( g y ) ) => g Int -> Bool #-}

--------------------------------------------------------------------------------

f4 :: (Eq a, Monad m) => a -> m a
f4 = return

-- Check we can deal with locally quantified variables in constraints,
-- in this case 'Monad (ST s)'.
{-# SPECIALISE f4 :: forall s b. Eq b => b -> ST s b #-}

f4_qc :: (Eq a, forall m. Monad m => Monad (t m)) => t m a -> ()
f4_qc _ = ()

-- Like 'f4' but with a quantified constraint.
{-# SPECIALISE f4_qc :: forall r n b. (forall m. Monad m => Monad (r m)) => r n Int -> () #-}

--------------------------------------------------------------------------------

type family T a where
  T Int = Word
data D a = D a (T a)
deriving stock instance (Eq a, Eq (T a)) => Eq (D a)

f5 :: Eq a => a -> Bool
f5 x = x == x

-- Discharge a dictionary constraint using a top-level instance
-- whose context contains a type family application.
{-# SPECIALISE f5 :: D Int -> Bool #-}


f5_qc :: ( Eq a, Eq ( T a ), forall x. ( Eq x, Eq ( T x ) ) => Eq ( f x ) ) => f a -> Bool
f5_qc z = z == z

-- Discharge a quantified constraint using a top-level instance
-- whose context includes a type family application.
{-# SPECIALISE f5_qc :: D Int -> Bool #-}

-- Quantify over this same quantified constraint, but discharge the
-- other dictionary constraints.
{-# SPECIALISE f5_qc :: ( forall y. ( Eq y, Eq ( T y ) ) => Eq ( g y ) ) => g Int -> Bool #-}

--------------------------------------------------------------------------------

f6 :: ( Eq a, Ord b, Num c ) => a -> b -> c -> Char
f6 _ _ _ = 'c'

-- Check that we do perform simplification among Wanteds that we quantify over.
{-# SPECIALISE f6 :: Ord c => c -> c -> Word -> Char #-}


f6_qc :: ( forall x. Eq x => Eq ( f x ), forall y. Eq y => Ord ( g y ), Num c ) => Proxy f -> Proxy g -> c -> Char
f6_qc _ _ _ = 'd'

-- Like 'f6', but with quantified constraints.
{-# SPECIALISE f6_qc :: ( forall z. Eq z => Ord ( h z ) ) => Proxy h -> Proxy h -> Word -> Char #-}

--------------------------------------------------------------------------------

type Cls :: Type -> Constraint
class Cls a where {}

type TF :: Type -> Type
type family TF a = r | r -> a where
  TF Int = Bool

f7 :: ( Cls ( TF a ), Eq a ) => a -> a
f7 x = x
{-# SPECIALISE f7 :: Cls Bool => Int -> Int #-}

--------------------------------------------------------------------------------
-- An example taken from the Cabal library (buildInfoFieldGrammar).

type F :: ( Type -> Constraint ) -> ( Type -> Type ) -> Constraint
class F c g | g -> c where {}

type C :: Type -> Constraint
class C a where
instance C (a, b) where {}

type G :: Type -> Type
data G a where

instance F C G

qcfd :: ( F c g, forall a b. c (a, b) ) => g ()
qcfd = let x = x in x
{-# SPECIALISE qcfd :: G () #-}

--------------------------------------------------------------------------------
