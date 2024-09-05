{-# LANGUAGE LinearTypes, RequiredTypeArguments, DataKinds, TypeData #-}

module Modifiers where

import GHC.Types (Multiplicity(..))

%()
data D
  = %() D1 Int
  | %True D2 String
  | %False D3 { d3 %True :: () }
  | %Bool Int :* Bool

%()
data D' where
  %() D1' :: Int -> D'
  %True D2' :: String -> D'
  %False D3' :: { d3' %True :: () } -> D'
  %Bool (:*!) :: Int -> Bool -> D'

%()
newtype N = %True N { n %False :: Int }

%()
newtype N' where
  %True N' :: { n' %False :: Int } -> N'

%()
type data TD
  = %() TD1 Int
  | %True TD2 String

%()
type data TD' where
  %() TD1' :: Int -> TD'
  %True TD2' :: String -> TD'

%() %True
class C a

%()

%True;; %False;
instance C D

%()
default (Int)

%True
foreign import ccall "test" someImport :: Int

%True
foreign export ccall someImport :: Int

%()
sigDecl :: ()
sigDecl = undefined

patBind :: ()
%() patBind = ()

pat :: ()
(%1 pat) = () -- without parens, this would be a linear binding, forbidden here
  where x = \(%() y) -> y

f1 :: forall (m :: Bool) a b . a %m -> b
f1 = undefined

f2 :: forall (m :: Multiplicity) a b . a %m -> b
f2 = undefined

f3 :: a %(m :: Multiplicity) -> b %(m :: Multiplicity) -> c
f3 = undefined

f4 :: a %() -> b
f4 = undefined

-- These should probably fail, since they're all poly-kinded
f5a :: a %(Just m) -> b
f5a = undefined

f5b :: a %Just -> b
f5b = undefined

f5c :: a %Nothing -> b
f5c = undefined

-- It's kinda weird that this one works but f8 fails
f6 :: a %m -> b %(m :: Multiplicity) -> c
f6 = undefined

-- These are expected to fail, not tested in this test case:
-- f7 :: a %m -> b
-- f8 :: a %(m :: Multiplicity) -> b %m -> c

idt :: forall t -> t -> t
idt _ x = x

visForallApp :: Int -> Int
visForallApp = idt (Int %() -> Int) (+ 1)

-- Which type variables get exposed to modifiers?
--
-- We haven't made a concrete decision on what the results should be in these
-- situations. The commented out ones fail to compile, but that's not currently
-- tested.

%a
data FV1 a

data FV2 a = %a FV2

-- data FV2' a where
--   %a FV2' :: FV2'

-- data FV3 a = FV3 { fv3 %a :: () }

-- data FV3' a where
--   FV3' :: { fv3' %a :: () } -> FV3

-- This one seems especially sketchy.
data FV4 = %a forall a . FV4 a

data FV4' where
  %a FV4' :: a -> FV4'

-- data FV5 = FV5 { fv5 %a :: forall a . a }

-- %a
-- class FV6 a

-- %a
-- instance B a

-- %a
-- fv7 :: forall a . a
-- fv7 = undefined

-- %a
-- fv8 :: a
-- fv8 = undefined

-- fv9 :: forall a . a
-- %a fv9 = undefined

-- fv10 :: a
-- %a fv10 = undefined

-- And which concrete types?

%CT1
data CT1

%CT2Con
data CT2 = CT2Con

%CT2Con'
data CT2' where
  CT2Con' :: CT2'

%CT3
class CT3 a
