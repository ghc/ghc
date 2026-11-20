{-# LANGUAGE GADTs, DataKinds, NoExperimentalDependentTypes, TypeApplications, TypeFamilies, PolyKinds, TypeOperators #-}
module NativeTypeCaseClient where

import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl))
import Data.Proxy (Proxy)
import NativeCase
import qualified NativeNestedCase as N

left :: F 'True :~: Int
left = Refl

right :: F 'False :~: Bool
right = Refl

field :: Field ('Just 'True) :~: '( 'True, 'True)
field = Refl

fixed :: Same 'False :~: Int
fixed = Refl

capture :: Capture Int ('Just 'True) :~: '(Int, 'True)
capture = Refl

given :: (a ~ 'True) => Proxy a -> Same a :~: Int
given _ = Refl

brace :: Braces 'False :~: Bool
brace = Refl

fixedKind :: Fixed Ordering 'LT :~: (Proxy 'LT, Proxy 'True)
fixedKind = Refl

captured :: Proxy a -> Captured a 'False :~: Maybe (PayloadKind a)
captured _ = Refl

retained :: Retain Ordering 'LT :~: Proxy 'LT
retained = Refl

shadow :: PatternShadow 'True :~: (Proxy 'False, Maybe Ordering)
shadow = Refl

scrutineeShadow :: ScrutineeShadow 'True :~: Int
scrutineeShadow = Refl

nestedScope :: NestedScope :~: Proxy 'False
nestedScope = Refl

paired :: Paired Ordering 'LT Bool 'True 'True :~: Proxy '( 'LT, 'True)
paired = Refl

inferredCapture :: InferredCapture 'LT 'True :~: Proxy 'LT
inferredCapture = Refl

contextKind :: ContextKind 'True :~: ContextKind 'False
contextKind = Refl

separateKinds :: SeparateKinds 'False :~: Bool
separateKinds = Refl

nested :: ( N.Pick ('Just ('Just 'True)) :~: Proxy 'True
          , N.Pick ('Just 'Nothing) :~: Int
          , N.SameFallback ('Just 'Nothing) :~: Int )
nested = (Refl, Refl, Refl)

wildcard :: N.IgnoreLeft ('N.Pair unknown ('Just 'True)) :~: Bool
wildcard = Refl

indexed :: ( N.IndexedPick @Bool ('Just ('N.BooleanTag 'False)) :~: Proxy 'False
           , N.IndexedPick @Ordering ('Just ('N.OrderingTag 'GT)) :~: Proxy 'GT )
indexed = (Refl, Refl)

type family Known (a :: Type) :: Type where
  Known Bool = Ordering

typeHeads :: ( N.TypeHead ('Just Bool) :~: Int, N.TypeHead ('Just Ordering) :~: Char
             , N.TypeHead ('Just [a]) :~: (), N.TypeHead 'Nothing :~: ()
             , N.UniformTypeHead ('Just Bool) :~: Int
             , N.Wrapped ('Just (Maybe a)) :~: Proxy a
             , N.TypeHead ('Just (Known Bool)) :~: Char
             , N.WrappedBool ('Just (Maybe Bool)) :~: Int
             , N.WrappedBool ('Just (Maybe Char)) :~: () )
typeHeads = (Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl)

transparentAliases ::
  ( N.PickFlipped ('Just (Maybe (Either Char Bool))) :~: Proxy Char
  , N.PickFlipped ('Just (Maybe (Either Bool Char))) :~: ()
  , N.DiscardConstant ('Just (Maybe Ordering)) :~: Proxy Ordering
  , N.CaptureFlipped 'LT ('Just (Either Char Bool)) :~: '(Proxy 'LT, Proxy Bool, Proxy Char)
  , N.WrittenFirst ('Just (Either unknown Char)) :~: Int )
transparentAliases = (Refl, Refl, Refl, Refl, Refl)

aliasMatch :: N.AliasFirst ('N.Pair ('Just 'True) ('Just 'False)) :~: Int
aliasMatch = Refl

aliasMiss :: N.AliasFirst ('N.Pair 'Nothing later) :~: Int
aliasMiss = Refl

mixedAliasMiss :: N.MixedAlias ('N.Pair ('Just 'False) later) :~: Int
mixedAliasMiss = Refl

knownIndex :: N.FamilyGatePick ('Just 'N.IndexedGate) :~: Bool
knownIndex = Refl

ordinaryIndex :: N.FamilyGatePick ('Just ('N.PlainGate 'True)) :~: Int
ordinaryIndex = Refl

writtenIndex :: N.WrittenGatePick ('Just 'N.IndexedGate) :~: Int
writtenIndex = Refl

apartIndex :: N.ApartGatePick ('Just ('N.PlainGate 'False)) :~: Bool
apartIndex = Refl
