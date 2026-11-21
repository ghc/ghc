{-# LANGUAGE CPP, DataKinds, GADTs, StandaloneKindSignatures, TypeApplications, TypeFamilies, TypeOperators #-}
module TuplePromotion (Selected, Refined, proof, refinementProof, pairIndex, reflected) where

import Data.Type.Equality ((:~:)(Refl))
import Type.Reflection (typeRep)
import TupleProvider
#ifdef WRONG_COERCION
import Data.Type.Coercion (Coercion(Coercion))
#endif

type Inspect :: Tuple Bool Bool -> Bool
type family Inspect x where
    Inspect ('Tup2 'True b) = b
    Inspect ('Tup2 'False _) = 'False

type Selected = Inspect ('Tup2 'True 'True)

proof :: Selected :~: 'True
proof = Refl

type family HasRefinement (x :: C (Int, Bool)) :: Bool where
    HasRefinement 'Refines = 'True

type Refined = HasRefinement ('Refines :: C (Int, Bool))

refinementProof :: Refined :~: 'True
refinementProof = Refl

pairIndex :: At ('Tup2 'True 'False :: Tuple Bool Bool)
pairIndex = At

emptyIndex :: At ('Tup0 :: Tuple)
emptyIndex = At

singleIndex :: At ('Tup1 'True :: Tuple Bool)
singleIndex = At

reflected :: String
reflected = show (typeRep @('Tup2 'True 'False :: Tuple Bool Bool))

#ifdef WRONG_KIND
wrongKind :: At ('Tup2 'True 'False :: Tuple Int Bool)
wrongKind = At
#endif

#ifdef WRONG_REFINEMENT
wrongRefinement :: At ('Refines :: C (Char, Bool))
wrongRefinement = At
#endif

#ifdef WRONG_PROOF
wrongProof :: 'Tup2 'True 'False :~: ('Tup2 'True 'True :: Tuple Bool Bool)
wrongProof = Refl
#endif

#ifdef WRONG_COERCION
wrongCoercion :: Coercion ('Tup2 'True 'False) ('Tup2 'True 'True :: Tuple Bool Bool)
wrongCoercion = Coercion
#endif

#ifdef UNSUPPORTED_CONSTRUCTORS
context :: At ('MkC 'True)
context = At

newtypeCon :: At ('MkN 'True)
newtypeCon = At
#endif
