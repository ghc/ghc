{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, ScopedTypeVariables, TypeApplications, TypeOperators #-}
module OpenMapProof (eachEmpty, eachCons, negateHead, redStops, ordinaryRedStops, firstRowHead) where

import qualified ClosedTableLibrary as C
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (FUN, U, Multiplicity(Many))
import OpenMapSource (Each, Heads, NegateAll, OrdinarySignals, Signals)
import qualified OpenMapWorker as W
import qualified SpineWorker as S

-- | An empty list needs no callback application.
eachEmpty :: forall (f :: FUN @U 'Many Bool Bool).
             Proxy f -> Each f '[] :~: '[]
eachEmpty _ = Refl

-- | The result retains both the callback application and the recursive call.
eachCons :: forall (f :: FUN @U 'Many Bool Bool) (x :: Bool) (xs :: [Bool]).
            Proxy f -> Proxy x -> Proxy xs
            -> Each f (x ': xs) :~: (f x ': Each f xs)
eachCons _ _ _ = Refl

-- | Complement the head without supplying the rest of the list.
negateHead :: forall (xs :: [Bool]). Proxy xs
           -> NegateAll ('True ': xs) :~: ('False ': NegateAll xs)
negateHead _ = Refl

-- | A red light contributes one stop instruction for any remaining lights.
redStops :: forall (xs :: [C.Lamp]). Proxy xs
         -> Signals ('C.Red ': xs) :~: ('C.Stop ': Signals xs)
redStops _ = Refl

-- | Observe the ordinary callback's first result with a rigid list tail.
ordinaryRedStops :: forall (xs :: [W.Lamp]). Proxy xs
                 -> OrdinarySignals ('W.Red ': xs) :~: ('W.Stop ': OrdinarySignals xs)
ordinaryRedStops _ = Refl

-- | The head of the first row determines the first result.
firstRowHead :: forall (x :: Bool) (tail :: S.Spine Bool) (rows :: [S.Spine Bool]).
                Proxy x -> Proxy tail -> Proxy rows
             -> Heads ('S.More x tail ': rows) :~: ('Just x ': Heads rows)
firstRowHead _ _ _ = Refl
