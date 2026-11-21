{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module ConcreteGuardedListProof (showConcreteGuardedLists) where

import ConcreteGuardedList (Signal(..), dropBool, dropMarkers, isMarker)
import ConcreteGuardedListSource (DropBool, DropMarkers, IsMarker, Negated)
import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))

-- | Return the input tail when negation accepts the Boolean head.
removeFalse :: Proxy xs -> DropBool Negated ('False ': xs) :~: xs
removeFalse _ = Refl

-- | Return the original cons when the signal predicate rejects an entry.
preserveEntry :: Proxy xs -> DropMarkers IsMarker ('Entry ': xs) :~: ('Entry ': xs)
preserveEntry _ = Refl

-- | Observe each equality and the first output element of each function.
showConcreteGuardedLists :: String
showConcreteGuardedLists =
    show ( removeFalse (Proxy :: Proxy '[]), preserveEntry (Proxy :: Proxy '[])
         , take 1 (dropBool not (False : True : error "dropBool forced a later tail"))
         , take 1 (dropMarkers isMarker (Marker : Entry : error "dropMarkers forced the rejected tail")) )
