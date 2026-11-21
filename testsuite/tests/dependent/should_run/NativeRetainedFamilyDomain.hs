{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, FlexibleContexts, Modifiers, RankNTypes, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
module NativeRetainedFamilyDomain where

import ConstrainedElements (Collects(..), FieldKey, KnownIndex(..))
import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import GHC.Matchability (U)

#if defined(FORGOTTEN) || defined(VALID_FORGOTTEN)
type Ignore a = forall b. Show b => b -> String
#ifdef VALID_FORGOTTEN
forgotten :: Ignore (Elem [Bool])
#else
forgotten :: Ignore (Elem [Int])
#endif
forgotten x = show x
#elif defined(KNOWN_PREFIX)
data Uses (f :: Maybe Bool %U -> Maybe Bool) = Uses
prefix :: Uses Echo
prefix = Uses
#elif defined(MISSING_PREFIX)
data Uses (f :: Maybe Int %U -> Maybe Int) = Uses
prefix :: Uses Echo
prefix = Uses
#elif defined(MISSING_KIND)
missingKind :: Echo ('Nothing :: Maybe Int) :~: 'Nothing
missingKind = Refl
#elif defined(WRONG_TAG)
different :: foreach (a :: Bool) -> foreach (b :: Bool)
          -> Collects (FieldKey a) => Proxy '(a, Elem (FieldKey b))
different _ _ = Proxy
#elif defined(LATE_GIVEN)
earlier :: Proxy (Elem (FieldKey 'False))
        -> foreach (b :: Bool) -> Collects (FieldKey 'False) => Proxy b
earlier _ _ = Proxy
#elif defined(CALLBACK_ESCAPE)
escape :: forall a. (Collects a => Proxy (Elem a)) -> Proxy (Elem a)
escape _ = Proxy
#elif defined(CALLBACK_MISSING)
unqualified :: (forall c. Proxy c -> Elem c -> Elem c) -> Bool
unqualified _ = True
#else
missing :: foreach (tag :: Bool) -> Proxy (Elem (FieldKey tag))
missing _ = Proxy
#endif
