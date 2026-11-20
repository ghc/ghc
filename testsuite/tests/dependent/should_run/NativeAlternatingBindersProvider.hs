{-# LANGUAGE DataKinds, ExperimentalDependentTypes, PolyKinds, ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions, TypeFamilies #-}
module NativeAlternatingBindersProvider
  ( Reading(..), Marker(..), keepAfter, keepExplicit, keepImplicit
  , forwardKind, sameSpelling, twice, plainFirst, showAfter ) where
import Data.Kind (Type)
import Data.Proxy (Proxy(..))

data Reading (b :: Bool) (m :: Maybe a) = Reading Int

{-# NOINLINE keepAfter #-}
keepAfter :: foreach (b :: Bool) -> forall (a :: Type).
             foreach (m :: Maybe a) -> Reading b m -> Reading b m
keepAfter _ _ x = x

{-# INLINE keepExplicit #-}
keepExplicit :: foreach (b :: Bool) -> forall (a :: Type).
                foreach (m :: Maybe a) -> Reading b m -> Reading b m
keepExplicit _ @a _ x = case Proxy @a of Proxy -> x

keepImplicit :: foreach (b :: Bool) -> forall {a :: Type}.
                foreach (m :: Maybe a) -> Reading b m -> Reading b m
keepImplicit _ _ x = x

data Marker (k :: Type) (a :: k) = Marker

type family K (b :: Bool) :: Type where
  K True = Bool
  K False = Ordering

keepKind :: foreach (b :: Bool) -> forall (a :: K b). Marker (K b) a -> Marker (K b) a
keepKind _ x = x

forwardKind :: foreach (b :: Bool) -> forall (a :: K b). Marker (K b) a -> Marker (K b) a
forwardKind b @a x = keepKind b @a x

sameSpelling :: foreach (b :: Bool) -> forall (b :: Ordering). Proxy b -> Proxy b
sameSpelling _ x = x

twice :: foreach (b :: Bool) -> forall (a :: Type). foreach (m :: Maybe a) ->
         forall (c :: Type). foreach (n :: Maybe c) ->
         Reading b m -> (Reading b m, Proxy n)
twice _ _ _ x = (x, Proxy)

plainFirst :: () -> forall (a :: Type). foreach (m :: Maybe a) ->
              Reading True m -> Reading True m
plainFirst _ _ x = x

showAfter :: foreach (b :: Bool) -> forall (a :: Type). Show a =>
             foreach (m :: Maybe a) -> a -> Reading b m -> (String, Int)
showAfter _ _ value (Reading n) = (show value, n)
