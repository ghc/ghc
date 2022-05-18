{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}


module T21577 (bar) where

import GHC.Generics
import GHC.Exts (Constraint)
import Data.Coerce (coerce)
import Data.Kind (Type)

data A = A ()
  deriving Generic

data B = B A

-- This avoids callstack stuff in core dumps
undefined' :: a
undefined' = undefined'

ba :: Optic A_Setter B B A A
ba = castOptic $ lens (\(B a) -> a) (\_ -> B)

aunit :: Optic A_Setter A A () ()
aunit = case foo (Market id Right) of
  Market _ _ -> Optic undefined'
 where
   foo :: Profunctor p => p i () () -> p i A A
   foo = dimap from to . dimap coerce coerce

bar :: Monad m => m [B]
bar = do
      _ <- pure []
      pure $ over (mapped %% ba) inner []
    where
      -- NB: inlining inner hides the bug
      inner = over aunit id

class Profunctor p where
  dimap :: (a -> b) -> (c -> d) -> p i b c -> p i a d

class Mapping p where
  -- Uncommenting this line avoids the OOM
  --wibble :: p i a b -> ()
  roam :: ((a -> b) -> s -> t) -> p i a b -> p i s t

first'  :: Mapping p => p i a b -> p i (a, c) (b, c)
first' = roam (\f (a, x) -> (f a, x))

-----
-- Some minimised code originating from optics-core and indexed-profunctors follows

newtype Optic (k :: OpticKind) s t a b
  = Optic (forall p i. (Profunctor p, Constraints k p) => p i a b -> p i s t)

castOptic :: forall s t a b.  Optic A_Lens  s t a b -> Optic A_Setter s t a b
castOptic (Optic o) = Optic o

infixl 9 %%
(%%) :: forall s t u v a b.  Optic A_Setter s t u v -> Optic A_Setter u v a b -> Optic A_Setter s t a b
Optic o %% Optic o' = Optic (o . o')

over
  :: forall s t a b.  Optic A_Setter s t a b -> (a -> b) -> s -> t
over (Optic o) f = runFunArrow $ o (FunArrow f)

lens :: (s -> a) -> (s -> b -> t) -> Optic A_Lens s t a b
lens get set = Optic $ dimap (\s -> (get s, s)) (\(b, s) -> set s b) . first'

mapped :: Functor f => Optic A_Setter (f a) (f b) a b
mapped = Optic (roam fmap)

type OpticKind = Type

data A_Lens :: OpticKind
data A_Setter :: OpticKind

-- Changing this into a synonym hides the OOM bug
type family Constraints (k :: OpticKind) (p :: Type -> Type -> Type -> Type) :: Constraint where
  Constraints A_Lens   p = Mapping p
  Constraints A_Setter p = Mapping p

data Market a b i s t = Market (b -> t) (s -> Either t a)

instance Profunctor (Market a b) where
  dimap f g (Market bt seta) = Market (g . bt) (either (Left . g) Right . seta . f)

-- NB: changing this to data hides the OOM bug
newtype FunArrow i a b = FunArrow { runFunArrow :: a -> b }

instance Profunctor FunArrow where
  dimap f g (FunArrow k) = FunArrow (g . k . f)

instance Mapping FunArrow where
  --wibble _ = ()
  roam  f (FunArrow k) = FunArrow $ f k
