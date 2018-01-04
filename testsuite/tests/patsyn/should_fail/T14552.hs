{-# Language RankNTypes, ViewPatterns, PatternSynonyms, TypeOperators, ScopedTypeVariables,
             KindSignatures, PolyKinds, DataKinds, TypeFamilies, TypeInType, GADTs #-}

module T14552 where

import Data.Kind
import Data.Proxy

data family Sing a

type a --> b = (a, b) -> Type

type family F (f::a --> b) (x::a) :: b

newtype Limit :: (k --> Type) -> Type where
  Limit :: (forall xx. Proxy xx -> F f xx) -> Limit f

data Exp :: [Type] -> Type -> Type where
  TLam :: (forall aa. Proxy aa -> Exp xs (F w aa))
        -> Exp xs (Limit w)

pattern FOO f <- TLam (($ Proxy) -> f)


{-
TLam :: forall (xs::[Type]) (b::Type).   -- Universal
         forall k (w :: k --> Type).      -- Existential
         (b ~ Limit w) =>
         => (forall (aa :: k). Proxy aa -> Exp xs (F w aa))
         -> Exp xs b

-}

{-
mfoo :: Exp xs b
     -> (forall k (w :: k --> Type).
         (b ~ Limit w)
         => Exp xs (F w aa)
         -> r)
     -> r
mfoo scrut k = case srcut of
                 TLam g -> k (g Proxy)
-}
