{-# LANGUAGE DataKinds, ExperimentalDependentTypes, Modifiers #-}
{-# LANGUAGE ScopedTypeVariables, StandaloneKindSignatures, TypeApplications #-}
{-# LANGUAGE TypeFamilies, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module PreludeFmapEitherProof (MappedEither, fmapLeft, fmapRight, example) where

import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (U)
import Prelude (Bool(..), Either(..))
import qualified Prelude as P

-- | Name the checked application with an abstract left type.
type MappedEither :: forall e a b. (a %U -> b) -> Either e a -> Either e b
type MappedEither f x = P.fmap f x

-- | The Either instance keeps the original Left field.
fmapLeft :: forall e a b (f :: a %U -> b) (x :: e).
            Proxy f -> Proxy x -> P.fmap f ('Left x :: Either e a) :~: ('Left x :: Either e b)
fmapLeft _ _ = Refl

-- | The Either instance applies the callback to the original Right field.
fmapRight :: forall e a b (f :: a %U -> b) (x :: a).
             Proxy f -> Proxy x -> P.fmap f ('Right x :: Either e a) :~: ('Right (f x) :: Either e b)
fmapRight _ _ = Refl

type family Flip (x :: Bool) :: Bool where
    Flip 'False = 'True
    Flip 'True = 'False

-- | Apply both equations with the same Boolean callback.
example :: (('Left 'x' :: Either P.Char Bool) :~: 'Left 'x',
            ('Right 'False :: Either Bool Bool) :~: 'Right 'False)
example = (fmapLeft @P.Char (Proxy @Flip) (Proxy @'x'),
           fmapRight @Bool (Proxy @Flip) (Proxy @'True))
