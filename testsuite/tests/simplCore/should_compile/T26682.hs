{-# LANGUAGE Haskell2010 #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fspecialise-aggressively #-}

-- This is the result of @sheaf's work in minimising
-- @mikolaj's original bug report for #26682

module T26682 ( tensorADOnceMnistTests2 ) where

import Prelude

import Data.Proxy
  ( Proxy (Proxy) )

import GHC.TypeNats
import Data.Kind

import T26682a


data Concrete2 x = Concrete2

instance Eq ( Concrete2 a ) where
  _ == _ = error "no"
  {-# OPAQUE (==) #-}

type X :: Type -> TK
type family X a

type instance X (target y) = y
type instance X (a, b) = TKProduct (X a) (X b)
type instance X (a, b, c) = TKProduct (TKProduct (X a) (X b)) (X c)

tensorADOnceMnistTests2 :: Int -> Bool
tensorADOnceMnistTests2 seed0 =
  withSomeSNat 999 $ \ _ ->
    let seed1 =
          randomValue2
            @(Concrete2 (X (ADFcnnMnist2ParametersShaped Concrete2 101 101 Double Double)))
            seed0
        art = mnistTrainBench2VTOGradient3 seed1

        gg :: Concrete2
                (TKProduct
                   (TKProduct
                      (TKProduct
                         (TKProduct (TKR2 2 (TKScalar Double)) (TKR2 1 (TKScalar Double)))
                         (TKProduct (TKR2 2 (TKScalar Double)) (TKR2 1 (TKScalar Double))))
                      (TKProduct (TKR2 2 (TKScalar Double)) (TKR2 1 (TKScalar Double))))
                   (TKProduct (TKR 1 Double) (TKR 1 Double)))
        gg = undefined
        value1 = revInterpretArtifact2 art gg
    in
      value1 == value1

mnistTrainBench2VTOGradient3
  :: Int
  -> AstArtifactRev2
        (TKProduct
           (XParams2 Double Double)
           (TKProduct (TKR2 1 (TKScalar Double))
                      (TKR2 1 (TKScalar Double))))
        (TKScalar Double)
mnistTrainBench2VTOGradient3 !_
  | Dict0 <- lemTKScalarAllNumAD2 (Proxy @Double)
  = undefined

type ADFcnnMnist2ParametersShaped
       (target :: TK -> Type) (widthHidden :: Nat) (widthHidden2 :: Nat) r q =
  ( ( target (TKS '[widthHidden, 784] r)
    , target (TKS '[widthHidden] r) )
  , ( target (TKS '[widthHidden2, widthHidden] q)
    , target (TKS '[widthHidden2] r) )
  , ( target (TKS '[10, widthHidden2] r)
    , target (TKS '[10] r) )
  )

-- | The differentiable type of all trainable parameters of this nn.
type ADFcnnMnist2Parameters (target :: TK -> Type) r q =
  ( ( target (TKR 2 r)
    , target (TKR 1 r) )
  , ( target (TKR 2 q)
    , target (TKR 1 r) )
  , ( target (TKR 2 r)
    , target (TKR 1 r) )
  )

type XParams2 r q = X (ADFcnnMnist2Parameters Concrete2 r q)

data AstArtifactRev2 x z = AstArtifactRev2

revInterpretArtifact2
  :: AstArtifactRev2 x z
  -> Concrete2 x
  -> Concrete2 z
{-# OPAQUE revInterpretArtifact2 #-}
revInterpretArtifact2 _ _ = error "no"
