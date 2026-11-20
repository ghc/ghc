{-# LANGUAGE ExperimentalDependentTypes, RequiredTypeArguments, ScopedTypeVariables #-}
module MixedPackageReject where

import qualified GHC.Exists as Exists
import MixedPackageProvider

wrongWitness :: Mixed
wrongWitness = Exists.Pack @(Inner Bool) (type Int) (True, False, 11)

wrongMode :: Mixed
wrongMode = Exists.Pack @Mixed (type Bool) inner

type Diagonal = exists b | (b, b, Int)

diagonal :: Diagonal
diagonal = Exists.Pack (type Bool) (True, True, 11)

crossed :: Mixed
crossed = diagonal

escapeOuter :: forall a. Mixed -> a
escapeOuter p = case p of
  Exists.Pack _ (x, _, _) -> x

escapeInner :: forall a. Mixed -> a
escapeInner p = case p of
  Exists.Pack (type witness) (_, y :: witness, _) -> y
