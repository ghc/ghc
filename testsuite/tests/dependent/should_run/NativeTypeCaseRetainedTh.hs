{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TemplateHaskell #-}
module NativeTypeCaseRetainedTh (fromQuote, fromReify) where

import Language.Haskell.TH
import qualified Data.Kind as K
import Data.Proxy (Proxy)
import NativeRetainedTH (Select, quoted)
import NativeTypeCaseRetainedThProvider (shadowed)

fromQuote :: $([t| forall (b :: K.Type). Proxy b -> (foreach (b :: Bool) -> Select b) |])
fromQuote _ = quoted

fromReify :: $(do
  ty <- reifyType 'shadowed
  let scoped (ForallMatchT _ body) = scoped body
      scoped (ForallT [KindedTV erased _ _] []
        (AppT (AppT ArrowT arg) (ForEachT BndrReq runtime _ (AppT _ (IndexVarT used))))) =
          erased /= runtime && runtime == used && usesErased erased arg
      scoped _ = False
      usesErased b (VarT name) = b == name
      usesErased b (AppT f x) = usesErased b f || usesErased b x
      usesErased b (SigT ty _) = usesErased b ty
      usesErased _ _ = False
  if scoped ty then pure ty else fail (show ty))
fromReify = shadowed
