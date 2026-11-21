{-# LANGUAGE ExperimentalDependentTypes, RequiredTypeArguments #-}
module NativeRetainedTypeOperandContext where

data TypeRep a = LocalRep

wrong :: TypeRep Bool
wrong = (type Bool)

unknown = (type Bool)
