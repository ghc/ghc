{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeOperators #-}
module NativeTypeNumeralWrong where

import Data.Proxy
import Data.Type.Equality
import PackedVecLibrary

two :: Vec Bool 2
two = VCons True VNil

infixr 5 <||>
(<||>) :: Bool -> Bool -> Bool
False <||> b = b
True  <||> b = True

unknown :: Proxy b -> (b <||> 'False) :~: 'True
unknown _ = Refl
