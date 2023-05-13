{-# LANGUAGE
    DefaultSignatures
  , MultiParamTypeClasses
  , GADTs
  , PolyKinds
  , ScopedTypeVariables
  , StandaloneKindSignatures
#-}

module ThSyntax where

import Data.Kind
import GHC.Exts

type Code :: forall r. TYPE r -> Type
data Code a = Code

unTypeCode :: forall (r :: RuntimeRep) (a :: TYPE r) . Code a -> ()
unTypeCode _ = ()

type Lift :: forall r -> TYPE r -> Constraint
class Lift r t where
  lift :: t -> ()
  default lift :: (r ~ ('BoxedRep 'Lifted)) => t -> ()
  lift = unTypeCode . liftTyped
  liftTyped :: t -> Code t
