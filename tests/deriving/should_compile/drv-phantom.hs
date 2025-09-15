{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE DeriveTraversable #-}

module DrvPhantom where
import GHC.Generics (Generic, Generic1)
import Data.Data (Data)
import Language.Haskell.TH.Syntax (Lift)

data NotAList a = Nil | NotCons (NotAList a)
  deriving (Functor, Foldable, Traversable)

type role NotAList phantom
