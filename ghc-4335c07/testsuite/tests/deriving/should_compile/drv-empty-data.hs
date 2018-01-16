{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE DeriveTraversable, DeriveGeneric, EmptyCase,
    DeriveDataTypeable, StandaloneDeriving, DeriveLift #-}

module DrvEmptyData where
import GHC.Generics (Generic, Generic1)
import Data.Data (Data)
import Language.Haskell.TH.Syntax (Lift)

data Void a deriving (Functor, Foldable, Traversable, Generic, Generic1, Lift)

-- We don't want to invoke the special case for phantom types here.
type role Void nominal

deriving instance Data a => Data (Void a)
deriving instance Eq (Void a)
deriving instance Ord (Void a)
deriving instance Show (Void a)
deriving instance Read (Void a)
