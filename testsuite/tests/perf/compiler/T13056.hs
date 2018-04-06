{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE RoleAnnotations #-}

module Bug where
import Data.Typeable
import GHC.Generics
import Data.Data

data Condition v = Condition
    deriving (Functor, Foldable)
-- We don't want the phantom optimization to kick
-- in here and confuse the test.
type role Condition representational

data CondTree v c a = CondNode
    { condTreeData        :: a
    , condTreeConstraints :: c
    , condTreeComponents  :: [CondBranch v c a]
    }
    deriving (Functor, Foldable)

data CondBranch v c a = CondBranch
    { condBranchCondition :: Condition v
    , condBranchIfTrue    :: CondTree v c a
    , condBranchIfFalse   :: Maybe (CondTree v c a)
    }
    deriving (Functor, Foldable)
