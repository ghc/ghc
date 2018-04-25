{-# LANGUAGE CPP #-}
#ifdef DEBUG_CONFLICT_SETS
{-# LANGUAGE ImplicitParams #-}
#endif
-- | Conflict sets
--
-- Intended for double import
--
-- > import Distribution.Solver.Modular.ConflictSet (ConflictSet)
-- > import qualified Distribution.Solver.Modular.ConflictSet as CS
module Distribution.Solver.Modular.ConflictSet (
    ConflictSet -- opaque
  , ConflictMap
#ifdef DEBUG_CONFLICT_SETS
  , conflictSetOrigin
#endif
  , showConflictSet
  , showCSSortedByFrequency
  , showCSWithFrequency
    -- Set-like operations
  , toList
  , union
  , unions
  , insert
  , empty
  , singleton
  , member
  , filter
  , fromList
  ) where

import Prelude hiding (filter)
import Data.List (intercalate, sortBy)
import Data.Map (Map)
import Data.Set (Set)
import Data.Function (on)
import qualified Data.Set as S
import qualified Data.Map as M

#ifdef DEBUG_CONFLICT_SETS
import Data.Tree
import GHC.Stack
#endif

import Distribution.Solver.Modular.Var
import Distribution.Solver.Types.PackagePath

-- | The set of variables involved in a solver conflict
--
-- Since these variables should be preprocessed in some way, this type is
-- kept abstract.
data ConflictSet = CS {
    -- | The set of variables involved on the conflict
    conflictSetToSet :: Set (Var QPN)

#ifdef DEBUG_CONFLICT_SETS
    -- | The origin of the conflict set
    --
    -- When @DEBUG_CONFLICT_SETS@ is defined @(-f debug-conflict-sets)@,
    -- we record the origin of every conflict set. For new conflict sets
    -- ('empty', 'fromVars', ..) we just record the 'CallStack'; for operations
    -- that construct new conflict sets from existing conflict sets ('union',
    -- 'filter', ..)  we record the 'CallStack' to the call to the combinator
    -- as well as the 'CallStack's of the input conflict sets.
    --
    -- Requires @GHC >= 7.10@.
  , conflictSetOrigin :: Tree CallStack
#endif
  }
  deriving (Show)

instance Eq ConflictSet where
  (==) = (==) `on` conflictSetToSet

instance Ord ConflictSet where
  compare = compare `on` conflictSetToSet

showConflictSet :: ConflictSet -> String
showConflictSet = intercalate ", " . map showVar . toList

showCSSortedByFrequency :: ConflictMap -> ConflictSet -> String
showCSSortedByFrequency = showCS False

showCSWithFrequency :: ConflictMap -> ConflictSet -> String
showCSWithFrequency = showCS True

showCS :: Bool -> ConflictMap -> ConflictSet -> String
showCS showCount cm =
    intercalate ", " . map showWithFrequency . indexByFrequency
  where
    indexByFrequency = sortBy (flip compare `on` snd) . map (\c -> (c, M.lookup c cm)) . toList
    showWithFrequency (conflict, maybeFrequency) = case maybeFrequency of
      Just frequency
        | showCount -> showVar conflict ++ " (" ++ show frequency ++ ")"
      _             -> showVar conflict

{-------------------------------------------------------------------------------
  Set-like operations
-------------------------------------------------------------------------------}

toList :: ConflictSet -> [Var QPN]
toList = S.toList . conflictSetToSet

union ::
#ifdef DEBUG_CONFLICT_SETS
  (?loc :: CallStack) =>
#endif
  ConflictSet -> ConflictSet -> ConflictSet
union cs cs' = CS {
      conflictSetToSet = S.union (conflictSetToSet cs) (conflictSetToSet cs')
#ifdef DEBUG_CONFLICT_SETS
    , conflictSetOrigin = Node ?loc (map conflictSetOrigin [cs, cs'])
#endif
    }

unions ::
#ifdef DEBUG_CONFLICT_SETS
  (?loc :: CallStack) =>
#endif
  [ConflictSet] -> ConflictSet
unions css = CS {
      conflictSetToSet = S.unions (map conflictSetToSet css)
#ifdef DEBUG_CONFLICT_SETS
    , conflictSetOrigin = Node ?loc (map conflictSetOrigin css)
#endif
    }

insert ::
#ifdef DEBUG_CONFLICT_SETS
  (?loc :: CallStack) =>
#endif
  Var QPN -> ConflictSet -> ConflictSet
insert var cs = CS {
      conflictSetToSet = S.insert var (conflictSetToSet cs)
#ifdef DEBUG_CONFLICT_SETS
    , conflictSetOrigin = Node ?loc [conflictSetOrigin cs]
#endif
    }

empty ::
#ifdef DEBUG_CONFLICT_SETS
  (?loc :: CallStack) =>
#endif
  ConflictSet
empty = CS {
      conflictSetToSet = S.empty
#ifdef DEBUG_CONFLICT_SETS
    , conflictSetOrigin = Node ?loc []
#endif
    }

singleton ::
#ifdef DEBUG_CONFLICT_SETS
  (?loc :: CallStack) =>
#endif
  Var QPN -> ConflictSet
singleton var = CS {
      conflictSetToSet = S.singleton var
#ifdef DEBUG_CONFLICT_SETS
    , conflictSetOrigin = Node ?loc []
#endif
    }

member :: Var QPN -> ConflictSet -> Bool
member var = S.member var . conflictSetToSet

filter ::
#ifdef DEBUG_CONFLICT_SETS
  (?loc :: CallStack) =>
#endif
  (Var QPN -> Bool) -> ConflictSet -> ConflictSet
filter p cs = CS {
      conflictSetToSet = S.filter p (conflictSetToSet cs)
#ifdef DEBUG_CONFLICT_SETS
    , conflictSetOrigin = Node ?loc [conflictSetOrigin cs]
#endif
    }

fromList ::
#ifdef DEBUG_CONFLICT_SETS
  (?loc :: CallStack) =>
#endif
  [Var QPN] -> ConflictSet
fromList vars = CS {
      conflictSetToSet = S.fromList vars
#ifdef DEBUG_CONFLICT_SETS
    , conflictSetOrigin = Node ?loc []
#endif
    }

type ConflictMap = Map (Var QPN) Int

