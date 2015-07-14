{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Base (
    module Development.Shake,
    module Development.Shake.FilePath,
    module Control.Applicative,
    module Data.Function,
    module Data.Monoid,
    module Data.List,
    Stage (..),
    Arg, ArgList,
    ShowArg (..), ShowArgs (..),
    Condition (..),
    filterOut,
    productArgs, concatArgs
    ) where

import Development.Shake hiding ((*>), alternatives)
import Development.Shake.FilePath
import Control.Applicative
import Data.Function
import Data.Monoid
import Data.List
import GHC.Generics
import Development.Shake.Classes

data Stage = Stage0 | Stage1 | Stage2 | Stage3 deriving (Eq, Enum, Generic)

instance Show Stage where
    show = show . fromEnum

-- Instances for storing Target in the Shake database
instance Binary Stage
instance Hashable Stage

-- The returned string or list of strings is a part of an argument list
-- to be passed to a Builder
type Arg     = Action String
type ArgList = Action [String]

type Condition = Action Bool

instance Monoid a => Monoid (Action a) where
    mempty = return mempty
    mappend p q = mappend <$> p <*> q

class ShowArg a where
    showArg :: a -> Arg

instance ShowArg String where
    showArg = return

instance ShowArg a => ShowArg (Action a) where
    showArg = (showArg =<<)

class ShowArgs a where
    showArgs :: a -> ArgList

instance ShowArgs [String] where
    showArgs = return

instance ShowArgs [Arg] where
    showArgs = sequence

instance ShowArgs [ArgList] where
    showArgs = mconcat

instance ShowArgs a => ShowArgs (Action a) where
    showArgs = (showArgs =<<)

-- Filter out given arg(s) from a collection
filterOut :: ShowArgs a => ArgList -> a -> ArgList
filterOut as exclude = do
    exclude' <- showArgs exclude
    filter (`notElem` exclude') <$> as

-- Generate a cross product collection of two argument collections
-- Example: productArgs ["-a", "-b"] "c" = args ["-a", "c", "-b", "c"]
productArgs :: (ShowArgs a, ShowArgs b) => a -> b -> ArgList
productArgs as bs = do
    as' <- showArgs as
    bs' <- showArgs bs
    return $ concat $ sequence [as', bs']

-- Similar to productArgs but concat resulting arguments pairwise
-- Example: concatArgs ["-a", "-b"] "c" = args ["-ac", "-bc"]
concatArgs :: (ShowArgs a, ShowArgs b) => a -> b -> ArgList
concatArgs as bs = do
    as' <- showArgs as
    bs' <- showArgs bs
    return $ map concat $ sequence [as', bs']
