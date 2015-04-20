{-# LANGUAGE FlexibleInstances #-}

module Base (
    module Development.Shake,
    module Development.Shake.FilePath,
    module Control.Applicative,
    module Data.Function,
    module Data.Monoid,
    module Data.List,
    Stage (..),
    TargetDir (..),
    Arg, Args,
    ShowArg (..), ShowArgs (..),
    arg, args,
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

data Stage = Stage0 | Stage1 | Stage2 | Stage3 deriving (Eq, Enum)

instance Show Stage where
    show = show . fromEnum

-- Need TargetDir and FilePath to be distinct types
newtype TargetDir = TargetDir { fromTargetDir :: FilePath } deriving (Show, Eq)

-- The returned string or list of strings is a part of an argument list
-- to be passed to a Builder
type Arg  = Action String
type Args = Action [String]

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
    showArgs :: a -> Args

instance ShowArgs [String] where
    showArgs = return

instance ShowArgs [Arg] where
    showArgs = sequence

instance ShowArgs [Args] where
    showArgs = mconcat

instance ShowArgs a => ShowArgs (Action a) where
    showArgs = (showArgs =<<)

args :: ShowArgs a => a -> Args
args = showArgs

arg :: ShowArg a => a -> Args
arg a = args [showArg a]

-- Filter out given arg(s) from a collection
filterOut :: ShowArgs a => Args -> a -> Args
filterOut as exclude = do
    exclude' <- showArgs exclude
    filter (`notElem` exclude') <$> as

-- Generate a cross product collection of two argument collections
-- Example: productArgs ["-a", "-b"] "c" = args ["-a", "c", "-b", "c"]
productArgs :: (ShowArgs a, ShowArgs b) => a -> b -> Args
productArgs as bs = do
    as' <- showArgs as
    bs' <- showArgs bs
    return $ concat $ sequence [as', bs']

-- Similar to productArgs but concat resulting arguments pairwise
-- Example: concatArgs ["-a", "-b"] "c" = args ["-ac", "-bc"]
concatArgs :: (ShowArgs a, ShowArgs b) => a -> b -> Args
concatArgs as bs = do
    as' <- showArgs as
    bs' <- showArgs bs
    return $ map concat $ sequence [as', bs']
