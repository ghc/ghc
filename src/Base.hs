{-# LANGUAGE FlexibleInstances #-}

module Base (
    module Development.Shake,
    module Development.Shake.FilePath,
    module Control.Applicative,
    module Data.Function,
    module Data.Monoid,
    module Data.List,
    Stage (..),
    Args, arg, ShowArgs (..),
    Condition (..),
    (<+>),
    filterOut,
    productArgs, concatArgs
    ) where

import Development.Shake hiding ((*>))
import Development.Shake.FilePath
import Control.Applicative
import Data.Function
import Data.Monoid
import Data.List

data Stage = Stage0 | Stage1 | Stage2 | Stage3 deriving (Eq, Enum)

instance Show Stage where
    show = show . fromEnum

type Args = Action [String]

type Condition = Action Bool

instance Monoid a => Monoid (Action a) where
    mempty = return mempty
    mappend p q = mappend <$> p <*> q

-- Using the Creators' trick for overlapping String instances
class ShowArgs a where
    showArgs     :: a -> Args
    showListArgs :: [a] -> Args
    showListArgs = mconcat . map showArgs

instance ShowArgs Char where
    showArgs c     = return [[c]]
    showListArgs s = return [s]

instance ShowArgs a => ShowArgs [a] where
    showArgs = showListArgs

instance ShowArgs a => ShowArgs (Action a) where
    showArgs = (showArgs =<<)

arg :: ShowArgs a => a -> Args
arg = showArgs

-- Combine two heterogeneous ShowArgs values
(<+>) :: (ShowArgs a, ShowArgs b) => a -> b -> Args
a <+> b = (<>) <$> showArgs a <*> showArgs b

infixr 6 <+>

-- Filter out given arg(s) from a collection
filterOut :: ShowArgs a => Args -> a -> Args
filterOut as exclude = do
    exclude' <- showArgs exclude
    filter (`notElem` exclude') <$> as

-- Generate a cross product collection of two argument collections
-- Example: productArgs ["-a", "-b"] "c" = arg ["-a", "c", "-b", "c"]
productArgs :: (ShowArgs a, ShowArgs b) => a -> b -> Args
productArgs as bs = do
    as' <- showArgs as
    bs' <- showArgs bs
    return $ concat $ sequence [as', bs']

-- Similar to productArgs but concat resulting arguments pairwise
-- Example: concatArgs ["-a", "-b"] "c" = arg ["-ac", "-bc"]
concatArgs :: (ShowArgs a, ShowArgs b) => a -> b -> Args
concatArgs as bs = do
    as' <- showArgs as
    bs' <- showArgs bs
    return $ map concat $ sequence [as', bs']
