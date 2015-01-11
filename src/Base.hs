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
    prefixArgs
    ) where

import Development.Shake
import Development.Shake.FilePath
import Control.Applicative hiding ((*>))
import Data.Function
import Data.Monoid
import Data.List

data Stage = Stage0 | Stage1 | Stage2 | Stage3 deriving (Eq, Enum)

type Args = Action [String]

type Condition = Action Bool

instance Monoid a => Monoid (Action a) where
    mempty = return mempty
    mappend p q = mappend <$> p <*> q

class ShowArgs a where
    showArgs     :: a -> Args
    showListArgs :: [a] -> Args -- the Creators' trick for overlapping String instances
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

-- Prefix each arg in a collection with a given prefix
prefixArgs :: (ShowArgs a, ShowArgs b) => a -> b -> Args
prefixArgs prefix as = do
    prefix' <- showArgs prefix
    concatMap (\a -> prefix' ++ [a]) <$> showArgs as
