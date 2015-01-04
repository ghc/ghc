{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Base (
    module Development.Shake,
    module Development.Shake.FilePath,
    module Control.Applicative,
    module Data.Monoid,
    module Data.List,
    Stage (..),
    Args, arg, args, ShowAction (..),
    Condition (..),
    joinArgs, joinArgsSpaced, splitArgs,
    filterOut
    ) where

import Development.Shake
import Development.Shake.FilePath
import Control.Applicative hiding ((*>))
import Data.Monoid
import Data.List

data Stage = Stage0 | Stage1 | Stage2 | Stage3 deriving (Eq, Enum)

type Args = Action [String]

type Condition = Action Bool

instance Monoid a => Monoid (Action a) where
    mempty = return mempty
    mappend p q = mappend <$> p <*> q

class ShowAction a where
    showAction     :: a -> Args
    showListAction :: [a] -> Args -- the Creators' trick for overlapping String instances
    showListAction = mconcat . map showAction

instance ShowAction Char where
    showAction c     = return [[c]]
    showListAction s = return [s]

instance ShowAction a => ShowAction [a] where
    showAction = showListAction

instance ShowAction a => ShowAction (Action a) where
    showAction = (showAction =<<)

arg :: ShowAction a => a -> Args
arg = showAction

type ArgsCombine = Args -> Args -> Args

class Collect a where
    collect :: ArgsCombine -> Args -> a

instance Collect Args where
    collect = const id

instance (ShowAction a, Collect r) => Collect (a -> r) where
    collect combine x = \y -> collect combine $ x `combine` arg y

args :: Collect a => a
args = collect (<>) mempty

joinArgs :: Collect a => a
joinArgs = collect (\x y -> intercalateArgs "" $ x <> y) mempty

joinArgsSpaced :: Collect a => a
joinArgsSpaced = collect (\x y -> intercalateArgs " " $ x <> y) mempty

intercalateArgs :: String -> Args -> Args
intercalateArgs s as = do
    as' <- as
    return [intercalate s as']

splitArgs :: Args -> Args
splitArgs = fmap (concatMap words)

filterOut :: Args -> [String] -> Args
filterOut as list = filter (`notElem` list) <$> as
