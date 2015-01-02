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
    joinArgs, joinArgsWithSpaces, splitArgs,
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
    showAction :: a -> Action String

instance ShowAction String where
    showAction = return

arg :: ShowAction a => [a] -> Args
arg = mapM showAction

class Collect a where
    collect :: Args -> a

instance Collect Args where
    collect = id

instance (ShowAction a, Collect r) => Collect (a -> r) where
    collect prev next = collect $ do
        next' <- showAction next
        prev <> return [next']

args :: Collect a => a
args = collect mempty

intercalateArgs :: String -> Args -> Args
intercalateArgs s args = do
    as <- args
    return [intercalate s as]

joinArgsWithSpaces :: Args -> Args
joinArgsWithSpaces = intercalateArgs " "

joinArgs :: Args -> Args
joinArgs = intercalateArgs ""

splitArgs :: Args -> Args
splitArgs = fmap (concatMap words)

filterOut :: Args -> [String] -> Args
filterOut args list = filter (`notElem` list) <$> args
