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

instance ShowAction (Action String) where
    showAction = id

arg :: ShowAction a => [a] -> Args
arg = mapM showAction

type ArgsCombine = Args -> Args -> Args

class Collect a where
    collect :: ArgsCombine -> Args -> a

instance Collect Args where
    collect = const id

instance (ShowAction a, Collect r) => Collect (a -> r) where
    collect combine x = \y -> collect combine $ x `combine` arg [y]

instance Collect r => Collect (Args -> r) where
    collect combine x = \y -> collect combine $ x `combine` y

args :: Collect a => a
args = collect (<>) mempty

joinArgs :: Collect a => a
joinArgs = collect (\x y -> intercalateArgs "" x <> y) mempty

joinArgsWithSpaces :: Collect a => a
joinArgsWithSpaces = collect (\x y -> intercalateArgs " " x <> y) mempty

intercalateArgs :: String -> Args -> Args
intercalateArgs s as = do
    as' <- as
    return [intercalate s as']

splitArgs :: Args -> Args
splitArgs = fmap (concatMap words)

filterOut :: Args -> [String] -> Args
filterOut as list = filter (`notElem` list) <$> as
