{-# LANGUAGE FlexibleInstances #-}

module Base (
    module Development.Shake,
    module Development.Shake.FilePath,
    module Control.Applicative,
    module Data.Monoid,
    module Data.List,
    Stage (..),
    Args, arg,
    joinArgs, joinArgsWithSpaces,
    filterOut,
    replaceChar
    ) where

import Development.Shake hiding ((*>))
import Development.Shake.FilePath
import Control.Applicative
import Data.Monoid
import Data.List

data Stage = Stage0 | Stage1 | Stage2 | Stage3 deriving (Eq, Enum)

type Args = Action [String]

instance Monoid a => Monoid (Action a) where
    mempty = return mempty
    mappend p q = mappend <$> p <*> q

arg :: [String] -> Args
arg = return

intercalateArgs :: String -> Args -> Args
intercalateArgs s args = do
    as <- args
    return [intercalate s as]

joinArgsWithSpaces :: Args -> Args
joinArgsWithSpaces = intercalateArgs " "

joinArgs :: Args -> Args
joinArgs = intercalateArgs ""

filterOut :: Args -> [String] -> Args
filterOut args list = filter (`notElem` list) <$> args

replaceChar :: Char -> Char -> String -> String
replaceChar from to = (go from) . if from == '/' then go '\\' else id
  where
    go from' = map (\c -> if c == from' then to else c)
