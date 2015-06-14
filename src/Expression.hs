{-# LANGUAGE NoImplicitPrelude, FlexibleInstances #-}
module Expression (
    module Control.Monad.Reader,
    module Data.Monoid,
    Expr, DiffExpr, fromDiff,
    Predicate,
    Settings, Ways, Packages,
    Environment (..), defaultEnvironment,
    append, appendM, remove, appendSub, appendSubD, filterSub, removeSub,
    interpret, interpretDiff,
    applyPredicate, (?), (??), stage, builder, package,
    configKeyValue, configKeyValues,
    configKeyYes, configKeyNo, configKeyNonEmpty
    ) where

import Base hiding (arg, args, Args, TargetDir)
import Ways
import Oracles
import Package
import Data.Monoid
import Control.Monad.Reader

data Environment = Environment
     {
        getStage   :: Stage,
        getBuilder :: Builder,
        getPackage :: Package
     }

defaultEnvironment :: Environment
defaultEnvironment = Environment
    {
        getStage   = error "Stage not set in the environment",
        getBuilder = error "Builder not set in the environment",
        getPackage = error "Package not set in the environment"
    }

type Expr a = ReaderT Environment Action a
type DiffExpr a = Expr (Dual (Endo a))

type Predicate = Expr Bool

type Settings = DiffExpr [String]
type Ways     = DiffExpr [Way]
type Packages = DiffExpr [Package]

instance Monoid a => Monoid (Expr a) where
    mempty  = return mempty
    mappend = liftM2 mappend

append :: Monoid a => a -> DiffExpr a
append x = return . Dual . Endo $ (<> x)

appendM :: Monoid a => Action a -> DiffExpr a
appendM mx = lift mx >>= append

remove :: Eq a => [a] -> DiffExpr [a]
remove xs = return . Dual . Endo $ filter (`notElem` xs)

-- appendSub appends a list of sub-arguments to all arguments starting with a
-- given prefix. If there is no argument with such prefix then a new argument
-- of the form 'prefix=listOfSubarguments' is appended to the expression.
-- Note: nothing is done if the list of sub-arguments is empty.
appendSub :: String -> [String] -> Settings
appendSub prefix xs
    | xs' == [] = mempty
    | otherwise = return . Dual . Endo $ go False
  where
    xs' = filter (/= "") xs
    go True  []     = []
    go False []     = [prefix ++ "=" ++ unwords xs']
    go found (y:ys) = if prefix `isPrefixOf` y
                      then unwords (y : xs') : go True ys
                      else y : go found ys

-- appendSubD is similar to appendSub but it extracts the list of sub-arguments
-- from the given DiffExpr.
appendSubD :: String -> Settings -> Settings
appendSubD prefix diffExpr = fromDiff diffExpr >>= appendSub prefix

filterSub :: String -> (String -> Bool) -> Settings
filterSub prefix p = return . Dual . Endo $ map filterSubstr
  where
    filterSubstr s
        | prefix `isPrefixOf` s = unwords . filter p . words $ s
        | otherwise             = s

removeSub :: String -> [String] -> Settings
removeSub prefix xs = filterSub prefix (`notElem` xs)

interpret :: Environment -> Expr a -> Action a
interpret = flip runReaderT

fromDiff :: Monoid a => DiffExpr a -> Expr a
fromDiff = fmap (($ mempty) . appEndo . getDual)

interpretDiff :: Monoid a => Environment -> DiffExpr a -> Action a
interpretDiff env = interpret env . fromDiff

applyPredicate :: Monoid a => Predicate -> Expr a -> Expr a
applyPredicate predicate expr = do
    bool <- predicate
    if bool then expr else return mempty

(?) :: Monoid a => Predicate -> Expr a -> Expr a
(?) = applyPredicate

(??) :: Monoid a => Predicate -> (Expr a, Expr a) -> Expr a
p ?? (t, f) = p ? t <> (liftM not p) ? f

infixr 8 ?

stage :: Stage -> Predicate
stage s = liftM (s ==) (asks getStage)

builder :: Builder -> Predicate
builder b = liftM (b ==) (asks getBuilder)

package :: Package -> Predicate
package p = liftM (p ==) (asks getPackage)

configKeyValue :: String -> String -> Predicate
configKeyValue key value = liftM (value ==) (lift $ askConfig key)

-- checks if there is at least one match
configKeyValues :: String -> [String] -> Predicate
configKeyValues key values = liftM (`elem` values) (lift $ askConfig key)

configKeyYes :: String -> Predicate
configKeyYes key = configKeyValue key "YES"

configKeyNo :: String -> Predicate
configKeyNo key = configKeyValue key "NO"

configKeyNonEmpty :: String -> Predicate
configKeyNonEmpty key = liftM not $ configKeyValue key ""
