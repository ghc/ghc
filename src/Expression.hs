{-# LANGUAGE NoImplicitPrelude, FlexibleInstances #-}
module Expression (
    module Control.Monad.Reader,
    module Data.Monoid,
    Expr, DiffExpr, fromDiffExpr,
    Predicate,
    Settings, Ways, Packages,
    Target (..), stageTarget, stagePackageTarget,
    append, appendM, remove, appendSub, appendSubD, filterSub, removeSub,
    interpret, interpretDiff,
    applyPredicate, (?), (??), stage, package, builder, file, way,
    configKeyValue, configKeyValues
    ) where

import Base hiding (arg, args, Args, TargetDir)
import Ways
import Oracles
import Package
import Data.Monoid
import Control.Monad.Reader

data Target = Target
     {
        getStage   :: Stage,
        getPackage :: Package,
        getBuilder :: Builder,
        getFile    :: FilePath, -- TODO: handle multple files?
        getWay     :: Way
     }

stageTarget :: Stage -> Target
stageTarget stage = Target
    {
        getStage   = stage,
        getPackage = error "stageTarget: Package not set",
        getBuilder = error "stageTarget: Builder not set",
        getFile    = error "stageTarget: File not set",
        getWay     = error "stageTarget: Way not set"
    }

stagePackageTarget :: Stage -> Package -> Target
stagePackageTarget stage package = Target
    {
        getStage   = stage,
        getPackage = package,
        getBuilder = error "stagePackageTarget: Builder not set",
        getFile    = error "stagePackageTarget: File not set",
        getWay     = error "stagePackageTarget: Way not set"
    }

-- We could use Dual (Endo a) instead of Diff a, but the former may look scary.
newtype Diff a = Diff { fromDiff :: a -> a }

instance Monoid (Diff a) where
    mempty = Diff id
    Diff x `mappend` Diff y = Diff $ y . x

type Expr a = ReaderT Target Action a
type DiffExpr a = Expr (Diff a)

type Predicate       = Expr Bool
type Settings        = DiffExpr [String] -- TODO: rename to Args
type Ways            = DiffExpr [Way]
type Packages        = DiffExpr [Package]

instance Monoid a => Monoid (Expr a) where
    mempty  = return mempty
    mappend = liftM2 mappend

-- Basic operations on expressions:
-- 1) append something to an expression
append :: Monoid a => a -> DiffExpr a
append x = return . Diff $ (<> x)

-- 2) remove given elements from a list expression
remove :: Eq a => [a] -> DiffExpr [a]
remove xs = return . Diff $ filter (`notElem` xs)

-- 3) apply a predicate to an expression
applyPredicate :: Monoid a => Predicate -> Expr a -> Expr a
applyPredicate predicate expr = do
    bool <- predicate
    if bool then expr else return mempty

-- A convenient operator for predicate application
(?) :: Monoid a => Predicate -> Expr a -> Expr a
(?) = applyPredicate

infixr 8 ?

-- A monadic version of append
appendM :: Monoid a => Action a -> DiffExpr a
appendM mx = lift mx >>= append

-- appendSub appends a list of sub-arguments to all arguments starting with a
-- given prefix. If there is no argument with such prefix then a new argument
-- of the form 'prefix=listOfSubarguments' is appended to the expression.
-- Note: nothing is done if the list of sub-arguments is empty.
appendSub :: String -> [String] -> Settings
appendSub prefix xs
    | xs' == [] = mempty
    | otherwise = return . Diff $ go False
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
appendSubD prefix diffExpr = fromDiffExpr diffExpr >>= appendSub prefix

filterSub :: String -> (String -> Bool) -> Settings
filterSub prefix p = return . Diff $ map filterSubstr
  where
    filterSubstr s
        | prefix `isPrefixOf` s = unwords . filter p . words $ s
        | otherwise             = s

-- Remove given elements from a list of sub-arguments with a given prefix
-- Example: removeSub "--configure-option=CFLAGS" ["-Werror"]
removeSub :: String -> [String] -> Settings
removeSub prefix xs = filterSub prefix (`notElem` xs)

-- Interpret a given expression in a given environment
interpret :: Target -> Expr a -> Action a
interpret = flip runReaderT

-- Extract an expression from a difference expression
fromDiffExpr :: Monoid a => DiffExpr a -> Expr a
fromDiffExpr = fmap (($ mempty) . fromDiff)

-- Interpret a given difference expression in a given environment
interpretDiff :: Monoid a => Target -> DiffExpr a -> Action a
interpretDiff target = interpret target . fromDiffExpr

-- An equivalent of if-then-else for predicates
(??) :: Monoid a => Predicate -> (Expr a, Expr a) -> Expr a
p ?? (t, f) = p ? t <> (liftM not p) ? f

-- Basic predicates (see Switches.hs for derived predicates)
stage :: Stage -> Predicate
stage s = liftM (s ==) (asks getStage)

package :: Package -> Predicate
package p = liftM (p ==) (asks getPackage)

builder :: Builder -> Predicate
builder b = liftM (b ==) (asks getBuilder)

file :: FilePattern -> Predicate
file f = liftM (f ?==) (asks getFile)

way :: Way -> Predicate
way w = liftM (w ==) (asks getWay)

configKeyValue :: String -> String -> Predicate
configKeyValue key value = liftM (value ==) (lift $ askConfig key)

-- Check if there is at least one match
-- Example: configKeyValues "host-os-cpp" ["mingw32", "cygwin32"]
configKeyValues :: String -> [String] -> Predicate
configKeyValues key values = liftM (`elem` values) (lift $ askConfig key)
