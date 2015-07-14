{-# LANGUAGE NoImplicitPrelude, FlexibleInstances #-}
module Expression (
    module Target,
    module Data.Monoid,
    module Control.Monad.Reader,
    Expr, DiffExpr, fromDiffExpr,
    Predicate, Settings, Ways, Packages,
    append, appendM, remove, appendSub, appendSubD, filterSub, removeSub,
    interpret, interpretExpr,
    applyPredicate, (?), (??), stage, package, builder, file, way,
    configKeyValue, configKeyValues
    ) where

import Base hiding (arg, args, Args, TargetDir)
import Ways
import Target
import Oracles
import Package
import Data.Monoid
import Control.Monad.Reader

-- Expr a is a computation that produces a value of type Action a and can read
-- parameters of the current build Target.
type Expr a = ReaderT Target Action a

-- If values of type a form a Monoid then so do computations of type Expr a:
-- * the empty computation returns the identity element of the underlying type
-- * two computations can be combined by combining their results
instance Monoid a => Monoid (Expr a) where
    mempty  = return mempty
    mappend = liftM2 mappend

-- Diff a holds functions of type a -> a and is equipped with a Monoid instance.
-- We could use Dual (Endo a) instead of Diff a, but the former may look scary.
-- The name comes from "difference lists".
newtype Diff a = Diff { fromDiff :: a -> a }

-- DiffExpr a is a computation that builds a difference list (i.e., a function
-- of type Action (a -> a)) and can read parameters of the current build Target.
type DiffExpr a = Expr (Diff a)

-- Note the reverse order of function composition (y . x), which ensures that
-- when two DiffExpr computations c1 and c2 are combined (c1 <> c2), then c1 is
-- applied first, and c2 is applied second.
instance Monoid (Diff a) where
    mempty = Diff id
    Diff x `mappend` Diff y = Diff $ y . x

-- The following expressions are used throughout the build system for
-- specifying conditions (Predicate), lists of arguments (Settings), Ways and
-- Packages.
type Predicate = Expr Bool
type Settings  = DiffExpr [String] -- TODO: rename to Args
type Ways      = DiffExpr [Way]
type Packages  = DiffExpr [Package]

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
interpretExpr :: Target -> Expr a -> Action a
interpretExpr = flip runReaderT

-- Extract an expression from a difference expression
fromDiffExpr :: Monoid a => DiffExpr a -> Expr a
fromDiffExpr = fmap (($ mempty) . fromDiff)

-- Interpret a given difference expression in a given environment
interpret :: Monoid a => Target -> DiffExpr a -> Action a
interpret target = interpretExpr target . fromDiffExpr

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
