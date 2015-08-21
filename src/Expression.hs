{-# LANGUAGE FlexibleInstances #-}
module Expression (
    module Target,
    module Data.Monoid,
    module Control.Monad.Reader,
    Expr, DiffExpr, fromDiffExpr,
    Predicate, PredicateLike (..), applyPredicate, (??),
    Args, Ways, Packages,
    apply, append, appendM, remove,
    appendSub, appendSubD, filterSub, removeSub,
    interpret, interpretDiff,
    getStage, getPackage, getBuilder, getFiles, getFile,
    getSources, getSource, getWay
    ) where

import Way
import Base
import Stage
import Builder
import Package
import Target (Target)
import Target hiding (Target(..))
import qualified Target
import Data.List
import Data.Monoid
import Control.Monad.Reader hiding (liftIO)

-- Expr a is a computation that produces a value of type Action a and can read
-- parameters of the current build Target.
type Expr a = ReaderT Target Action a

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
-- specifying conditions (Predicate), lists of arguments (Args), Ways and
-- Packages.
type Predicate = Expr Bool
type Args      = DiffExpr [String]
type Packages  = DiffExpr [Package]
type Ways      = DiffExpr [Way]

-- Basic operations on expressions:
-- 1) transform an expression by applying a given function
apply :: (a -> a) -> DiffExpr a
apply = return . Diff

-- 2) append something to an expression
append :: Monoid a => a -> DiffExpr a
append x = apply (<> x)

-- 3) remove given elements from a list expression
remove :: Eq a => [a] -> DiffExpr [a]
remove xs = apply . filter $ (`notElem` xs)

-- 4) apply a predicate to an expression
applyPredicate :: Monoid a => Predicate -> Expr a -> Expr a
applyPredicate predicate expr = do
    bool <- predicate
    if bool then expr else return mempty

-- A convenient operator for predicate application
class PredicateLike a where
    (?)  :: Monoid m => a -> Expr m -> Expr m
    notP :: a -> Predicate

infixr 8 ?

instance PredicateLike Predicate where
    (?)  = applyPredicate
    notP = liftM not

instance PredicateLike Bool where
    (?)  = applyPredicate . return
    notP = return . not

instance PredicateLike (Action Bool) where
    (?)  = applyPredicate . lift
    notP = lift . fmap not

-- An equivalent of if-then-else for predicates
(??) :: (PredicateLike a, Monoid m) => a -> (Expr m, Expr m) -> Expr m
p ?? (t, f) = p ? t <> notP p ? f

-- A monadic version of append
appendM :: Monoid a => Action a -> DiffExpr a
appendM = (append =<<) . lift

-- appendSub appends a list of sub-arguments to all arguments starting with a
-- given prefix. If there is no argument with such prefix then a new argument
-- of the form 'prefix=listOfSubarguments' is appended to the expression.
-- Note: nothing is done if the list of sub-arguments is empty.
appendSub :: String -> [String] -> Args
appendSub prefix xs
    | xs' == [] = mempty
    | otherwise = apply . go $ False
  where
    xs' = filter (/= "") xs
    go True  []     = []
    go False []     = [prefix ++ "=" ++ unwords xs']
    go found (y:ys) = if prefix `isPrefixOf` y
                      then unwords (y : xs') : go True ys
                      else y : go found ys

-- appendSubD is similar to appendSub but it extracts the list of sub-arguments
-- from the given DiffExpr.
appendSubD :: String -> Args -> Args
appendSubD prefix diffExpr = fromDiffExpr diffExpr >>= appendSub prefix

filterSub :: String -> (String -> Bool) -> Args
filterSub prefix p = apply . map $ filterSubstr
  where
    filterSubstr s
        | prefix `isPrefixOf` s = unwords . filter p . words $ s
        | otherwise             = s

-- Remove given elements from a list of sub-arguments with a given prefix
-- Example: removeSub "--configure-option=CFLAGS" ["-Werror"]
removeSub :: String -> [String] -> Args
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

-- Convenient getters for target parameters
getStage :: Expr Stage
getStage = asks Target.stage

getPackage :: Expr Package
getPackage = asks Target.package

getBuilder :: Expr Builder
getBuilder = asks Target.builder

getWay :: Expr Way
getWay = asks Target.way

getSources :: Expr [FilePath]
getSources = asks Target.sources

getSource :: Expr FilePath
getSource = do
    target <- ask
    srcs   <- getSources
    case srcs of
        [src] -> return src
        _     -> error $ "Exactly one source expected in target " ++ show target

getFiles :: Expr [FilePath]
getFiles = asks Target.files

-- Run getFiles and check that it contains a single file only
getFile :: Expr FilePath
getFile = do
    target <- ask
    files  <- getFiles
    case files of
        [res] -> return res
        _     -> error $ "Exactly one file expected in target " ++ show target
