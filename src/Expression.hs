{-# LANGUAGE FlexibleInstances, LambdaCase #-}
module Expression (
    -- * Expressions
    Expr, DiffExpr, fromDiffExpr,
    -- ** Operators
    apply, append, arg, remove,
    appendSub, appendSubD, filterSub, removeSub,
    -- ** Evaluation
    interpret, interpretInContext, interpretDiff,
    -- ** Predicates
    Predicate, (?), applyPredicate,
    -- ** Common expressions
    Args, Ways, Packages,
    -- ** Context and Target
    Context, vanillaContext, stageContext, Target, dummyTarget,

    -- * Convenient accessors
    getContext, getStage, getPackage, getBuilder, getOutputs, getInputs, getWay,
    getInput, getOutput, getSingleton,

    -- * Re-exports
    module Control.Monad.Trans.Reader,
    module Data.Monoid,
    module Builder,
    module Package,
    module Stage,
    module Way
    ) where

import Control.Monad.Trans.Reader
import Data.Monoid

import Base
import Builder
import Context
import Package
import Stage
import Target
import Way

-- | @Expr a@ is a computation that produces a value of type @Action a@ and can
-- read parameters of the current build 'Target'.
type Expr a = ReaderT Target Action a

-- | @Diff a@ is a /difference list/ containing values of type @a@. A difference
-- list is a list with efficient concatenation, encoded as a value @a -> a@. We
-- could use @Dual (Endo a)@ instead of @Diff a@, but the former may look scary.
newtype Diff a = Diff { fromDiff :: a -> a }

-- | @DiffExpr a@ is a computation that builds a difference list (i.e., a
-- function of type @'Action' (a -> a)@) and can read parameters of the current
-- build 'Target'.
type DiffExpr a = Expr (Diff a)

-- Note the reverse order of function composition (y . x), which ensures that
-- when two DiffExpr computations c1 and c2 are combined (c1 <> c2), then c1 is
-- applied first, and c2 is applied second.
instance Monoid (Diff a) where
    mempty = Diff id
    Diff x `mappend` Diff y = Diff $ y . x

-- | The following expressions are used throughout the build system for
-- specifying conditions ('Predicate'), lists of arguments ('Args'), 'Ways'
-- and 'Packages'.
type Predicate = Expr Bool
type Args      = DiffExpr [String]
type Packages  = DiffExpr [Package]
type Ways      = DiffExpr [Way]

-- Basic operations on expressions:
-- | Transform an expression by applying a given function.
apply :: (a -> a) -> DiffExpr a
apply = return . Diff

-- | Append something to an expression.
append :: Monoid a => a -> DiffExpr a
append x = apply (<> x)

-- | Remove given elements from a list expression.
remove :: Eq a => [a] -> DiffExpr [a]
remove xs = apply $ filter (`notElem` xs)

-- | Apply a predicate to an expression.
applyPredicate :: Monoid a => Predicate -> Expr a -> Expr a
applyPredicate predicate expr = do
    bool <- predicate
    if bool then expr else return mempty

-- | Add a single argument to 'Args'.
arg :: String -> Args
arg = append . return

-- | A convenient operator for predicate application.
class PredicateLike a where
    (?) :: Monoid m => a -> Expr m -> Expr m

infixr 3 ?

instance PredicateLike Predicate where
    (?) = applyPredicate

instance PredicateLike Bool where
    (?) = applyPredicate . return

instance PredicateLike (Action Bool) where
    (?) = applyPredicate . lift

-- | @appendSub@ appends a list of sub-arguments to all arguments starting with a
-- given prefix. If there is no argument with such prefix then a new argument
-- of the form @prefix=listOfSubarguments@ is appended to the expression.
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

-- | @appendSubD@ is similar to 'appendSub' but it extracts the list of sub-arguments
-- from the given 'DiffExpr'.
appendSubD :: String -> Args -> Args
appendSubD prefix diffExpr = fromDiffExpr diffExpr >>= appendSub prefix

filterSub :: String -> (String -> Bool) -> Args
filterSub prefix p = apply $ map filterSubstr
  where
    filterSubstr s
        | prefix `isPrefixOf` s = unwords . filter p . words $ s
        | otherwise             = s

-- | Remove given elements from a list of sub-arguments with a given prefix
-- Example: removeSub "--configure-option=CFLAGS" ["-Werror"].
removeSub :: String -> [String] -> Args
removeSub prefix xs = filterSub prefix (`notElem` xs)

-- | Interpret a given expression according to the given 'Target'.
interpret :: Target -> Expr a -> Action a
interpret = flip runReaderT

-- | Interpret a given expression by looking only at the given 'Context'.
interpretInContext :: Context -> Expr a -> Action a
interpretInContext = interpret . dummyTarget

-- | Extract an expression from a difference expression.
fromDiffExpr :: Monoid a => DiffExpr a -> Expr a
fromDiffExpr = fmap (($ mempty) . fromDiff)

-- | Interpret a given difference expression in a given environment.
interpretDiff :: Monoid a => Target -> DiffExpr a -> Action a
interpretDiff target = interpret target . fromDiffExpr

-- | Get the current build 'Context'.
getContext :: Expr Context
getContext = asks context

-- | Get the 'Stage' of the current 'Context'.
getStage :: Expr Stage
getStage = stage <$> asks context

-- | Get the 'Package' of the current 'Context'.
getPackage :: Expr Package
getPackage = package <$> asks context

-- | Get the 'Way' of the current 'Context'.
getWay :: Expr Way
getWay = way <$> asks context

-- | Get the 'Builder' for the current 'Target'.
getBuilder :: Expr Builder
getBuilder = asks builder

-- | Get the input files of the current 'Target'.
getInputs :: Expr [FilePath]
getInputs = asks inputs

-- | Run 'getInputs' and check that the result contains one input file only.
getInput :: Expr FilePath
getInput = do
    target <- ask
    getSingleton ("Exactly one input file expected in " ++ show target)
        <$> getInputs

-- | Get the files produced by the current 'Target'.
getOutputs :: Expr [FilePath]
getOutputs = asks outputs

-- | Run 'getOutputs' and check that the result contains one output file only.
getOutput :: Expr FilePath
getOutput = do
    target <- ask
    getSingleton ("Exactly one output file expected in " ++ show target)
        <$> getOutputs

-- | Extract a value from a singleton list, or raise an error if the list does
-- not contain exactly one value.
getSingleton :: String -> [a] -> a
getSingleton _ [res] = res
getSingleton msg _   = error msg
