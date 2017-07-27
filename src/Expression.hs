{-# LANGUAGE DeriveFunctor, FlexibleInstances, LambdaCase #-}
module Expression (
    -- * Expressions
    Expr, expr, exprIO,
    -- ** Operators
    append, arg, remove,
    -- ** Evaluation
    interpret, interpretInContext,
    -- ** Predicates
    Predicate, (?), applyPredicate,
    -- ** Common expressions
    Args, Ways, Packages,
    -- ** Context and Target
    Context, vanillaContext, stageContext, Target, dummyTarget,

    -- * Convenient accessors
    getContext, getStage, getPackage, getBuilder, getOutputs, getInputs, getWay,
    getInput, getOutput, getSingleton, getSetting, getSettingList, getFlag,
    getTopDirectory,

    -- * Re-exports
    module Data.Monoid,
    module Builder,
    module Package,
    module Stage,
    module Way
    ) where

import Control.Monad.Trans.Reader
import Control.Monad.Trans
import Data.Monoid

import Base
import Builder
import Context
import Package
import Stage
import Target
import Way

import Oracles.Config.Flag
import Oracles.Config.Setting
import Oracles.Path

-- | @Expr a@ is a computation that produces a value of type @Action a@ and can
-- read parameters of the current build 'Target'.
newtype Expr a = Expr (ReaderT Target Action a) deriving Functor

expr :: Action a -> Expr a
expr = Expr . lift

exprIO :: IO a -> Expr a
exprIO = Expr . liftIO

instance Monoid a => Monoid (Expr a) where
    mempty                    = Expr $ return mempty
    mappend (Expr x) (Expr y) = Expr $ (<>) <$> x <*> y

instance Applicative Expr where
    pure  = Expr . pure
    (<*>) = ap

instance Monad Expr where
    return       = pure
    Expr e >>= f = Expr $ do
        re <- e
        let Expr rf = f re
        rf

-- | The following expressions are used throughout the build system for
-- specifying conditions ('Predicate'), lists of arguments ('Args'), 'Ways'
-- and 'Packages'.
type Predicate = Expr Bool
type Args      = Expr [String]
type Packages  = Expr [Package]
type Ways      = Expr [Way]

-- Basic operations on expressions:

-- | Append something to an expression.
append :: Monoid a => a -> Expr a
append = Expr . return

-- | Remove given elements from a list expression.
remove :: Eq a => [a] -> Expr [a] -> Expr [a]
remove xs e = filter (`notElem` xs) <$> e

-- | Apply a predicate to an expression.
applyPredicate :: Monoid a => Predicate -> Expr a -> Expr a
applyPredicate predicate expr = do
    bool <- predicate
    if bool then expr else mempty

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
    (?) = applyPredicate . Expr . return

instance PredicateLike (Action Bool) where
    (?) = applyPredicate . expr

-- | Interpret a given expression according to the given 'Target'.
interpret :: Target -> Expr a -> Action a
interpret target (Expr e) = runReaderT e target

-- | Interpret a given expression by looking only at the given 'Context'.
interpretInContext :: Context -> Expr a -> Action a
interpretInContext = interpret . dummyTarget

-- | Get the current build 'Context'.
getContext :: Expr Context
getContext = Expr $ asks context

-- | Get the 'Stage' of the current 'Context'.
getStage :: Expr Stage
getStage = Expr $ stage <$> asks context

-- | Get the 'Package' of the current 'Context'.
getPackage :: Expr Package
getPackage = Expr $ package <$> asks context

-- | Get the 'Way' of the current 'Context'.
getWay :: Expr Way
getWay = Expr $ way <$> asks context

-- | Get the 'Builder' for the current 'Target'.
getBuilder :: Expr Builder
getBuilder = Expr $ asks builder

-- | Get the input files of the current 'Target'.
getInputs :: Expr [FilePath]
getInputs = Expr $ asks inputs

-- | Run 'getInputs' and check that the result contains one input file only.
getInput :: Expr FilePath
getInput = Expr $ do
    target <- ask
    getSingleton ("Exactly one input file expected in " ++ show target) <$> asks inputs

-- | Get the files produced by the current 'Target'.
getOutputs :: Expr [FilePath]
getOutputs = Expr $ asks outputs

-- | Run 'getOutputs' and check that the result contains one output file only.
getOutput :: Expr FilePath
getOutput = Expr $ do
    target <- ask
    getSingleton ("Exactly one output file expected in " ++ show target) <$> asks outputs

-- | Extract a value from a singleton list, or raise an error if the list does
-- not contain exactly one value.
getSingleton :: String -> [a] -> a
getSingleton _ [res] = res
getSingleton msg _   = error msg

getSetting :: Setting -> Expr String
getSetting = expr . setting

getSettingList :: SettingList -> Expr [String]
getSettingList = expr . settingList

getFlag :: Flag -> Predicate
getFlag = expr . flag

getTopDirectory :: Expr FilePath
getTopDirectory = expr topDirectory
