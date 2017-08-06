module Expression (
    -- * Expressions
    Expr, Predicate, Args, Ways, Packages,

    -- ** Construction and modification
    expr, exprIO, append, arg, remove, (?),

    -- ** Evaluation
    interpret, interpretInContext,

    -- ** Context and Target
    Context, vanillaContext, stageContext, Target,

    -- * Convenient accessors
    getContext, getStage, getPackage, getBuilder, getOutputs, getInputs, getWay,
    getInput, getOutput, getSetting, getSettingList, getFlag,

    -- * Re-exports
    module Data.Semigroup,
    module Builder,
    module Package,
    module Stage,
    module Way
    ) where

import Data.Semigroup

import qualified Hadrian.Expression as H
import Hadrian.Expression hiding (Expr, Predicate, Args)

import Builder
import Context
import Package
import Stage
import Target
import Way

import Oracles.Config.Flag
import Oracles.Config.Setting

-- | @Expr a@ is a computation that produces a value of type @Action a@ and can
-- read parameters of the current build 'Target'.
type Expr a = H.Expr Context Builder a

-- | The following expressions are used throughout the build system for
-- specifying conditions ('Predicate'), lists of arguments ('Args'), 'Ways'
-- and 'Packages'.
type Predicate = H.Predicate Context Builder
type Args      = H.Args      Context Builder
type Packages  = Expr [Package]
type Ways      = Expr [Way]

-- Basic operations on expressions:

-- | Append something to an expression.
append :: a -> Expr a
append = pure

-- | Get the 'Stage' of the current 'Context'.
getStage :: Expr Stage
getStage = stage <$> getContext

-- | Get the 'Package' of the current 'Context'.
getPackage :: Expr Package
getPackage = package <$> getContext

-- | Get the 'Way' of the current 'Context'.
getWay :: Expr Way
getWay = way <$> getContext

getSetting :: Setting -> Expr String
getSetting = expr . setting

getSettingList :: SettingList -> Expr [String]
getSettingList = expr . settingList

getFlag :: Flag -> Predicate
getFlag = expr . flag
