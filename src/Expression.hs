module Expression (
    -- * Expressions
    Expr, Predicate, Args, Ways, Packages,

    -- ** Construction and modification
    expr, exprIO, arg, remove,

    -- ** Predicates
    (?), stage, stage0, stage1, stage2, notStage0, package, notPackage,
    libraryPackage, way, input, inputs, output, outputs,

    -- ** Evaluation
    interpret, interpretInContext,

    -- ** Context and Target
    Context, vanillaContext, stageContext, Target,

    -- * Convenient accessors
    getContext, getStage, getPackage, getBuilder, getOutputs, getInputs, getWay,
    getInput, getOutput, getSetting, getSettingList, getStagedSettingList,

    -- * Re-exports
    module Data.Semigroup,
    module Builder,
    module Package,
    module Stage,
    module Way
    ) where

import Control.Monad.Extra
import Data.Semigroup

import qualified Hadrian.Expression as H
import Hadrian.Expression hiding (Expr, Predicate, Args)

import Builder
import Context (Context, vanillaContext, stageContext, getStage, getPackage, getWay)
import Package
import Stage
import Target hiding (builder, inputs, outputs)
import Way

import Oracles.Setting

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

-- | Get a configuration setting.
getSetting :: Setting -> Expr String
getSetting = expr . setting

-- | Get a list of configuration settings.
getSettingList :: SettingList -> Args
getSettingList = expr . settingList

-- | Get a list of configuration settings for the current stage.
getStagedSettingList :: (Stage -> SettingList) -> Args
getStagedSettingList f = getSettingList . f =<< getStage

-- | Is the build currently in the provided stage?
stage :: Stage -> Predicate
stage s = (s ==) <$> getStage

-- | Is a particular package being built?
package :: Package -> Predicate
package p = (p ==) <$> getPackage

-- | Is the current build 'Way' equal to a certain value?
way :: Way -> Predicate
way w = (w ==) <$> getWay

-- | Is the build currently in stage 0?
stage0 :: Predicate
stage0 = stage Stage0

-- | Is the build currently in stage 1?
stage1 :: Predicate
stage1 = stage Stage1

-- | Is the build currently in stage 2?
stage2 :: Predicate
stage2 = stage Stage2

-- | Is the build /not/ in stage 0 right now?
notStage0 :: Predicate
notStage0 = notM stage0

-- | Is a certain package /not/ built right now?
notPackage :: Package -> Predicate
notPackage = notM . package

-- | Is a library package currently being built?
libraryPackage :: Predicate
libraryPackage = isLibrary <$> getPackage
