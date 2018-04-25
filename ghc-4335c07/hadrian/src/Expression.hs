module Expression (
    -- * Expressions
    Expr, Predicate, Args, Ways,

    -- ** Construction and modification
    expr, exprIO, arg, remove,

    -- ** Predicates
    (?), stage, stage0, stage1, stage2, notStage0, package, notPackage,
    libraryPackage, builder, way, input, inputs, output, outputs,

    -- ** Evaluation
    interpret, interpretInContext,

    -- * Convenient accessors
    getBuildRoot, getContext, getPkgData, getPkgDataList, getOutputs, getInputs,
    getInput, getOutput,

    -- * Re-exports
    module Base,
    module Builder,
    module Context,
    module GHC
    ) where

import qualified Hadrian.Expression as H
import Hadrian.Expression hiding (Expr, Predicate, Args)

import Base
import Builder
import GHC
import Context hiding (stage, package, way)
import Oracles.PackageData

-- | @Expr a@ is a computation that produces a value of type @Action a@ and can
-- read parameters of the current build 'Target'.
type Expr a = H.Expr Context Builder a

-- | The following expressions are used throughout the build system for
-- specifying conditions ('Predicate'), lists of arguments ('Args'), 'Ways'
-- and 'Packages'.
type Predicate = H.Predicate Context Builder
type Args      = H.Args      Context Builder
type Ways      = Expr [Way]

-- | Get a value from the @package-data.mk@ file of the current context.
getPkgData :: (FilePath -> PackageData) -> Expr String
getPkgData key = expr . pkgData . key =<< getBuildPath

-- | Get a list of values from the @package-data.mk@ file of the current context.
getPkgDataList :: (FilePath -> PackageDataList) -> Expr [String]
getPkgDataList key = expr . pkgDataList . key =<< getBuildPath

-- | Is the build currently in the provided stage?
stage :: Stage -> Predicate
stage s = (s ==) <$> getStage

-- | Is a particular package being built?
package :: Package -> Predicate
package p = (p ==) <$> getPackage

-- | This type class allows the user to construct both precise builder
-- predicates, such as @builder (Ghc CompileHs Stage1)@, as well as predicates
-- covering a set of similar builders. For example, @builder (Ghc CompileHs)@
-- matches any stage, and @builder Ghc@ matches any stage and any GHC mode.
class BuilderPredicate a where
    -- | Is a particular builder being used?
    builder :: a -> Predicate

instance BuilderPredicate Builder where
    builder b = (b ==) <$> getBuilder

instance BuilderPredicate a => BuilderPredicate (Stage -> a) where
    builder f = builder . f =<< getStage

instance BuilderPredicate a => BuilderPredicate (CcMode -> a) where
    builder f = do
        b <- getBuilder
        case b of
            Cc  c _ -> builder (f c)
            _       -> return False

instance BuilderPredicate a => BuilderPredicate (GhcMode -> a) where
    builder f = do
        b <- getBuilder
        case b of
            Ghc c _ -> builder (f c)
            _       -> return False

instance BuilderPredicate a => BuilderPredicate (FilePath -> a) where
    builder f = do
        b <- getBuilder
        case b of
            Configure path -> builder (f path)
            _              -> return False

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
