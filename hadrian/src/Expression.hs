{-# LANGUAGE FlexibleContexts #-}

module Expression (
    -- * Expressions
    Expr, Predicate, Args, Ways,

    -- ** Construction and modification
    expr, exprIO, arg, remove, cabalFlag,

    -- ** Predicates
    (?), stage, stage0, stage1, stage2, notStage0, buildingCompilerStage,
    buildingCompilerStage', threadedBootstrapper,
     package, notPackage, packageOneOf,
     libraryPackage, builder, way, input, inputs, output, outputs,

    -- ** Evaluation
    interpret, interpretInContext,

    -- * Convenient accessors
    getBuildRoot, getContext, getOutputs, getInputs,
    getInput, getOutput, getContextData,

    -- * Re-exports
    module Base,
    module Builder,
    module Context,
    ) where

import Base
import Builder
import Context hiding (stage, package, way)
import Expression.Type
import Oracles.Flag
import Hadrian.Expression hiding (Expr, Predicate, Args)
import Hadrian.Haskell.Cabal.Type
import Hadrian.Oracles.Cabal

-- | Get values from a configured cabal stage.
getContextData :: (ContextData -> a) -> Expr a
getContextData key = do
    contextData <- expr . readContextData =<< getContext
    return $ key contextData

-- | Is the build currently in the provided stage?
stage :: Stage -> Predicate
stage s = (s ==) <$> getStage

-- | Is a particular package being built?
package :: Package -> Predicate
package p = (p ==) <$> getPackage

packageOneOf :: [Package] -> Predicate
packageOneOf ps = (`elem` ps) <$> getPackage


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

instance BuilderPredicate a => BuilderPredicate (TestMode -> a) where
    builder f = do
        b <- getBuilder
        case b of
            Testsuite mode -> builder (f mode)
            _              -> return False

-- | Is the current build 'Way' equal to a certain value?
way :: Way -> Predicate
way w = (w ==) <$> getWay

{-
Note [Stage Names]
~~~~~~~~~~~~~~~~~~
Code referring to specific stages can be a bit tricky. In Hadrian, the stages
have the same names they carried in the autoconf build system, but they are
often referred to by the stage used to construct them. For example, the stage 1
artifacts will be placed in _build/stage0, because they are constructed by the
stage 0 compiler. The stage predicates in this module behave the same way,
'stage0' will return 'True' while stage 0 is being used to build the stage 1
compiler.
-}

-- | Is the build currently in stage 0?
stage0 :: Predicate
stage0 =  p <$> getStage
  where
    p (Stage0 {}) = True
    p _ = False

-- | Is the build currently in stage 1?
stage1 :: Predicate
stage1 = stage Stage1

-- | Is the build currently in stage 2?
stage2 :: Predicate
stage2 = stage Stage2

-- | Is the build /not/ in stage 0 right now?
notStage0 :: Predicate
notStage0 = notM Expression.stage0

-- | Are we currently building a compiler for a particular stage?
buildingCompilerStage :: Stage -> Predicate
buildingCompilerStage s = buildingCompilerStage' (== s)

-- | Like 'buildingCompilerStage', but lifts an arbitrary predicate on 'Stage',
-- which is useful for checking flavour fields like 'ghcProfiled' and
-- 'ghcDebugged'.
buildingCompilerStage' :: (Stage -> Bool) -> Predicate
buildingCompilerStage' f = f . succStage <$> getStage


-- | Whether or not the bootstrapping compiler provides a threaded RTS. We need
--   to know this when building stage 1, since stage 1 links against the
--   compiler's RTS ways. See Note [Linking ghc-bin against threaded stage0 RTS]
--   in Settings.Packages for details.
threadedBootstrapper :: Predicate
threadedBootstrapper = expr (flag BootstrapThreadedRts)

-- | Is a certain package /not/ built right now?
notPackage :: Package -> Predicate
notPackage = notM . package

-- | Is a library package currently being built?
libraryPackage :: Predicate
libraryPackage = isLibrary <$> getPackage

-- | Either @-flagName@ or @flagName@, depending upon a predicate.
-- For use in @Cabal Flags@ argument lists.
cabalFlag :: ToPredicate p Context Builder => p -> String -> Args
cabalFlag pred flagName = do
    ifM (toPredicate pred) (arg flagName) (arg $ "-"<>flagName)

infixr 3 `cabalFlag`
