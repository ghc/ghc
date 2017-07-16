{-# LANGUAGE FlexibleInstances, LambdaCase #-}
-- | Convenient predicates
module Predicate (
    module Expression, stage, stage0, stage1, stage2, notStage0, builder,
    package, notPackage, input, inputs, output, outputs, way, libraryPackage
    ) where

import Base
import Expression

-- | Is the build currently in the provided stage?
stage :: Stage -> Predicate
stage s = (s ==) <$> getStage

-- | Is a particular package being built?
package :: Package -> Predicate
package p = (p ==) <$> getPackage

-- | Is a particular builder being used?
class BuilderLike a where
    builder :: a -> Predicate

-- TODO: Move this elsewhere to avoid orhpan instances
instance BuilderLike Builder where
    builder b = (b ==) <$> getBuilder

instance BuilderLike a => BuilderLike (Stage -> a) where
    builder s2b = builder . s2b =<< getStage

instance BuilderLike a => BuilderLike (CcMode -> a) where
    builder c2b = do
        b <- getBuilder
        case b of
            Cc  c _ -> builder $ c2b c
            _       -> return False

instance BuilderLike a => BuilderLike (GhcMode -> a) where
    builder c2b = do
        b <- getBuilder
        case b of
            Ghc c _ -> builder $ c2b c
            _       -> return False

instance BuilderLike a => BuilderLike (FilePath -> a) where
    builder f2b = do
        b <- getBuilder
        case b of
            Configure f -> builder $ f2b f
            _           -> return False

-- | Does any of the input files match a given pattern?
input :: FilePattern -> Predicate
input f = any (f ?==) <$> getInputs

-- | Does any of the input files match any of the given patterns?
inputs :: [FilePattern] -> Predicate
inputs = anyM input

-- | Does any of the output files match a given pattern?
output :: FilePattern -> Predicate
output f = any (f ?==) <$> getOutputs

-- | Does any of the output files match any of the given patterns?
outputs :: [FilePattern] -> Predicate
outputs = anyM output

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
