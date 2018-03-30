module Context.Paths where

import Base
import Context.Type
import Hadrian.Expression

-- | The directory to the current stage
stageDir :: Context -> FilePath
stageDir Context {..} = stageString stage

-- | The path to the current stage
stagePath :: Context -> Action FilePath
stagePath context = buildRoot <&> (-/- stageDir context)

getStagePath :: Expr Context b FilePath
getStagePath = expr . stagePath =<< getContext

-- | The directory in 'buildRoot' containing build artifacts of a given 'Context'.
contextDir :: Context -> FilePath
contextDir Context {..} = stageString stage -/- pkgPath package

-- | Path to the context directory, containing the "build folder"
contextPath :: Context -> Action FilePath
contextPath context = buildRoot <&> (-/- contextDir context)

getContextPath :: Expr Context b FilePath
getContextPath = expr . contextPath =<< getContext

-- | The directory in 'buildRoot' containing the object artifacts.
buildDir :: Context -> FilePath
buildDir context = contextDir context -/- "build"

-- | Path to the directory containing build artifacts of a given 'Context'.
buildPath :: Context -> Action FilePath
buildPath context = buildRoot <&> (-/- buildDir context)

-- | Get the build path of the current 'Context'.
getBuildPath :: Expr Context b FilePath
getBuildPath = expr . buildPath =<< getContext
