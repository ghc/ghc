
module TH_Depends_Dir_External where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib
import System.Directory (listDirectory, doesDirectoryExist)

-- | This function checks the contents of a dependent directory and a non-dependent directory.
-- So its value will change if the contents of the dependent directory change.
-- It will not change if the contents of the non-dependent directory change.
checkDirectoryContent :: Q Exp
checkDirectoryContent = do
  let dependentDir = "TRIGGER_RECOMP"
  let nonDependentDir = "DONT_TRIGGER_RECOMP"

  -- this will error when dependentDir does not exist
  -- which is the last thing we test for in the Makefile
  exists <- qRunIO $ doesDirectoryExist dependentDir
  dep_str <- if exists 
    then do
      qAddDependentDirectory dependentDir
      l <- qRunIO $ listDirectory dependentDir
      case l of
        [] -> pure "dependent directory is empty"
        _  -> pure "dependent directory is non-empty"
    else do
      -- note that once we are here we no longer depend on the directory
      -- so no more recompilation will happen.
      pure "dependent directory does not exist" 

  -- Now the part that shouldn't trigger recompilation.
  -- This is somewhat of a sanity check, if we change nonDependentDir
  -- and it triggers recompilation, then something must be wrong 
  -- with the recompilation logic.
  non_deps <- qRunIO $ listDirectory nonDependentDir
  non_dep_str <- case non_deps of
    [] -> pure "non-dependent directory is empty."
    _  -> pure "non-dependent directory is non-empty."
    
  -- Return the result as a string expression
  stringE $ dep_str ++ ", " ++ non_dep_str
