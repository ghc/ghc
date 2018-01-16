
module DPH.War.FileType
	( FileType(..)
	, classifyFile)
where
import Data.List
import System.FilePath

-- | Classification of a file that is interesting to us.
--   This is a file that we have to build, or might contain output we need to compare against.
data FileType
	= 
	-- | Some file that we don't care about.
	  FileBoring	

	-- | A Main.hs file we should compile with GHC and run.
	| FileMainHS

	-- | A Main.sh script we should run.
	| FileMainSH

	-- Check output when running.
	-- | Check the stdout of a program against this.
	| FileRunStdoutCheck

	-- | Check the stderr of a program against this.
	| FileRunStderrCheck
	deriving (Eq, Show)
	

-- | Classify a file name to decide if it's required for the war.
classifyFile :: FilePath -> FileType
classifyFile path
	-- Main files
	-- Test compile files
	| (_base, ext) <- splitExtension path
	, ext == ".hs"				= FileMainHS

	| name	== "Main.sh"			= FileMainSH
		
	-- Check output when running programs.
	| isSuffixOf ".stdout.check" name	= FileRunStdoutCheck
	| isSuffixOf ".stderr.check" name	= FileRunStderrCheck
	
	| otherwise				= FileBoring
	where	name	= takeFileName path


