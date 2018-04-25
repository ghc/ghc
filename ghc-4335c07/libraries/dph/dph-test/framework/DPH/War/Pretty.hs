
module DPH.War.Pretty
	( pprJobResult
	, diagnoseJobResults) 
where
import DPH.War.Job
import DPH.War.Result
import Util.Terminal.VT100
import BuildBox
import System.FilePath
import Data.List


pprJobResult :: Int -> Bool -> FilePath	-> Job -> [Result] -> Doc
pprJobResult width useColor workingDir job results
 = snd $ diagnoseJobResults width useColor workingDir job results
 

-- | Diagnose whether some job results represent success or failure, 
--   and generate a Doc describing them.
diagnoseJobResults
	:: Int 		-- ^ Width of reports.
	-> Bool 	-- ^ Whether to use color in reports.
	-> FilePath	-- ^ Working directory to show test files relative to.
	-> Job 		-- ^ Job to pretty print.
	-> [Result]	-- ^ Returned results of job.
	-> (Bool, Doc)
	
diagnoseJobResults width useColor workingDir job aspects
 = let	

        pprResult strFile strAction colorResult docResult 
	 = let  -- Don't show the output dir in the file path, 
                -- show it as if the output files were in the source dir.
                wayDir      = "war-" ++ jobWayName job
                pathParts   = splitPath $ makeRelative workingDir strFile
                pathParts'  = delete (wayDir ++ "/") pathParts
                strFile'    = joinPath pathParts'

	   in   padL width (text strFile') 
	         <> (padL 10 $ text $ jobWayName job)
	         <> (padL 10 $ text strAction)
	         <> pprAsColor useColor colorResult (docResult)

   in case job of

	-- Compile ----------------------------
	JobCompile{}
	 | or $ map isResultUnexpectedFailure aspects
	 -> (False, pprResult (jobFile job) "compile"
		        Red 	(text "failed"))

	 | Just time	<- takeResultTime aspects
	 -> (True, pprResult (jobFile job) "compile" 
		        Blue	(text "time" <> (parens $ padR 7 $ ppr time)))


	-- Shell --------------------------------
	JobShell{}
	 | or $ map isResultUnexpectedFailure aspects
	 -> (False, pprResult (jobShellSource job) "shell"
		        Red 	(text "failed"))

	 | or $ map isResultUnexpectedSuccess aspects
	 -> (False, pprResult (jobShellSource job) "shell"
		        Red 	(text "unexpected success"))

	 | Just time	<- takeResultTime aspects
	 -> (True, pprResult (jobShellSource job) "shell"
		        Black 	(text "time" <> (parens $ padR 7 $ ppr time)))
		
	-- Run ----------------------------------
	-- run was ok.
	JobRun{}
	 | or $ map isResultUnexpectedFailure aspects
	 -> (False, pprResult (jobFileBin job) "run"
		        Red 	(text "failed"))

	 | Just time	<- takeResultTime aspects
	 -> (True, pprResult (jobFileBin job) "run"
		        Green	(text "time" <> (parens $ padR 7 $ ppr time)))

	
	
	-- Diff ---------------------------------
	-- diffed files were different.
	JobDiff{}
   	 | Just _		<- takeResultDiff aspects
	 -> (False, pprResult (jobFileOut job) "diff"
		        Red	(text "failed"))

	-- diffed files were identical, all ok.
	JobDiff{}
	 -> (True, pprResult (jobFileOut job) "diff"
		        Black	(text "ok"))


pprAsColor :: Bool -> Color -> Doc -> Doc
pprAsColor True color doc
	=  text (setMode [Bold, Foreground color]) 
	<> doc
	<> text (setMode [Reset])

pprAsColor False _ doc
	= doc


