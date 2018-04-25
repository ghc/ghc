
module DPH.War.Job.Shell
	(jobShell)
where
import DPH.War.Job
import DPH.War.Result
import BuildBox


-- | Run a binary
jobShell :: Job -> Build [Result]
jobShell (JobShell testName _wayName
		mainSH sourceDir scratchDir
		mainRunOut mainRunErr
		shouldSucceed)
 = do	needs mainSH
	ensureDir scratchDir
	
	-- Run the binary.
	(time, (code, strOut, strErr))
	 <- runTimedCommand 
	 $  systemTee False 
		("sh " ++ mainSH ++ " " ++ sourceDir ++ " " ++ scratchDir) 
		""
		
	-- Write its output to files.
	atomicWriteFile mainRunOut strOut
	atomicWriteFile mainRunErr strErr
		
	return  $  [ ResultAspect $ Time TotalWall `secs` (fromRational $ toRational time)]

		-- check for unexpected failure
		++ (if shouldSucceed && code /= ExitSuccess 
			then [ResultUnexpectedFailure] else [])

		-- check for unexpected success
		++ (if not shouldSucceed && code == ExitSuccess
			then [ResultUnexpectedSuccess] else [])		 	
