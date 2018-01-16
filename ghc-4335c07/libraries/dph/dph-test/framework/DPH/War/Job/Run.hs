
module DPH.War.Job.Run
	(jobRun)
where
import DPH.War.Job
import DPH.War.Result
import BuildBox


-- | Run a binary
jobRun :: Job -> Build [Result]
jobRun (JobRun	testName _wayName _fileName
		mainBin mainRunOut mainRunErr)
 = do	needs mainBin
 
	-- Run the binary.
	(time, (code, strOut, strErr))
	 <- runTimedCommand 
	 $  systemTee False mainBin ""

	-- Write its output to files.
	atomicWriteFile mainRunOut strOut
	atomicWriteFile mainRunErr strErr
	
	return  $  [ResultAspect $ Time TotalWall `secs` (fromRational $ toRational time)]
 	        ++ (case code of
	                ExitFailure _ -> [ResultUnexpectedFailure]
	                _             -> [])