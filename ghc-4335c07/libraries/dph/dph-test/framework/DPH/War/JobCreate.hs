
module DPH.War.JobCreate
	( dispatchJob
	, createJobs)
where
import DPH.War.Config
import DPH.War.Job.Compile
import DPH.War.Job.Run
import DPH.War.Job.Shell
import DPH.War.Job.Diff
import DPH.War.Job
import DPH.War.Way
import DPH.War.FileType
import DPH.War.Result
import System.FilePath
import qualified Data.Set	as Set
import Data.Set			(Set)
import BuildBox


dispatchJob :: Job -> Build [Result]
dispatchJob job
 = case job of
	JobCompile{}	-> jobCompile	job
	JobRun{}	-> jobRun	job
	JobShell{}	-> jobShell	job
	JobDiff{}	-> jobDiff 	job



-- | Create some jobs based on a test file.
--   There can be several jobs associated with a test, depending on what comparison files
--   are available. If any of the jobs in a test fail unexpectedly then the other jobs
--   in that test are ignored.
createJobs
	:: Config
	-> Way 			-- ^ Name of the way we're compiling, used to create build dirs.
	-> Set FilePath		-- ^ All files available. The jobs created for a particular file
				--   like Main.ds depend on the existance of others like Main.error.check
	-> FilePath		-- ^ File we want to create jobs for.
	-> [Job]

createJobs config way allFiles filePath
 = let	fileName	= takeFileName filePath
	sourceDir	= takeDirectory  filePath
	buildDir	= sourceDir </> "war-" ++ wayName way
	testName	= sourceDir
   in	case classifyFile filePath of
	 -- Ignore boring files.
	 FileBoring			-> []

	 -- Run stdout and stderr diffs are handled by the FileMainDS rule.
	 FileRunStdoutCheck		-> []
	 FileRunStderrCheck		-> []


	 -- For .hs files, build them with GHC
	 FileMainHS
	  -> let testBin	  = buildDir  </> replaceExtension fileName ".bin"
		 testCompStdout   = buildDir  </> replaceExtension fileName ".compile.stdout"
		 testCompStderr	  = buildDir  </> replaceExtension fileName ".compile.stderr"
		 testCompDiff     = buildDir  </> replaceExtension fileName ".compile.stderr.diff"

		 testRunStdout	  = buildDir  </> replaceExtension fileName ".run.stdout"
		 testRunStderr	  = buildDir  </> replaceExtension fileName ".run.stderr"

		 testStdoutCheck  = sourceDir </> replaceExtension fileName ".stdout.check"
		 testStdoutDiff   = buildDir  </> replaceExtension fileName ".stdout.diff"
		 shouldDiffStdout = Set.member testStdoutCheck allFiles

		 testStderrCheck  = sourceDir </> replaceExtension fileName ".stderr.check"
		 testStderrDiff   = buildDir  </> replaceExtension fileName ".stderr.diff"
		 shouldDiffStderr = Set.member testStderrCheck allFiles

		 -- compile the .hs into a .bin
		 compile 	= JobCompile 	testName (wayName way) filePath
		 				(wayOptsComp way)
						buildDir testCompStdout testCompStderr
						testBin

		 -- run the binary
		 run		= JobRun  	testName (wayName way) filePath testBin
						testRunStdout testRunStderr

		 -- diff the stdout of the run
		 diffStdout	= JobDiff	testName (wayName way) testStdoutCheck
						testRunStdout testStdoutDiff

		 -- diff the stderr of the run
		 diffStderr	= JobDiff	testName (wayName way) testStderrCheck
						testRunStderr testStderrDiff

	     in	[compile, run]
			++ (if shouldDiffStdout then [diffStdout] else [])
			++ (if shouldDiffStderr then [diffStderr] else [])

