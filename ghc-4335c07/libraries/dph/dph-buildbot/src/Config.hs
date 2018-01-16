{-# LANGUAGE PatternGuards #-}

module Config 
	( Config(..)
	, defaultMailer
	, slurpConfig)
where
import BuildBox
import Args
import System.Console.ParseArgs	hiding (args)
import System.Directory
import Control.Monad
import Data.Maybe
import qualified Data.Traversable         as T

-- Config -----------------------------------------------------------------------------------------
-- | Buildbot command line configuration.
data Config
	= Config
	{ configVerbose		:: Bool

	-- Building GHC and its libs
	, configScratchDir	:: Maybe String
	, configGhcUnpack	:: Maybe FilePath
	, configGhcBuild	:: Maybe FilePath

	-- Depending on the above options, these will get filled in with 
	-- paths to the actual ghc and ghc-pkg binaries we should be using.
	, configWithGhc		:: FilePath
	, configWithGhcPkg	:: FilePath

	-- What libraries we should install into the GHC tree.
	--   TODO: If we had a better options parser we wouldn't have to hack everything into the same arg.
	, configLibs		:: Maybe String

	-- Test stages ------------------------------------
        -- What libraries to test.
	, configDoTestDPH	:: Maybe FilePath
	, configDoTestRepa	:: Maybe FilePath
	, configDoTestNoSlow	:: Maybe FilePath

	-- Testing config ---------------------------------
	, configIterations	:: Int
	, configAgainstResults	:: Maybe FilePath 
	, configSwingFraction	:: Maybe Double

	-- What do with the results -----------------------
	, configWriteResults	:: Maybe (FilePath, Bool)
	, configMailFromTo	:: Maybe (String, String)
	, configMailFailTo	:: Maybe String
	, configMailBanner	:: Maybe String
	, configMailBranchName  :: Maybe String
	, configUploadResults	:: Maybe String }
	deriving Show



-- Hard Coded -------------------------------------------------------------------------------------
-- Hard coded config.
--	Can't be bothered turning these into cmd line args.
defaultMailer :: Mailer
defaultMailer
	= MailerMSMTP
	{ mailerPath		= "msmtp"
	, mailerPort		= Just 587 }
	

-- Slurp ------------------------------------------------------------------------------------------
-- | Slurp configuration information from the command line arguments.
slurpConfig :: Args BuildArg -> IO Config
slurpConfig args 
 = do 	let Just iterations	= getArg args ArgTestIterations

	mScratchDir	<- maybe
				(return Nothing)
				(liftM Just . canonicalizePath)
				(getArg args ArgScratchDir)	
	
	-- If we've been given the --ghc-use flag then use that compiler first off.
	-- Otherwise use whatever is in the PATH.
	withGhc		<- maybe 
				(return "ghc")
				(\dir -> canonicalizePath $ dir ++ "/inplace/bin/ghc-stage2")
				(getArg args ArgGhcUse)
				
	withGhcPkg	<- maybe
				(return "ghc-pkg")
				(\dir -> canonicalizePath $ dir ++ "/inplace/bin/ghc-pkg")
				(getArg args ArgGhcUse)


	-- If we're supposed to unpack a GHC snapshot tarball, 
	-- 	then canonicalize its path.
	let ghcUnpack'
		| Just path	<- getArg args ArgGhcUnpack
		= liftM Just $ canonicalizePath path
		
		| Just path	<- getArg args ArgGhcUnpackBuild
		= liftM Just $ canonicalizePath path
		
		| otherwise
		= return Nothing

	ghcUnpack	<- ghcUnpack'
	
	-- If we're supposed to build a GHC, then determine where the build tree will be.
	let ghcBuild'
		| Just path	<- getArg args ArgGhcBuild
		= liftM Just $ canonicalizePath path
		
		| Just _	<- (getArg args ArgGhcUnpackBuild) :: Maybe String
		, scratchDir	<- fromMaybe (error "must specifiy --scratch with --ghc-unpack-build") mScratchDir
		= return $ Just $ scratchDir ++ "/ghc-head"
		
		| otherwise
		= return Nothing
	
	ghcBuild	<- ghcBuild'

        -- Testing --------------------------------------------------
        doTestDPH
         <- T.mapM canonicalizePath
         $  if gotArg args ArgDoTestDPH
             then msum  [ getArg args ArgUseDPH
                        , liftM (++ "/ghc-head/libraries/dph") mScratchDir
                        , Just $ error "must specify --scratch or --use-dph with --test-dph" ]
             else Nothing

        doTestRepa
         <- T.mapM canonicalizePath
         $  if gotArg args ArgDoTestRepa
             then msum  [ getArg args ArgUseDPH
                        , liftM (++ "/repa-head") mScratchDir
                        , Just $ error "must specify --scratch or --use-repa with --test-repa" ]
             else Nothing

        doTestNoSlow
         <- T.mapM canonicalizePath
         $  if gotArg args ArgDoTestNoSlow
             then msum  [ getArg args ArgUseDPH
                        , liftM (++ "/NoSlow-head") mScratchDir
                        , Just $ error "must specify --scratch or --use-noslow with --test-noslow" ]
             else Nothing
             

	-- Build the final config -----------------------------------
    	return $ Config
		{ configVerbose		= gotArg args ArgVerbose
		, configScratchDir	= mScratchDir
		, configGhcUnpack	= ghcUnpack
		, configGhcBuild	= ghcBuild
		, configWithGhc 	= withGhc
		, configWithGhcPkg	= withGhcPkg
		, configLibs		= getArg args ArgLibs

		-- Testing stages
		, configDoTestDPH	= doTestDPH
		, configDoTestRepa	= doTestRepa
		, configDoTestNoSlow	= doTestNoSlow

		-- Testing config.
		, configIterations	= iterations 
		, configWriteResults	= let result
						| Just name	<- getArg args ArgWriteResultsStamped
						= Just (name, True)

						| Just name	<- getArg args ArgWriteResults
						= Just (name, False)
						
						| otherwise
						= Nothing
					  in  result
		
		, configUploadResults	= getArg args ArgUploadResults				
		, configAgainstResults	= getArg args ArgAgainstResults
		, configSwingFraction	= getArg args ArgSwingFraction

		-- TODO: check we have both args
		, configMailFromTo	= let result	
						| Just from	<- getArg args ArgMailFrom
						, Just to	<- getArg args ArgMailTo
						= Just (from, to)
							
						| otherwise
						= Nothing
				  		in	result

		, configMailFailTo	= getArg args ArgMailFailTo
		, configMailBanner	= getArg args ArgMailBanner
		, configMailBranchName  = getArg args ArgMailBranchName
		}
