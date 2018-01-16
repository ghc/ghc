{-# LANGUAGE PatternGuards #-}

-- | Build stages for building cabal packages and registering them with a GHC build
module BuildLibs
--	(libsBuild)
where
import Config
import BuildBox
import Control.Monad

-- LibSpec ----------------------------------------------------------------------------------------
data LibSpec 
	-- | Use "cabal install" to download and install a package.
	= Cabal String

	-- | Build and install an existing package source checkout in a local dir.
	| Repo  FilePath

	-- | Use darcs to get a repo that contains subpackages.
	| GetSub  String [LibSub]
	deriving (Eq, Show, Read)


-- | Packages that can live inside the main repo.
data LibSub
	= Install 	String
	| Build		String
	deriving (Eq, Show, Read)


-- | Whether we should run "cabal update" before installing this library.
specNeedsCabalUpdate :: LibSpec -> Bool
specNeedsCabalUpdate spec
 = case spec of
	Cabal{}		-> True
	Repo{}		-> False
	GetSub{}	-> False

-- | Whether installation of this library requires the darcs binary.
specNeedsDarcs :: LibSpec -> Bool
specNeedsDarcs spec
 = case spec of
	Cabal{}		-> False
	Repo{}		-> False
	GetSub{}	-> True
	
loadLibSpecs :: String -> [LibSpec]
loadLibSpecs str
	= read
	$ "[" ++ map (\c -> if c == '\'' then '"' else c) str ++ "]"

breaks :: (a -> Bool) -> [a] -> [[a]]
breaks _ []	= []
breaks p xx@(x1 : xs)
 	| p x1
 	= [x1] : breaks p xs

 	| otherwise
 	= case break p xx of
		([], y)	 -> [y]
		(x,  []) -> [x]
		(x, y)	 -> x : breaks p y


-- Building Libraries -----------------------------------------------------------------------------
-- | Build some libraries.
libsBuild :: Config -> Build ()
libsBuild config
 | Just libspec	<- configLibs config
 = do	outLn "* Building base libraries."

	let specs	= loadLibSpecs libspec

	outCheckOk "  - Checking for cabal."
	 $ HasExecutable "cabal"

	when (or $ map specNeedsDarcs specs)
	 $ outCheckOk "  - Checking for darcs."
	 $ HasExecutable "darcs"
	
	when (or $ map specNeedsCabalUpdate specs)
	 $ do	outLn      "  - Updating cabal package database."
		qssystem $ "cabal update"
		
	mapM_ (libBuild config) specs
	
 | otherwise
 = error "libsBuild: need spec"		


-- | Gettin' and Buildin' packages.
libBuild :: Config -> LibSpec -> Build ()
libBuild config spec
 = case spec of
	Cabal pkg
	 -> do	outLn 	$ "  - Installing " ++ pkg
		ssystem 	$ "cabal install " ++ pkg
		 		++ " --reinstall"
				++ " --with-compiler=" ++ configWithGhc config
				++ " --with-hc-pkg="   ++ configWithGhcPkg config
		outBlank

 	Repo dir
	 -> do	outLn	$ "  - Building " ++ dir
		inDir dir
		 $ do	-- pkgClean
			pkgConfigure config
			pkgBuild
			pkgInstall
			outBlank

	GetSub url subs
	 -> do	let Just scratchDir	= configScratchDir config
		let pkgName : _		= reverse $ breaks (== '/') url
		inDir scratchDir
		 $ do	clobberDir pkgName
			outLn	$ "  - Getting package " ++ url
			ssystem $ "darcs get " ++ url
		
			inDir pkgName 
			 $ mapM_ (libBuildSub config) subs
		

-- | Building sub packages in a single repo
libBuildSub :: Config -> LibSub -> Build ()
libBuildSub config sub
 = case sub of
	Install pkg
	 -> inDir pkg
	 $ do	pkgClean
		pkgConfigure config
	 	pkgBuild
		pkgInstall
		
	Build pkg
	 -> inDir pkg
	 $ do	pkgClean
		pkgConfigure config
		pkgBuild
		

-- Pkg Commands -----------------------------------------------------------------------------------
pkgConfigure :: Config -> Build ()
pkgConfigure config 
 = do	setupFile	<- findSetupFile
	ssystem $ "runghc " ++ setupFile ++ " configure"
		++ " --user"
		++ " --with-compiler=" ++ configWithGhc config
		++ " --with-hc-pkg="   ++ configWithGhcPkg config


pkgClean :: Build ()
pkgClean
 = do	setupFile	<- findSetupFile
	ssystem $ "runghc " ++ setupFile ++ " clean"


pkgBuild :: Build ()
pkgBuild
 = do	setupFile	<- findSetupFile
	ssystem $ "runghc " ++ setupFile ++ " build"


pkgInstall :: Build ()
pkgInstall 
 = do	setupFile	<- findSetupFile
	ssystem $ "runghc " ++ setupFile ++ " install"


findSetupFile :: Build FilePath
findSetupFile
 = do	hasSetupHs	<- test $ HasFile "Setup.hs"
	hasSetupLhs	<- test $ HasFile "Setup.lhs"
			
	setupFile	<- if      hasSetupHs  then return "Setup.hs"
			   else if hasSetupLhs then return "Setup.lhs"
			   else throw $ ErrorOther "No Setup.(l)hs file for library"

	return setupFile
