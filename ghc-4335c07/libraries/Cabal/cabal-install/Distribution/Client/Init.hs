-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Init
-- Copyright   :  (c) Brent Yorgey 2009
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Implementation of the 'cabal init' command, which creates an initial .cabal
-- file for a project.
--
-----------------------------------------------------------------------------

module Distribution.Client.Init (

    -- * Commands
    initCabal
  , pvpize
  , incVersion

  ) where

import Prelude ()
import Distribution.Client.Compat.Prelude hiding (empty)

import System.IO
  ( hSetBuffering, stdout, BufferMode(..) )
import System.Directory
  ( getCurrentDirectory, doesDirectoryExist, doesFileExist, copyFile
  , getDirectoryContents, createDirectoryIfMissing )
import System.FilePath
  ( (</>), (<.>), takeBaseName, equalFilePath )
import Data.Time
  ( getCurrentTime, utcToLocalTime, toGregorian, localDay, getCurrentTimeZone )

import Data.List
  ( groupBy, (\\) )
import Data.Function
  ( on )
import qualified Data.Map as M
import Control.Monad
  ( (>=>), join, forM_, mapM, mapM_ )
import Control.Arrow
  ( (&&&), (***) )

import Text.PrettyPrint hiding (mode, cat)

import Distribution.Version
  ( Version, mkVersion, alterVersion
  , orLaterVersion, earlierVersion, intersectVersionRanges, VersionRange )
import Distribution.Verbosity
  ( Verbosity )
import Distribution.ModuleName
  ( ModuleName )  -- And for the Text instance
import Distribution.InstalledPackageInfo
  ( InstalledPackageInfo, exposed )
import qualified Distribution.Package as P
import Language.Haskell.Extension ( Language(..) )

import Distribution.Client.Init.Types
  ( InitFlags(..), PackageType(..), Category(..) )
import Distribution.Client.Init.Licenses
  ( bsd2, bsd3, gplv2, gplv3, lgpl21, lgpl3, agplv3, apache20, mit, mpl20, isc )
import Distribution.Client.Init.Heuristics
  ( guessPackageName, guessAuthorNameMail, guessMainFileCandidates,
    SourceFileEntry(..),
    scanForModules, neededBuildPrograms )

import Distribution.License
  ( License(..), knownLicenses )

import Distribution.ReadE
  ( runReadE, readP_to_E )
import Distribution.Simple.Setup
  ( Flag(..), flagToMaybe )
import Distribution.Simple.Configure
  ( getInstalledPackages )
import Distribution.Simple.Compiler
  ( PackageDBStack, Compiler )
import Distribution.Simple.Program
  ( ProgramDb )
import Distribution.Simple.PackageIndex
  ( InstalledPackageIndex, moduleNameIndex )
import Distribution.Text
  ( display, Text(..) )

import Distribution.Solver.Types.PackageIndex
  ( elemByPackageName )

import Distribution.Client.IndexUtils
  ( getSourcePackages )
import Distribution.Client.Types
  ( SourcePackageDb(..) )
import Distribution.Client.Setup
  ( RepoContext(..) )

initCabal :: Verbosity
          -> PackageDBStack
          -> RepoContext
          -> Compiler
          -> ProgramDb
          -> InitFlags
          -> IO ()
initCabal verbosity packageDBs repoCtxt comp progdb initFlags = do

  installedPkgIndex <- getInstalledPackages verbosity comp packageDBs progdb
  sourcePkgDb <- getSourcePackages verbosity repoCtxt

  hSetBuffering stdout NoBuffering

  initFlags' <- extendFlags installedPkgIndex sourcePkgDb initFlags

  case license initFlags' of
    Flag PublicDomain -> return ()
    _                 -> writeLicense initFlags'
  writeSetupFile initFlags'
  writeChangeLog initFlags'
  createSourceDirectories initFlags'
  createMainHs initFlags'
  success <- writeCabalFile initFlags'

  when success $ generateWarnings initFlags'

---------------------------------------------------------------------------
--  Flag acquisition  -----------------------------------------------------
---------------------------------------------------------------------------

-- | Fill in more details by guessing, discovering, or prompting the
--   user.
extendFlags :: InstalledPackageIndex -> SourcePackageDb -> InitFlags -> IO InitFlags
extendFlags pkgIx sourcePkgDb =
      getPackageName sourcePkgDb
  >=> getVersion
  >=> getLicense
  >=> getAuthorInfo
  >=> getHomepage
  >=> getSynopsis
  >=> getCategory
  >=> getExtraSourceFiles
  >=> getLibOrExec
  >=> getSrcDir
  >=> getLanguage
  >=> getGenComments
  >=> getModulesBuildToolsAndDeps pkgIx

-- | Combine two actions which may return a value, preferring the first. That
--   is, run the second action only if the first doesn't return a value.
infixr 1 ?>>
(?>>) :: IO (Maybe a) -> IO (Maybe a) -> IO (Maybe a)
f ?>> g = do
  ma <- f
  if isJust ma
    then return ma
    else g

-- | Witness the isomorphism between Maybe and Flag.
maybeToFlag :: Maybe a -> Flag a
maybeToFlag = maybe NoFlag Flag

-- | Get the package name: use the package directory (supplied, or the current
--   directory by default) as a guess. It looks at the SourcePackageDb to avoid
--   using an existing package name.
getPackageName :: SourcePackageDb -> InitFlags -> IO InitFlags
getPackageName sourcePkgDb flags = do
  guess    <-     traverse guessPackageName (flagToMaybe $ packageDir flags)
              ?>> Just `fmap` (getCurrentDirectory >>= guessPackageName)

  let guess' | isPkgRegistered guess = Nothing
             | otherwise = guess

  pkgName' <-     return (flagToMaybe $ packageName flags)
              ?>> maybePrompt flags (prompt "Package name" guess')
              ?>> return guess'

  chooseAgain <- if isPkgRegistered pkgName'
                    then promptYesNo promptOtherNameMsg (Just True)
                    else return False

  if chooseAgain
    then getPackageName sourcePkgDb flags
    else return $ flags { packageName = maybeToFlag pkgName' }

  where
    isPkgRegistered (Just pkg) = elemByPackageName (packageIndex sourcePkgDb) pkg
    isPkgRegistered Nothing    = False

    promptOtherNameMsg = "This package name is already used by another " ++
                         "package on hackage. Do you want to choose a " ++
                         "different name"

-- | Package version: use 0.1.0.0 as a last resort, but try prompting the user
--  if possible.
getVersion :: InitFlags -> IO InitFlags
getVersion flags = do
  let v = Just $ mkVersion [0,1,0,0]
  v' <-     return (flagToMaybe $ version flags)
        ?>> maybePrompt flags (prompt "Package version" v)
        ?>> return v
  return $ flags { version = maybeToFlag v' }

-- | Choose a license.
getLicense :: InitFlags -> IO InitFlags
getLicense flags = do
  lic <-     return (flagToMaybe $ license flags)
         ?>> fmap (fmap (either UnknownLicense id))
                  (maybePrompt flags
                    (promptList "Please choose a license" listedLicenses (Just BSD3) display True))

  if isLicenseInvalid lic
    then putStrLn promptInvalidOtherLicenseMsg >> getLicense flags
    else return $ flags { license = maybeToFlag lic }

  where
    isLicenseInvalid (Just (UnknownLicense t)) = any (not . isAlphaNum) t
    isLicenseInvalid _ = False

    promptInvalidOtherLicenseMsg = "\nThe license must be alphanumeric. " ++
                                   "If your license name has many words, " ++
                                   "the convention is to use camel case (e.g. PublicDomain). " ++
                                   "Please choose a different license."

    listedLicenses =
      knownLicenses \\ [GPL Nothing, LGPL Nothing, AGPL Nothing
                       , Apache Nothing, OtherLicense]

-- | The author's name and email. Prompt, or try to guess from an existing
--   darcs repo.
getAuthorInfo :: InitFlags -> IO InitFlags
getAuthorInfo flags = do
  (authorName, authorEmail)  <-
    (flagToMaybe *** flagToMaybe) `fmap` guessAuthorNameMail
  authorName'  <-     return (flagToMaybe $ author flags)
                  ?>> maybePrompt flags (promptStr "Author name" authorName)
                  ?>> return authorName

  authorEmail' <-     return (flagToMaybe $ email flags)
                  ?>> maybePrompt flags (promptStr "Maintainer email" authorEmail)
                  ?>> return authorEmail

  return $ flags { author = maybeToFlag authorName'
                 , email  = maybeToFlag authorEmail'
                 }

-- | Prompt for a homepage URL.
getHomepage :: InitFlags -> IO InitFlags
getHomepage flags = do
  hp  <- queryHomepage
  hp' <-     return (flagToMaybe $ homepage flags)
         ?>> maybePrompt flags (promptStr "Project homepage URL" hp)
         ?>> return hp

  return $ flags { homepage = maybeToFlag hp' }

-- | Right now this does nothing, but it could be changed to do some
--   intelligent guessing.
queryHomepage :: IO (Maybe String)
queryHomepage = return Nothing     -- get default remote darcs repo?

-- | Prompt for a project synopsis.
getSynopsis :: InitFlags -> IO InitFlags
getSynopsis flags = do
  syn <-     return (flagToMaybe $ synopsis flags)
         ?>> maybePrompt flags (promptStr "Project synopsis" Nothing)

  return $ flags { synopsis = maybeToFlag syn }

-- | Prompt for a package category.
--   Note that it should be possible to do some smarter guessing here too, i.e.
--   look at the name of the top level source directory.
getCategory :: InitFlags -> IO InitFlags
getCategory flags = do
  cat <-     return (flagToMaybe $ category flags)
         ?>> fmap join (maybePrompt flags
                         (promptListOptional "Project category" [Codec ..]))
  return $ flags { category = maybeToFlag cat }

-- | Try to guess extra source files (don't prompt the user).
getExtraSourceFiles :: InitFlags -> IO InitFlags
getExtraSourceFiles flags = do
  extraSrcFiles <-     return (extraSrc flags)
                   ?>> Just `fmap` guessExtraSourceFiles flags

  return $ flags { extraSrc = extraSrcFiles }

defaultChangeLog :: FilePath
defaultChangeLog = "ChangeLog.md"

-- | Try to guess things to include in the extra-source-files field.
--   For now, we just look for things in the root directory named
--   'readme', 'changes', or 'changelog', with any sort of
--   capitalization and any extension.
guessExtraSourceFiles :: InitFlags -> IO [FilePath]
guessExtraSourceFiles flags = do
  dir <-
    maybe getCurrentDirectory return . flagToMaybe $ packageDir flags
  files <- getDirectoryContents dir
  let extraFiles = filter isExtra files
  if any isLikeChangeLog extraFiles
    then return extraFiles
    else return (defaultChangeLog : extraFiles)

  where
    isExtra = likeFileNameBase ("README" : changeLogLikeBases)
    isLikeChangeLog = likeFileNameBase changeLogLikeBases
    likeFileNameBase candidates = (`elem` candidates) . map toUpper . takeBaseName
    changeLogLikeBases = ["CHANGES", "CHANGELOG"]

-- | Ask whether the project builds a library or executable.
getLibOrExec :: InitFlags -> IO InitFlags
getLibOrExec flags = do
  isLib <-     return (flagToMaybe $ packageType flags)
           ?>> maybePrompt flags (either (const Library) id `fmap`
                                   promptList "What does the package build"
                                   [Library, Executable]
                                   Nothing display False)
           ?>> return (Just Library)
  mainFile <- if isLib /= Just Executable then return Nothing else
                    getMainFile flags

  return $ flags { packageType = maybeToFlag isLib
                 , mainIs = maybeToFlag mainFile
                 }

-- | Try to guess the main file of the executable, and prompt the user to choose
-- one of them. Top-level modules including the word 'Main' in the file name
-- will be candidates, and shorter filenames will be preferred.
getMainFile :: InitFlags -> IO (Maybe FilePath)
getMainFile flags =
  return (flagToMaybe $ mainIs flags)
  ?>> do
    candidates <- guessMainFileCandidates flags
    let showCandidate = either (++" (does not yet exist, but will be created)") id
        defaultFile = listToMaybe candidates
    maybePrompt flags (either id (either id id) `fmap`
                       promptList "What is the main module of the executable"
                       candidates
                       defaultFile showCandidate True)
      ?>> return (fmap (either id id) defaultFile)

-- | Ask for the base language of the package.
getLanguage :: InitFlags -> IO InitFlags
getLanguage flags = do
  lang <-     return (flagToMaybe $ language flags)
          ?>> maybePrompt flags
                (either UnknownLanguage id `fmap`
                  promptList "What base language is the package written in"
                  [Haskell2010, Haskell98]
                  (Just Haskell2010) display True)
          ?>> return (Just Haskell2010)

  if invalidLanguage lang
    then putStrLn invalidOtherLanguageMsg >> getLanguage flags
    else return $ flags { language = maybeToFlag lang }

  where
    invalidLanguage (Just (UnknownLanguage t)) = any (not . isAlphaNum) t
    invalidLanguage _ = False

    invalidOtherLanguageMsg = "\nThe language must be alphanumeric. " ++
                              "Please enter a different language."

-- | Ask whether to generate explanatory comments.
getGenComments :: InitFlags -> IO InitFlags
getGenComments flags = do
  genComments <-     return (not <$> flagToMaybe (noComments flags))
                 ?>> maybePrompt flags (promptYesNo promptMsg (Just False))
                 ?>> return (Just False)
  return $ flags { noComments = maybeToFlag (fmap not genComments) }
  where
    promptMsg = "Add informative comments to each field in the cabal file (y/n)"

-- | Ask for the source root directory.
getSrcDir :: InitFlags -> IO InitFlags
getSrcDir flags = do
  srcDirs <- return (sourceDirs flags)
             ?>> fmap (:[]) `fmap` guessSourceDir flags
             ?>> fmap (>>= fmap ((:[]) . either id id)) (maybePrompt
                      flags
                      (promptListOptional' "Source directory" ["src"] id))

  return $ flags { sourceDirs = srcDirs }

-- | Try to guess source directory. Could try harder; for the
--   moment just looks to see whether there is a directory called 'src'.
guessSourceDir :: InitFlags -> IO (Maybe String)
guessSourceDir flags = do
  dir      <-
    maybe getCurrentDirectory return . flagToMaybe $ packageDir flags
  srcIsDir <- doesDirectoryExist (dir </> "src")
  return $ if srcIsDir
             then Just "src"
             else Nothing

-- | Check whether a potential source file is located in one of the
--   source directories.
isSourceFile :: Maybe [FilePath] -> SourceFileEntry -> Bool
isSourceFile Nothing        sf = isSourceFile (Just ["."]) sf
isSourceFile (Just srcDirs) sf = any (equalFilePath (relativeSourcePath sf)) srcDirs

-- | Get the list of exposed modules and extra tools needed to build them.
getModulesBuildToolsAndDeps :: InstalledPackageIndex -> InitFlags -> IO InitFlags
getModulesBuildToolsAndDeps pkgIx flags = do
  dir <- maybe getCurrentDirectory return . flagToMaybe $ packageDir flags

  sourceFiles0 <- scanForModules dir

  let sourceFiles = filter (isSourceFile (sourceDirs flags)) sourceFiles0

  Just mods <-      return (exposedModules flags)
           ?>> (return . Just . map moduleName $ sourceFiles)

  tools <-     return (buildTools flags)
           ?>> (return . Just . neededBuildPrograms $ sourceFiles)

  deps <-      return (dependencies flags)
           ?>> Just <$> importsToDeps flags
                        (fromString "Prelude" :  -- to ensure we get base as a dep
                           (   nub   -- only need to consider each imported package once
                             . filter (`notElem` mods)  -- don't consider modules from
                                                        -- this package itself
                             . concatMap imports
                             $ sourceFiles
                           )
                        )
                        pkgIx

  exts <-     return (otherExts flags)
          ?>> (return . Just . nub . concatMap extensions $ sourceFiles)

  return $ flags { exposedModules = Just mods
                 , buildTools     = tools
                 , dependencies   = deps
                 , otherExts      = exts
                 }

importsToDeps :: InitFlags -> [ModuleName] -> InstalledPackageIndex -> IO [P.Dependency]
importsToDeps flags mods pkgIx = do

  let modMap :: M.Map ModuleName [InstalledPackageInfo]
      modMap  = M.map (filter exposed) $ moduleNameIndex pkgIx

      modDeps :: [(ModuleName, Maybe [InstalledPackageInfo])]
      modDeps = map (id &&& flip M.lookup modMap) mods

  message flags "\nGuessing dependencies..."
  nub . catMaybes <$> mapM (chooseDep flags) modDeps

-- Given a module and a list of installed packages providing it,
-- choose a dependency (i.e. package + version range) to use for that
-- module.
chooseDep :: InitFlags -> (ModuleName, Maybe [InstalledPackageInfo])
          -> IO (Maybe P.Dependency)

chooseDep flags (m, Nothing)
  = message flags ("\nWarning: no package found providing " ++ display m ++ ".")
    >> return Nothing

chooseDep flags (m, Just [])
  = message flags ("\nWarning: no package found providing " ++ display m ++ ".")
    >> return Nothing

    -- We found some packages: group them by name.
chooseDep flags (m, Just ps)
  = case pkgGroups of
      -- if there's only one group, i.e. multiple versions of a single package,
      -- we make it into a dependency, choosing the latest-ish version (see toDep).
      [grp] -> Just <$> toDep grp
      -- otherwise, we refuse to choose between different packages and make the user
      -- do it.
      grps  -> do message flags ("\nWarning: multiple packages found providing "
                                 ++ display m
                                 ++ ": " ++ intercalate ", " (map (display . P.pkgName . head) grps))
                  message flags "You will need to pick one and manually add it to the Build-depends: field."
                  return Nothing
  where
    pkgGroups = groupBy ((==) `on` P.pkgName) (map P.packageId ps)

    -- Given a list of available versions of the same package, pick a dependency.
    toDep :: [P.PackageIdentifier] -> IO P.Dependency

    -- If only one version, easy.  We change e.g. 0.4.2  into  0.4.*
    toDep [pid] = return $ P.Dependency (P.pkgName pid) (pvpize . P.pkgVersion $ pid)

    -- Otherwise, choose the latest version and issue a warning.
    toDep pids  = do
      message flags ("\nWarning: multiple versions of " ++ display (P.pkgName . head $ pids) ++ " provide " ++ display m ++ ", choosing the latest.")
      return $ P.Dependency (P.pkgName . head $ pids)
                            (pvpize . maximum . map P.pkgVersion $ pids)

-- | Given a version, return an API-compatible (according to PVP) version range.
--
-- Example: @0.4.1@ produces the version range @>= 0.4 && < 0.5@ (which is the
-- same as @0.4.*@).
pvpize :: Version -> VersionRange
pvpize v = orLaterVersion v'
           `intersectVersionRanges`
           earlierVersion (incVersion 1 v')
  where v' = alterVersion (take 2) v

-- | Increment the nth version component (counting from 0).
incVersion :: Int -> Version -> Version
incVersion n = alterVersion (incVersion' n)
  where
    incVersion' 0 []     = [1]
    incVersion' 0 (v:_)  = [v+1]
    incVersion' m []     = replicate m 0 ++ [1]
    incVersion' m (v:vs) = v : incVersion' (m-1) vs

---------------------------------------------------------------------------
--  Prompting/user interaction  -------------------------------------------
---------------------------------------------------------------------------

-- | Run a prompt or not based on the nonInteractive flag of the
--   InitFlags structure.
maybePrompt :: InitFlags -> IO t -> IO (Maybe t)
maybePrompt flags p =
  case nonInteractive flags of
    Flag True -> return Nothing
    _         -> Just `fmap` p

-- | Create a prompt with optional default value that returns a
--   String.
promptStr :: String -> Maybe String -> IO String
promptStr = promptDefault' Just id

-- | Create a yes/no prompt with optional default value.
--
promptYesNo :: String -> Maybe Bool -> IO Bool
promptYesNo =
    promptDefault' recogniseYesNo showYesNo
  where
    recogniseYesNo s | s == "y" || s == "Y" = Just True
                     | s == "n" || s == "N" = Just False
                     | otherwise            = Nothing
    showYesNo True  = "y"
    showYesNo False = "n"

-- | Create a prompt with optional default value that returns a value
--   of some Text instance.
prompt :: Text t => String -> Maybe t -> IO t
prompt = promptDefault'
           (either (const Nothing) Just . runReadE (readP_to_E id parse))
           display

-- | Create a prompt with an optional default value.
promptDefault' :: (String -> Maybe t)       -- ^ parser
               -> (t -> String)             -- ^ pretty-printer
               -> String                    -- ^ prompt message
               -> Maybe t                   -- ^ optional default value
               -> IO t
promptDefault' parser pretty pr def = do
  putStr $ mkDefPrompt pr (pretty `fmap` def)
  inp <- getLine
  case (inp, def) of
    ("", Just d)  -> return d
    _  -> case parser inp of
            Just t  -> return t
            Nothing -> do putStrLn $ "Couldn't parse " ++ inp ++ ", please try again!"
                          promptDefault' parser pretty pr def

-- | Create a prompt from a prompt string and a String representation
--   of an optional default value.
mkDefPrompt :: String -> Maybe String -> String
mkDefPrompt pr def = pr ++ "?" ++ defStr def
  where defStr Nothing  = " "
        defStr (Just s) = " [default: " ++ s ++ "] "

promptListOptional :: (Text t, Eq t)
                   => String            -- ^ prompt
                   -> [t]               -- ^ choices
                   -> IO (Maybe (Either String t))
promptListOptional pr choices = promptListOptional' pr choices display

promptListOptional' :: Eq t
                   => String            -- ^ prompt
                   -> [t]               -- ^ choices
                   -> (t -> String)     -- ^ show an item
                   -> IO (Maybe (Either String t))
promptListOptional' pr choices displayItem =
    fmap rearrange
  $ promptList pr (Nothing : map Just choices) (Just Nothing)
               (maybe "(none)" displayItem) True
  where
    rearrange = either (Just . Left) (fmap Right)

-- | Create a prompt from a list of items.
promptList :: Eq t
           => String            -- ^ prompt
           -> [t]               -- ^ choices
           -> Maybe t           -- ^ optional default value
           -> (t -> String)     -- ^ show an item
           -> Bool              -- ^ whether to allow an 'other' option
           -> IO (Either String t)
promptList pr choices def displayItem other = do
  putStrLn $ pr ++ ":"
  let options1 = map (\c -> (Just c == def, displayItem c)) choices
      options2 = zip ([1..]::[Int])
                     (options1 ++ [(False, "Other (specify)") | other])
  mapM_ (putStrLn . \(n,(i,s)) -> showOption n i ++ s) options2
  promptList' displayItem (length options2) choices def other
 where showOption n i | n < 10 = " " ++ star i ++ " " ++ rest
                      | otherwise = " " ++ star i ++ rest
                  where rest = show n ++ ") "
                        star True = "*"
                        star False = " "

promptList' :: (t -> String) -> Int -> [t] -> Maybe t -> Bool -> IO (Either String t)
promptList' displayItem numChoices choices def other = do
  putStr $ mkDefPrompt "Your choice" (displayItem `fmap` def)
  inp <- getLine
  case (inp, def) of
    ("", Just d) -> return $ Right d
    _  -> case readMaybe inp of
            Nothing -> invalidChoice inp
            Just n  -> getChoice n
 where invalidChoice inp = do putStrLn $ inp ++ " is not a valid choice."
                              promptList' displayItem numChoices choices def other
       getChoice n | n < 1 || n > numChoices = invalidChoice (show n)
                   | n < numChoices ||
                     (n == numChoices && not other)
                                  = return . Right $ choices !! (n-1)
                   | otherwise    = Left `fmap` promptStr "Please specify" Nothing

---------------------------------------------------------------------------
--  File generation  ------------------------------------------------------
---------------------------------------------------------------------------

writeLicense :: InitFlags -> IO ()
writeLicense flags = do
  message flags "\nGenerating LICENSE..."
  year <- show <$> getYear
  let authors = fromMaybe "???" . flagToMaybe . author $ flags
  let licenseFile =
        case license flags of
          Flag BSD2
            -> Just $ bsd2 authors year

          Flag BSD3
            -> Just $ bsd3 authors year

          Flag (GPL (Just v)) | v == mkVersion [2]
            -> Just gplv2

          Flag (GPL (Just v)) | v == mkVersion [3]
            -> Just gplv3

          Flag (LGPL (Just v)) | v == mkVersion [2,1]
            -> Just lgpl21

          Flag (LGPL (Just v)) | v == mkVersion [3]
            -> Just lgpl3

          Flag (AGPL (Just v)) | v == mkVersion [3]
            -> Just agplv3

          Flag (Apache (Just v)) | v == mkVersion [2,0]
            -> Just apache20

          Flag MIT
            -> Just $ mit authors year

          Flag (MPL v) | v == mkVersion [2,0]
            -> Just mpl20

          Flag ISC
            -> Just $ isc authors year

          _ -> Nothing

  case licenseFile of
    Just licenseText -> writeFileSafe flags "LICENSE" licenseText
    Nothing -> message flags "Warning: unknown license type, you must put a copy in LICENSE yourself."

getYear :: IO Integer
getYear = do
  u <- getCurrentTime
  z <- getCurrentTimeZone
  let l = utcToLocalTime z u
      (y, _, _) = toGregorian $ localDay l
  return y

writeSetupFile :: InitFlags -> IO ()
writeSetupFile flags = do
  message flags "Generating Setup.hs..."
  writeFileSafe flags "Setup.hs" setupFile
 where
  setupFile = unlines
    [ "import Distribution.Simple"
    , "main = defaultMain"
    ]

writeChangeLog :: InitFlags -> IO ()
writeChangeLog flags = when ((defaultChangeLog `elem`) $ fromMaybe [] (extraSrc flags)) $ do
  message flags ("Generating "++ defaultChangeLog ++"...")
  writeFileSafe flags defaultChangeLog changeLog
 where
  changeLog = unlines
    [ "# Revision history for " ++ pname
    , ""
    , "## " ++ pver ++ " -- YYYY-mm-dd"
    , ""
    , "* First version. Released on an unsuspecting world."
    ]
  pname = maybe "" display $ flagToMaybe $ packageName flags
  pver = maybe "" display $ flagToMaybe $ version flags



writeCabalFile :: InitFlags -> IO Bool
writeCabalFile flags@(InitFlags{packageName = NoFlag}) = do
  message flags "Error: no package name provided."
  return False
writeCabalFile flags@(InitFlags{packageName = Flag p}) = do
  let cabalFileName = display p ++ ".cabal"
  message flags $ "Generating " ++ cabalFileName ++ "..."
  writeFileSafe flags cabalFileName (generateCabalFile cabalFileName flags)
  return True

-- | Write a file \"safely\", backing up any existing version (unless
--   the overwrite flag is set).
writeFileSafe :: InitFlags -> FilePath -> String -> IO ()
writeFileSafe flags fileName content = do
  moveExistingFile flags fileName
  writeFile fileName content

-- | Create source directories, if they were given.
createSourceDirectories :: InitFlags -> IO ()
createSourceDirectories flags = case sourceDirs flags of
                                  Just dirs -> forM_ dirs (createDirectoryIfMissing True)
                                  Nothing   -> return ()

-- | Create Main.hs, but only if we are init'ing an executable and
--   the mainIs flag has been provided.
createMainHs :: InitFlags -> IO ()
createMainHs flags@InitFlags{ sourceDirs = Just (srcPath:_)
                            , packageType = Flag Executable
                            , mainIs = Flag mainFile } =
  writeMainHs flags (srcPath </> mainFile)
createMainHs flags@InitFlags{ sourceDirs = _
                            , packageType = Flag Executable
                            , mainIs = Flag mainFile } =
  writeMainHs flags mainFile
createMainHs _ = return ()

-- | Write a main file if it doesn't already exist.
writeMainHs :: InitFlags -> FilePath -> IO ()
writeMainHs flags mainPath = do
  dir <- maybe getCurrentDirectory return (flagToMaybe $ packageDir flags)
  let mainFullPath = dir </> mainPath
  exists <- doesFileExist mainFullPath
  unless exists $ do
      message flags $ "Generating " ++ mainPath ++ "..."
      writeFileSafe flags mainFullPath mainHs

-- | Default Main.hs file.  Used when no Main.hs exists.
mainHs :: String
mainHs = unlines
  [ "module Main where"
  , ""
  , "main :: IO ()"
  , "main = putStrLn \"Hello, Haskell!\""
  ]

-- | Move an existing file, if there is one, and the overwrite flag is
--   not set.
moveExistingFile :: InitFlags -> FilePath -> IO ()
moveExistingFile flags fileName =
  unless (overwrite flags == Flag True) $ do
    e <- doesFileExist fileName
    when e $ do
      newName <- findNewName fileName
      message flags $ "Warning: " ++ fileName ++ " already exists, backing up old version in " ++ newName
      copyFile fileName newName

findNewName :: FilePath -> IO FilePath
findNewName oldName = findNewName' 0
  where
    findNewName' :: Integer -> IO FilePath
    findNewName' n = do
      let newName = oldName <.> ("save" ++ show n)
      e <- doesFileExist newName
      if e then findNewName' (n+1) else return newName

-- | Generate a .cabal file from an InitFlags structure.  NOTE: this
--   is rather ad-hoc!  What we would REALLY like is to have a
--   standard low-level AST type representing .cabal files, which
--   preserves things like comments, and to write an *inverse*
--   parser/pretty-printer pair between .cabal files and this AST.
--   Then instead of this ad-hoc code we could just map an InitFlags
--   structure onto a low-level AST structure and use the existing
--   pretty-printing code to generate the file.
generateCabalFile :: String -> InitFlags -> String
generateCabalFile fileName c =
  (++ "\n") .
  renderStyle style { lineLength = 79, ribbonsPerLine = 1.1 } $
  (if minimal c /= Flag True
    then showComment (Just $ "Initial " ++ fileName ++ " generated by cabal "
                          ++ "init.  For further documentation, see "
                          ++ "http://haskell.org/cabal/users-guide/")
         $$ text ""
    else empty)
  $$
  vcat [ field  "name"          (packageName   c)
                (Just "The name of the package.")
                True

       , field  "version"       (version       c)
                (Just $ "The package version.  See the Haskell package versioning policy (PVP) for standards guiding when and how versions should be incremented.\nhttps://wiki.haskell.org/Package_versioning_policy\n"
                ++ "PVP summary:      +-+------- breaking API changes\n"
                ++ "                  | | +----- non-breaking API additions\n"
                ++ "                  | | | +--- code changes with no API change")
                True

       , fieldS "synopsis"      (synopsis      c)
                (Just "A short (one-line) description of the package.")
                True

       , fieldS "description"   NoFlag
                (Just "A longer description of the package.")
                True

       , fieldS "homepage"      (homepage     c)
                (Just "URL for the project homepage or repository.")
                False

       , fieldS "bug-reports"   NoFlag
                (Just "A URL where users can report bugs.")
                False

       , field  "license"       (license      c)
                (Just "The license under which the package is released.")
                True

       , case (license c) of
           Flag PublicDomain -> empty
           _ -> fieldS "license-file" (Flag "LICENSE")
                       (Just "The file containing the license text.")
                       True

       , fieldS "author"        (author       c)
                (Just "The package author(s).")
                True

       , fieldS "maintainer"    (email        c)
                (Just "An email address to which users can send suggestions, bug reports, and patches.")
                True

       , case (license c) of
           Flag PublicDomain -> empty
           _ -> fieldS "copyright"     NoFlag
                       (Just "A copyright notice.")
                       True

       , fieldS "category"      (either id display `fmap` category c)
                Nothing
                True

       , fieldS "build-type"    (Flag "Simple")
                Nothing
                True

       , fieldS "extra-source-files" (listFieldS (extraSrc c))
                (Just "Extra files to be distributed with the package, such as examples or a README.")
                True

       , field  "cabal-version" (Flag $ orLaterVersion (mkVersion [1,10]))
                (Just "Constraint on the version of Cabal needed to build this package.")
                False

       , case packageType c of
           Flag Executable ->
             text "\nexecutable" <+>
             text (maybe "" display . flagToMaybe $ packageName c) $$
             nest 2 (vcat
             [ fieldS "main-is" (mainIs c) (Just ".hs or .lhs file containing the Main module.") True

             , generateBuildInfo Executable c
             ])
           Flag Library    -> text "\nlibrary" $$ nest 2 (vcat
             [ fieldS "exposed-modules" (listField (exposedModules c))
                      (Just "Modules exported by the library.")
                      True

             , generateBuildInfo Library c
             ])
           _               -> empty
       ]
 where
   generateBuildInfo :: PackageType -> InitFlags -> Doc
   generateBuildInfo pkgtype c' = vcat
     [ fieldS "other-modules" (listField (otherModules c'))
              (Just $ case pkgtype of
                 Library    -> "Modules included in this library but not exported."
                 Executable -> "Modules included in this executable, other than Main.")
              True

     , fieldS "other-extensions" (listField (otherExts c'))
              (Just "LANGUAGE extensions used by modules in this package.")
              True

     , fieldS "build-depends" (listField (dependencies c'))
              (Just "Other library packages from which modules are imported.")
              True

     , fieldS "hs-source-dirs" (listFieldS (sourceDirs c'))
              (Just "Directories containing source files.")
              True

     , fieldS "build-tools" (listFieldS (buildTools c'))
              (Just "Extra tools (e.g. alex, hsc2hs, ...) needed to build the source.")
              False

     , field  "default-language" (language c')
              (Just "Base language which the package is written in.")
              True
     ]

   listField :: Text s => Maybe [s] -> Flag String
   listField = listFieldS . fmap (map display)

   listFieldS :: Maybe [String] -> Flag String
   listFieldS = Flag . maybe "" (intercalate ", ")

   field :: Text t => String -> Flag t -> Maybe String -> Bool -> Doc
   field s f = fieldS s (fmap display f)

   fieldS :: String        -- ^ Name of the field
          -> Flag String   -- ^ Field contents
          -> Maybe String  -- ^ Comment to explain the field
          -> Bool          -- ^ Should the field be included (commented out) even if blank?
          -> Doc
   fieldS _ NoFlag _    inc | not inc || (minimal c == Flag True) = empty
   fieldS _ (Flag "") _ inc | not inc || (minimal c == Flag True) = empty
   fieldS s f com _ = case (isJust com, noComments c, minimal c) of
                        (_, _, Flag True) -> id
                        (_, Flag True, _) -> id
                        (True, _, _)      -> (showComment com $$) . ($$ text "")
                        (False, _, _)     -> ($$ text "")
                      $
                      comment f <<>> text s <<>> colon
                                <<>> text (replicate (20 - length s) ' ')
                                <<>> text (fromMaybe "" . flagToMaybe $ f)
   comment NoFlag    = text "-- "
   comment (Flag "") = text "-- "
   comment _         = text ""

   showComment :: Maybe String -> Doc
   showComment (Just t) = vcat
                        . map (text . ("-- "++)) . lines
                        . renderStyle style {
                            lineLength = 76,
                            ribbonsPerLine = 1.05
                          }
                        . vcat
                        . map (fcat . map text . breakLine)
                        . lines
                        $ t
   showComment Nothing  = text ""

   breakLine  [] = []
   breakLine  cs = case break (==' ') cs of (w,cs') -> w : breakLine' cs'
   breakLine' [] = []
   breakLine' cs = case span (==' ') cs of (w,cs') -> w : breakLine cs'

-- | Generate warnings for missing fields etc.
generateWarnings :: InitFlags -> IO ()
generateWarnings flags = do
  message flags ""
  when (synopsis flags `elem` [NoFlag, Flag ""])
       (message flags "Warning: no synopsis given. You should edit the .cabal file and add one.")

  message flags "You may want to edit the .cabal file and add a Description field."

-- | Possibly generate a message to stdout, taking into account the
--   --quiet flag.
message :: InitFlags -> String -> IO ()
message (InitFlags{quiet = Flag True}) _ = return ()
message _ s = putStrLn s
