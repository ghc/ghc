module Distribution.Client.Upload (upload, uploadDoc, report) where

import Distribution.Client.Types ( Username(..), Password(..)
                                 , RemoteRepo(..), maybeRepoRemote )
import Distribution.Client.HttpUtils
         ( HttpTransport(..), remoteRepoTryUpgradeToHttps )
import Distribution.Client.Setup
         ( IsCandidate(..), RepoContext(..) )

import Distribution.Simple.Utils (notice, warn, info, die')
import Distribution.Verbosity (Verbosity)
import Distribution.Text (display)
import Distribution.Client.Config

import qualified Distribution.Client.BuildReports.Anonymous as BuildReport
import qualified Distribution.Client.BuildReports.Upload as BuildReport

import Network.URI (URI(uriPath))
import Network.HTTP (Header(..), HeaderName(..))

import System.IO        (hFlush, stdout)
import System.IO.Echo   (withoutInputEcho)
import System.Exit      (exitFailure)
import System.FilePath  ((</>), takeExtension, takeFileName, dropExtension)
import qualified System.FilePath.Posix as FilePath.Posix ((</>))
import System.Directory
import Control.Monad (forM_, when, foldM)
import Data.Maybe (mapMaybe)
import Data.Char (isSpace)

type Auth = Maybe (String, String)

-- > stripExtensions ["tar", "gz"] "foo.tar.gz"
-- Just "foo"
-- > stripExtensions ["tar", "gz"] "foo.gz.tar"
-- Nothing
stripExtensions :: [String] -> FilePath -> Maybe String
stripExtensions exts path = foldM f path (reverse exts)
 where
  f p e
    | takeExtension p == '.':e = Just (dropExtension p)
    | otherwise = Nothing

upload :: Verbosity -> RepoContext
       -> Maybe Username -> Maybe Password -> IsCandidate -> [FilePath]
       -> IO ()
upload verbosity repoCtxt mUsername mPassword isCandidate paths = do
    let repos = repoContextRepos repoCtxt
    transport  <- repoContextGetTransport repoCtxt
    targetRepo <-
      case [ remoteRepo | Just remoteRepo <- map maybeRepoRemote repos ] of
        [] -> die' verbosity "Cannot upload. No remote repositories are configured."
        rs -> remoteRepoTryUpgradeToHttps verbosity transport (last rs)
    let targetRepoURI = remoteRepoURI targetRepo
        rootIfEmpty x = if null x then "/" else x
        uploadURI = targetRepoURI {
            uriPath = rootIfEmpty (uriPath targetRepoURI) FilePath.Posix.</>
              case isCandidate of
                IsCandidate -> "packages/candidates"
                IsPublished -> "upload"
        }
        packageURI pkgid = targetRepoURI {
            uriPath = rootIfEmpty (uriPath targetRepoURI)
                      FilePath.Posix.</> concat
              [ "package/", pkgid
              , case isCandidate of
                  IsCandidate -> "/candidate"
                  IsPublished -> ""
              ]
        }
    Username username <- maybe promptUsername return mUsername
    Password password <- maybe promptPassword return mPassword
    let auth = Just (username,password)
    forM_ paths $ \path -> do
      notice verbosity $ "Uploading " ++ path ++ "... "
      case fmap takeFileName (stripExtensions ["tar", "gz"] path) of
        Just pkgid -> handlePackage transport verbosity uploadURI
                                    (packageURI pkgid) auth isCandidate path
        -- This case shouldn't really happen, since we check in Main that we
        -- only pass tar.gz files to upload.
        Nothing -> die' verbosity $ "Not a tar.gz file: " ++ path

uploadDoc :: Verbosity -> RepoContext
          -> Maybe Username -> Maybe Password -> IsCandidate -> FilePath
          -> IO ()
uploadDoc verbosity repoCtxt mUsername mPassword isCandidate path = do
    let repos = repoContextRepos repoCtxt
    transport  <- repoContextGetTransport repoCtxt
    targetRepo <-
      case [ remoteRepo | Just remoteRepo <- map maybeRepoRemote repos ] of
        [] -> die' verbosity $ "Cannot upload. No remote repositories are configured."
        rs -> remoteRepoTryUpgradeToHttps verbosity transport (last rs)
    let targetRepoURI = remoteRepoURI targetRepo
        rootIfEmpty x = if null x then "/" else x
        uploadURI = targetRepoURI {
            uriPath = rootIfEmpty (uriPath targetRepoURI)
                      FilePath.Posix.</> concat
              [ "package/", pkgid
              , case isCandidate of
                IsCandidate -> "/candidate"
                IsPublished -> ""
              , "/docs"
              ]
        }
        packageUri = targetRepoURI {
            uriPath = rootIfEmpty (uriPath targetRepoURI)
                      FilePath.Posix.</> concat
              [ "package/", pkgid
              , case isCandidate of
                  IsCandidate -> "/candidate"
                  IsPublished -> ""
              ]
        }
        (reverseSuffix, reversePkgid) = break (== '-')
                                        (reverse (takeFileName path))
        pkgid = reverse $ tail reversePkgid
    when (reverse reverseSuffix /= "docs.tar.gz"
          || null reversePkgid || head reversePkgid /= '-') $
      die' verbosity "Expected a file name matching the pattern <pkgid>-docs.tar.gz"
    Username username <- maybe promptUsername return mUsername
    Password password <- maybe promptPassword return mPassword

    let auth = Just (username,password)
        headers =
          [ Header HdrContentType "application/x-tar"
          , Header HdrContentEncoding "gzip"
          ]
    notice verbosity $ "Uploading documentation " ++ path ++ "... "
    resp <- putHttpFile transport verbosity uploadURI path auth headers
    case resp of
      -- Hackage responds with 204 No Content when docs are uploaded
      -- successfully.
      (code,_) | code `elem` [200,204] -> do
        notice verbosity $ okMessage packageUri
      (code,err)  -> do
        notice verbosity $ "Error uploading documentation "
                        ++ path ++ ": "
                        ++ "http code " ++ show code ++ "\n"
                        ++ err
        exitFailure
  where
    okMessage packageUri = case isCandidate of
      IsCandidate ->
        "Documentation successfully uploaded for package candidate. "
        ++ "You can now preview the result at '" ++ show packageUri
        ++ "'. To upload non-candidate documentation, use 'cabal upload --publish'."
      IsPublished ->
        "Package documentation successfully published. You can now view it at '"
        ++ show packageUri ++ "'."


promptUsername :: IO Username
promptUsername = do
  putStr "Hackage username: "
  hFlush stdout
  fmap Username getLine

promptPassword :: IO Password
promptPassword = do
  putStr "Hackage password: "
  hFlush stdout
  -- save/restore the terminal echoing status (no echoing for entering the password)
  passwd <- withoutInputEcho $ fmap Password getLine
  putStrLn ""
  return passwd

report :: Verbosity -> RepoContext -> Maybe Username -> Maybe Password -> IO ()
report verbosity repoCtxt mUsername mPassword = do
  Username username <- maybe promptUsername return mUsername
  Password password <- maybe promptPassword return mPassword
  let auth        = (username, password)
      repos       = repoContextRepos repoCtxt
      remoteRepos = mapMaybe maybeRepoRemote repos
  forM_ remoteRepos $ \remoteRepo ->
      do dotCabal <- defaultCabalDir
         let srcDir = dotCabal </> "reports" </> remoteRepoName remoteRepo
         -- We don't want to bomb out just because we haven't built any packages
         -- from this repo yet.
         srcExists <- doesDirectoryExist srcDir
         when srcExists $ do
           contents <- getDirectoryContents srcDir
           forM_ (filter (\c -> takeExtension c ==".log") contents) $ \logFile ->
             do inp <- readFile (srcDir </> logFile)
                let (reportStr, buildLog) = read inp :: (String,String) -- TODO: eradicateNoParse
                case BuildReport.parse reportStr of
                  Left errs -> warn verbosity $ "Errors: " ++ errs -- FIXME
                  Right report' ->
                    do info verbosity $ "Uploading report for "
                         ++ display (BuildReport.package report')
                       BuildReport.uploadReports verbosity repoCtxt auth
                         (remoteRepoURI remoteRepo) [(report', Just buildLog)]
                       return ()

handlePackage :: HttpTransport -> Verbosity -> URI -> URI -> Auth
              -> IsCandidate -> FilePath -> IO ()
handlePackage transport verbosity uri packageUri auth isCandidate path =
  do resp <- postHttpFile transport verbosity uri path auth
     case resp of
       (code,warnings) | code `elem` [200, 204] ->
          notice verbosity $ okMessage isCandidate ++
            if null warnings then "" else "\n" ++ formatWarnings (trim warnings)
       (code,err)  -> do
          notice verbosity $ "Error uploading " ++ path ++ ": "
                          ++ "http code " ++ show code ++ "\n"
                          ++ err
          exitFailure
 where
  okMessage IsCandidate =
    "Package successfully uploaded as candidate. "
    ++ "You can now preview the result at '" ++ show packageUri
    ++ "'. To publish the candidate, use 'cabal upload --publish'."
  okMessage IsPublished =
    "Package successfully published. You can now view it at '"
    ++ show packageUri ++ "'."

formatWarnings :: String -> String
formatWarnings x = "Warnings:\n" ++ (unlines . map ("- " ++) . lines) x

-- Trim
trim :: String -> String
trim = f . f
      where f = reverse . dropWhile isSpace
