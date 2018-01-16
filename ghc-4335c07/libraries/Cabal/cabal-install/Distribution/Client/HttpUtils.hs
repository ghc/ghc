{-# LANGUAGE BangPatterns #-}
-----------------------------------------------------------------------------
-- | Separate module for HTTP actions, using a proxy server if one exists.
-----------------------------------------------------------------------------
module Distribution.Client.HttpUtils (
    DownloadResult(..),
    configureTransport,
    HttpTransport(..),
    HttpCode,
    downloadURI,
    transportCheckHttps,
    remoteRepoCheckHttps,
    remoteRepoTryUpgradeToHttps,
    isOldHackageURI
  ) where

import Prelude ()
import Distribution.Client.Compat.Prelude

import Network.HTTP
         ( Request (..), Response (..), RequestMethod (..)
         , Header(..), HeaderName(..), lookupHeader )
import Network.HTTP.Proxy ( Proxy(..), fetchProxy)
import Network.URI
         ( URI (..), URIAuth (..), uriToString )
import Network.Browser
         ( browse, setOutHandler, setErrHandler, setProxy
         , setAuthorityGen, request, setAllowBasicAuth, setUserAgent )
import qualified Control.Exception as Exception
import Control.Exception
         ( evaluate )
import Control.DeepSeq
         ( force )
import Control.Monad
         ( guard )
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Paths_cabal_install (version)
import Distribution.Verbosity (Verbosity)
import Distribution.Simple.Utils
         ( die', info, warn, debug, notice, writeFileAtomic
         , copyFileVerbose,  withTempFile )
import Distribution.Client.Utils
         ( withTempFileName )
import Distribution.Client.Types
         ( RemoteRepo(..) )
import Distribution.System
         ( buildOS, buildArch )
import Distribution.Text
         ( display )
import qualified System.FilePath.Posix as FilePath.Posix
         ( splitDirectories )
import System.FilePath
         ( (<.>), takeFileName, takeDirectory )
import System.Directory
         ( doesFileExist, renameFile, canonicalizePath )
import System.IO
         ( withFile, IOMode(ReadMode), hGetContents, hClose )
import System.IO.Error
         ( isDoesNotExistError )
import Distribution.Simple.Program
         ( Program, simpleProgram, ConfiguredProgram, programPath
         , ProgramInvocation(..), programInvocation
         , getProgramInvocationOutput )
import Distribution.Simple.Program.Db
         ( ProgramDb, emptyProgramDb, addKnownPrograms
         , configureAllKnownPrograms
         , requireProgram, lookupProgram )
import Distribution.Simple.Program.Run
         ( getProgramInvocationOutputAndErrors )
import Numeric (showHex)
import System.Random (randomRIO)
import System.Exit (ExitCode(..))


------------------------------------------------------------------------------
-- Downloading a URI, given an HttpTransport
--

data DownloadResult = FileAlreadyInCache
                    | FileDownloaded FilePath
  deriving (Eq)

downloadURI :: HttpTransport
            -> Verbosity
            -> URI      -- ^ What to download
            -> FilePath -- ^ Where to put it
            -> IO DownloadResult
downloadURI _transport verbosity uri path | uriScheme uri == "file:" = do
  copyFileVerbose verbosity (uriPath uri) path
  return (FileDownloaded path)
  -- Can we store the hash of the file so we can safely return path when the
  -- hash matches to avoid unnecessary computation?

downloadURI transport verbosity uri path = do

    let etagPath = path <.> "etag"
    targetExists   <- doesFileExist path
    etagPathExists <- doesFileExist etagPath
    -- In rare cases the target file doesn't exist, but the etag does.
    etag <- if targetExists && etagPathExists
              then Just <$> readFile etagPath
              else return Nothing

    -- Only use the external http transports if we actually have to
    -- (or have been told to do so)
    let transport'
          | uriScheme uri == "http:"
          , not (transportManuallySelected transport)
          = plainHttpTransport

          | otherwise
          = transport

    withTempFileName (takeDirectory path) (takeFileName path) $ \tmpFile -> do
      result <- getHttp transport' verbosity uri etag tmpFile []

      -- Only write the etag if we get a 200 response code.
      -- A 304 still sends us an etag header.
      case result of
        (200, Just newEtag) -> writeFile etagPath newEtag
        _ -> return ()

      case fst result of
        200 -> do
            info verbosity ("Downloaded to " ++ path)
            renameFile tmpFile path
            return (FileDownloaded path)
        304 -> do
            notice verbosity "Skipping download: local and remote files match."
            return FileAlreadyInCache
        errCode ->  die' verbosity $ "Failed to download " ++ show uri
                       ++ " : HTTP code " ++ show errCode

------------------------------------------------------------------------------
-- Utilities for repo url management
--

remoteRepoCheckHttps :: Verbosity -> HttpTransport -> RemoteRepo -> IO ()
remoteRepoCheckHttps verbosity transport repo
  | uriScheme (remoteRepoURI repo) == "https:"
  , not (transportSupportsHttps transport)
              = die' verbosity $ "The remote repository '" ++ remoteRepoName repo
                   ++ "' specifies a URL that " ++ requiresHttpsErrorMessage
  | otherwise = return ()

transportCheckHttps :: Verbosity -> HttpTransport -> URI -> IO ()
transportCheckHttps verbosity transport uri
  | uriScheme uri == "https:"
  , not (transportSupportsHttps transport)
              = die' verbosity $ "The URL " ++ show uri
                   ++ " " ++ requiresHttpsErrorMessage
  | otherwise = return ()

requiresHttpsErrorMessage :: String
requiresHttpsErrorMessage =
      "requires HTTPS however the built-in HTTP implementation "
   ++ "does not support HTTPS. The transport implementations with HTTPS "
   ++ "support are " ++ intercalate ", "
      [ name | (name, _, True, _ ) <- supportedTransports ]
   ++ ". One of these will be selected automatically if the corresponding "
   ++ "external program is available, or one can be selected specifically "
   ++ "with the global flag --http-transport="

remoteRepoTryUpgradeToHttps :: Verbosity -> HttpTransport -> RemoteRepo -> IO RemoteRepo
remoteRepoTryUpgradeToHttps verbosity transport repo
  | remoteRepoShouldTryHttps repo
  , uriScheme (remoteRepoURI repo) == "http:"
  , not (transportSupportsHttps transport)
  , not (transportManuallySelected transport)
  = die' verbosity $ "The builtin HTTP implementation does not support HTTPS, but using "
       ++ "HTTPS for authenticated uploads is recommended. "
       ++ "The transport implementations with HTTPS support are "
       ++ intercalate ", " [ name | (name, _, True, _ ) <- supportedTransports ]
       ++ "but they require the corresponding external program to be "
       ++ "available. You can either make one available or use plain HTTP by "
       ++ "using the global flag --http-transport=plain-http (or putting the "
       ++ "equivalent in the config file). With plain HTTP, your password "
       ++ "is sent using HTTP digest authentication so it cannot be easily "
       ++ "intercepted, but it is not as secure as using HTTPS."

  | remoteRepoShouldTryHttps repo
  , uriScheme (remoteRepoURI repo) == "http:"
  , transportSupportsHttps transport
  = return repo {
      remoteRepoURI = (remoteRepoURI repo) { uriScheme = "https:" }
    }

  | otherwise
  = return repo

-- | Utility function for legacy support.
isOldHackageURI :: URI -> Bool
isOldHackageURI uri
    = case uriAuthority uri of
        Just (URIAuth {uriRegName = "hackage.haskell.org"}) ->
            FilePath.Posix.splitDirectories (uriPath uri)
            == ["/","packages","archive"]
        _ -> False


------------------------------------------------------------------------------
-- Setting up a HttpTransport
--

data HttpTransport = HttpTransport {
      -- | GET a URI, with an optional ETag (to do a conditional fetch),
      -- write the resource to the given file and return the HTTP status code,
      -- and optional ETag.
      getHttp  :: Verbosity -> URI -> Maybe ETag -> FilePath -> [Header]
               -> IO (HttpCode, Maybe ETag),

      -- | POST a resource to a URI, with optional auth (username, password)
      -- and return the HTTP status code and any redirect URL.
      postHttp :: Verbosity -> URI -> String -> Maybe Auth
               -> IO (HttpCode, String),

      -- | POST a file resource to a URI using multipart\/form-data encoding,
      -- with optional auth (username, password) and return the HTTP status
      -- code and any error string.
      postHttpFile :: Verbosity -> URI -> FilePath -> Maybe Auth
                   -> IO (HttpCode, String),

      -- | PUT a file resource to a URI, with optional auth
      -- (username, password), extra headers and return the HTTP status code
      -- and any error string.
      putHttpFile :: Verbosity -> URI -> FilePath -> Maybe Auth -> [Header]
                  -> IO (HttpCode, String),

      -- | Whether this transport supports https or just http.
      transportSupportsHttps :: Bool,

      -- | Whether this transport implementation was specifically chosen by
      -- the user via configuration, or whether it was automatically selected.
      -- Strictly speaking this is not a property of the transport itself but
      -- about how it was chosen. Nevertheless it's convenient to keep here.
      transportManuallySelected :: Bool
    }
    --TODO: why does postHttp return a redirect, but postHttpFile return errors?

type HttpCode = Int
type ETag     = String
type Auth     = (String, String)

noPostYet :: Verbosity -> URI -> String -> Maybe (String, String)
          -> IO (Int, String)
noPostYet verbosity _ _ _ = die' verbosity "Posting (for report upload) is not implemented yet"

supportedTransports :: [(String, Maybe Program, Bool,
                         ProgramDb -> Maybe HttpTransport)]
supportedTransports =
    [ let prog = simpleProgram "curl" in
      ( "curl", Just prog, True
      , \db -> curlTransport <$> lookupProgram prog db )

    , let prog = simpleProgram "wget" in
      ( "wget", Just prog, True
      , \db -> wgetTransport <$> lookupProgram prog db )

    , let prog = simpleProgram "powershell" in
      ( "powershell", Just prog, True
      , \db -> powershellTransport <$> lookupProgram prog db )

    , ( "plain-http", Nothing, False
      , \_ -> Just plainHttpTransport )
    ]

configureTransport :: Verbosity -> Maybe String -> IO HttpTransport

configureTransport verbosity (Just name) =
    -- the user secifically selected a transport by name so we'll try and
    -- configure that one

    case find (\(name',_,_,_) -> name' == name) supportedTransports of
      Just (_, mprog, _tls, mkTrans) -> do

        progdb <- case mprog of
          Nothing   -> return emptyProgramDb
          Just prog -> snd <$> requireProgram verbosity prog emptyProgramDb
                       --      ^^ if it fails, it'll fail here

        let Just transport = mkTrans progdb
        return transport { transportManuallySelected = True }

      Nothing -> die' verbosity $ "Unknown HTTP transport specified: " ++ name
                    ++ ". The supported transports are "
                    ++ intercalate ", "
                         [ name' | (name', _, _, _ ) <- supportedTransports ]

configureTransport verbosity Nothing = do
    -- the user hasn't selected a transport, so we'll pick the first one we
    -- can configure successfully, provided that it supports tls

    -- for all the transports except plain-http we need to try and find
    -- their external executable
    progdb <- configureAllKnownPrograms  verbosity $
                addKnownPrograms
                  [ prog | (_, Just prog, _, _) <- supportedTransports ]
                  emptyProgramDb

    let availableTransports =
          [ (name, transport)
          | (name, _, _, mkTrans) <- supportedTransports
          , transport <- maybeToList (mkTrans progdb) ]
        -- there's always one because the plain one is last and never fails
    let (name, transport) = head availableTransports
    debug verbosity $ "Selected http transport implementation: " ++ name

    return transport { transportManuallySelected = False }


------------------------------------------------------------------------------
-- The HttpTransports based on external programs
--

curlTransport :: ConfiguredProgram -> HttpTransport
curlTransport prog =
    HttpTransport gethttp posthttp posthttpfile puthttpfile True False
  where
    gethttp verbosity uri etag destPath reqHeaders = do
        withTempFile (takeDirectory destPath)
                     "curl-headers.txt" $ \tmpFile tmpHandle -> do
          hClose tmpHandle
          let args = [ show uri
                   , "--output", destPath
                   , "--location"
                   , "--write-out", "%{http_code}"
                   , "--user-agent", userAgent
                   , "--silent", "--show-error"
                   , "--dump-header", tmpFile ]
                ++ concat
                   [ ["--header", "If-None-Match: " ++ t]
                   | t <- maybeToList etag ]
                ++ concat
                   [ ["--header", show name ++ ": " ++ value]
                   | Header name value <- reqHeaders ]

          resp <- getProgramInvocationOutput verbosity
                    (programInvocation prog args)
          withFile tmpFile ReadMode $ \hnd -> do
            headers <- hGetContents hnd
            (code, _err, etag') <- parseResponse verbosity uri resp headers
            evaluate $ force (code, etag')

    posthttp = noPostYet

    addAuthConfig auth progInvocation = progInvocation
      { progInvokeInput = do
          (uname, passwd) <- auth
          return $ unlines
            [ "--digest"
            , "--user " ++ uname ++ ":" ++ passwd
            ]
      , progInvokeArgs = ["--config", "-"] ++ progInvokeArgs progInvocation
      }

    posthttpfile verbosity uri path auth = do
        let args = [ show uri
                   , "--form", "package=@"++path
                   , "--write-out", "\n%{http_code}"
                   , "--user-agent", userAgent
                   , "--silent", "--show-error"
                   , "--header", "Accept: text/plain"
                   , "--location"
                   ]
        resp <- getProgramInvocationOutput verbosity $ addAuthConfig auth
                  (programInvocation prog args)
        (code, err, _etag) <- parseResponse verbosity uri resp ""
        return (code, err)

    puthttpfile verbosity uri path auth headers = do
        let args = [ show uri
                   , "--request", "PUT", "--data-binary", "@"++path
                   , "--write-out", "\n%{http_code}"
                   , "--user-agent", userAgent
                   , "--silent", "--show-error"
                   , "--location"
                   , "--header", "Accept: text/plain"
                   ]
                ++ concat
                   [ ["--header", show name ++ ": " ++ value]
                   | Header name value <- headers ]
        resp <- getProgramInvocationOutput verbosity $ addAuthConfig auth
                  (programInvocation prog args)
        (code, err, _etag) <- parseResponse verbosity uri resp ""
        return (code, err)

    -- on success these curl invocations produces an output like "200"
    -- and on failure it has the server error response first
    parseResponse :: Verbosity -> URI -> String -> String -> IO (Int, String, Maybe ETag)
    parseResponse verbosity uri resp headers =
      let codeerr =
            case reverse (lines resp) of
              (codeLine:rerrLines) ->
                case readMaybe (trim codeLine) of
                  Just i  -> let errstr = mkErrstr rerrLines
                              in Just (i, errstr)
                  Nothing -> Nothing
              []          -> Nothing

          mkErrstr = unlines . reverse . dropWhile (all isSpace)

          mb_etag :: Maybe ETag
          mb_etag  = listToMaybe $ reverse
                     [ etag
                     | ["ETag:", etag] <- map words (lines headers) ]

       in case codeerr of
            Just (i, err) -> return (i, err, mb_etag)
            _             -> statusParseFail verbosity uri resp


wgetTransport :: ConfiguredProgram -> HttpTransport
wgetTransport prog =
  HttpTransport gethttp posthttp posthttpfile puthttpfile True False
  where
    gethttp verbosity uri etag destPath reqHeaders =  do
        resp <- runWGet verbosity uri args

        -- wget doesn't support range requests.
        -- so, we not only ignore range request headers,
        -- but we also dispay a warning message when we see them.
        let hasRangeHeader =  any isRangeHeader reqHeaders
            warningMsg     =  "the 'wget' transport currently doesn't support"
                           ++ " range requests, which wastes network bandwidth."
                           ++ " To fix this, set 'http-transport' to 'curl' or"
                           ++ " 'plain-http' in '~/.cabal/config'."
                           ++ " Note that the 'plain-http' transport doesn't"
                           ++ " support HTTPS.\n"

        when (hasRangeHeader) $ warn verbosity warningMsg
        (code, etag') <- parseOutput verbosity uri resp
        return (code, etag')
      where
        args = [ "--output-document=" ++ destPath
               , "--user-agent=" ++ userAgent
               , "--tries=5"
               , "--timeout=15"
               , "--server-response" ]
            ++ concat
               [ ["--header", "If-None-Match: " ++ t]
               | t <- maybeToList etag ]
            ++ [ "--header=" ++ show name ++ ": " ++ value
               | hdr@(Header name value) <- reqHeaders
               , (not (isRangeHeader hdr)) ]

        -- wget doesn't support range requests.
        -- so, we ignore range request headers, lest we get errors.
        isRangeHeader :: Header -> Bool
        isRangeHeader (Header HdrRange _) = True
        isRangeHeader _ = False

    posthttp = noPostYet

    posthttpfile verbosity  uri path auth =
        withTempFile (takeDirectory path)
                     (takeFileName path) $ \tmpFile tmpHandle ->
        withTempFile (takeDirectory path) "response" $
        \responseFile responseHandle -> do
          hClose responseHandle
          (body, boundary) <- generateMultipartBody path
          BS.hPut tmpHandle body
          hClose tmpHandle
          let args = [ "--post-file=" ++ tmpFile
                     , "--user-agent=" ++ userAgent
                     , "--server-response"
                     , "--output-document=" ++ responseFile
                     , "--header=Accept: text/plain"
                     , "--header=Content-type: multipart/form-data; " ++
                                              "boundary=" ++ boundary ]
          out <- runWGet verbosity (addUriAuth auth uri) args
          (code, _etag) <- parseOutput verbosity uri out
          withFile responseFile ReadMode $ \hnd -> do
            resp <- hGetContents hnd
            evaluate $ force (code, resp)

    puthttpfile verbosity uri path auth headers =
        withTempFile (takeDirectory path) "response" $
        \responseFile responseHandle -> do
            hClose responseHandle
            let args = [ "--method=PUT", "--body-file="++path
                       , "--user-agent=" ++ userAgent
                       , "--server-response"
                       , "--output-document=" ++ responseFile
                       , "--header=Accept: text/plain" ]
                    ++ [ "--header=" ++ show name ++ ": " ++ value
                       | Header name value <- headers ]

            out <- runWGet verbosity (addUriAuth auth uri) args
            (code, _etag) <- parseOutput verbosity uri out
            withFile responseFile ReadMode $ \hnd -> do
              resp <- hGetContents hnd
              evaluate $ force (code, resp)

    addUriAuth Nothing uri = uri
    addUriAuth (Just (user, pass)) uri = uri
      { uriAuthority = Just a { uriUserInfo = user ++ ":" ++ pass ++ "@" }
      }
     where
      a = fromMaybe (URIAuth "" "" "") (uriAuthority uri)

    runWGet verbosity uri args = do
        -- We pass the URI via STDIN because it contains the users' credentials
        -- and sensitive data should not be passed via command line arguments.
        let
          invocation = (programInvocation prog ("--input-file=-" : args))
            { progInvokeInput = Just (uriToString id uri "")
            }

        -- wget returns its output on stderr rather than stdout
        (_, resp, exitCode) <- getProgramInvocationOutputAndErrors verbosity
                                 invocation
        -- wget returns exit code 8 for server "errors" like "304 not modified"
        if exitCode == ExitSuccess || exitCode == ExitFailure 8
          then return resp
          else die' verbosity $ "'" ++ programPath prog
                  ++ "' exited with an error:\n" ++ resp

    -- With the --server-response flag, wget produces output with the full
    -- http server response with all headers, we want to find a line like
    -- "HTTP/1.1 200 OK", but only the last one, since we can have multiple
    -- requests due to redirects.
    parseOutput verbosity uri resp =
      let parsedCode = listToMaybe
                     [ code
                     | (protocol:codestr:_err) <- map words (reverse (lines resp))
                     , "HTTP/" `isPrefixOf` protocol
                     , code <- maybeToList (readMaybe codestr) ]
          mb_etag :: Maybe ETag
          mb_etag  = listToMaybe
                    [ etag
                    | ["ETag:", etag] <- map words (reverse (lines resp)) ]
       in case parsedCode of
            Just i -> return (i, mb_etag)
            _      -> statusParseFail verbosity uri resp


powershellTransport :: ConfiguredProgram -> HttpTransport
powershellTransport prog =
    HttpTransport gethttp posthttp posthttpfile puthttpfile True False
  where
    gethttp verbosity uri etag destPath reqHeaders = do
      resp <- runPowershellScript verbosity $
        webclientScript
          (setupHeaders ((useragentHeader : etagHeader) ++ reqHeaders))
          [ "$wc.DownloadFile(" ++ escape (show uri)
              ++ "," ++ escape destPath ++ ");"
          , "Write-Host \"200\";"
          , "Write-Host $wc.ResponseHeaders.Item(\"ETag\");"
          ]
      parseResponse resp
      where
        parseResponse x = case readMaybe . unlines . take 1 . lines $ trim x of
          Just i  -> return (i, Nothing) -- TODO extract real etag
          Nothing -> statusParseFail verbosity uri x
        etagHeader = [ Header HdrIfNoneMatch t | t <- maybeToList etag ]

    posthttp = noPostYet

    posthttpfile verbosity uri path auth =
      withTempFile (takeDirectory path)
                   (takeFileName path) $ \tmpFile tmpHandle -> do
        (body, boundary) <- generateMultipartBody path
        BS.hPut tmpHandle body
        hClose tmpHandle
        fullPath <- canonicalizePath tmpFile

        let contentHeader = Header HdrContentType
              ("multipart/form-data; boundary=" ++ boundary)
        resp <- runPowershellScript verbosity $ webclientScript
          (setupHeaders (contentHeader : extraHeaders) ++ setupAuth auth)
          (uploadFileAction "POST" uri fullPath)
        parseUploadResponse verbosity uri resp

    puthttpfile verbosity uri path auth headers = do
      fullPath <- canonicalizePath path
      resp <- runPowershellScript verbosity $ webclientScript
        (setupHeaders (extraHeaders ++ headers) ++ setupAuth auth)
        (uploadFileAction "PUT" uri fullPath)
      parseUploadResponse verbosity uri resp

    runPowershellScript verbosity script = do
      let args =
            [ "-InputFormat", "None"
            -- the default execution policy doesn't allow running
            -- unsigned scripts, so we need to tell powershell to bypass it
            , "-ExecutionPolicy", "bypass"
            , "-NoProfile", "-NonInteractive"
            , "-Command", "-"
            ]
      getProgramInvocationOutput verbosity (programInvocation prog args)
        { progInvokeInput = Just (script ++ "\nExit(0);")
        }

    escape = show

    useragentHeader = Header HdrUserAgent userAgent
    extraHeaders = [Header HdrAccept "text/plain", useragentHeader]

    setupHeaders headers =
      [ "$wc.Headers.Add(" ++ escape (show name) ++ "," ++ escape value ++ ");"
      | Header name value <- headers
      ]

    setupAuth auth =
      [ "$wc.Credentials = new-object System.Net.NetworkCredential("
          ++ escape uname ++ "," ++ escape passwd ++ ",\"\");"
      | (uname,passwd) <- maybeToList auth
      ]

    uploadFileAction method uri fullPath =
      [ "$fileBytes = [System.IO.File]::ReadAllBytes(" ++ escape fullPath ++ ");"
      , "$bodyBytes = $wc.UploadData(" ++ escape (show uri) ++ ","
        ++ show method ++ ", $fileBytes);"
      , "Write-Host \"200\";"
      , "Write-Host (-join [System.Text.Encoding]::UTF8.GetChars($bodyBytes));"
      ]

    parseUploadResponse verbosity uri resp = case lines (trim resp) of
      (codeStr : message)
        | Just code <- readMaybe codeStr -> return (code, unlines message)
      _ -> statusParseFail verbosity uri resp

    webclientScript setup action = unlines
      [ "$wc = new-object system.net.webclient;"
      , unlines setup
      , "Try {"
      , unlines (map ("  " ++) action)
      , "} Catch [System.Net.WebException] {"
      , "  $exception = $_.Exception;"
      , "  If ($exception.Status -eq "
        ++ "[System.Net.WebExceptionStatus]::ProtocolError) {"
      , "    $response = $exception.Response -as [System.Net.HttpWebResponse];"
      , "    $reader = new-object "
        ++ "System.IO.StreamReader($response.GetResponseStream());"
      , "    Write-Host ($response.StatusCode -as [int]);"
      , "    Write-Host $reader.ReadToEnd();"
      , "  } Else {"
      , "    Write-Host $exception.Message;"
      , "  }"
      , "} Catch {"
      , "  Write-Host $_.Exception.Message;"
      , "}"
      ]


------------------------------------------------------------------------------
-- The builtin plain HttpTransport
--

plainHttpTransport :: HttpTransport
plainHttpTransport =
    HttpTransport gethttp posthttp posthttpfile puthttpfile False False
  where
    gethttp verbosity uri etag destPath reqHeaders = do
      let req = Request{
                  rqURI     = uri,
                  rqMethod  = GET,
                  rqHeaders = [ Header HdrIfNoneMatch t
                              | t <- maybeToList etag ]
                           ++ reqHeaders,
                  rqBody    = BS.empty
                }
      (_, resp) <- cabalBrowse verbosity Nothing (request req)
      let code  = convertRspCode (rspCode resp)
          etag' = lookupHeader HdrETag (rspHeaders resp)
      -- 206 Partial Content is a normal response to a range request; see #3385.
      when (code==200 || code==206) $
        writeFileAtomic destPath $ rspBody resp
      return (code, etag')

    posthttp = noPostYet

    posthttpfile verbosity uri path auth = do
      (body, boundary) <- generateMultipartBody path
      let headers = [ Header HdrContentType
                             ("multipart/form-data; boundary="++boundary)
                    , Header HdrContentLength (show (BS.length body))
                    , Header HdrAccept ("text/plain")
                    ]
          req = Request {
                  rqURI     = uri,
                  rqMethod  = POST,
                  rqHeaders = headers,
                  rqBody    = body
                }
      (_, resp) <- cabalBrowse verbosity auth (request req)
      return (convertRspCode (rspCode resp), rspErrorString resp)

    puthttpfile verbosity uri path auth headers = do
      body <- BS.readFile path
      let req = Request {
                  rqURI     = uri,
                  rqMethod  = PUT,
                  rqHeaders = Header HdrContentLength (show (BS.length body))
                            : Header HdrAccept "text/plain"
                            : headers,
                  rqBody    = body
                }
      (_, resp) <- cabalBrowse verbosity auth (request req)
      return (convertRspCode (rspCode resp), rspErrorString resp)

    convertRspCode (a,b,c) = a*100 + b*10 + c

    rspErrorString resp =
      case lookupHeader HdrContentType (rspHeaders resp) of
        Just contenttype
           | takeWhile (/= ';') contenttype == "text/plain"
          -> BS.unpack (rspBody resp)
        _ -> rspReason resp

    cabalBrowse verbosity auth act = do
      p <- fixupEmptyProxy <$> fetchProxy True
      Exception.handleJust
        (guard . isDoesNotExistError)
        (const . die' verbosity $ "Couldn't establish HTTP connection. "
                    ++ "Possible cause: HTTP proxy server is down.") $
        browse $ do
          setProxy p
          setErrHandler (warn verbosity . ("http error: "++))
          setOutHandler (debug verbosity)
          setUserAgent  userAgent
          setAllowBasicAuth False
          setAuthorityGen (\_ _ -> return auth)
          act

    fixupEmptyProxy (Proxy uri _) | null uri = NoProxy
    fixupEmptyProxy p = p


------------------------------------------------------------------------------
-- Common stuff used by multiple transport impls
--

userAgent :: String
userAgent = concat [ "cabal-install/", display Paths_cabal_install.version
                   , " (", display buildOS, "; ", display buildArch, ")"
                   ]

statusParseFail :: Verbosity -> URI -> String -> IO a
statusParseFail verbosity uri r =
    die' verbosity $ "Failed to download " ++ show uri ++ " : "
       ++ "No Status Code could be parsed from response: " ++ r

-- Trim
trim :: String -> String
trim = f . f
      where f = reverse . dropWhile isSpace


------------------------------------------------------------------------------
-- Multipart stuff partially taken from cgi package.
--

generateMultipartBody :: FilePath -> IO (BS.ByteString, String)
generateMultipartBody path = do
    content  <- BS.readFile path
    boundary <- genBoundary
    let !body = formatBody content (BS.pack boundary)
    return (body, boundary)
  where
    formatBody content boundary =
        BS.concat $
        [ crlf, dd, boundary, crlf ]
     ++ [ BS.pack (show header) | header <- headers ]
     ++ [ crlf
        , content
        , crlf, dd, boundary, dd, crlf ]

    headers =
      [ Header (HdrCustom "Content-disposition")
               ("form-data; name=package; " ++
                "filename=\"" ++ takeFileName path ++ "\"")
      , Header HdrContentType "application/x-gzip"
      ]

    crlf = BS.pack "\r\n"
    dd   = BS.pack "--"

genBoundary :: IO String
genBoundary = do
    i <- randomRIO (0x10000000000000,0xFFFFFFFFFFFFFF) :: IO Integer
    return $ showHex i ""
