{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
-- | Implementation of 'HttpLib' using cabal-install's own 'HttpTransport'
module Distribution.Client.Security.HTTP (HttpLib, transportAdapter) where

-- stdlibs
import Control.Exception
         ( Exception(..), IOException )
import Data.List
         ( intercalate )
import Data.Typeable
         ( Typeable )
import System.Directory
         ( getTemporaryDirectory )
import Network.URI
         ( URI )
import qualified Data.ByteString.Lazy as BS.L
import qualified Network.HTTP         as HTTP

-- Cabal/cabal-install
import Distribution.Verbosity
         ( Verbosity )
import Distribution.Client.HttpUtils
         ( HttpTransport(..), HttpCode )
import Distribution.Client.Utils
         ( withTempFileName )

-- hackage-security
import Hackage.Security.Client
import Hackage.Security.Client.Repository.HttpLib
import Hackage.Security.Util.Checked
import Hackage.Security.Util.Pretty
import qualified Hackage.Security.Util.Lens as Lens

{-------------------------------------------------------------------------------
  'HttpLib' implementation
-------------------------------------------------------------------------------}

-- | Translate from hackage-security's 'HttpLib' to cabal-install's 'HttpTransport'
--
-- NOTE: The match between these two APIs is currently not perfect:
--
-- * We don't get any response headers back from the 'HttpTransport', so we
--   don't know if the server supports range requests. For now we optimistically
--   assume that it does.
-- * The 'HttpTransport' wants to know where to place the resulting file,
--   whereas the 'HttpLib' expects an 'IO' action which streams the download;
--   the security library then makes sure that the file gets written to a
--   location which is suitable (in particular, to a temporary file in the
--   directory where the file needs to end up, so that it can "finalize" the
--   file simply by doing 'renameFile'). Right now we write the file to a
--   temporary file in the system temp directory here and then read it again
--   to pass it to the security library; this is a problem for two reasons: it
--   is a source of inefficiency; and it means that the security library cannot
--   insist on a minimum download rate (potential security attack).
--   Fixing it however would require changing the 'HttpTransport'.
transportAdapter :: Verbosity -> IO HttpTransport -> HttpLib
transportAdapter verbosity getTransport = HttpLib{
      httpGet      = \headers uri callback -> do
                        transport <- getTransport
                        get verbosity transport headers uri callback
    , httpGetRange = \headers uri range callback -> do
                        transport <- getTransport
                        getRange verbosity transport headers uri range callback
    }

get :: Throws SomeRemoteError
    => Verbosity
    -> HttpTransport
    -> [HttpRequestHeader] -> URI
    -> ([HttpResponseHeader] -> BodyReader -> IO a)
    -> IO a
get verbosity transport reqHeaders uri callback = wrapCustomEx $ do
  get' verbosity transport reqHeaders uri Nothing $ \code respHeaders br ->
    case code of
      200 -> callback respHeaders br
      _   -> throwChecked $ UnexpectedResponse uri code

getRange :: Throws SomeRemoteError
         => Verbosity
         -> HttpTransport
         -> [HttpRequestHeader] -> URI -> (Int, Int)
         -> (HttpStatus -> [HttpResponseHeader] -> BodyReader -> IO a)
         -> IO a
getRange verbosity transport reqHeaders uri range callback = wrapCustomEx $ do
  get' verbosity transport reqHeaders uri (Just range) $ \code respHeaders br ->
    case code of
       200 -> callback HttpStatus200OK             respHeaders br
       206 -> callback HttpStatus206PartialContent respHeaders br
       _   -> throwChecked $ UnexpectedResponse uri code

-- | Internal generalization of 'get' and 'getRange'
get' :: Verbosity
     -> HttpTransport
     -> [HttpRequestHeader] -> URI -> Maybe (Int, Int)
     -> (HttpCode -> [HttpResponseHeader] -> BodyReader -> IO a)
     -> IO a
get' verbosity transport reqHeaders uri mRange callback = do
    tempDir <- getTemporaryDirectory
    withTempFileName tempDir "transportAdapterGet" $ \temp -> do
      (code, _etag) <- getHttp transport verbosity uri Nothing temp reqHeaders'
      br <- bodyReaderFromBS =<< BS.L.readFile temp
      callback code [HttpResponseAcceptRangesBytes] br
  where
    reqHeaders' = mkReqHeaders reqHeaders mRange

{-------------------------------------------------------------------------------
  Request headers
-------------------------------------------------------------------------------}

mkRangeHeader :: Int -> Int -> HTTP.Header
mkRangeHeader from to = HTTP.Header HTTP.HdrRange rangeHeader
  where
    -- Content-Range header uses inclusive rather than exclusive bounds
    -- See <http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html>
    rangeHeader = "bytes=" ++ show from ++ "-" ++ show (to - 1)

mkReqHeaders :: [HttpRequestHeader] -> Maybe (Int, Int) -> [HTTP.Header]
mkReqHeaders reqHeaders mRange = concat [
      tr [] reqHeaders
    , [mkRangeHeader fr to | Just (fr, to) <- [mRange]]
    ]
  where
    tr :: [(HTTP.HeaderName, [String])] -> [HttpRequestHeader] -> [HTTP.Header]
    tr acc [] =
      concatMap finalize acc
    tr acc (HttpRequestMaxAge0:os) =
      tr (insert HTTP.HdrCacheControl ["max-age=0"] acc) os
    tr acc (HttpRequestNoTransform:os) =
      tr (insert HTTP.HdrCacheControl ["no-transform"] acc) os

    -- Some headers are comma-separated, others need multiple headers for
    -- multiple options.
    --
    -- TODO: Right we we just comma-separate all of them.
    finalize :: (HTTP.HeaderName, [String]) -> [HTTP.Header]
    finalize (name, strs) = [HTTP.Header name (intercalate ", " (reverse strs))]

    insert :: Eq a => a -> [b] -> [(a, [b])] -> [(a, [b])]
    insert x y = Lens.modify (Lens.lookupM x) (++ y)

{-------------------------------------------------------------------------------
  Custom exceptions
-------------------------------------------------------------------------------}

data UnexpectedResponse = UnexpectedResponse URI Int
  deriving (Typeable)

instance Pretty UnexpectedResponse where
  pretty (UnexpectedResponse uri code) = "Unexpected response " ++ show code
                                      ++ "for " ++ show uri

#if MIN_VERSION_base(4,8,0)
deriving instance Show UnexpectedResponse
instance Exception UnexpectedResponse where displayException = pretty
#else
instance Show UnexpectedResponse where show = pretty
instance Exception UnexpectedResponse
#endif

wrapCustomEx :: ( ( Throws UnexpectedResponse
                  , Throws IOException
                  ) => IO a)
             -> (Throws SomeRemoteError => IO a)
wrapCustomEx act = handleChecked (\(ex :: UnexpectedResponse) -> go ex)
                 $ handleChecked (\(ex :: IOException)        -> go ex)
                 $ act
  where
    go ex = throwChecked (SomeRemoteError ex)
