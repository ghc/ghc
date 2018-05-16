-- | See GHC #10762 and #15021.
module GHC.HandleEncoding (configureHandleEncoding) where

import GHC.IO.Encoding (textEncodingName)
import System.Environment
import System.IO

-- | Handle GHC-specific character encoding flags, allowing us to control how
-- GHC produces output regardless of OS.
configureHandleEncoding :: IO ()
configureHandleEncoding = do
   env <- getEnvironment
   case lookup "GHC_CHARENC" env of
    Just "UTF-8" -> do
     hSetEncoding stdout utf8
     hSetEncoding stderr utf8
    _ -> do
     -- Avoid GHC erroring out when trying to display unhandled characters
     hSetTranslit stdout
     hSetTranslit stderr

-- | Change the character encoding of the given Handle to transliterate
-- on unsupported characters instead of throwing an exception
hSetTranslit :: Handle -> IO ()
hSetTranslit h = do
    menc <- hGetEncoding h
    case fmap textEncodingName menc of
        Just name | '/' `notElem` name -> do
            enc' <- mkTextEncoding $ name ++ "//TRANSLIT"
            hSetEncoding h enc'
        _ -> return ()
