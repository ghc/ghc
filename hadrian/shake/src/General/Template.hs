{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}

-- Keep using iso8601DateFormat, since the alternative was introduced in time-1.9
-- while GHC 8.6 still has time-1.8.
-- Safe once we no longer support GHC 8.6.
{-# OPTIONS_GHC -Wno-deprecations #-}

#ifdef FILE_EMBED
{-# LANGUAGE TemplateHaskell #-}
#endif

module General.Template(runTemplate) where

import System.FilePath.Posix
import Control.Exception.Extra
import Data.Char
import Data.Time
import System.IO.Unsafe
import Development.Shake.Internal.Paths
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Language.Javascript.DGTable as DGTable
import qualified Language.Javascript.Flot as Flot
import qualified Language.Javascript.JQuery as JQuery

#ifdef FILE_EMBED
import Data.FileEmbed
import Language.Haskell.TH.Syntax ( runIO )
#endif

{- HLINT ignore "Redundant bracket" -} -- a result of CPP expansion

-- Very hard to abstract over TH, so we do it with CPP
#ifdef FILE_EMBED
#define FILE(x) (pure (LBS.fromStrict $(embedFile =<< runIO (x))))
#else
#define FILE(x) (LBS.readFile =<< (x))
#endif

libraries :: [(String, IO LBS.ByteString)]
libraries =
    [("jquery.js",            FILE(JQuery.file))
    ,("jquery.dgtable.js",    FILE(DGTable.file))
    ,("jquery.flot.js",       FILE(Flot.file Flot.Flot))
    ,("jquery.flot.stack.js", FILE(Flot.file Flot.FlotStack))
    ]


-- | Template Engine. Perform the following replacements on a line basis:
--
-- * <script src="foo"></script> ==> <script>[[foo]]</script>
--
-- * <link href="foo" rel="stylesheet" type="text/css" /> ==> <style type="text/css">[[foo]]</style>
runTemplate :: (FilePath -> IO LBS.ByteString) -> LBS.ByteString -> IO LBS.ByteString
runTemplate ask = lbsMapLinesIO f
    where
        link = LBS.pack "<link href=\""
        script = LBS.pack "<script src=\""

        f x | Just file <- lbsStripPrefix script y = do res <- grab file; pure $ LBS.pack "<script>\n" `LBS.append` res `LBS.append` LBS.pack "\n</script>"
            | Just file <- lbsStripPrefix link y = do res <- grab file; pure $ LBS.pack "<style type=\"text/css\">\n" `LBS.append` res `LBS.append` LBS.pack "\n</style>"
            | otherwise = pure x
            where
                y = LBS.dropWhile isSpace x
                grab = asker . takeWhile (/= '\"') . LBS.unpack

        asker o@(splitFileName -> ("lib/",x)) =
            case lookup x libraries of
                Nothing -> errorIO $ "Template library, unknown library: " ++ o
                Just act -> act

        asker "shake.js" = readDataFileHTML "shake.js"
        asker "data/metadata.js" = do
            time <- getCurrentTime
            pure $ LBS.pack $
                "var version = " ++ show shakeVersionString ++
                "\nvar generated = " ++ show (formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S")) time)
        asker x = ask x

-- Perform a mapM on each line and put the result back together again
lbsMapLinesIO :: (LBS.ByteString -> IO LBS.ByteString) -> LBS.ByteString -> IO LBS.ByteString
-- If we do the obvious @fmap LBS.unlines . mapM f@ then all the monadic actions are run on all the lines
-- before it starts producing the lazy result, killing streaming and having more stack usage.
-- The real solution (albeit with too many dependencies for something small) is a streaming library,
-- but a little bit of unsafePerformIO does the trick too.
lbsMapLinesIO f = pure . LBS.unlines . map (unsafePerformIO . f) . LBS.lines


---------------------------------------------------------------------
-- COMPATIBILITY

-- available in bytestring-0.10.8.0, GHC 8.0 and above
-- alternative implementation below
lbsStripPrefix :: LBS.ByteString -> LBS.ByteString -> Maybe LBS.ByteString
lbsStripPrefix prefix text = if a == prefix then Just b else Nothing
    where (a,b) = LBS.splitAt (LBS.length prefix) text
