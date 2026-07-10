{-# LANGUAGE CPP #-}

#ifdef FILE_EMBED
{-# LANGUAGE TemplateHaskell #-}
#endif

-- | The information from Paths_shake cleaned up
module Development.Shake.Internal.Paths(
    shakeVersionString,
    initDataDirectory,
    hasManualData, copyManualData,
    readDataFileHTML
    ) where

import Control.Monad.Extra
import Data.Version
import System.FilePath
import General.Extra
import qualified Data.ByteString.Lazy as LBS
import Paths_shake

#ifdef FILE_EMBED
import qualified Data.ByteString as BS
import Data.FileEmbed
#else
import Control.Exception
import System.Directory
import System.Info.Extra
import System.IO.Unsafe
import System.Environment
#endif

shakeVersionString :: String
shakeVersionString = showVersion version

#ifdef FILE_EMBED

initDataDirectory :: IO ()
initDataDirectory = pure ()

htmlDataFiles :: [(FilePath, BS.ByteString)]
htmlDataFiles =
  [ ("profile.html",  $(embedFile "html/profile.html"))
  , ("progress.html", $(embedFile "html/progress.html"))
  , ("shake.js",      $(embedFile "html/shake.js"))
  ]

readDataFileHTML :: FilePath -> IO LBS.ByteString
readDataFileHTML file = do
    case lookup file htmlDataFiles of
      Nothing -> fail $ "Could not find data file " ++ file ++ " in embedded data files!"
      Just x  -> pure (LBS.fromStrict x)

manualDirData :: [(FilePath, BS.ByteString)]
manualDirData = $(embedDir "docs/manual")

hasManualData :: IO Bool
hasManualData = pure True

copyManualData :: FilePath -> IO ()
copyManualData dest = do
    createDirectoryRecursive dest
    forM_ manualDirData $ \(file, bs) -> do
        BS.writeFile (dest </> file) bs

#else
-- We want getDataFileName to be relative to the current directory on program startup,
-- even if we issue a change directory command. Therefore, first call caches, future ones read.
{-# NOINLINE dataDirs #-}
dataDirs :: [String]
dataDirs = unsafePerformIO $ do
    datdir <- getDataDir
    exedir <- takeDirectory <$> getExecutablePath `catchIO` \_ -> pure ""
    curdir <- getCurrentDirectory
    pure $ [datdir] ++ [exedir | exedir /= ""] ++ [curdir]

-- The data files may be located relative to the current directory, if so cache it in advance
initDataDirectory :: IO ()
initDataDirectory = void $ evaluate dataDirs

getDataFile :: FilePath -> IO FilePath
getDataFile file = do
    let poss = map (</> file) dataDirs
    res <- filterM doesFileExist_ poss
    case res of
        [] -> fail $ unlines $ ("Could not find data file " ++ file ++ ", looked in:") : map ("  " ++) poss
        x:_ -> pure x

hasDataFile :: FilePath -> IO Bool
hasDataFile file = anyM (\dir -> doesFileExist_ $ dir </> file) dataDirs

readDataFileHTML :: FilePath -> IO LBS.ByteString
readDataFileHTML file = LBS.readFile =<< getDataFile ("html" </> file)

manualFiles :: [FilePath]
manualFiles = map ("docs/manual" </>) ["Shakefile.hs","main.c","constants.c","constants.h","build" <.> if isWindows then "bat" else "sh"]

hasManualData :: IO Bool
hasManualData = allM hasDataFile manualFiles

copyManualData :: FilePath -> IO ()
copyManualData dest = do
    createDirectoryRecursive dest
    forM_ manualFiles $ \file -> do
        src <- getDataFile file
        copyFile src (dest </> takeFileName file)
#endif
