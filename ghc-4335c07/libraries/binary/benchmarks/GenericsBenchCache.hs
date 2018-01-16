{-# LANGUAGE DeriveGeneric, StandaloneDeriving, BangPatterns, CPP #-}
module GenericsBenchCache (readPackageDescriptionCache) where

import qualified Text.ParserCombinators.ReadP                  as Read

import qualified Data.ByteString.Lazy                          as L
import qualified Data.ByteString.Lazy.Char8                    as LC8

import           Data.Version                                  (parseVersion)
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Configuration
import           Distribution.PackageDescription.Parse
import           Distribution.Version                          (Version)

import qualified Codec.Archive.Tar                             as Tar
import qualified Codec.Compression.GZip                        as GZip
import qualified Data.HashMap.Lazy                             as Map
import           System.Directory
import           System.Exit

import           GenericsBenchTypes                            ()

#if ! MIN_VERSION_base(4,8,0)
import           Control.Applicative                           ((<$>))
#endif

readTar :: String -> Int -> IO [PackageDescription]
readTar tarPath limit = do
  entries <- Tar.read . GZip.decompress <$> L.readFile tarPath
  let contents = Tar.foldEntries unpack [] (error "tar error") entries
  let !pkgs = Map.fromListWith pick
                      [ (pkg, (version, content))
                      | (path, content) <- contents
                      , Just (pkg, version) <- return (readFilePath path) ]

  return $ take limit [ flattenPackageDescription gpd
                      | (_, (_, content)) <- Map.toList pkgs
                      , ParseOk _warns gpd <- return (parsePackageDescription (LC8.unpack content)) ]
    where
      pick (v,a) (w,b) | v >= w = (v,a)
                       | otherwise = (w,b)
      unpack e acc =
        case Tar.entryContent e of
          Tar.NormalFile content _ -> (Tar.entryPath e, content):acc
          _ -> acc

readFilePath :: String -> Maybe (String, Version)
readFilePath str = extract (Read.readP_to_S parse str)
  where
    extract [(result,_)] = Just result
    extract _ = Nothing
    parse = do
      packageName <- Read.many1 (Read.satisfy (/='/'))
      _ <- Read.char '/'
      version <- parseVersion
      _ <- Read.char '/'
      return (packageName, version)

writePackageDescriptionCache :: String -> [PackageDescription] -> IO ()
writePackageDescriptionCache path = writeFile path . show

readPackageDescriptionCache :: Int -> IO [PackageDescription]
readPackageDescriptionCache amount = do
  let cacheFilePath' = cacheFilePath ++ "-" ++ (show amount)
  createPackageDescriptionCache cacheFilePath' amount
  pds <- read <$> readFile cacheFilePath'
  -- PackageDescription doesn't implement NFData, let's force with the following line
  (length (show pds)) `seq` return pds

cacheFilePath :: String
cacheFilePath = "generics-bench.cache"

createPackageDescriptionCache :: String -> Int -> IO ()
createPackageDescriptionCache path amount = do
  cacheExists <- doesFileExist path
  if cacheExists
    then putStrLn "reusing cache from previous run"
    else do
      putStr "creating cabal cache file... "
      tarFilePath <- (++"/.cabal/packages/hackage.haskell.org/00-index.tar.gz") <$> getHomeDirectory
      fileExists <- doesFileExist tarFilePath
      if fileExists
        then do
          pds <- readTar tarFilePath amount
          writePackageDescriptionCache path pds
          putStrLn "done"
        else do
          putStrLn (tarFilePath ++ " missing, aborting")
          exitFailure
