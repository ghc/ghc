{-# LANGUAGE OverloadedStrings #-}
module Main where

import GHC
import GHC.Data.FastString (mkFastStringByteString)
import qualified GHC.Data.ShortText as ST
import GHC.Driver.Monad
import GHC.Driver.Env.Types
import GHC.Driver.Session.Units
import GHC.Internal.Heap.Closures (closureSize, asBox)
import GHC.Platform
import GHC.Profiling
import GHC.Unit.Info
import GHC.Unit.Types (stringToUnitId)

import Control.Monad (guard, when)
import Data.Version (makeVersion)
import qualified Data.List.NonEmpty as NE
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import System.Environment
import qualified System.Exit as Exit
import System.Mem

main :: IO ()
main = do
    libdir:numberOfGlobalPkgsStr:restArgs <- getArgs
    runGhc (Just libdir) $ do
      initGhcM (read numberOfGlobalPkgsStr) restArgs

initGhcM :: Int -> [String] -> Ghc ()
initGhcM numOfPkgs xs = do
    session <- getSession
    df1 <- getSessionDynFlags
    let (units, args) = extractUnits xs
    let cmdOpts = ["-fforce-recomp"]  ++ args

    (df2, leftovers, _) <- parseDynamicFlags (hsc_logger session) df1 (map noLoc cmdOpts)
    setSessionDynFlags df2

    targets <- case NE.nonEmpty units of
      Nothing ->
        traverse (\s -> guessTarget s Nothing Nothing) $ map unLoc leftovers
      Just mhu -> do
        ts <- initMulti mhu (\ _ _ _ _ -> pure ())
        traverse (\(n, mu, mp) -> guessTarget n mu mp) ts

    setTargets targets
    success <- load LoadAllTargets

    liftIO $ when (failed success) $ do
      putStrLn "Failed to load targets"
      Exit.exitFailure

    liftIO $ do
      requestHeapCensus
      performGC
      putStrLn "### Heap Census"
      putStrLn "There are exactly two GenericUnitInfo closures alive per on-disk package "
      num <- getNumberOfGenericUnitInfo

      -- assertion
      let
        plat :: Platform
        plat = targetPlatform df2
        word_size = case platformWordSize plat of
          PW8 -> 8
          PW4 -> 4
        -- get the size of 'UnitInfo' in bytes
        genericUnitInfoSizeInWords = closureSize (asBox $! minimalUnitInfo)
        genericUnitInfoSizeInBytes = genericUnitInfoSizeInWords * word_size

      let
        expectedNumberOfUnitInfos =
          numOfPkgs * 2
        expectedSizeInBytes =
          expectedNumberOfUnitInfos * genericUnitInfoSizeInBytes

      -- The output should be:
      --
      -- @
      --   2 * number of packages in global unit db * word_size * sizeof(struct GenericUnitInfo)
      -- @
      --
      -- This test simply assures, that when we load a session, we don't leak 'UnitInfo's
      --
      -- If this number changes without a good reason, DO NOT ACCEPT THE CHANGES, you have introduced a space leak.
      -- We say less than the expected size is accepted, because in the multiple-home-units case, we don't force the second
      -- UnitInfo closure enough after initial processing.
      when (num > expectedSizeInBytes) $ do
        putStrLn "Space leak detected by generic-unit-info-space test:"
        putStrLn $ (show (num `div` genericUnitInfoSizeInBytes)) ++ " live GenericUnitInfo when <= (" ++ show expectedNumberOfUnitInfos ++ ") are expected"
        readFile hpFile >>= putStrLn
        Exit.exitFailure
    return ()

hpFile :: FilePath
hpFile = "generic-unit-info-space.hp"

getNumberOfGenericUnitInfo :: IO Int
getNumberOfGenericUnitInfo = do
  content <- lines <$> readFile hpFile
  let census = Maybe.mapMaybe isGenericUnitInfo content
  case census of
    [v] -> pure v
    xs -> fail $ "Unexpected number of GenericUnitInfo lines, expected 1 but got: " ++ show (length xs)

isGenericUnitInfo :: String -> Maybe Int
isGenericUnitInfo l =
  getCensusValueFor (":GHC.Unit.Database.GenericUnitInfo") l

getCensusValueFor :: String -> String -> Maybe Int
getCensusValueFor lineInfix l = do
  guard (lineInfix `List.isInfixOf` l)
  case List.unsnoc (words l) of
    Nothing -> error "input is unexpectedly empty"
    Just (_, lst) -> Just $ read lst

-- This is annoying, but the easiest way to get a compile-time assured size of 'GenericUnitInfo'
minimalUnitInfo :: UnitInfo
minimalUnitInfo = GenericUnitInfo
  { unitId                  = stringToUnitId "my-unit"
  , unitInstanceOf          = stringToUnitId "my-unit"
  , unitInstantiations      = []
  , unitPackageId           = PackageId   (mkFastStringByteString "my-pkg-0.1")
  , unitPackageName         = PackageName (mkFastStringByteString "my-pkg")
  , unitPackageVersion      = makeVersion [0,1]
  , unitComponentName       = Nothing
  , unitAbiHash             = ST.pack ""
  , unitDepends             = []
  , unitAbiDepends          = []
  , unitImportDirs          = []
  , unitLibraries           = []
  , unitExtDepLibsSys       = []
  , unitExtDepLibsStaticSys = []
  , unitExtDepLibsGhc       = []
  , unitLibraryDirs         = []
  , unitLibraryDirsStatic   = []
  , unitLibraryDynDirs      = []
  , unitLibraryBytecodeDirs = []
  , unitExtDepFrameworks    = []
  , unitExtDepFrameworkDirs = []
  , unitLinkerOptions       = []
  , unitCcOptions           = []
  , unitIncludes            = []
  , unitIncludeDirs         = []
  , unitHaddockInterfaces   = []
  , unitHaddockHTMLs        = []
  , unitExposedModules      = []
  , unitHiddenModules       = []
  , unitIsIndefinite        = False
  , unitIsExposed           = True
  , unitIsTrusted           = False
  }

extractUnits :: [String] -> ([String], [String])
extractUnits = go [] []
  where
    -- TODO: we should likely use the 'processCmdLineP' instead
    go units rest ("-unit" : x : xs) = go (x : units) rest xs
    go units rest (x : xs)           = go units (x : rest) xs
    go units rest []                 = (reverse units, reverse rest)
