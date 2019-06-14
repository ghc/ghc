module Rules.Register (configurePackageRules, registerPackageRules) where

import Base
import Context
import Hadrian.BuildPath
import Hadrian.Expression
import Hadrian.Haskell.Cabal
import Oracles.Setting
import Packages
import Rules.Gmp
import Rules.Rts
import Settings
import Target
import Utilities
import Rules.Library
import Hadrian.Oracles.Cabal
import Hadrian.Haskell.Cabal.Type

import Distribution.Version (Version)
import qualified Distribution.Parsec as Cabal
import qualified Distribution.Types.PackageName as Cabal
import qualified Distribution.Types.PackageId as Cabal
import qualified Distribution.Version as Cabal

import qualified Hadrian.Haskell.Cabal.Parse as Cabal
import qualified System.Directory            as IO
import qualified Text.Parsec                 as Parsec
import qualified Distribution.Compat.CharParsing as P
import Rules.SimpleTargets

-- * Configuring

-- | Configure a package and build its @setup-config@ file, as well as files in
-- the @build/pkgName/build/autogen@ directory.
configurePackageRules :: Rules ()
configurePackageRules = do
    root <- buildRootRules
    root -/- "**/setup-config" %> \out -> do
        (stage, path) <- parsePath (parseSetupConfig root) "<setup config path parser>" out
        let pkg = unsafeFindPackageByPath path
        Cabal.configurePackage (Context stage pkg vanilla)

    root -/- "**/autogen/cabal_macros.h" %> \out -> do
        (stage, path) <- parsePath (parseToBuildSubdirectory root) "<cabal macros path parser>" out
        let pkg = unsafeFindPackageByPath path
        Cabal.buildAutogenFiles (Context stage pkg vanilla)

    root -/- "**/autogen/Paths_*.hs" %> \out ->
        need [takeDirectory out -/- "cabal_macros.h"]

parseSetupConfig :: FilePath -> Parsec.Parsec String () (Stage, FilePath)
parseSetupConfig root = do
    _ <- Parsec.string root *> Parsec.optional (Parsec.char '/')
    stage <- parseStage
    _ <- Parsec.char '/'
    pkgPath <- Parsec.manyTill Parsec.anyChar
        (Parsec.try $ Parsec.string "/setup-config")
    return (stage, pkgPath)

parseToBuildSubdirectory :: FilePath -> Parsec.Parsec String () (Stage, FilePath)
parseToBuildSubdirectory root = do
    _ <- Parsec.string root *> Parsec.optional (Parsec.char '/')
    stage <- parseStage
    _ <- Parsec.char '/'
    pkgPath <- Parsec.manyTill Parsec.anyChar
        (Parsec.try $ Parsec.string "/build/")
    return (stage, pkgPath)

-- * Registering

-- | Register a package and initialise the corresponding package database if
-- need be. Note that we only register packages in 'Stage0' and 'Stage1'.
registerPackageRules :: [(Resource, Int)] -> Stage -> Rules ()
registerPackageRules rs stage = do
    root <- buildRootRules

    -- Initialise the package database.
    root -/- relativePackageDbPath stage -/- packageDbStamp %> \stamp ->
        writeFileLines stamp []

mungeGhc "ghc" = "ghc1"
mungeGhc x = x


getPackageNameFromConfFile :: FilePath -> Action String
getPackageNameFromConfFile conf
    | takeBaseName conf == "rts" = return "rts"
    | otherwise = case parseCabalName (takeBaseName conf) of
        Left err -> error $ "getPackageNameFromConfFile: Couldn't parse " ++
                            takeBaseName conf ++ ": " ++ err
        Right (name, _) -> return name

getPackageNameFromConfFileV :: FilePath -> Action (String, Version)
getPackageNameFromConfFileV conf
    | takeBaseName conf == "rts" = return ("rts", Cabal.mkVersion [1,0])
    | otherwise = case parseCabalName (takeBaseName conf) of
        Left err -> error $ "getPackageNameFromConfFile: Couldn't parse " ++
                            takeBaseName conf ++ ": " ++ err
        Right (name, version) -> return (name, version)

parseCabalName :: String -> Either String (String, Version)
parseCabalName =
    fmap f . Cabal.explicitEitherParsec ((,) <$> Cabal.parsec <*> optional (P.string "&boot"))
  where
    f :: (Cabal.PackageId, Maybe String) -> (String, Version)
    f (pkg_id, b) = (Cabal.unPackageName $ Cabal.pkgName pkg_id, Cabal.pkgVersion pkg_id)
