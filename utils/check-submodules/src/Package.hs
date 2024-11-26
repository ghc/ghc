{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Package
  ( Contact(..)
  , parseContact
  , contactRecipient

  , Package(..)
  , getPackageVersion
  , getPackageMaintainers
  ) where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Version
import Distribution.PackageDescription.Parsec qualified as C
import Distribution.Types.GenericPackageDescription qualified as C
import Distribution.Types.PackageDescription qualified as C
import Distribution.Types.PackageId qualified as C
import Distribution.Types.PackageName (PackageName)
import Distribution.Types.PackageName qualified as C
import Distribution.Types.Version qualified as C
import Distribution.Utils.ShortText qualified as C
import System.FilePath

data Contact = Contact { contactName, contactEmail :: T.Text }
    deriving (Eq, Ord, Show)

parseContact :: T.Text -> Contact
parseContact t
  | '<' `T.elem` t =
    let (name,email) = T.break (== '<') t
     in Contact (T.strip name) (T.strip $ T.takeWhile (/= '>') $ T.drop 1 email)
  | otherwise = Contact "" t

contactRecipient :: Contact -> T.Text
contactRecipient (Contact name email)
  | T.null name = email
  | otherwise = name <> " <" <> email <> ">"

data Package = Package { pkgName :: PackageName
                       , pkgPath :: FilePath
                       , pkgIsReleaseTag :: String -> Bool
                       }

getPackageDescription :: Package -> IO C.PackageDescription
getPackageDescription pkg = do
    Just gpd <- C.parseGenericPackageDescriptionMaybe <$> BS.readFile (pkgPath pkg </> C.unPackageName (pkgName pkg) <.> "cabal")
    return $ C.packageDescription gpd

getPackageMaintainers :: Package -> IO [Contact]
getPackageMaintainers pkg =
    map (parseContact . T.strip . T.filter (/= '\n')) . T.splitOn ","
    . T.pack . C.fromShortText . C.maintainer
    <$> getPackageDescription pkg

getPackageVersion :: Package -> IO Version
getPackageVersion pkg =
    Data.Version.makeVersion . C.versionNumbers . C.pkgVersion . C.package
    <$> getPackageDescription pkg

