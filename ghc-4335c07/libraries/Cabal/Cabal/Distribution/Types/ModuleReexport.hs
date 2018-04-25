{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Distribution.Types.ModuleReexport (
    ModuleReexport(..)
) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.ModuleName
import Distribution.Parsec.Class
import Distribution.Pretty
import Distribution.Text
import Distribution.Types.PackageName

import qualified Distribution.Compat.Parsec as P
import qualified Distribution.Compat.ReadP  as Parse
import           Text.PrettyPrint           ((<+>))
import qualified Text.PrettyPrint           as Disp

-- -----------------------------------------------------------------------------
-- Module re-exports

data ModuleReexport = ModuleReexport {
       moduleReexportOriginalPackage :: Maybe PackageName,
       moduleReexportOriginalName    :: ModuleName,
       moduleReexportName            :: ModuleName
    }
    deriving (Eq, Generic, Read, Show, Typeable, Data)

instance Binary ModuleReexport

instance Pretty ModuleReexport where
    pretty (ModuleReexport mpkgname origname newname) =
          maybe Disp.empty (\pkgname -> pretty pkgname <<>> Disp.char ':') mpkgname
       <<>> pretty origname
      <+> if newname == origname
            then Disp.empty
            else Disp.text "as" <+> pretty newname

instance Parsec ModuleReexport where
    parsec = do
        mpkgname <- P.optionMaybe (P.try $ parsec <* P.char ':')
        origname <- parsec
        newname  <- P.option origname $ P.try $ do
            P.spaces
            _ <- P.string "as"
            P.spaces
            parsec
        return (ModuleReexport mpkgname origname newname)

instance Text ModuleReexport where
    parse = do
      mpkgname <- Parse.option Nothing $ do
                    pkgname <- parse
                    _       <- Parse.char ':'
                    return (Just pkgname)
      origname <- parse
      newname  <- Parse.option origname $ do
                    Parse.skipSpaces
                    _ <- Parse.string "as"
                    Parse.skipSpaces
                    parse
      return (ModuleReexport mpkgname origname newname)
