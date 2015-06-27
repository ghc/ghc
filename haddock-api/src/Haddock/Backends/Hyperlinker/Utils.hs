module Haddock.Backends.Hyperlinker.Utils
    ( hypSrcDir, hypSrcModuleFile, hypSrcModuleFile'
    , hypSrcModuleUrl, hypSrcModuleUrl', hypSrcNameUrl, hypSrcModuleNameUrl
    , hypSrcModuleUrlFormat, hypSrcModuleNameUrlFormat,
    ) where

import Haddock.Backends.Xhtml.Utils

import GHC
import System.FilePath.Posix ((</>))


hypSrcDir :: FilePath
hypSrcDir = "src"

hypSrcModuleFile :: Module -> FilePath
hypSrcModuleFile = hypSrcModuleFile' . moduleName

hypSrcModuleFile' :: ModuleName -> FilePath
hypSrcModuleFile' mdl = spliceURL'
    Nothing (Just mdl) Nothing Nothing moduleFormat

hypSrcModuleUrl :: Module -> String
hypSrcModuleUrl = hypSrcModuleFile

hypSrcModuleUrl' :: ModuleName -> String
hypSrcModuleUrl' = hypSrcModuleFile'

hypSrcNameUrl :: Name -> String
hypSrcNameUrl name = spliceURL
    Nothing Nothing (Just name) Nothing nameFormat

hypSrcModuleNameUrl :: Module -> Name -> String
hypSrcModuleNameUrl mdl name = hypSrcModuleUrl mdl ++ "#" ++ hypSrcNameUrl name

hypSrcModuleUrlFormat :: String
hypSrcModuleUrlFormat = hypSrcDir </> moduleFormat

hypSrcModuleNameUrlFormat :: String
hypSrcModuleNameUrlFormat = hypSrcModuleUrlFormat ++ "#" ++ nameFormat

moduleFormat :: String
moduleFormat = "%{MODULE}.html"

nameFormat :: String
nameFormat = "%{NAME}"
