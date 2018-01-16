module Haddock.Backends.Hyperlinker.Utils
    ( hypSrcDir, hypSrcModuleFile, hypSrcModuleFile'
    , hypSrcModuleUrl, hypSrcModuleUrl'
    , hypSrcNameUrl
    , hypSrcLineUrl
    , hypSrcModuleNameUrl, hypSrcModuleLineUrl
    , hypSrcModuleUrlFormat
    , hypSrcModuleNameUrlFormat, hypSrcModuleLineUrlFormat
    ) where


import Haddock.Backends.Xhtml.Utils

import GHC
import FastString
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

hypSrcLineUrl :: Int -> String
hypSrcLineUrl line = spliceURL
    Nothing Nothing Nothing (Just spn) lineFormat
  where
    loc = mkSrcLoc nilFS line 1
    spn = mkSrcSpan loc loc

hypSrcModuleNameUrl :: Module -> Name -> String
hypSrcModuleNameUrl mdl name = hypSrcModuleUrl mdl ++ "#" ++ hypSrcNameUrl name

hypSrcModuleLineUrl :: Module -> Int -> String
hypSrcModuleLineUrl mdl line = hypSrcModuleUrl mdl ++ "#" ++ hypSrcLineUrl line

hypSrcModuleUrlFormat :: String
hypSrcModuleUrlFormat = hypSrcDir </> moduleFormat

hypSrcModuleNameUrlFormat :: String
hypSrcModuleNameUrlFormat = hypSrcModuleUrlFormat ++ "#" ++ nameFormat

hypSrcModuleLineUrlFormat :: String
hypSrcModuleLineUrlFormat = hypSrcModuleUrlFormat ++ "#" ++ lineFormat

moduleFormat :: String
moduleFormat = "%{MODULE}.html"

nameFormat :: String
nameFormat = "%{NAME}"

lineFormat :: String
lineFormat = "line-%{LINE}"
