{-# LANGUAGE OverloadedStrings #-}

module Packages (packages) where

import Package
import Data.Char (isDigit)
import qualified Distribution.Types.PackageName as C
import Data.List

packages :: [Package]
packages =
    [ Package "Cabal" "libraries/Cabal/Cabal"  (isStdPrefixTag "Cabal-")
    , Package "Cabal-syntax" "libraries/Cabal/Cabal-syntax" (isStdPrefixTag "Cabal-syntax-")
    , stdPackage "array" "libraries/array"
    , stdPackage "binary" "libraries/binary"
    , stdPackage "bytestring" "libraries/bytestring"
    , stdPackage "containers" "libraries/containers/containers"
    , stdPackage "deepseq" "libraries/deepseq"
    , stdPackage "directory" "libraries/directory"
    , stdPackage "file-io" "libraries/file-io"
    , stdPackage "filepath" "libraries/filepath"
    , stdPackage "haskeline" "libraries/haskeline"
    , stdPackage "hpc" "libraries/hpc"
    , stdPackage "hsc2hs" "utils/hsc2hs"
    , stdPackage "mtl" "libraries/mtl"
    , stdPackage "os-string" "libraries/os-string"
    , stdPackage "parsec" "libraries/parsec"
    , stdPackage "pretty" "libraries/pretty"
    , stdPackage "process" "libraries/process"
    , stdPackage "terminfo" "libraries/terminfo"
    , stdPackage "text" "libraries/text"
    , stdPackage "time" "libraries/time"
    , stdPackage "unix" "libraries/unix"
    , stdPackage "exceptions" "libraries/exceptions"
    , stdPackage "semaphore-compat" "libraries/semaphore-compat"
    , stdPackage "stm" "libraries/stm"
    , stdPackage "Win32" "libraries/Win32"
    , stdPackage "xhtml" "libraries/xhtml"
    ]

stdPackage :: C.PackageName -> FilePath -> Package
stdPackage name path = Package name path (isStdPrefixTag "")

looksLikeVersion :: String -> Bool
looksLikeVersion =
    all (\c -> isDigit c || c == '.')

isPrefixTag :: String -> String -> Bool
isPrefixTag prefix tag
  | Just rest <- prefix `stripPrefix` tag = looksLikeVersion rest
  | otherwise = False

isStdPrefixTag :: String -> String -> Bool
isStdPrefixTag prefix tag =
    isPrefixTag (prefix <> "v") tag || isPrefixTag prefix tag

