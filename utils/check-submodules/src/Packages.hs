{-# LANGUAGE OverloadedStrings #-}

module Packages (packages) where

import Package
import Data.Char (isDigit)
import qualified Distribution.Types.PackageName as C
import Data.List

packages :: [Package]
packages =
    [ stdPackage "file-io" "libraries/file-io"
    , stdPackage "hsc2hs" "utils/hsc2hs"
    , Package "Cabal" "libraries/Cabal/Cabal"  (isPrefixTag "Cabal-")
    , Package "Cabal-syntax" "libraries/Cabal/Cabal-syntax" (isPrefixTag "Cabal-syntax-")
    , stdPackage "bytestring" "libraries/bytestring"
    , stdPackage "binary" "libraries/binary"
    , stdPackage "array" "libraries/array"
    , stdPackage "containers" "libraries/containers/containers"
    , stdPackage "deepseq" "libraries/deepseq"
    , stdPackage "directory" "libraries/directory"
    , stdPackage "filepath" "libraries/filepath"
    , stdPackage "haskeline" "libraries/haskeline"
    , stdPackage "hpc" "libraries/hpc"
    , stdPackage "mtl" "libraries/mtl"
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
stdPackage name path = Package name path stdIsReleaseTag

looksLikeVersion :: String -> Bool
looksLikeVersion =
    all (\c -> isDigit c || c == '.')

isPrefixTag :: String -> String -> Bool
isPrefixTag prefix tag
  | Just rest <- prefix `stripPrefix` tag = looksLikeVersion rest
  | otherwise = False

stdIsReleaseTag :: String -> Bool
stdIsReleaseTag tag =
    isPrefixTag "v" tag || isPrefixTag "" tag
