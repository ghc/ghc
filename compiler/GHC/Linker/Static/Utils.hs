{-# LANGUAGE MultiWayIf #-}

module GHC.Linker.Static.Utils where

import GHC.Prelude
import GHC.Platform
import System.FilePath

-- | Compute the output file name of a program.
--
-- StaticLink boolean is used to indicate if the program is actually a static library
-- (e.g., on iOS).
--
-- Use the provided filename (if any), otherwise use "main.exe" (Windows),
-- "a.out (otherwise without StaticLink set), "liba.a". In every case, add the
-- extension if it is missing.
exeFileName :: ArchOS -> Bool -> Maybe FilePath -> FilePath
exeFileName (ArchOS arch os) staticLink output_fn
  | Just s <- output_fn = if
      | OSMinGW32      <- os   -> s <?.> "exe"
      | ArchJavaScript <- arch -> s <?.> "jsexe"
      | ArchWasm32     <- arch -> s <?.> "wasm"
      | staticLink             -> s <?.> "a"
      | otherwise              -> s
  | otherwise = if
      | OSMinGW32      <- os   -> "main.exe"
      | ArchJavaScript <- arch -> "main.jsexe"
      | staticLink             -> "liba.a"
      | otherwise              -> "a.out"
 where s <?.> ext | null (takeExtension s) = s <.> ext
                  | otherwise              = s
