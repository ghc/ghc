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
exeFileName :: Platform -> Bool -> Maybe FilePath -> FilePath
exeFileName platform staticLink output_fn
  | Just s <- output_fn =
      case platformOS platform of
          OSMinGW32 -> s <?.> "exe"
          _         -> if staticLink
                         then s <?.> "a"
                         else s
  | otherwise =
      if platformOS platform == OSMinGW32
      then "main.exe"
      else if staticLink
           then "liba.a"
           else "a.out"
 where s <?.> ext | null (takeExtension s) = s <.> ext
                  | otherwise              = s

