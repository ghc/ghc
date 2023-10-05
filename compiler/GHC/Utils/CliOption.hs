module GHC.Utils.CliOption
  ( Option (..)
  , showOpt
  ) where

import GHC.Prelude

-- -----------------------------------------------------------------------------
-- Command-line options

-- | When invoking external tools as part of the compilation pipeline, we
-- pass these a sequence of options on the command-line. Rather than
-- just using a list of Strings, we use a type that allows us to distinguish
-- between filepaths and 'other stuff'. The reason for this is that
-- this type gives us a handle on transforming filenames, and filenames only,
-- to whatever format they're expected to be on a particular platform.
data Option
 = FileOption -- an entry that _contains_ filename(s) / filepaths.
              String  -- a non-filepath prefix that shouldn't be
                      -- transformed (e.g., "/out=")
              String  -- the filepath/filename portion
 | Option     String
 deriving ( Eq )

showOpt :: Option -> String
showOpt (FileOption pre f) = pre ++ f
showOpt (Option s)  = s
