-----------------------------------------------------------------------------
-- |
-- Module     : Hadrian.Builder.Ar
-- Copyright  : (c) Andrey Mokhov 2014-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Support for invoking the archiving utility @ar@. We take care not to exceed
-- the limit on command line length, which differs across supported operating
-- systems (see 'cmdLineLengthLimit'). We need to handle @ar@ in a special way
-- because we sometimes archive __a lot__ of files (in the Cabal library, for
-- example, command line length can reach 2MB!). To work around the limit on the
-- command line length we pass the list of files to be archived via a temporary
-- file (see 'runAr'), or alternatively, we split the argument list into chunks
-- and call @ar@ multiple times, e.g. when passing arguments via a temporary
-- file is not supported (see 'runArWithoutTempFile').
-----------------------------------------------------------------------------
module Hadrian.Builder.Ar (ArMode (..), args, runAr, runArWithoutTempFile) where

import Control.Monad
import Development.Shake
import Development.Shake.Classes
import GHC.Generics
import Hadrian.Expression
import Hadrian.Utilities

-- | We support packing and unpacking archives with @ar@.
data ArMode = Pack | Unpack deriving (Eq, Generic, Show)

instance Binary   ArMode
instance Hashable ArMode
instance NFData   ArMode

-- NOTE: Make sure to appropriately update 'arFlagsCount' when changing 'args'.
-- | Default command line arguments for invoking the archiving utility @ar@.
args :: (ShakeValue c, ShakeValue b) => ArMode -> Args c b
args Pack   = mconcat [ arg "q", arg =<< getOutput, getInputs ]
args Unpack = mconcat [ arg "x", arg =<< getInput ]

-- This count includes "q" and the output file argumentes in 'args'. This is
-- only relevant for the 'Pack' @ar@ mode.
arFlagsCount :: Int
arFlagsCount = 2

-- | Invoke @ar@ given a path to it and a list of arguments. The list of files
-- to be archived is passed via a temporary file. Passing arguments via a
-- temporary file is not supported by some versions of @ar@, in which case you
-- should use 'runArWithoutTempFile' instead.
runAr :: FilePath -> [String] -> Action ()
runAr arPath argList = withTempFile $ \tmp -> do
    writeFile' tmp $ unwords fileArgs
    cmd [arPath] flagArgs ('@' : tmp)
  where
    flagArgs = take arFlagsCount argList
    fileArgs = drop arFlagsCount argList

-- | Invoke @ar@ given a path to it and a list of arguments. Note that @ar@
-- will be called multiple times if the list of files to be archived is too
-- long and doesn't fit into the command line length limit. This function is
-- typically much slower than 'runAr'.
runArWithoutTempFile :: FilePath -> [String] -> Action ()
runArWithoutTempFile arPath argList =
    forM_ (chunksOfSize cmdLineLengthLimit fileArgs) $ \argsChunk ->
        unit . cmd [arPath] $ flagArgs ++ argsChunk
  where
    flagArgs = take arFlagsCount argList
    fileArgs = drop arFlagsCount argList
