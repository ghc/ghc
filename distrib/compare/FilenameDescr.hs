
module FilenameDescr where

import Data.Either

import Utils
import Tar

-- We can't just compare plain filenames, because versions numbers of GHC
-- and the libaries will vary. So we use FilenameDescr instead, which
-- abstracts out the version numbers.
type FilenameDescr = [FilenameDescrBit]
data FilenameDescrBit = VersionOf String
                      | FP String
    deriving (Show, Eq, Ord)

normalise :: FilenameDescr -> FilenameDescr
normalise [] = []
normalise [x] = [x]
normalise (FP x1 : FP x2 : xs) = normalise (FP (x1 ++ x2) : xs)
normalise (x : xs) = x : normalise xs

-- Mapping from thing (e.g. "Cabal") to version (e.g. "1.10.0.0")
type ThingVersionMap = [(String, String)]

addThingVersion :: ThingVersionMap -> String -> String -> Maybe ThingVersionMap
addThingVersion mapping thing version
 = case lookup thing mapping of
   Just version' ->
       if version == version'
       then Just mapping
       else Nothing
   Nothing ->
       Just ((thing, version) : mapping)

-- Sanity check that the FilenameDescr matches the filename in the tar line
checkContent :: ThingVersionMap -> (FilenameDescr, TarLine) -> Errors
checkContent mapping (fd, tl)
 = let fn = tlFileName tl
   in case flattenFilenameDescr mapping fd of
      Right fn' ->
          if fn' == fn
          then []
          else ["checkContent: Can't happen: filename mismatch: " ++ show fn]
      Left errs ->
          errs

flattenFilenameDescr :: ThingVersionMap -> FilenameDescr
                     -> Either Errors FilePath
flattenFilenameDescr mapping fd = case partitionEithers (map f fd) of
                                  ([], strs) -> Right (concat strs)
                                  (errs, _) -> Left (concat errs)
    where f (FP fp) = Right fp
          f (VersionOf thing)
           = case lookup thing mapping of
             Just v -> Right v
             Nothing -> Left ["Can't happen: thing has no version in mapping"]

