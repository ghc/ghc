{-# LANGUAGE RecordWildCards #-}
module GHC.SrcLoc
  ( SrcLoc
  , srcLocPackage
  , srcLocModule
  , srcLocFile
  , srcLocStartLine
  , srcLocStartCol
  , srcLocEndLine
  , srcLocEndCol

  -- * Pretty printing
  , showSrcLoc
  ) where

-- | A single location in the source code.
data SrcLoc = SrcLoc
  { srcLocPackage   :: String
  , srcLocModule    :: String
  , srcLocFile      :: String
  , srcLocStartLine :: Int
  , srcLocStartCol  :: Int
  , srcLocEndLine   :: Int
  , srcLocEndCol    :: Int
  } deriving (Show, Eq)

showSrcLoc :: SrcLoc -> String
showSrcLoc SrcLoc {..}
  = concat [ srcLocFile, ":"
           , show srcLocStartLine, ":"
           , show srcLocStartCol, " in "
           , srcLocPackage, ":", srcLocModule
           ]
