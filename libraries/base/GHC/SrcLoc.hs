{-# LANGUAGE RecordWildCards #-}

-- | @since 4.8.2.0
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
--
-- @since 4.8.2.0
data SrcLoc = SrcLoc
  { srcLocPackage   :: String
  , srcLocModule    :: String
  , srcLocFile      :: String
  , srcLocStartLine :: Int
  , srcLocStartCol  :: Int
  , srcLocEndLine   :: Int
  , srcLocEndCol    :: Int
  } deriving (Show, Eq)

-- | Pretty print 'SrcLoc'
--
-- @since 4.8.2.0
showSrcLoc :: SrcLoc -> String
showSrcLoc SrcLoc {..}
  = concat [ srcLocFile, ":"
           , show srcLocStartLine, ":"
           , show srcLocStartCol, " in "
           , srcLocPackage, ":", srcLocModule
           ]
