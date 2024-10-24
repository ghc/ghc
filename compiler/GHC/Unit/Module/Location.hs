{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
-- | Module location
module GHC.Unit.Module.Location
   ( ModLocation
    ( ..
    , ml_hs_file
    , ml_hi_file
    , ml_dyn_hi_file
    , ml_obj_file
    , ml_dyn_obj_file
    , ml_hie_file
    )
   , pattern ModLocation
   , addBootSuffix
   , addBootSuffixLocn
   , addBootSuffixLocnOut
   , removeBootSuffix
   , mkFileSrcSpan
   )
where

import GHC.Prelude

import GHC.Data.OsPath
import GHC.Types.SrcLoc
import GHC.Utils.Outputable
import GHC.Data.FastString (mkFastString)

import qualified System.OsString as OsString

-- | Module Location
--
-- Where a module lives on the file system: the actual locations
-- of the .hs, .hi, .dyn_hi, .o, .dyn_o and .hie files, if we have them.
--
-- For a module in another unit, the ml_hs_file_ospath and ml_obj_file_ospath components of
-- ModLocation are undefined.
--
-- The locations specified by a ModLocation may or may not
-- correspond to actual files yet: for example, even if the object
-- file doesn't exist, the ModLocation still contains the path to
-- where the object file will reside if/when it is created.
--
-- The paths of anything which can affect recompilation should be placed inside
-- ModLocation.
--
-- When a ModLocation is created none of the filepaths will have -boot suffixes.
-- This is because in --make mode the ModLocation is put in the finder cache which
-- is indexed by ModuleName, when a ModLocation is retrieved from the FinderCache
-- the boot suffixes are appended.
-- The other case is in -c mode, there the ModLocation immediately gets given the
-- boot suffixes in mkOneShotModLocation.

data ModLocation
   = OsPathModLocation {
        ml_hs_file_ospath   :: Maybe OsPath,
                -- ^ The source file, if we have one.  Package modules
                -- probably don't have source files.

        ml_hi_file_ospath   :: OsPath,
                -- ^ Where the .hi file is, whether or not it exists
                -- yet.  Always of form foo.hi, even if there is an
                -- hi-boot file (we add the -boot suffix later)

        ml_dyn_hi_file_ospath :: OsPath,
                -- ^ Where the .dyn_hi file is, whether or not it exists
                -- yet.

        ml_obj_file_ospath  :: OsPath,
                -- ^ Where the .o file is, whether or not it exists yet.
                -- (might not exist either because the module hasn't
                -- been compiled yet, or because it is part of a
                -- unit with a .a file)

        ml_dyn_obj_file_ospath :: OsPath,
                -- ^ Where the .dy file is, whether or not it exists
                -- yet.

        ml_hie_file_ospath  :: OsPath
                -- ^ Where the .hie file is, whether or not it exists
                -- yet.
  } deriving Show

instance Outputable ModLocation where
   ppr = text . show

-- | Add the @-boot@ suffix to .hs, .hi and .o files
addBootSuffix :: OsPath -> OsPath
addBootSuffix path = path `mappend` os "-boot"

-- | Remove the @-boot@ suffix to .hs, .hi and .o files
removeBootSuffix :: OsPath -> OsPath
removeBootSuffix pathWithBootSuffix =
  case OsString.stripSuffix (os "-boot") pathWithBootSuffix of
    Just path -> path
    Nothing -> error "removeBootSuffix: no -boot suffix"

-- | Add the @-boot@ suffix to all file paths associated with the module
addBootSuffixLocn :: ModLocation -> ModLocation
addBootSuffixLocn locn
  = addBootSuffixLocnOut locn { ml_hs_file_ospath = fmap addBootSuffix (ml_hs_file_ospath locn) }

-- | Add the @-boot@ suffix to all output file paths associated with the
-- module, not including the input file itself
addBootSuffixLocnOut :: ModLocation -> ModLocation
addBootSuffixLocnOut locn
  = locn { ml_hi_file_ospath = addBootSuffix (ml_hi_file_ospath locn)
         , ml_dyn_hi_file_ospath = addBootSuffix (ml_dyn_hi_file_ospath locn)
         , ml_obj_file_ospath = addBootSuffix (ml_obj_file_ospath locn)
         , ml_dyn_obj_file_ospath = addBootSuffix (ml_dyn_obj_file_ospath locn)
         , ml_hie_file_ospath = addBootSuffix (ml_hie_file_ospath locn)
         }

-- | Compute a 'SrcSpan' from a 'ModLocation'.
mkFileSrcSpan :: ModLocation -> SrcSpan
mkFileSrcSpan mod_loc
  = case ml_hs_file mod_loc of
      Just file_path -> mkGeneralSrcSpan (mkFastString file_path)
      Nothing        -> interactiveSrcSpan   -- Presumably

-- ----------------------------------------------------------------------------
-- Helpers for backwards compatibility
-- ----------------------------------------------------------------------------

{-# COMPLETE ModLocation #-}

pattern ModLocation :: Maybe FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> ModLocation
pattern ModLocation
  { ml_hs_file
  , ml_hi_file
  , ml_dyn_hi_file
  , ml_obj_file
  , ml_dyn_obj_file
  , ml_hie_file
  } <- OsPathModLocation
    { ml_hs_file_ospath = (fmap unsafeDecodeUtf -> ml_hs_file)
    , ml_hi_file_ospath = (unsafeDecodeUtf -> ml_hi_file)
    , ml_dyn_hi_file_ospath = (unsafeDecodeUtf -> ml_dyn_hi_file)
    , ml_obj_file_ospath = (unsafeDecodeUtf -> ml_obj_file)
    , ml_dyn_obj_file_ospath = (unsafeDecodeUtf -> ml_dyn_obj_file)
    , ml_hie_file_ospath = (unsafeDecodeUtf -> ml_hie_file)
    } where
      ModLocation ml_hs_file ml_hi_file ml_dyn_hi_file ml_obj_file ml_dyn_obj_file ml_hie_file
        = OsPathModLocation
          { ml_hs_file_ospath = fmap unsafeEncodeUtf ml_hs_file
          , ml_hi_file_ospath = unsafeEncodeUtf ml_hi_file
          , ml_dyn_hi_file_ospath = unsafeEncodeUtf ml_dyn_hi_file
          , ml_obj_file_ospath = unsafeEncodeUtf ml_obj_file
          , ml_dyn_obj_file_ospath = unsafeEncodeUtf ml_dyn_obj_file
          , ml_hie_file_ospath = unsafeEncodeUtf ml_hie_file
          }
