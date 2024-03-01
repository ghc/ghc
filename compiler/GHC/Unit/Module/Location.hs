-- | Module location
module GHC.Unit.Module.Location
   ( ModLocation(..)
   , addBootSuffix
   , addBootSuffix_maybe
   , addBootSuffixLocn_maybe
   , addBootSuffixLocn
   , addBootSuffixLocnOut
   , removeBootSuffix
   , ml_hs_file
   , ml_hi_file
   , ml_dyn_hi_file
   , ml_obj_file
   , ml_dyn_obj_file
   , ml_hie_file
   , unsafeEncodeUtf
   , unsafeDecodeUtf
   )
where

import Data.Either
import GHC.Prelude
import GHC.Unit.Types
import GHC.Utils.Outputable
import qualified GHC.Data.Strict as Strict
import System.OsPath

-- | Module Location
--
-- Where a module lives on the file system: the actual locations
-- of the .hs, .hi, .dyn_hi, .o, .dyn_o and .hie files, if we have them.
--
-- For a module in another unit, the ml_hs_file and ml_obj_file components of
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
   = ModLocation {
        ml_hs_file_   :: Strict.Maybe OsPath,
                -- ^ The source file, if we have one.  Package modules
                -- probably don't have source files.

        ml_hi_file_   :: !OsPath,
                -- ^ Where the .hi file is, whether or not it exists
                -- yet.  Always of form foo.hi, even if there is an
                -- hi-boot file (we add the -boot suffix later)

        ml_dyn_hi_file_ :: !OsPath,
                -- ^ Where the .dyn_hi file is, whether or not it exists
                -- yet.

        ml_obj_file_  :: !OsPath,
                -- ^ Where the .o file is, whether or not it exists yet.
                -- (might not exist either because the module hasn't
                -- been compiled yet, or because it is part of a
                -- unit with a .a file)

        ml_dyn_obj_file_ :: !OsPath,
                -- ^ Where the .dy file is, whether or not it exists
                -- yet.

        ml_hie_file_  :: !OsPath
                -- ^ Where the .hie file is, whether or not it exists
                -- yet.
  } deriving Show

instance Outputable ModLocation where
   ppr = text . show

unsafeEncodeUtf :: FilePath -> OsPath
unsafeEncodeUtf = fromRight (error "unsafeEncodeUtf: Internal error") . encodeUtf

unsafeDecodeUtf :: OsPath -> FilePath
unsafeDecodeUtf = fromRight (error "unsafeEncodeUtf: Internal error") . decodeUtf

-- | Add the @-boot@ suffix to .hs, .hi and .o files
addBootSuffix :: OsPath -> OsPath
addBootSuffix path = path `mappend` unsafeEncodeUtf "-boot"

-- | Remove the @-boot@ suffix to .hs, .hi and .o files
removeBootSuffix :: FilePath -> FilePath
removeBootSuffix "-boot" = []
removeBootSuffix (x:xs)  = x : removeBootSuffix xs
removeBootSuffix []      = error "removeBootSuffix: no -boot suffix"


-- | Add the @-boot@ suffix if the @Bool@ argument is @True@
addBootSuffix_maybe :: IsBootInterface -> OsPath -> OsPath
addBootSuffix_maybe is_boot path = case is_boot of
  IsBoot -> addBootSuffix path
  NotBoot -> path

addBootSuffixLocn_maybe :: IsBootInterface -> ModLocation -> ModLocation
addBootSuffixLocn_maybe is_boot locn = case is_boot of
  IsBoot -> addBootSuffixLocn locn
  _ -> locn

-- | Add the @-boot@ suffix to all file paths associated with the module
addBootSuffixLocn :: ModLocation -> ModLocation
addBootSuffixLocn locn
  = locn { ml_hs_file_  = fmap addBootSuffix (ml_hs_file_ locn)
         , ml_hi_file_  = addBootSuffix (ml_hi_file_ locn)
         , ml_dyn_hi_file_ = addBootSuffix (ml_dyn_hi_file_ locn)
         , ml_obj_file_ = addBootSuffix (ml_obj_file_ locn)
         , ml_dyn_obj_file_ = addBootSuffix (ml_dyn_obj_file_ locn)
         , ml_hie_file_ = addBootSuffix (ml_hie_file_ locn) }

-- | Add the @-boot@ suffix to all output file paths associated with the
-- module, not including the input file itself
addBootSuffixLocnOut :: ModLocation -> ModLocation
addBootSuffixLocnOut locn
  = locn { ml_hi_file_  = addBootSuffix (ml_hi_file_ locn)
         , ml_dyn_hi_file_ = addBootSuffix (ml_dyn_hi_file_ locn)
         , ml_obj_file_ = addBootSuffix (ml_obj_file_ locn)
         , ml_dyn_obj_file_ = addBootSuffix (ml_dyn_obj_file_ locn)
         , ml_hie_file_ = addBootSuffix (ml_hie_file_ locn)
         }

ml_hs_file :: ModLocation -> Maybe FilePath
ml_hs_file = fmap unsafeDecodeUtf . Strict.toLazy . ml_hs_file_

ml_hi_file :: ModLocation -> FilePath
ml_hi_file  = unsafeDecodeUtf . ml_hi_file_

ml_dyn_hi_file :: ModLocation -> FilePath
ml_dyn_hi_file = unsafeDecodeUtf . ml_dyn_hi_file_

ml_obj_file :: ModLocation -> FilePath
ml_obj_file = unsafeDecodeUtf . ml_obj_file_

ml_dyn_obj_file :: ModLocation -> FilePath
ml_dyn_obj_file = unsafeDecodeUtf . ml_dyn_obj_file_

ml_hie_file :: ModLocation -> FilePath
ml_hie_file = unsafeDecodeUtf . ml_hie_file_
