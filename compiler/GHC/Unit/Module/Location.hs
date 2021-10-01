-- | Module location
module GHC.Unit.Module.Location
   ( ModLocation(..)
   , addBootSuffix
   , addBootSuffix_maybe
   , addBootSuffixLocn_maybe
   , addBootSuffixLocn
   , addBootSuffixLocnOut
   , removeBootSuffix
   )
where

import GHC.Prelude
import GHC.Unit.Types
import GHC.Utils.Outputable

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
        ml_hs_file   :: Maybe FilePath,
                -- ^ The source file, if we have one.  Package modules
                -- probably don't have source files.

        ml_hi_file   :: FilePath,
                -- ^ Where the .hi file is, whether or not it exists
                -- yet.  Always of form foo.hi, even if there is an
                -- hi-boot file (we add the -boot suffix later)

        ml_dyn_hi_file :: FilePath,
                -- ^ Where the .dyn_hi file is, whether or not it exists
                -- yet.

        ml_obj_file  :: FilePath,
                -- ^ Where the .o file is, whether or not it exists yet.
                -- (might not exist either because the module hasn't
                -- been compiled yet, or because it is part of a
                -- unit with a .a file)

        ml_dyn_obj_file :: FilePath,
                -- ^ Where the .dy file is, whether or not it exists
                -- yet.

        ml_hie_file  :: FilePath
                -- ^ Where the .hie file is, whether or not it exists
                -- yet.
  } deriving Show

instance Outputable ModLocation where
   ppr = text . show

-- | Add the @-boot@ suffix to .hs, .hi and .o files
addBootSuffix :: FilePath -> FilePath
addBootSuffix path = path ++ "-boot"

-- | Remove the @-boot@ suffix to .hs, .hi and .o files
removeBootSuffix :: FilePath -> FilePath
removeBootSuffix "-boot" = []
removeBootSuffix (x:xs)  = x : removeBootSuffix xs
removeBootSuffix []      = error "removeBootSuffix: no -boot suffix"


-- | Add the @-boot@ suffix if the @Bool@ argument is @True@
addBootSuffix_maybe :: IsBootInterface -> FilePath -> FilePath
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
  = locn { ml_hs_file  = fmap addBootSuffix (ml_hs_file locn)
         , ml_hi_file  = addBootSuffix (ml_hi_file locn)
         , ml_dyn_hi_file = addBootSuffix (ml_dyn_hi_file locn)
         , ml_obj_file = addBootSuffix (ml_obj_file locn)
         , ml_dyn_obj_file = addBootSuffix (ml_dyn_obj_file locn)
         , ml_hie_file = addBootSuffix (ml_hie_file locn) }

-- | Add the @-boot@ suffix to all output file paths associated with the
-- module, not including the input file itself
addBootSuffixLocnOut :: ModLocation -> ModLocation
addBootSuffixLocnOut locn
  = locn { ml_hi_file  = addBootSuffix (ml_hi_file locn)
         , ml_dyn_hi_file = addBootSuffix (ml_dyn_hi_file locn)
         , ml_obj_file = addBootSuffix (ml_obj_file locn)
         , ml_dyn_obj_file = addBootSuffix (ml_dyn_obj_file locn)
         , ml_hie_file = addBootSuffix (ml_hie_file locn)
         }


