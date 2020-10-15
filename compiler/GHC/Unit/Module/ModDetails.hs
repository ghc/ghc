module GHC.Unit.Module.ModDetails
   ( ModDetails (..)
   , emptyModDetails
   )
where

import GHC.Core         ( CoreRule )
import GHC.Core.FamInstEnv
import GHC.Core.InstEnv ( ClsInst )

import GHC.Types.Avail
import GHC.Types.CompleteMatch
import GHC.Types.TypeEnv
import GHC.Types.Annotations ( Annotation )

-- | The 'ModDetails' is essentially a cache for information in the 'ModIface'
-- for home modules only. Information relating to packages will be loaded into
-- global environments in 'ExternalPackageState'.
data ModDetails = ModDetails
   { -- The next two fields are created by the typechecker
     md_exports   :: [AvailInfo]
   , md_types     :: !TypeEnv
      -- ^ Local type environment for this particular module
      -- Includes Ids, TyCons, PatSyns

   , md_insts     :: ![ClsInst]
      -- ^ 'DFunId's for the instances in this module

   , md_fam_insts :: ![FamInst]
   , md_rules     :: ![CoreRule]
      -- ^ Domain may include 'Id's from other modules

   , md_anns      :: ![Annotation]
      -- ^ Annotations present in this module: currently
      -- they only annotate things also declared in this module

   , md_complete_matches :: [CompleteMatch]
      -- ^ Complete match pragmas for this module
   }

-- | Constructs an empty ModDetails
emptyModDetails :: ModDetails
emptyModDetails = ModDetails
   { md_types            = emptyTypeEnv
   , md_exports          = []
   , md_insts            = []
   , md_rules            = []
   , md_fam_insts        = []
   , md_anns             = []
   , md_complete_matches = []
   }
