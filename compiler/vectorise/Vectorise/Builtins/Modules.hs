
-- | Modules that contain builtin functions used by the vectoriser.
module Vectorise.Builtins.Modules
	( Modules(..)
	, dph_Modules
	, dph_Orphans)
where
import Module
import FastString
	
-- | Ids of the modules that contain our DPH builtins.
data Modules 
  = Modules 
  { dph_PArray_Base             :: Module
  , dph_PArray_Scalar           :: Module
  , dph_PArray_ScalarInstances  :: Module
  , dph_PArray_PRepr            :: Module
  , dph_PArray_PReprInstances   :: Module
  , dph_PArray_PData            :: Module
  , dph_PArray_PDataInstances   :: Module
  , dph_PArray_Types            :: Module
	
  , dph_Closure	                :: Module
  , dph_Unboxed	                :: Module
  , dph_Scalar	                :: Module

  , dph_Prelude_Tuple           :: Module
  }


-- | The locations of builtins in the current DPH library.
dph_Modules :: PackageId -> Modules
dph_Modules pkg 
  = Modules 
  { dph_PArray_Base             = mk (fsLit "Data.Array.Parallel.PArray.Base")
  , dph_PArray_Scalar           = mk (fsLit "Data.Array.Parallel.PArray.Scalar")
  , dph_PArray_ScalarInstances  = mk (fsLit "Data.Array.Parallel.PArray.ScalarInstances")
  , dph_PArray_PRepr            = mk (fsLit "Data.Array.Parallel.PArray.PRepr")
  , dph_PArray_PReprInstances   = mk (fsLit "Data.Array.Parallel.PArray.PReprInstances")
  , dph_PArray_PData            = mk (fsLit "Data.Array.Parallel.PArray.PData")
  , dph_PArray_PDataInstances   = mk (fsLit "Data.Array.Parallel.PArray.PDataInstances")
  , dph_PArray_Types            = mk (fsLit "Data.Array.Parallel.PArray.Types")
	
  , dph_Closure                 = mk (fsLit "Data.Array.Parallel.Lifted.Closure")
  , dph_Unboxed                 = mk (fsLit "Data.Array.Parallel.Lifted.Unboxed")
  , dph_Scalar                  = mk (fsLit "Data.Array.Parallel.Lifted.Scalar")

  , dph_Prelude_Tuple           = mk (fsLit "Data.Array.Parallel.Prelude.Base.Tuple")
  }
  where	mk = mkModule pkg . mkModuleNameFS


dph_Orphans :: [Modules -> Module]
dph_Orphans
 = [ dph_PArray_Scalar
   , dph_PArray_ScalarInstances
   , dph_PArray_PReprInstances
   , dph_PArray_PDataInstances
   , dph_Scalar
   ]
