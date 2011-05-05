
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
	{ dph_PArray		:: Module
        , dph_Repr		:: Module
        , dph_Closure		:: Module
        , dph_Unboxed		:: Module
        , dph_Instances		:: Module
        , dph_Combinators	:: Module
        , dph_Scalar		:: Module
        , dph_PArray_Scalar     :: Module
        , dph_PArray_PRepr      :: Module
        , dph_PArray_PData      :: Module
        , dph_Prelude_PArr	:: Module
        , dph_Prelude_Int	:: Module
        , dph_Prelude_Word8	:: Module
        , dph_Prelude_Double	:: Module
        , dph_Prelude_Bool	:: Module
        , dph_Prelude_Tuple	:: Module
	}


-- | The locations of builtins in the current DPH library.
dph_Modules :: PackageId -> Modules
dph_Modules pkg 
	= Modules 
	{ dph_PArray         = mk (fsLit "Data.Array.Parallel.Lifted.PArray")
	, dph_Repr           = mk (fsLit "Data.Array.Parallel.Lifted.Repr")
	, dph_Closure        = mk (fsLit "Data.Array.Parallel.Lifted.Closure")
	, dph_Unboxed        = mk (fsLit "Data.Array.Parallel.Lifted.Unboxed")
	, dph_Instances      = mk (fsLit "Data.Array.Parallel.Lifted.Instances")
	, dph_Combinators    = mk (fsLit "Data.Array.Parallel.Lifted.Combinators")
	, dph_Scalar         = mk (fsLit "Data.Array.Parallel.Lifted.Scalar")

        , dph_PArray_Scalar  = mk (fsLit "Data.Array.Parallel.PArray.Scalar")
        , dph_PArray_PRepr   = mk (fsLit "Data.Array.Parallel.PArray.PRepr")
        , dph_PArray_PData   = mk (fsLit "Data.Array.Parallel.PArray.PData")

	, dph_Prelude_PArr   = mk (fsLit "Data.Array.Parallel.Prelude.Base.PArr")
	, dph_Prelude_Int    = mk (fsLit "Data.Array.Parallel.Prelude.Base.Int")
	, dph_Prelude_Word8  = mk (fsLit "Data.Array.Parallel.Prelude.Base.Word8")
	, dph_Prelude_Double = mk (fsLit "Data.Array.Parallel.Prelude.Base.Double")
	, dph_Prelude_Bool   = mk (fsLit "Data.Array.Parallel.Prelude.Base.Bool")
	, dph_Prelude_Tuple  = mk (fsLit "Data.Array.Parallel.Prelude.Base.Tuple")
	}
	where	mk = mkModule pkg . mkModuleNameFS


-- | Project out ids of modules that contain orphan instances that we need to load.
dph_Orphans :: [Modules -> Module]
dph_Orphans = [dph_Repr, dph_Instances]
