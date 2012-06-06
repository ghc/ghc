-----------------------------------------------------------------------------
--
-- Stg to C-- code generation
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module StgCmm ( codeGen ) where

#define FAST_STRING_NOT_NEEDED
#include "HsVersions.h"

import StgCmmProf
import StgCmmMonad
import StgCmmEnv
import StgCmmBind
import StgCmmCon
import StgCmmLayout
import StgCmmUtils
import StgCmmClosure
import StgCmmHpc
import StgCmmTicky

import Cmm
import CLabel

import StgSyn
import DynFlags

import HscTypes
import CostCentre
import Id
import IdInfo
import Type
import DataCon
import Name
import TyCon
import Module
import ErrUtils
import Outputable
import Util

codeGen :: DynFlags
	 -> Module
	 -> [TyCon]
         -> CollectedCCs                -- (Local/global) cost-centres needing declaring/registering.
	 -> [(StgBinding,[(Id,[Id])])]	-- Bindings to convert, with SRTs
	 -> HpcInfo
         -> IO [CmmGroup]         -- Output

codeGen dflags this_mod data_tycons
        cost_centre_info stg_binds hpc_info
  = do  { showPass dflags "New CodeGen"

-- Why?
--   ; mapM_ (\x -> seq x (return ())) data_tycons

        ; code_stuff <- initC dflags this_mod $ do 
                { cmm_binds  <- mapM (getCmm . cgTopBinding dflags) stg_binds
                ; cmm_tycons <- mapM cgTyCon data_tycons
                ; cmm_init   <- getCmm (mkModuleInit cost_centre_info
                                             this_mod hpc_info)
                ; return (cmm_init : cmm_binds ++ cmm_tycons)
                }
                -- Put datatype_stuff after code_stuff, because the
                -- datatype closure table (for enumeration types) to
                -- (say) PrelBase_True_closure, which is defined in
                -- code_stuff

                -- N.B. returning '[Cmm]' and not 'Cmm' here makes it
                -- possible for object splitting to split up the
                -- pieces later.

                -- Note [codegen-split-init] the cmm_init block must
                -- come FIRST.  This is because when -split-objs is on
                -- we need to combine this block with its
                -- initialisation routines; see Note
                -- [pipeline-split-init].

        ; return code_stuff }


---------------------------------------------------------------
--	Top-level bindings
---------------------------------------------------------------

{- 'cgTopBinding' is only used for top-level bindings, since they need
to be allocated statically (not in the heap) and need to be labelled.
No unboxed bindings can happen at top level.

In the code below, the static bindings are accumulated in the
@MkCgState@, and transferred into the ``statics'' slot by @forkStatics@.
This is so that we can write the top level processing in a compositional
style, with the increasing static environment being plumbed as a state
variable. -}

cgTopBinding :: DynFlags -> (StgBinding,[(Id,[Id])]) -> FCode ()
cgTopBinding dflags (StgNonRec id rhs, _srts)
  = do	{ id' <- maybeExternaliseId dflags id
	; info <- cgTopRhs id' rhs
	; addBindC (cg_id info) info -- Add the *un-externalised* Id to the envt,
				     -- so we find it when we look up occurrences
	}

cgTopBinding dflags (StgRec pairs, _srts)
  = do	{ let (bndrs, rhss) = unzip pairs
	; bndrs' <- mapFCs (maybeExternaliseId dflags) bndrs
	; let pairs' = zip bndrs' rhss
	; fixC_(\ new_binds -> do 
		{ addBindsC new_binds
		; mapFCs ( \ (b,e) -> cgTopRhs b e ) pairs' })
	; return () }

-- Urgh!  I tried moving the forkStatics call from the rhss of cgTopRhs
-- to enclose the listFCs in cgTopBinding, but that tickled the
-- statics "error" call in initC.  I DON'T UNDERSTAND WHY!

cgTopRhs :: Id -> StgRhs -> FCode CgIdInfo
	-- The Id is passed along for setting up a binding...
	-- It's already been externalised if necessary

cgTopRhs bndr (StgRhsCon _cc con args)
  = forkStatics (cgTopRhsCon bndr con args)

cgTopRhs bndr (StgRhsClosure cc bi fvs upd_flag srt args body)
  = ASSERT(null fvs)    -- There should be no free variables
    setSRTLabel (mkSRTLabel (idName bndr) (idCafInfo bndr)) $
    forkStatics (cgTopRhsClosure bndr cc bi upd_flag srt args body)


---------------------------------------------------------------
--	Module initialisation code
---------------------------------------------------------------

{- The module initialisation code looks like this, roughly:

	FN(__stginit_Foo) {
 	  JMP_(__stginit_Foo_1_p)
	}

	FN(__stginit_Foo_1_p) {
	...
	}

   We have one version of the init code with a module version and the
   'way' attached to it.  The version number helps to catch cases
   where modules are not compiled in dependency order before being
   linked: if a module has been compiled since any modules which depend on
   it, then the latter modules will refer to a different version in their
   init blocks and a link error will ensue.

   The 'way' suffix helps to catch cases where modules compiled in different
   ways are linked together (eg. profiled and non-profiled).

   We provide a plain, unadorned, version of the module init code
   which just jumps to the version with the label and way attached.  The
   reason for this is that when using foreign exports, the caller of
   startupHaskell() must supply the name of the init function for the "top"
   module in the program, and we don't want to require that this name
   has the version and way info appended to it.

We initialise the module tree by keeping a work-stack, 
	* pointed to by Sp
	* that grows downward
	* Sp points to the last occupied slot
-}

mkModuleInit 
        :: CollectedCCs         -- cost centre info
	-> Module
        -> HpcInfo
	-> FCode ()

mkModuleInit cost_centre_info this_mod hpc_info
  = do  { initHpc this_mod hpc_info
        ; initCostCentres cost_centre_info
            -- For backwards compatibility: user code may refer to this
            -- label for calling hs_add_root().
        ; emitDecl (CmmData Data (Statics (mkPlainModuleInitLabel this_mod) []))
        }

---------------------------------------------------------------
--	Generating static stuff for algebraic data types
---------------------------------------------------------------

{-	[These comments are rather out of date]

Macro  		    	     Kind of constructor
CONST_INFO_TABLE@	Zero arity (no info -- compiler uses static closure)
CHARLIKE_INFO_TABLE	Charlike   (no info -- compiler indexes fixed array)
INTLIKE_INFO_TABLE	Intlike; the one macro generates both info tbls
SPEC_INFO_TABLE		SPECish, and bigger than or equal to MIN_UPD_SIZE
GEN_INFO_TABLE		GENish (hence bigger than or equal to MIN_UPD_SIZE@)

Possible info tables for constructor con:

* _con_info:
  Used for dynamically let(rec)-bound occurrences of
  the constructor, and for updates.  For constructors
  which are int-like, char-like or nullary, when GC occurs,
  the closure tries to get rid of itself.

* _static_info:
  Static occurrences of the constructor macro: STATIC_INFO_TABLE.

For zero-arity constructors, \tr{con}, we NO LONGER generate a static closure;
it's place is taken by the top level defn of the constructor.

For charlike and intlike closures there is a fixed array of static
closures predeclared.
-}

cgTyCon :: TyCon -> FCode CmmGroup  -- All constructors merged together
cgTyCon tycon
  = do	{ constrs <- mapM (getCmm . cgDataCon) (tyConDataCons tycon)

	    -- Generate a table of static closures for an enumeration type
	    -- Put the table after the data constructor decls, because the
	    -- datatype closure table (for enumeration types)
	    -- to (say) PrelBase_$wTrue_closure, which is defined in code_stuff
            -- Note that the closure pointers are tagged.

            -- N.B. comment says to put table after constructor decls, but
            -- code puts it before --- NR 16 Aug 2007
	; extra <- cgEnumerationTyCon tycon

        ; return (concat (extra ++ constrs))
        }

cgEnumerationTyCon :: TyCon -> FCode [CmmGroup]
cgEnumerationTyCon tycon
  | isEnumerationTyCon tycon
  = do	{ tbl <- getCmm $ 
		 emitRODataLits (mkLocalClosureTableLabel (tyConName tycon) NoCafRefs)
	      	   [ CmmLabelOff (mkLocalClosureLabel (dataConName con) NoCafRefs) 
				 (tagForCon con)
    	      	   | con <- tyConDataCons tycon]
	; return [tbl] }
  | otherwise
  = return []

cgDataCon :: DataCon -> FCode ()
-- Generate the entry code, info tables, and (for niladic constructor)
-- the static closure, for a constructor.
cgDataCon data_con
  = do	{ let
            (tot_wds, --  #ptr_wds + #nonptr_wds
    	     ptr_wds, --  #ptr_wds
    	     arg_things) = mkVirtConstrOffsets arg_reps

            nonptr_wds   = tot_wds - ptr_wds

            sta_info_tbl = mkDataConInfoTable data_con True  ptr_wds nonptr_wds
            dyn_info_tbl = mkDataConInfoTable data_con False ptr_wds nonptr_wds

            emit_info info_tbl ticky_code
                = emitClosureAndInfoTable info_tbl NativeDirectCall []
                             $ mk_code ticky_code

	    mk_code ticky_code
	      = 	-- NB: We don't set CC when entering data (WDP 94/06)
 	        do { _ <- ticky_code
		   ; ldvEnter (CmmReg nodeReg)
		   ; tickyReturnOldCon (length arg_things)
		   ; emitReturn [cmmOffsetB (CmmReg nodeReg)
					    (tagForCon data_con)] }
                        -- The case continuation code expects a tagged pointer

	    arg_reps :: [(PrimRep, UnaryType)]
	    arg_reps = [(typePrimRep rep_ty, rep_ty) | ty <- dataConRepArgTys data_con, rep_ty <- flattenRepType (repType ty)]

	    -- Dynamic closure code for non-nullary constructors only
	; whenC (not (isNullaryRepDataCon data_con))
                (emit_info dyn_info_tbl tickyEnterDynCon)

		-- Dynamic-Closure first, to reduce forward references
        ; emit_info sta_info_tbl tickyEnterStaticCon }


---------------------------------------------------------------
--	Stuff to support splitting
---------------------------------------------------------------

-- If we're splitting the object, we need to externalise all the
-- top-level names (and then make sure we only use the externalised
-- one in any C label we use which refers to this name).

maybeExternaliseId :: DynFlags -> Id -> FCode Id
maybeExternaliseId dflags id
  | dopt Opt_SplitObjs dflags, 	-- Externalise the name for -split-objs
    isInternalName name = do { mod <- getModuleName
			     ; returnFC (setIdName id (externalise mod)) }
  | otherwise		= returnFC id
  where
    externalise mod = mkExternalName uniq mod new_occ loc
    name    = idName id
    uniq    = nameUnique name
    new_occ = mkLocalOcc uniq (nameOccName name)
    loc     = nameSrcSpan name
	-- We want to conjure up a name that can't clash with any
	-- existing name.  So we generate
	--	Mod_$L243foo
	-- where 243 is the unique.
