-----------------------------------------------------------------------------
--
-- Stg to C-- code generation
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

module StgCmm ( codeGen ) where

#define FAST_STRING_NOT_NEEDED
#include "HsVersions.h"

import StgCmmProf
import StgCmmMonad
import StgCmmEnv
import StgCmmBind
import StgCmmCon
import StgCmmLayout
import StgCmmHeap
import StgCmmUtils
import StgCmmClosure
import StgCmmHpc
import StgCmmTicky

import MkGraph
import CmmDecl
import CmmExpr
import CmmUtils
import CLabel
import PprCmm

import StgSyn
import PrelNames
import DynFlags
import StaticFlags

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

codeGen :: DynFlags
	 -> Module
	 -> [TyCon]
	 -> [Module]			-- Directly-imported modules
	 -> CollectedCCs		-- (Local/global) cost-centres needing declaring/registering.
	 -> [(StgBinding,[(Id,[Id])])]	-- Bindings to convert, with SRTs
	 -> HpcInfo
	 -> IO [Cmm]		-- Output

codeGen dflags this_mod data_tycons imported_mods 
        cost_centre_info stg_binds hpc_info
  = do  { showPass dflags "New CodeGen"
        ; let way = buildTag dflags
              main_mod = mainModIs dflags

-- Why?
--   ; mapM_ (\x -> seq x (return ())) data_tycons

        ; code_stuff <- initC dflags this_mod $ do 
                { cmm_binds  <- mapM (getCmm . cgTopBinding dflags) stg_binds
                ; cmm_tycons <- mapM cgTyCon data_tycons
                ; cmm_init   <- getCmm (mkModuleInit way cost_centre_info 
                                             this_mod main_mod
                                             imported_mods hpc_info)
                ; return (cmm_binds ++ concat cmm_tycons ++ [cmm_init])
                }
                -- Put datatype_stuff after code_stuff, because the
                -- datatype closure table (for enumeration types) to
                -- (say) PrelBase_True_closure, which is defined in
                -- code_stuff

                -- N.B. returning '[Cmm]' and not 'Cmm' here makes it
                -- possible for object splitting to split up the
                -- pieces later.

        ; dumpIfSet_dyn dflags Opt_D_dump_cmmz "New Cmm" (pprCmms code_stuff)

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
	:: String		-- the "way"
	-> CollectedCCs         -- cost centre info
	-> Module
	-> Module		-- name of the Main module
	-> [Module]
	-> HpcInfo
	-> FCode ()
mkModuleInit way cost_centre_info this_mod main_mod imported_mods hpc_info
  = do	{ -- Allocate the static boolean that records if this
          -- module has been registered already
	  emitData Data [CmmDataLabel moduleRegdLabel, 
		         CmmStaticLit zeroCLit]

        ; init_hpc  <- initHpc this_mod hpc_info
	; init_prof <- initCostCentres cost_centre_info

          -- We emit a recursive descent module search for all modules
	  -- and *choose* to chase it in :Main, below.
          -- In this way, Hpc enabled modules can interact seamlessly with
	  -- not Hpc enabled moduled, provided Main is compiled with Hpc.

        ; updfr_sz <- getUpdFrameOff
        ; tail <- getCode (pushUpdateFrame imports
                       (do updfr_sz' <- getUpdFrameOff
                           emit $ mkReturn (ret_e updfr_sz') [] (pop_ret_loc updfr_sz')))
        ; emitSimpleProc real_init_lbl $ (withFreshLabel "ret_block" $ \retId -> catAGraphs
		[ check_already_done retId updfr_sz
		, init_prof
		, init_hpc
                , tail])
 	    -- Make the "plain" procedure jump to the "real" init procedure
	; emitSimpleProc plain_init_lbl (jump_to_init updfr_sz)

	-- When compiling the module in which the 'main' function lives,
	-- (that is, this_mod == main_mod)
	-- we inject an extra stg_init procedure for stg_init_ZCMain, for the 
	-- RTS to invoke.  We must consult the -main-is flag in case the
	-- user specified a different function to Main.main
 
        -- Notice that the recursive descent is optional, depending on what options
	-- are enabled.


	; whenC (this_mod == main_mod)
		(emitSimpleProc plain_main_init_lbl (rec_descent_init updfr_sz))
    }
  where
    plain_init_lbl = mkPlainModuleInitLabel this_mod
    real_init_lbl  = mkModuleInitLabel this_mod way
    plain_main_init_lbl = mkPlainModuleInitLabel rOOT_MAIN

    jump_to_init updfr_sz = mkJump (mkLblExpr real_init_lbl) [] updfr_sz


    -- Main refers to GHC.TopHandler.runIO, so make sure we call the
    -- init function for GHC.TopHandler.
    extra_imported_mods
	| this_mod == main_mod = [gHC_TOP_HANDLER]
	| otherwise	       = []
    all_imported_mods = imported_mods ++ extra_imported_mods
    imports = map (\mod -> mkLblExpr (mkModuleInitLabel mod way))
                  (filter (gHC_PRIM /=) all_imported_mods)

    mod_reg_val = CmmLoad (mkLblExpr moduleRegdLabel) bWord
    check_already_done retId updfr_sz
     = mkCmmIfThenElse (cmmNeWord (CmmLit zeroCLit) mod_reg_val)
		       (mkLabel retId <*> mkReturn (ret_e updfr_sz) [] (pop_ret_loc updfr_sz)) mkNop
	<*>  	-- Set mod_reg to 1 to record that we've been here
	    mkStore (mkLblExpr moduleRegdLabel) (CmmLit (mkIntCLit 1))

                    -- The return-code pops the work stack by 
                    -- incrementing Sp, and then jumps to the popped item
    ret_e updfr_sz = CmmLoad (CmmStackSlot (CallArea Old) updfr_sz) gcWord
    ret_code updfr_sz = mkJump (ret_e updfr_sz) [] (pop_ret_loc updfr_sz)
      -- mkAssign spReg (cmmRegOffW spReg 1) <*>
      -- mkJump (CmmLoad (cmmRegOffW spReg (-1)) bWord) [] updfr_sz

    pop_ret_loc updfr_sz = updfr_sz - widthInBytes (typeWidth bWord)

    rec_descent_init updfr_sz =
      if opt_SccProfilingOn || isHpcUsed hpc_info
      then jump_to_init updfr_sz
      else ret_code updfr_sz

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

cgTyCon :: TyCon -> FCode [Cmm]  -- All constructors merged together
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

        ; return (extra ++ constrs)
        }

cgEnumerationTyCon :: TyCon -> FCode [Cmm]
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
	    -- To allow the debuggers, interpreters, etc to cope with
	    -- static data structures (ie those built at compile
	    -- time), we take care that info-table contains the
	    -- information we need.
	    (static_cl_info, _) = layOutStaticConstr data_con arg_reps
	    (dyn_cl_info, arg_things) = layOutDynConstr data_con arg_reps

	    emit_info cl_info ticky_code
		= emitClosureAndInfoTable cl_info NativeDirectCall []
                                        $ mk_code ticky_code

	    mk_code ticky_code
	      = 	-- NB: We don't set CC when entering data (WDP 94/06)
 	        do { _ <- ticky_code
		   ; ldvEnter (CmmReg nodeReg)
		   ; tickyReturnOldCon (length arg_things)
		   ; emitReturn [cmmOffsetB (CmmReg nodeReg)
					    (tagForCon data_con)] }
                        -- The case continuation code expects a tagged pointer

	    arg_reps :: [(PrimRep, Type)]
	    arg_reps = [(typePrimRep ty, ty) | ty <- dataConRepArgTys data_con]

	    -- Dynamic closure code for non-nullary constructors only
	; whenC (not (isNullaryRepDataCon data_con))
	 	(emit_info dyn_cl_info tickyEnterDynCon)

		-- Dynamic-Closure first, to reduce forward references
	; emit_info static_cl_info tickyEnterStaticCon }


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
