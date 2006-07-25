%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[CodeGen]{@CodeGen@: main module of the code generator}

This module says how things get going at the top level.

@codeGen@ is the interface to the outside world.  The \tr{cgTop*}
functions drive the mangling of top-level bindings.

%************************************************************************
%*									*
\subsection[codeGen-outside-interface]{The code generator's offering to the world}
%*									*
%************************************************************************

\begin{code}
module CodeGen ( codeGen ) where

#include "HsVersions.h"

-- Kludge (??) so that CgExpr is reached via at least one non-SOURCE
-- import.  Before, that wasn't the case, and CM therefore didn't 
-- bother to compile it.
import CgExpr           ( {-NOTHING!-} )	-- DO NOT DELETE THIS IMPORT
import CgProf
import CgMonad
import CgBindery	( CgIdInfo, addBindC, addBindsC, getCgIdInfo,
			  cgIdInfoId )
import CgClosure	( cgTopRhsClosure )
import CgCon		( cgTopRhsCon, cgTyCon )
import CgUtils		( cmmRegOffW, emitRODataLits, cmmNeWord )

import CLabel
import Cmm
import CmmUtils		( zeroCLit, mkIntCLit, mkLblExpr )
import PprCmm		( pprCmms )
import MachOp		( wordRep )

import StgSyn
import PrelNames	( gHC_PRIM, rOOT_MAIN, gHC_TOP_HANDLER )
import DynFlags		( DynFlags(..), DynFlag(..), dopt )
import StaticFlags	( opt_SccProfilingOn )

import PackageConfig	( PackageId )
import HscTypes		( ForeignStubs(..) )
import CostCentre       ( CollectedCCs )
import Id               ( Id, idName, setIdName )
import Name		( nameSrcLoc, nameOccName, nameUnique, isInternalName, mkExternalName )
import OccName		( mkLocalOcc )
import TyCon            ( TyCon )
import Module		( Module )
import ErrUtils		( dumpIfSet_dyn, showPass )

#ifdef DEBUG
import Panic		( assertPanic )
#endif
\end{code}

\begin{code}
codeGen :: DynFlags
	-> Module
	-> [TyCon]
	-> ForeignStubs
	-> [Module]		-- directly-imported modules
	-> CollectedCCs		-- (Local/global) cost-centres needing declaring/registering.
	-> [(StgBinding,[(Id,[Id])])]	-- Bindings to convert, with SRTs
	-> IO [Cmm]		-- Output

codeGen dflags this_mod data_tycons foreign_stubs imported_mods 
	cost_centre_info stg_binds
  = do	
  { showPass dflags "CodeGen"
  ; let way = buildTag dflags
        main_mod = mainModIs dflags

-- Why?
--   ; mapM_ (\x -> seq x (return ())) data_tycons

  ; code_stuff <- initC dflags this_mod $ do 
		{ cmm_binds  <- mapM (getCmm . cgTopBinding dflags) stg_binds
		; cmm_tycons <- mapM cgTyCon data_tycons
		; cmm_init   <- getCmm (mkModuleInit dflags way cost_centre_info 
					     this_mod main_mod
				  	     foreign_stubs imported_mods)
		; return (cmm_binds ++ concat cmm_tycons ++ [cmm_init])
		}
		-- Put datatype_stuff after code_stuff, because the
		-- datatype closure table (for enumeration types) to
		-- (say) PrelBase_True_closure, which is defined in
		-- code_stuff

  ; dumpIfSet_dyn dflags Opt_D_dump_cmm "Cmm" (pprCmms code_stuff)

  ; return code_stuff }
\end{code}

%************************************************************************
%*									*
\subsection[codegen-init]{Module initialisation code}
%*									*
%************************************************************************

/* -----------------------------------------------------------------------------
   Module initialisation

   The module initialisation code looks like this, roughly:

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
   --------------------------------------------------------------------------  */

We initialise the module tree by keeping a work-stack, 
	* pointed to by Sp
	* that grows downward
	* Sp points to the last occupied slot


\begin{code}
mkModuleInit 
	:: DynFlags
	-> String		-- the "way"
	-> CollectedCCs         -- cost centre info
	-> Module
	-> Module		-- name of the Main module
	-> ForeignStubs
	-> [Module]
	-> Code
mkModuleInit dflags way cost_centre_info this_mod main_mod foreign_stubs imported_mods
  = do	{ 	
        if opt_SccProfilingOn
            then do { -- Allocate the static boolean that records if this
                      -- module has been registered already
	              emitData Data [CmmDataLabel moduleRegdLabel, 
		                     CmmStaticLit zeroCLit]

                    ; emitSimpleProc real_init_lbl $ do
                        { ret_blk <- forkLabelledCode ret_code

                        ; init_blk <- forkLabelledCode $ do
                                        { mod_init_code; stmtC (CmmBranch ret_blk) }
                                    
                        ; stmtC (CmmCondBranch (cmmNeWord (CmmLit zeroCLit) mod_reg_val)
                                    ret_blk)
                        ; stmtC (CmmBranch init_blk)	    
                        }
                    }
            else emitSimpleProc real_init_lbl ret_code

 	    -- Make the "plain" procedure jump to the "real" init procedure
	; emitSimpleProc plain_init_lbl jump_to_init

	-- When compiling the module in which the 'main' function lives,
	-- (that is, this_mod == main_mod)
	-- we inject an extra stg_init procedure for stg_init_ZCMain, for the 
	-- RTS to invoke.  We must consult the -main-is flag in case the
	-- user specified a different function to Main.main
	; whenC (this_mod == main_mod)
		(emitSimpleProc plain_main_init_lbl jump_to_init)
    }
  where
    this_pkg = thisPackage dflags

    plain_init_lbl = mkPlainModuleInitLabel this_pkg this_mod
    real_init_lbl  = mkModuleInitLabel this_pkg this_mod way
    plain_main_init_lbl = mkPlainModuleInitLabel this_pkg rOOT_MAIN

    jump_to_init = stmtC (CmmJump (mkLblExpr real_init_lbl) [])

    mod_reg_val = CmmLoad (mkLblExpr moduleRegdLabel) wordRep

    -- Main refers to GHC.TopHandler.runIO, so make sure we call the
    -- init function for GHC.TopHandler.
    extra_imported_mods
	| this_mod == main_mod = [gHC_TOP_HANDLER]
	| otherwise	       = []

    mod_init_code = do
	{ 	-- Set mod_reg to 1 to record that we've been here
	  stmtC (CmmStore (mkLblExpr moduleRegdLabel) (CmmLit (mkIntCLit 1)))

		-- Now do local stuff
	; initCostCentres cost_centre_info
	; mapCs (registerModuleImport this_pkg way) 
		(imported_mods++extra_imported_mods)
	} 

                    -- The return-code pops the work stack by 
                    -- incrementing Sp, and then jumpd to the popped item
    ret_code = stmtsC [ CmmAssign spReg (cmmRegOffW spReg 1)
                      , CmmJump (CmmLoad (cmmRegOffW spReg (-1)) wordRep) [] ]

-----------------------
registerModuleImport :: PackageId -> String -> Module -> Code
registerModuleImport this_pkg way mod 
  | mod == gHC_PRIM
  = nopC 
  | otherwise 	-- Push the init procedure onto the work stack
  = stmtsC [ CmmAssign spReg (cmmRegOffW spReg (-1))
	   , CmmStore (CmmReg spReg) (mkLblExpr (mkModuleInitLabel this_pkg mod way)) ]
\end{code}



Cost-centre profiling: Besides the usual stuff, we must produce
declarations for the cost-centres defined in this module;

(The local cost-centres involved in this are passed into the
code-generator.)

\begin{code}
initCostCentres :: CollectedCCs -> Code
-- Emit the declarations, and return code to register them
initCostCentres (local_CCs, ___extern_CCs, singleton_CCSs)
  | not opt_SccProfilingOn = nopC
  | otherwise
  = do	{ mapM_ emitCostCentreDecl  	 local_CCs
	; mapM_ emitCostCentreStackDecl  singleton_CCSs
	; mapM_ emitRegisterCC           local_CCs
	; mapM_ emitRegisterCCS          singleton_CCSs
	}
\end{code}

%************************************************************************
%*									*
\subsection[codegen-top-bindings]{Converting top-level STG bindings}
%*									*
%************************************************************************

@cgTopBinding@ is only used for top-level bindings, since they need
to be allocated statically (not in the heap) and need to be labelled.
No unboxed bindings can happen at top level.

In the code below, the static bindings are accumulated in the
@MkCgState@, and transferred into the ``statics'' slot by @forkStatics@.
This is so that we can write the top level processing in a compositional
style, with the increasing static environment being plumbed as a state
variable.

\begin{code}
cgTopBinding :: DynFlags -> (StgBinding,[(Id,[Id])]) -> Code
cgTopBinding dflags (StgNonRec id rhs, srts)
  = do	{ id' <- maybeExternaliseId dflags id
	; mapM_ (mkSRT (thisPackage dflags) [id']) srts
	; (id,info) <- cgTopRhs id' rhs
	; addBindC id info 	-- Add the *un-externalised* Id to the envt,
				-- so we find it when we look up occurrences
	}

cgTopBinding dflags (StgRec pairs, srts)
  = do	{ let (bndrs, rhss) = unzip pairs
	; bndrs' <- mapFCs (maybeExternaliseId dflags) bndrs
	; let pairs' = zip bndrs' rhss
	; mapM_ (mkSRT (thisPackage dflags) bndrs')  srts
	; _new_binds <- fixC (\ new_binds -> do 
		{ addBindsC new_binds
		; mapFCs ( \ (b,e) -> cgTopRhs b e ) pairs' })
	; nopC }

mkSRT :: PackageId -> [Id] -> (Id,[Id]) -> Code
mkSRT this_pkg these (id,[])  = nopC
mkSRT this_pkg these (id,ids)
  = do	{ ids <- mapFCs remap ids
	; id  <- remap id
	; emitRODataLits (mkSRTLabel (idName id)) 
		       (map (CmmLabel . mkClosureLabel this_pkg . idName) ids)
	}
  where
	-- Sigh, better map all the ids against the environment in 
	-- case they've been externalised (see maybeExternaliseId below).
    remap id = case filter (==id) these of
		(id':_) -> returnFC id'
		[] -> do { info <- getCgIdInfo id; return (cgIdInfoId info) }

-- Urgh!  I tried moving the forkStatics call from the rhss of cgTopRhs
-- to enclose the listFCs in cgTopBinding, but that tickled the
-- statics "error" call in initC.  I DON'T UNDERSTAND WHY!

cgTopRhs :: Id -> StgRhs -> FCode (Id, CgIdInfo)
	-- The Id is passed along for setting up a binding...
	-- It's already been externalised if necessary

cgTopRhs bndr (StgRhsCon cc con args)
  = forkStatics (cgTopRhsCon bndr con args)

cgTopRhs bndr (StgRhsClosure cc bi fvs upd_flag srt args body)
  = ASSERT(null fvs)    -- There should be no free variables
    setSRTLabel (mkSRTLabel (idName bndr)) $ 
    forkStatics (cgTopRhsClosure bndr cc bi srt upd_flag args body)
\end{code}


%************************************************************************
%*									*
\subsection{Stuff to support splitting}
%*									*
%************************************************************************

If we're splitting the object, we need to externalise all the top-level names
(and then make sure we only use the externalised one in any C label we use
which refers to this name).

\begin{code}
maybeExternaliseId :: DynFlags -> Id -> FCode Id
maybeExternaliseId dflags id
  | dopt Opt_SplitObjs dflags, 	-- Externalise the name for -split-objs
    isInternalName name = do { mod <- moduleName
			     ; returnFC (setIdName id (externalise mod)) }
  | otherwise		= returnFC id
  where
    externalise mod = mkExternalName uniq mod new_occ Nothing loc
    name    = idName id
    uniq    = nameUnique name
    new_occ = mkLocalOcc uniq (nameOccName name)
    loc     = nameSrcLoc name
	-- We want to conjure up a name that can't clash with any
	-- existing name.  So we generate
	--	Mod_$L243foo
	-- where 243 is the unique.
\end{code}
