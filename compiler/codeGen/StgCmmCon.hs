-----------------------------------------------------------------------------
--
-- Stg to C--: code generation for constructors
--
-- This module provides the support code for StgCmm to deal with with
-- constructors on the RHSs of let(rec)s.
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

module StgCmmCon (
	cgTopRhsCon, buildDynCon, bindConArgs 
    ) where

#include "HsVersions.h"

import StgSyn
import CoreSyn	( AltCon(..) )

import StgCmmMonad
import StgCmmEnv
import StgCmmHeap
import StgCmmUtils
import StgCmmClosure
import StgCmmProf

import CmmExpr
import CLabel
import MkGraph
import SMRep
import CostCentre
import Module
import Constants
import DataCon
import FastString
import Id
import Literal
import PrelInfo
import Outputable
import Util             ( lengthIs )

import Data.Char

#if defined(mingw32_TARGET_OS)
import StaticFlags	( opt_PIC )
#endif


---------------------------------------------------------------
--	Top-level constructors
---------------------------------------------------------------

cgTopRhsCon :: Id		-- Name of thing bound to this RHS
	    -> DataCon		-- Id
	    -> [StgArg]		-- Args
	    -> FCode CgIdInfo
cgTopRhsCon id con args
  = do { 
#if mingw32_TARGET_OS
        -- Windows DLLs have a problem with static cross-DLL refs.
	; this_pkg <- getThisPackage
        ; ASSERT( not (isDllConApp this_pkg con args) ) return ()
#endif
	; ASSERT( args `lengthIs` dataConRepArity con ) return ()

	-- LAY IT OUT
	; let
	    name          = idName id
	    lf_info	  = mkConLFInfo con
    	    closure_label = mkClosureLabel name $ idCafInfo id
	    caffy         = any stgArgHasCafRefs args
	    (closure_info, nv_args_w_offsets) 
			= layOutStaticConstr con (addArgReps args)

	    get_lit (arg, _offset) = do { CmmLit lit <- getArgAmode arg
				        ; return lit }

	; payload <- mapM get_lit nv_args_w_offsets
		-- NB1: nv_args_w_offsets is sorted into ptrs then non-ptrs
		-- NB2: all the amodes should be Lits!

	; let closure_rep = mkStaticClosureFields
	    		     closure_info
	    		     dontCareCCS		-- Because it's static data
	    		     caffy			-- Has CAF refs
			     payload

		-- BUILD THE OBJECT
	; emitDataLits closure_label closure_rep

		-- RETURN
	; return $ litIdInfo id lf_info (CmmLabel closure_label) }


---------------------------------------------------------------
--	Lay out and allocate non-top-level constructors
---------------------------------------------------------------

buildDynCon :: Id		  -- Name of the thing to which this constr will
				  -- be bound
	    -> CostCentreStack	  -- Where to grab cost centre from;
				  -- current CCS if currentOrSubsumedCCS
	    -> DataCon		  -- The data constructor
	    -> [StgArg] 	  -- Its args
	    -> FCode (CgIdInfo, CmmAGraph)
               -- Return details about how to find it and initialization code

{- We used to pass a boolean indicating whether all the
args were of size zero, so we could use a static
construtor; but I concluded that it just isn't worth it.
Now I/O uses unboxed tuples there just aren't any constructors
with all size-zero args.

The reason for having a separate argument, rather than looking at
the addr modes of the args is that we may be in a "knot", and
premature looking at the args will cause the compiler to black-hole!
-}


-------- buildDynCon: Nullary constructors --------------
-- First we deal with the case of zero-arity constructors.  They
-- will probably be unfolded, so we don't expect to see this case much,
-- if at all, but it does no harm, and sets the scene for characters.
-- 
-- In the case of zero-arity constructors, or, more accurately, those
-- which have exclusively size-zero (VoidRep) args, we generate no code
-- at all.

buildDynCon binder _cc con []
  = return (litIdInfo binder (mkConLFInfo con)
		(CmmLabel (mkClosureLabel (dataConName con) (idCafInfo binder))),
            mkNop)

-------- buildDynCon: Charlike and Intlike constructors -----------
{- The following three paragraphs about @Char@-like and @Int@-like
closures are obsolete, but I don't understand the details well enough
to properly word them, sorry. I've changed the treatment of @Char@s to
be analogous to @Int@s: only a subset is preallocated, because @Char@
has now 31 bits. Only literals are handled here. -- Qrczak

Now for @Char@-like closures.  We generate an assignment of the
address of the closure to a temporary.  It would be possible simply to
generate no code, and record the addressing mode in the environment,
but we'd have to be careful if the argument wasn't a constant --- so
for simplicity we just always asssign to a temporary.

Last special case: @Int@-like closures.  We only special-case the
situation in which the argument is a literal in the range
@mIN_INTLIKE@..@mAX_INTLILKE@.  NB: for @Char@-like closures we can
work with any old argument, but for @Int@-like ones the argument has
to be a literal.  Reason: @Char@ like closures have an argument type
which is guaranteed in range.

Because of this, we use can safely return an addressing mode. 

We don't support this optimisation when compiling into Windows DLLs yet
because they don't support cross package data references well.
-}

buildDynCon binder _cc con [arg]
  | maybeIntLikeCon con 
#if defined(mingw32_TARGET_OS)
  , not opt_PIC
#endif
  , StgLitArg (MachInt val) <- arg
  , val <= fromIntegral mAX_INTLIKE 	-- Comparisons at type Integer!
  , val >= fromIntegral mIN_INTLIKE	-- ...ditto...
  = do 	{ let intlike_lbl   = mkCmmGcPtrLabel rtsPackageId (fsLit "stg_INTLIKE_closure")
	      val_int = fromIntegral val :: Int
	      offsetW = (val_int - mIN_INTLIKE) * (fixedHdrSize + 1)
		-- INTLIKE closures consist of a header and one word payload
	      intlike_amode = cmmLabelOffW intlike_lbl offsetW
	; return (litIdInfo binder (mkConLFInfo con) intlike_amode, mkNop) }

buildDynCon binder _cc con [arg]
  | maybeCharLikeCon con
#if defined(mingw32_TARGET_OS)
  , not opt_PIC
#endif
  , StgLitArg (MachChar val) <- arg
  , let val_int = ord val :: Int
  , val_int <= mAX_CHARLIKE
  , val_int >= mIN_CHARLIKE
  = do 	{ let charlike_lbl   = mkCmmGcPtrLabel rtsPackageId (fsLit "stg_CHARLIKE_closure")
	      offsetW = (val_int - mIN_CHARLIKE) * (fixedHdrSize + 1)
		-- CHARLIKE closures consist of a header and one word payload
	      charlike_amode = cmmLabelOffW charlike_lbl offsetW
	; return (litIdInfo binder (mkConLFInfo con) charlike_amode, mkNop) }

-------- buildDynCon: the general case -----------
buildDynCon binder ccs con args
  = do	{ let (cl_info, args_w_offsets) = layOutDynConstr con (addArgReps args)
		-- No void args in args_w_offsets
	; (tmp, init) <- allocDynClosure cl_info use_cc blame_cc args_w_offsets
 	; return (regIdInfo binder lf_info tmp, init) }
  where
    lf_info = mkConLFInfo con

    use_cc	-- cost-centre to stick in the object
      | currentOrSubsumedCCS ccs = curCCS
      | otherwise		 = CmmLit (mkCCostCentreStack ccs)

    blame_cc = use_cc -- cost-centre on which to blame the alloc (same)


---------------------------------------------------------------
--	Binding constructor arguments
---------------------------------------------------------------

bindConArgs :: AltCon -> LocalReg -> [Id] -> FCode [LocalReg]
-- bindConArgs is called from cgAlt of a case
-- (bindConArgs con args) augments the environment with bindings for the
-- binders args, assuming that we have just returned from a 'case' which
-- found a con
bindConArgs (DataAlt con) base args
  = ASSERT(not (isUnboxedTupleCon con))
    mapM bind_arg args_w_offsets
  where
    (_, args_w_offsets) = layOutDynConstr con (addIdReps args)

    tag = tagForCon con

          -- The binding below forces the masking out of the tag bits
          -- when accessing the constructor field.
    bind_arg :: (NonVoid Id, VirtualHpOffset) -> FCode LocalReg
    bind_arg (arg, offset) 
	= do { emit $ mkTaggedObjectLoad (idToReg arg) base offset tag
	     ; bindArgToReg arg }

bindConArgs _other_con _base args
  = ASSERT( null args ) return []



