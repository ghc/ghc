%
% (c) The University of Glasgow 2006
% (c) The GRASP Project, Glasgow University, 1992-1998
%
\section[CgCon]{Code generation for constructors}

This module provides the support code for @StgToAbstractC@ to deal
with {\em constructors} on the RHSs of let(rec)s.  See also
@CgClosure@, which deals with closures.

\begin{code}
module CgCon (
        cgTopRhsCon, buildDynCon,
        bindConArgs, bindUnboxedTupleComponents,
        cgReturnDataCon,
        cgTyCon
    ) where

#include "HsVersions.h"

import CgMonad
import StgSyn

import CgBindery
import CgStackery
import CgUtils
import CgCallConv
import CgHeapery
import CgTailCall
import CgProf
import CgTicky
import CgInfoTbls
import CLabel
import ClosureInfo
import OldCmmUtils
import OldCmm
import SMRep
import CostCentre
import Constants
import TyCon
import DataCon
import Id
import IdInfo
import Type
import PrelInfo
import Outputable
import ListSetOps
import Util
import Module
import DynFlags
import FastString
import Platform

import Control.Monad
\end{code}


%************************************************************************
%*                                                                      *
\subsection[toplevel-constructors]{Top-level constructors}
%*                                                                      *
%************************************************************************

\begin{code}
cgTopRhsCon :: Id               -- Name of thing bound to this RHS
            -> DataCon          -- Id
            -> [StgArg]         -- Args
            -> FCode (Id, CgIdInfo)
cgTopRhsCon id con args
  = do { dflags <- getDynFlags
        ; when (platformOS (targetPlatform dflags) == OSMinGW32) $
              -- Windows DLLs have a problem with static cross-DLL refs.
              ASSERT( not (isDllConApp dflags con args) ) return ()
        ; ASSERT( args `lengthIs` dataConRepRepArity con ) return ()

        -- LAY IT OUT
        ; amodes <- getArgAmodes args

        ; let
            name          = idName id
            lf_info       = mkConLFInfo con
            closure_label = mkClosureLabel name $ idCafInfo id
            caffy         = any stgArgHasCafRefs args
            (closure_info, amodes_w_offsets) = layOutStaticConstr dflags con amodes
            closure_rep = mkStaticClosureFields
                             dflags
                             closure_info
                             dontCareCCS                -- Because it's static data
                             caffy                      -- Has CAF refs
                             payload

            payload = map get_lit amodes_w_offsets
            get_lit (CmmLit lit, _offset) = lit
            get_lit other = pprPanic "CgCon.get_lit" (ppr other)
                -- NB1: amodes_w_offsets is sorted into ptrs first, then non-ptrs
                -- NB2: all the amodes should be Lits!

                -- BUILD THE OBJECT
        ; emitDataLits closure_label closure_rep

                -- RETURN
        ; returnFC (id, taggedStableIdInfo id (mkLblExpr closure_label) lf_info con) }
\end{code}

%************************************************************************
%*                                                                      *
%* non-top-level constructors                                           *
%*                                                                      *
%************************************************************************
\subsection[code-for-constructors]{The code for constructors}

\begin{code}
buildDynCon :: Id                 -- Name of the thing to which this constr will
                                  -- be bound
            -> CostCentreStack    -- Where to grab cost centre from;
                                  -- current CCS if currentOrSubsumedCCS
            -> DataCon            -- The data constructor
            -> [(CgRep,CmmExpr)]  -- Its args
            -> FCode CgIdInfo     -- Return details about how to find it
buildDynCon binder ccs con args
    = do dflags <- getDynFlags
         buildDynCon' dflags (targetPlatform dflags) binder ccs con args

buildDynCon' :: DynFlags
             -> Platform
             -> Id
             -> CostCentreStack
             -> DataCon
             -> [(CgRep,CmmExpr)]
             -> FCode CgIdInfo

-- We used to pass a boolean indicating whether all the
-- args were of size zero, so we could use a static
-- construtor; but I concluded that it just isn't worth it.
-- Now I/O uses unboxed tuples there just aren't any constructors
-- with all size-zero args.
--
-- The reason for having a separate argument, rather than looking at
-- the addr modes of the args is that we may be in a "knot", and
-- premature looking at the args will cause the compiler to black-hole!
\end{code}

First we deal with the case of zero-arity constructors.  Now, they
will probably be unfolded, so we don't expect to see this case much,
if at all, but it does no harm, and sets the scene for characters.

In the case of zero-arity constructors, or, more accurately, those
which have exclusively size-zero (VoidRep) args, we generate no code
at all.

\begin{code}
buildDynCon' _ _ binder _ con []
  = returnFC (taggedStableIdInfo binder
                           (mkLblExpr (mkClosureLabel (dataConName con)
                                      (idCafInfo binder)))
                           (mkConLFInfo con)
                           con)
\end{code}

The following three paragraphs about @Char@-like and @Int@-like
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

\begin{code}


buildDynCon' dflags platform binder _ con [arg_amode]
  | maybeIntLikeCon con
  , platformOS platform /= OSMinGW32 || not (dopt Opt_PIC dflags)
  , (_, CmmLit (CmmInt val _)) <- arg_amode
  , let val_int = (fromIntegral val) :: Int
  , val_int <= mAX_INTLIKE && val_int >= mIN_INTLIKE
  = do  { let intlike_lbl   = mkCmmGcPtrLabel rtsPackageId (fsLit "stg_INTLIKE_closure")
              offsetW = (val_int - mIN_INTLIKE) * (fixedHdrSize dflags + 1)
                -- INTLIKE closures consist of a header and one word payload
              intlike_amode = CmmLit (cmmLabelOffW intlike_lbl offsetW)
        ; returnFC (taggedStableIdInfo binder intlike_amode (mkConLFInfo con) con) }

buildDynCon' dflags platform binder _ con [arg_amode]
  | maybeCharLikeCon con
  , platformOS platform /= OSMinGW32 || not (dopt Opt_PIC dflags)
  , (_, CmmLit (CmmInt val _)) <- arg_amode
  , let val_int = (fromIntegral val) :: Int
  , val_int <= mAX_CHARLIKE && val_int >= mIN_CHARLIKE
  = do  { let charlike_lbl   = mkCmmGcPtrLabel rtsPackageId (fsLit "stg_CHARLIKE_closure")
              offsetW = (val_int - mIN_CHARLIKE) * (fixedHdrSize dflags + 1)
                -- CHARLIKE closures consist of a header and one word payload
              charlike_amode = CmmLit (cmmLabelOffW charlike_lbl offsetW)
        ; returnFC (taggedStableIdInfo binder charlike_amode (mkConLFInfo con) con) }

\end{code}

Now the general case.

\begin{code}
buildDynCon' dflags _ binder ccs con args
  = do  {
        ; let
            (closure_info, amodes_w_offsets) = layOutDynConstr dflags con args

        ; hp_off <- allocDynClosure closure_info use_cc blame_cc amodes_w_offsets
        ; returnFC (taggedHeapIdInfo binder hp_off lf_info con) }
  where
    lf_info = mkConLFInfo con

    use_cc  -- cost-centre to stick in the object
      | isCurrentCCS ccs = curCCS
      | otherwise        = panic "buildDynCon: non-current CCS not implemented"

    blame_cc = use_cc -- cost-centre on which to blame the alloc (same)
\end{code}


%************************************************************************
%*                                                                      *
%* constructor-related utility function:                                *
%*              bindConArgs is called from cgAlt of a case              *
%*                                                                      *
%************************************************************************
\subsection[constructor-utilities]{@bindConArgs@: constructor-related utility}

@bindConArgs@ $con args$ augments the environment with bindings for the
binders $args$, assuming that we have just returned from a @case@ which
found a $con$.

\begin{code}
bindConArgs :: DataCon -> [Id] -> Code
bindConArgs con args
  = do dflags <- getDynFlags
       let
          -- The binding below forces the masking out of the tag bits
          -- when accessing the constructor field.
          bind_arg (arg, offset) = bindNewToUntagNode arg offset (mkLFArgument arg) (tagForCon con)
          (_, args_w_offsets)    = layOutDynConstr dflags con (addIdReps args)
        --
       ASSERT(not (isUnboxedTupleCon con)) return ()
       mapCs bind_arg args_w_offsets
\end{code}

Unboxed tuples are handled slightly differently - the object is
returned in registers and on the stack instead of the heap.

\begin{code}
bindUnboxedTupleComponents
        :: [Id]                         -- Args
        -> FCode ([(Id,GlobalReg)],     -- Regs assigned
                  WordOff,              -- Number of pointer stack slots
                  WordOff,              -- Number of non-pointer stack slots
                  VirtualSpOffset)      -- Offset of return address slot
                                        -- (= realSP on entry)

bindUnboxedTupleComponents args
 =  do  {
          dflags <- getDynFlags

        ; vsp <- getVirtSp
        ; rsp <- getRealSp

           -- Assign as many components as possible to registers
        ; let (reg_args, stk_args) = assignReturnRegs dflags (addIdReps args)

                -- Separate the rest of the args into pointers and non-pointers
              (ptr_args, nptr_args) = separateByPtrFollowness stk_args

                -- Allocate the rest on the stack
                -- The real SP points to the return address, above which any
                -- leftover unboxed-tuple components will be allocated
              (ptr_sp,  ptr_offsets)  = mkVirtStkOffsets rsp    ptr_args
              (nptr_sp, nptr_offsets) = mkVirtStkOffsets ptr_sp nptr_args
              ptrs  = ptr_sp  - rsp
              nptrs = nptr_sp - ptr_sp

            -- The stack pointer points to the last stack-allocated component
        ; setRealAndVirtualSp nptr_sp

            -- We have just allocated slots starting at real SP + 1, and set the new
            -- virtual SP to the topmost allocated slot.
            -- If the virtual SP started *below* the real SP, we've just jumped over
            -- some slots that won't be in the free-list, so put them there
            -- This commonly happens because we've freed the return-address slot
            -- (trimming back the virtual SP), but the real SP still points to that slot
        ; freeStackSlots [vsp+1,vsp+2 .. rsp]

        ; bindArgsToRegs reg_args
        ; bindArgsToStack ptr_offsets
        ; bindArgsToStack nptr_offsets

        ; returnFC (reg_args, ptrs, nptrs, rsp) }
\end{code}

%************************************************************************
%*                                                                      *
        Actually generate code for a constructor return
%*                                                                      *
%************************************************************************


Note: it's the responsibility of the @cgReturnDataCon@ caller to be
sure the @amodes@ passed don't conflict with each other.
\begin{code}
cgReturnDataCon :: DataCon -> [(CgRep, CmmExpr)] -> Code

cgReturnDataCon con amodes = do
  dflags <- getDynFlags
  if isUnboxedTupleCon con then returnUnboxedTuple amodes
  -- when profiling we can't shortcut here, we have to enter the closure
  -- for it to be marked as "used" for LDV profiling.
   else if dopt Opt_SccProfilingOn dflags then build_it_then (enter_it dflags)
   else ASSERT( amodes `lengthIs` dataConRepRepArity con )
     do { EndOfBlockInfo _ sequel <- getEndOfBlockInfo
        ; case sequel of
            CaseAlts _ (Just (alts, deflt_lbl)) bndr
              ->    -- Ho! We know the constructor so we can
                    -- go straight to the right alternative
                 case assocMaybe alts (dataConTagZ con) of {
                    Just join_lbl -> build_it_then (jump_to join_lbl);
                    Nothing
                        -- Special case!  We're returning a constructor to the default case
                        -- of an enclosing case.  For example:
                        --
                        --      case (case e of (a,b) -> C a b) of
                        --        D x -> ...
                        --        y   -> ...<returning here!>...
                        --
                        -- In this case,
                        --      if the default is a non-bind-default (ie does not use y),
                        --      then we should simply jump to the default join point;

                        | isDeadBinder bndr -> performReturn (jump_to deflt_lbl)
                        | otherwise         -> build_it_then (jump_to deflt_lbl) }

            _otherwise  -- The usual case
              -> build_it_then $ emitReturnInstr node_live
        }
  where
    node_live   = Just [node]
    enter_it dflags
                = stmtsC [ CmmAssign nodeReg (cmmUntag (CmmReg nodeReg)),
                           CmmJump (entryCode dflags $ closureInfoPtr $ CmmReg nodeReg)
                                   node_live
                         ]
    jump_to lbl = stmtC $ CmmJump (CmmLit lbl) node_live
    build_it_then return_code
      = do {    -- BUILD THE OBJECT IN THE HEAP
                -- The first "con" says that the name bound to this
                -- closure is "con", which is a bit of a fudge, but it only
                -- affects profiling

                -- This Id is also used to get a unique for a
                -- temporary variable, if the closure is a CHARLIKE.
                -- funnily enough, this makes the unique always come
                -- out as '54' :-)
             tickyReturnNewCon (length amodes)
           ; idinfo <- buildDynCon (dataConWorkId con) currentCCS con amodes
           ; amode <- idInfoToAmode idinfo
           ; checkedAbsC (CmmAssign nodeReg amode)
           ; performReturn return_code }
\end{code}


%************************************************************************
%*                                                                      *
        Generating static stuff for algebraic data types
%*                                                                      *
%************************************************************************

        [These comments are rather out of date]

\begin{tabular}{lll}
Info tbls &      Macro  &            Kind of constructor \\
\hline
info & @CONST_INFO_TABLE@&    Zero arity (no info -- compiler uses static closure)\\
info & @CHARLIKE_INFO_TABLE@& Charlike   (no info -- compiler indexes fixed array)\\
info & @INTLIKE_INFO_TABLE@&  Intlike; the one macro generates both info tbls\\
info & @SPEC_INFO_TABLE@&     SPECish, and bigger than or equal to @MIN_UPD_SIZE@\\
info & @GEN_INFO_TABLE@&      GENish (hence bigger than or equal to @MIN_UPD_SIZE@)\\
\end{tabular}

Possible info tables for constructor con:

\begin{description}
\item[@_con_info@:]
Used for dynamically let(rec)-bound occurrences of
the constructor, and for updates.  For constructors
which are int-like, char-like or nullary, when GC occurs,
the closure tries to get rid of itself.

\item[@_static_info@:]
Static occurrences of the constructor
macro: @STATIC_INFO_TABLE@.
\end{description}

For zero-arity constructors, \tr{con}, we NO LONGER generate a static closure;
it's place is taken by the top level defn of the constructor.

For charlike and intlike closures there is a fixed array of static
closures predeclared.

\begin{code}
cgTyCon :: TyCon -> FCode CmmGroup  -- each constructor gets a separate CmmGroup
cgTyCon tycon
  = do  { constrs <- mapM (getCmm . cgDataCon) (tyConDataCons tycon)

            -- Generate a table of static closures for an enumeration type
            -- Put the table after the data constructor decls, because the
            -- datatype closure table (for enumeration types)
            -- to (say) PrelBase_$wTrue_closure, which is defined in code_stuff
            -- Note that the closure pointers are tagged.

            -- XXX comment says to put table after constructor decls, but
            -- code appears to put it before --- NR 16 Aug 2007
        ; extra <-
           if isEnumerationTyCon tycon then do
                tbl <- getCmm (emitRODataLits "cgTyCon" (mkLocalClosureTableLabel (tyConName tycon) NoCafRefs)
                           [ CmmLabelOff (mkLocalClosureLabel (dataConName con) NoCafRefs) (tagForCon con)
                           | con <- tyConDataCons tycon])
                return [tbl]
           else
                return []

        ; return (concat (extra ++ constrs))
    }
\end{code}

Generate the entry code, info tables, and (for niladic constructor) the
static closure, for a constructor.

\begin{code}
cgDataCon :: DataCon -> Code
cgDataCon data_con
  = do  { dflags <- getDynFlags
        -- Don't need any dynamic closure code for zero-arity constructors

        ; let
            -- To allow the debuggers, interpreters, etc to cope with
            -- static data structures (ie those built at compile
            -- time), we take care that info-table contains the
            -- information we need.
            (static_cl_info, _) =
                layOutStaticConstr dflags data_con arg_reps

            (dyn_cl_info, arg_things) =
                layOutDynConstr    dflags data_con arg_reps

            emit_info cl_info ticky_code
                = do { code_blks <- getCgStmts the_code
                     ; emitClosureCodeAndInfoTable cl_info [] code_blks }
                where
                  the_code = do { _ <- ticky_code
                                ; ldvEnter (CmmReg nodeReg)
                                ; body_code }

            arg_reps :: [(CgRep, UnaryType)]
            arg_reps = [(typeCgRep rep_ty, rep_ty) | ty <- dataConRepArgTys data_con, rep_ty <- flattenRepType (repType ty)]

            body_code = do {
                        -- NB: We don't set CC when entering data (WDP 94/06)
                             tickyReturnOldCon (length arg_things)
                           -- The case continuation code is expecting a tagged pointer
                           ; stmtC (CmmAssign nodeReg
                                              (tagCons data_con (CmmReg nodeReg)))
                           ; performReturn $ emitReturnInstr (Just []) }
                                -- noStmts: Ptr to thing already in Node

        ; whenC (not (isNullaryRepDataCon data_con))
                (emit_info dyn_cl_info tickyEnterDynCon)

                -- Dynamic-Closure first, to reduce forward references
        ; emit_info static_cl_info tickyEnterStaticCon }
\end{code}
