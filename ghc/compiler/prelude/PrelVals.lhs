%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[PrelVals]{Prelude values the compiler ``knows about''}

\begin{code}
module PrelVals where

#include "HsVersions.h"

import {-# SOURCE #-} CoreUnfold ( mkUnfolding )

import Id		( Id, mkVanillaId, setIdInfo, mkTemplateLocals  )

-- friends:
import PrelMods
import TysPrim
import TysWiredIn

-- others:
import CoreSyn		-- quite a bit
import IdInfo		-- quite a bit
import Name		( mkWiredInIdName, mkSrcVarOcc, Module )
import Type		
import Var		( TyVar )
import Demand		( wwStrict )
import Unique		-- lots of *Keys

import IOExts
\end{code}

%************************************************************************
%*									*
\subsection{Un-definable}
%*									*
%************************************************************************

These two can't be defined in Haskell.


unsafeCoerce# isn't so much a PrimOp as a phantom identifier, that
just gets expanded into a type coercion wherever it occurs.  Hence we
add it as a built-in Id with an unfolding here.

The type variables we use here are "open" type variables: this means
they can unify with both unlifted and lifted types.  Hence we provide
another gun with which to shoot yourself in the foot.

\begin{code}
unsafeCoerceId
  = pcMiscPrelId unsafeCoerceIdKey pREL_GHC SLIT("unsafeCoerce#") ty
	(mk_inline_unfolding template)
  where
    (alphaTyVar:betaTyVar:_) = openAlphaTyVars
    alphaTy  = mkTyVarTy alphaTyVar
    betaTy   = mkTyVarTy betaTyVar
    ty = mkForAllTys [alphaTyVar,betaTyVar] (mkFunTy alphaTy betaTy)
    [x] = mkTemplateLocals [alphaTy]
    template = mkLams [alphaTyVar,betaTyVar,x] $
	       Note (Coerce betaTy alphaTy) (Var x)
\end{code}


@realWorld#@ used to be a magic literal, \tr{void#}.  If things get
nasty as-is, change it back to a literal (@Literal@).

\begin{code}
realWorldPrimId
  = pcMiscPrelId realWorldPrimIdKey pREL_GHC SLIT("realWorld#")
	realWorldStatePrimTy
	noCafIdInfo
\end{code}


%************************************************************************
%*									*
\subsection[PrelVals-error-related]{@error@ and friends; @trace@}
%*									*
%************************************************************************

GHC randomly injects these into the code.

@patError@ is just a version of @error@ for pattern-matching
failures.  It knows various ``codes'' which expand to longer
strings---this saves space!

@absentErr@ is a thing we put in for ``absent'' arguments.  They jolly
well shouldn't be yanked on, but if one is, then you will get a
friendly message from @absentErr@ (rather than a totally random
crash).

@parError@ is a special version of @error@ which the compiler does
not know to be a bottoming Id.  It is used in the @_par_@ and @_seq_@
templates, but we don't ever expect to generate code for it.

\begin{code}
pc_bottoming_Id key mod name ty
 = pcMiscPrelId key mod name ty bottoming_info
 where
    bottoming_info = mkStrictnessInfo ([wwStrict], True) False `setStrictnessInfo` noCafIdInfo
	-- these "bottom" out, no matter what their arguments

eRROR_ID
  = pc_bottoming_Id errorIdKey pREL_ERR SLIT("error") errorTy

generic_ERROR_ID u n
  = pc_bottoming_Id u pREL_ERR n errorTy

rEC_SEL_ERROR_ID
  = generic_ERROR_ID recSelErrIdKey SLIT("patError")
pAT_ERROR_ID
  = generic_ERROR_ID patErrorIdKey SLIT("patError")
rEC_CON_ERROR_ID
  = generic_ERROR_ID recConErrorIdKey SLIT("recConError")
rEC_UPD_ERROR_ID
  = generic_ERROR_ID recUpdErrorIdKey SLIT("recUpdError")
iRREFUT_PAT_ERROR_ID
  = generic_ERROR_ID irrefutPatErrorIdKey SLIT("irrefutPatError")
nON_EXHAUSTIVE_GUARDS_ERROR_ID
  = generic_ERROR_ID nonExhaustiveGuardsErrorIdKey SLIT("nonExhaustiveGuardsError")
nO_METHOD_BINDING_ERROR_ID
  = generic_ERROR_ID noMethodBindingErrorIdKey SLIT("noMethodBindingError")

aBSENT_ERROR_ID
  = pc_bottoming_Id absentErrorIdKey pREL_ERR SLIT("absentErr")
	(mkSigmaTy [openAlphaTyVar] [] openAlphaTy)

pAR_ERROR_ID
  = pcMiscPrelId parErrorIdKey pREL_ERR SLIT("parError")
    (mkSigmaTy [openAlphaTyVar] [] openAlphaTy) noCafIdInfo

openAlphaTy = mkTyVarTy openAlphaTyVar

errorTy  :: Type
errorTy  = mkSigmaTy [openAlphaTyVar] [] (mkFunTys [mkListTy charTy] openAlphaTy)
    -- Notice the openAlphaTyVar.  It says that "error" can be applied
    -- to unboxed as well as boxed types.  This is OK because it never
    -- returns, so the return type is irrelevant.
\end{code}


%************************************************************************
%*									*
\subsection{Utilities}
%*									*
%************************************************************************

Note IMustBeINLINEd below.  These things have the same status as
constructor functions, i.e. they will *always* be inlined, wherever
they occur.

\begin{code}
mk_inline_unfolding expr = setUnfoldingInfo (mkUnfolding expr)	$
			   setInlinePragInfo IMustBeINLINEd  noIdInfo

exactArityInfo n = exactArity n `setArityInfo` noIdInfo

pcMiscPrelId :: Unique{-IdKey-} -> Module -> FAST_STRING -> Type -> IdInfo -> Id

pcMiscPrelId key mod str ty info
  = let
	name = mkWiredInIdName key mod (mkSrcVarOcc str) imp
	imp  = mkVanillaId name ty `setIdInfo` info -- the usual case...
    in
    imp
    -- We lie and say the thing is imported; otherwise, we get into
    -- a mess with dependency analysis; e.g., core2stg may heave in
    -- random calls to GHCbase.unpackPS__.  If GHCbase is the module
    -- being compiled, then it's just a matter of luck if the definition
    -- will be in "the right place" to be in scope.

-- very useful...
noCafIdInfo = NoCafRefs `setCafInfo` noIdInfo
\end{code}

