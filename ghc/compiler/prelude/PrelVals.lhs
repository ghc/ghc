%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[PrelVals]{Prelude values the compiler ``knows about''}

\begin{code}
#include "HsVersions.h"

module PrelVals where

import Ubiq
import IdLoop		( UnfoldingGuidance(..) )
import Id		( Id(..), GenId, mkPreludeId, mkTemplateLocals )
import PrelLoop

-- friends:
import PrelMods
import TysPrim
import TysWiredIn

-- others:
import CoreSyn		-- quite a bit
import IdInfo		-- quite a bit
import Literal		( mkMachInt )
import PrimOp		( PrimOp(..) )
import SpecEnv		( SpecEnv(..), nullSpecEnv )
import TyVar		( alphaTyVar, betaTyVar )
import Unique		-- lots of *Keys
import Util		( panic )
\end{code}




\begin{code}
-- only used herein:
pcMiscPrelId :: Unique{-IdKey-} -> FAST_STRING -> FAST_STRING -> Type -> IdInfo -> Id

pcMiscPrelId key mod name ty info
 = mkPreludeId (mkBuiltinName key mod name) ty info
\end{code}

%************************************************************************
%*									*
\subsection[PrelVals-error-related]{@error@ and friends; @trace@}
%*									*
%************************************************************************

GHC randomly injects these into the code.

@patError#@ is just a version of @error@ for pattern-matching
failures.  It knows various ``codes'' which expand to longer
strings---this saves space!

@absent#@ is a thing we put in for ``absent'' arguments.  They jolly
well shouldn't be yanked on, but if one is, then you will get a
friendly message from @absent#@ (rather a totally random crash).

@parError#@ is a special version of @error@ which the compiler does
not know to be a bottoming Id.  It is used in the @_par_@ and @_seq_@
templates, but we don't ever expect to generate code for it.

\begin{code}
pc_bottoming_Id key mod name ty
 = pcMiscPrelId key mod name ty bottoming_info
 where
    bottoming_info = noIdInfo `addInfo` mkBottomStrictnessInfo
	-- these "bottom" out, no matter what their arguments

eRROR_ID
  = pc_bottoming_Id errorIdKey pRELUDE_BUILTIN SLIT("error") errorTy

generic_ERROR_ID u n
  = pc_bottoming_Id u pRELUDE_BUILTIN n errorTy

pAT_ERROR_ID
  = generic_ERROR_ID patErrorIdKey SLIT("patError#")
rEC_CON_ERROR_ID
  = generic_ERROR_ID recConErrorIdKey SLIT("recConError#")
rEC_UPD_ERROR_ID
  = generic_ERROR_ID recUpdErrorIdKey SLIT("recUpdError#")
iRREFUT_PAT_ERROR_ID
  = generic_ERROR_ID irrefutPatErrorIdKey SLIT("irrefutPatError#")
nON_EXHAUSTIVE_GUARDS_ERROR_ID
  = generic_ERROR_ID nonExhaustiveGuardsErrorIdKey SLIT("nonExhaustiveGuardsError#")

aBSENT_ERROR_ID
  = pc_bottoming_Id absentErrorIdKey pRELUDE_BUILTIN SLIT("absent#")
	(mkSigmaTy [alphaTyVar] [] alphaTy)

pAR_ERROR_ID
  = pcMiscPrelId parErrorIdKey pRELUDE_BUILTIN SLIT("parError#")
    (mkSigmaTy [alphaTyVar] [] alphaTy) noIdInfo

errorTy  :: Type
errorTy  = mkSigmaTy [alphaTyVar] [] (mkFunTys [mkListTy charTy] alphaTy)
\end{code}

We want \tr{_trace} (NB: name not in user namespace) to be wired in
because we don't want the strictness analyser to get ahold of it,
decide that the second argument is strict, evaluate that first (!!),
and make a jolly old mess.  Having \tr{_trace} wired in also helps when
attempting to re-export it---because it's in \tr{PreludeBuiltin}, it
won't get an \tr{import} declaration in the interface file, so the
importing-subsequently module needs to know it's magic.
\begin{code}
tRACE_ID
  = pcMiscPrelId traceIdKey pRELUDE_BUILTIN SLIT("_trace") traceTy
	(noIdInfo `addInfo` pcGenerateSpecs traceIdKey tRACE_ID noIdInfo traceTy)
  where
    traceTy = mkSigmaTy [alphaTyVar] [] (mkFunTys [mkListTy charTy, alphaTy] alphaTy)
\end{code}

%************************************************************************
%*									*
\subsection[PrelVals-Integer-support]{To support @Integer@ and @String@ literals}
%*									*
%************************************************************************

\begin{code}
packStringForCId
  = pcMiscPrelId packCStringIdKey{-ToDo:rename-} pRELUDE_PS SLIT("_packStringForC")
	(mkFunTys [stringTy] byteArrayPrimTy) noIdInfo

--------------------------------------------------------------------

unpackCStringId
  = pcMiscPrelId unpackCStringIdKey pRELUDE_BUILTIN SLIT("unpackPS#")
		 (mkFunTys [addrPrimTy{-a char *-}] stringTy) noIdInfo
-- Andy says:
--	(FunTy addrPrimTy{-a char *-} stringTy) (noIdInfo `addInfo` mkArityInfo 1)
-- but I don't like wired-in IdInfos (WDP)

unpackCString2Id -- for cases when a string has a NUL in it
  = pcMiscPrelId unpackCString2IdKey pRELUDE_BUILTIN SLIT("unpackPS2#")
		 (mkFunTys [addrPrimTy{-a char *-}, intPrimTy{-length-}] stringTy)
		 noIdInfo

--------------------------------------------------------------------
unpackCStringAppendId
  = pcMiscPrelId unpackCStringAppendIdKey pRELUDE_BUILTIN SLIT("unpackAppendPS#")
		(mkFunTys [addrPrimTy{-a "char *" pointer-},stringTy] stringTy)
		((noIdInfo
		 `addInfo_UF` mkMagicUnfolding unpackCStringAppendIdKey)
		 `addInfo` mkArityInfo 2)

unpackCStringFoldrId
  = pcMiscPrelId unpackCStringFoldrIdKey pRELUDE_BUILTIN SLIT("unpackFoldrPS#")
		(mkSigmaTy [alphaTyVar] []
		(mkFunTys [addrPrimTy{-a "char *" pointer-},
			   mkFunTys [charTy, alphaTy] alphaTy,
			   alphaTy]
			  alphaTy))
		((noIdInfo
		 `addInfo_UF` mkMagicUnfolding unpackCStringFoldrIdKey)
		 `addInfo` mkArityInfo 3)
\end{code}

OK, this is Will's idea: we should have magic values for Integers 0,
+1, +2, and -1 (go ahead, fire me):
\begin{code}
integerZeroId
  = pcMiscPrelId integerZeroIdKey     pRELUDE_CORE SLIT("__integer0")  integerTy noIdInfo
integerPlusOneId
  = pcMiscPrelId integerPlusOneIdKey  pRELUDE_CORE SLIT("__integer1")  integerTy noIdInfo
integerPlusTwoId
  = pcMiscPrelId integerPlusTwoIdKey  pRELUDE_CORE SLIT("__integer2")  integerTy noIdInfo
integerMinusOneId
  = pcMiscPrelId integerMinusOneIdKey pRELUDE_CORE SLIT("__integerm1") integerTy noIdInfo
\end{code}

%************************************************************************
%*									*
\subsection[PrelVals-parallel]{@seq@ and @par@: for parallel operation (only)}
%*									*
%************************************************************************

\begin{code}
--------------------------------------------------------------------
-- seqId :: "_seq_", used w/ GRIP, etc., is really quite similar to
-- dangerousEval
{-
   OLDER:
   _seq_ = /\ a b -> \ x y -> case x of { _ -> y }

   OLD:
   _seq_ = /\ a b -> \ x y -> case seq# x y of { _Lift y' -> y' }

   NEW (95/05):
   _seq_ = /\ a b -> \ x::a y::b -> case seq# x of { 0# -> parError#; _ -> y; }

-}

seqId = pcMiscPrelId seqIdKey pRELUDE_BUILTIN SLIT("_seq_")
		  (mkSigmaTy [alphaTyVar, betaTyVar] []
		    (mkFunTys [alphaTy, betaTy] betaTy))
		  (noIdInfo `addInfo_UF` (mkUnfolding EssentialUnfolding seq_template))
  where
    [x, y, z]
      = mkTemplateLocals [
    	{-x-} alphaTy,
    	{-y-} betaTy,
	{-z-} intPrimTy
    	]

    seq_template
      = mkLam [alphaTyVar, betaTyVar] [x, y] (
		Case (Prim SeqOp [TyArg alphaTy, VarArg x]) (
		  PrimAlts
		    [(mkMachInt 0, mkTyApp (Var pAR_ERROR_ID) [betaTy])]
		    (BindDefault z (Var y))))

--------------------------------------------------------------------
-- parId :: "_par_", also used w/ GRIP, etc.
{-
    OLDER:

    par = /\ a b -> \ x y -> case (par# (case x of { _ -> () })) of { _ -> y }

    OLD:

    _par_ = /\ a b -> \ x y -> case par# x y of { _Lift y' -> y' }

    NEW (95/05):

    _par_ = /\ a b -> \ x::a y::b -> case par# x of { 0# -> parError#; _ -> y; }

-}
parId = pcMiscPrelId parIdKey pRELUDE_BUILTIN SLIT("_par_")
		  (mkSigmaTy [alphaTyVar, betaTyVar] []
		    (mkFunTys [alphaTy, betaTy] betaTy))
		  (noIdInfo `addInfo_UF` (mkUnfolding EssentialUnfolding par_template))
  where
    [x, y, z]
      = mkTemplateLocals [
    	{-x-} alphaTy,
    	{-y-} betaTy,
	{-z-} intPrimTy
    	]

    par_template
      = mkLam [alphaTyVar, betaTyVar] [x, y] (
		Case (Prim ParOp [TyArg alphaTy, VarArg x]) (
		  PrimAlts
		    [(mkMachInt 0, mkTyApp (Var pAR_ERROR_ID) [betaTy])]
		    (BindDefault z (Var y))))

-- forkId :: "_fork_", for *required* concurrent threads
{-
   _fork_ = /\ a b -> \ x::a y::b -> case fork# x of { 0# -> parError#; _ -> y; }
-}
forkId = pcMiscPrelId forkIdKey pRELUDE_BUILTIN SLIT("_fork_")
		  (mkSigmaTy [alphaTyVar, betaTyVar] []
		    (mkFunTys [alphaTy, betaTy] betaTy))
		  (noIdInfo `addInfo_UF` (mkUnfolding EssentialUnfolding fork_template))
  where
    [x, y, z]
      = mkTemplateLocals [
    	{-x-} alphaTy,
    	{-y-} betaTy,
	{-z-} intPrimTy
    	]

    fork_template
      = mkLam [alphaTyVar, betaTyVar] [x, y] (
		Case (Prim ForkOp [TyArg alphaTy, VarArg x]) (
		  PrimAlts
		    [(mkMachInt 0, mkTyApp (Var pAR_ERROR_ID) [betaTy])]
		    (BindDefault z (Var y))))

\end{code}

\begin{code}
#ifdef GRAN

parLocalId = pcMiscPrelId parLocalIdKey pRELUDE_BUILTIN SLIT("_parLocal_")
		  (mkSigmaTy [alphaTyVar, betaTyVar] []
		    (mkFunTys [intPrimTy, alphaTy, betaTy] betaTy))
		  (noIdInfo `addInfo_UF` (mkUnfolding EssentialUnfolding parLocal_template))
  where
    [w, x, y, z]
      = mkTemplateLocals [
	{-w-} intPrimTy,
    	{-x-} alphaTy,
    	{-y-} betaTy,
	{-z-} betaTy
    	]

    parLocal_template
      = mkLam [alphaTyVar, betaTyVar] [w, x, y] (
		Case (Prim ParLocalOp [TyArg alphaTy, TyArg betaTy, VarArg x, VarArg w, VarArg y]) (
		  AlgAlts
		    [(liftDataCon, [z], Var z)]
		    (NoDefault)))

parGlobalId = pcMiscPrelId parGlobalIdKey pRELUDE_BUILTIN SLIT("_parGlobal_")
		  (mkSigmaTy [alphaTyVar, betaTyVar] []
		    (mkFunTys [intPrimTy,alphaTy,betaTy] betaTy))
		  (noIdInfo `addInfo_UF` (mkUnfolding EssentialUnfolding parGlobal_template))
  where
    [w, x, y, z]
      = mkTemplateLocals [
    	{-w-} intPrimTy,
    	{-x-} alphaTy,
    	{-y-} betaTy,
	{-z-} betaTy
    	]

    parGlobal_template
      = mkLam [alphaTyVar, betaTyVar] [w, x, y] (
		Case (Prim ParGlobalOp [TyArg alphaTy, TyArg betaTy, VarArg x, VarArg w, VarArg y]) (
		  AlgAlts
		    [(liftDataCon, [z], Var z)]
		    (NoDefault)))

#endif {-GRAN-}
\end{code}

%************************************************************************
%*									*
\subsection[PrelVals-deriving]{Values known about mainly for doing derived instance decls}
%*									*
%************************************************************************

map		:: (a -> b) -> [a] -> [b]
	-- this is up in the here-because-of-unfolding list

--??showChar	:: Char -> ShowS
showSpace	:: ShowS	-- non-std: == "showChar ' '"
showString	:: String -> ShowS
showParen	:: Bool -> ShowS -> ShowS

(++)		:: [a] -> [a] -> [a]
readParen	:: Bool -> ReadS a -> ReadS a
lex		:: ReadS String

%************************************************************************
%*									*
\subsection[PrelVals-void]{@void#@: Magic value of type @Void#@}
%*									*
%************************************************************************

I don't think this is available to the user; it's used in the
simplifier (WDP 94/06).
\begin{code}
voidPrimId
  = pcMiscPrelId voidPrimIdKey pRELUDE_BUILTIN SLIT("void#")
	voidPrimTy noIdInfo
\end{code}

%************************************************************************
%*									*
\subsection[PrelVals-runST]{@_runST@: Magic start-state-transformer function}
%*									*
%************************************************************************

@_runST@ has a non-Haskell-able type:
\begin{verbatim}
-- _runST :: forall a. (forall s. _ST s a) -> a
-- which is to say ::
--	     forall a. (forall s. (_State s -> (a, _State s))) -> a

_runST a m = case m _RealWorld (S# _RealWorld realWorld#) of
	       (r :: a, wild :: _State _RealWorld) -> r
\end{verbatim}
We unfold always, just for simplicity:
\begin{code}
runSTId
  = pcMiscPrelId runSTIdKey pRELUDE_BUILTIN SLIT("_runST") run_ST_ty id_info
  where
    s_tv = betaTyVar
    s	 = betaTy

    st_ty a = mkSigmaTy [s_tv] [] (mkStateTransformerTy s a)

    run_ST_ty
      = mkSigmaTy [alphaTyVar] [] (mkFunTys [st_ty alphaTy] alphaTy)
	    -- NB: rank-2 polymorphism! (forall inside the st_ty...)

    id_info
      = noIdInfo
	`addInfo` mkArityInfo 1
	`addInfo` mkStrictnessInfo [WwStrict] Nothing
	`addInfo` mkArgUsageInfo [ArgUsage 1]
	-- ABSOLUTELY NO UNFOLDING, e.g.: (mkUnfolding EssentialUnfolding run_ST_template)
	-- see example below
{- OUT:
    [m, t, r, wild]
      = mkTemplateLocals [
	{-m-} st_ty alphaTy,
	{-t-} realWorldStateTy,
	{-r-} alphaTy,
	{-_-} realWorldStateTy
	]

    run_ST_template
      = mkLam [alphaTyVar] [m] (
	    Let (NonRec t (Con stateDataCon [TyArg realWorldTy, VarArg realWorldPrimId])) (
	      Case (App (mkTyApp (Var m) [realWorldTy]) (VarArg t)) (
		AlgAlts
		  [(mkTupleCon 2, [r, wild], Var r)]
		  NoDefault)))
-}
\end{code}

SLPJ 95/04: Why @_runST@ must not have an unfolding; consider:
\begin{verbatim}
f x =
  _runST ( \ s -> let
		    (a, s')  = newArray# 100 [] s
		    (_, s'') = fill_in_array_or_something a x s'
		  in
		  freezeArray# a s'' )
\end{verbatim}
If we inline @_runST@, we'll get:
\begin{verbatim}
f x = let
	(a, s')  = newArray# 100 [] realWorld#{-NB-}
	(_, s'') = fill_in_array_or_something a x s'
      in
      freezeArray# a s''
\end{verbatim}
And now the @newArray#@ binding can be floated to become a CAF, which
is totally and utterly wrong:
\begin{verbatim}
f = let
    (a, s')  = newArray# 100 [] realWorld#{-NB-} -- YIKES!!!
    in
    \ x ->
	let (_, s'') = fill_in_array_or_something a x s' in
	freezeArray# a s''
\end{verbatim}
All calls to @f@ will share a {\em single} array!  End SLPJ 95/04.

@realWorld#@ used to be a magic literal, \tr{void#}.  If things get
nasty as-is, change it back to a literal (@Literal@).
\begin{code}
realWorldPrimId
  = pcMiscPrelId realWorldPrimIdKey pRELUDE_BUILTIN SLIT("realWorld#")
	realWorldStatePrimTy
	noIdInfo
\end{code}

%************************************************************************
%*									*
\subsection[PrelVals-foldr-build]{Values known about for ``foldr/build''}
%*									*
%************************************************************************

\begin{code}
buildId
  = pcMiscPrelId buildIdKey pRELUDE_CORE SLIT("_build") buildTy
	((((noIdInfo
		`addInfo_UF` mkMagicUnfolding buildIdKey)
		`addInfo` mkStrictnessInfo [WwStrict] Nothing)
		`addInfo` mkArgUsageInfo [ArgUsage 2])
		`addInfo` pcGenerateSpecs buildIdKey buildId noIdInfo{-ToDo-} buildTy)
	-- cheating, but since _build never actually exists ...
  where
    -- The type of this strange object is:
    --  \/ a . (\/ b . (a -> b -> b) -> b -> b) -> [a]

    buildTy = mkSigmaTy [alphaTyVar] [] (mkFunTys [build_ty] (mkListTy alphaTy))
	where
	    build_ty = mkSigmaTy [betaTyVar] []
			(mkFunTys [alphaTy, mkFunTys [betaTy] betaTy, betaTy] betaTy)
\end{code}

@mkBuild@ is sugar for building a build!

@mkbuild ty tv c n e@ $Rightarrow$ @build ty (/\ tv -> \ c n -> e)@
@ty@ is the type of the list.
@tv@ is always a new type variable.
@c,n@ are Id's for the abstract cons and nil, @g@ for let binding the argument argument.
	c :: a -> b -> b
	n :: b
	v :: (\/ b . (a -> b -> b) -> b -> b) -> [a]
--  \/ a .  (\/ b . (a -> b -> b) -> b -> b) -> [a]
@e@ is the object right inside the @build@

\begin{code}
mkBuild :: Type
	-> TyVar
	-> Id
	-> Id
	-> Id
	-> CoreExpr -- template
	-> CoreExpr -- template

mkBuild ty tv c n g expr
  = Let (NonRec g (mkLam [tv] [c,n] expr))
	(App (mkTyApp (Var buildId) [ty]) (VarArg g))
\end{code}

\begin{code}
augmentId
  = pcMiscPrelId augmentIdKey pRELUDE_CORE SLIT("_augment") augmentTy
	(((noIdInfo
		`addInfo_UF` mkMagicUnfolding augmentIdKey)
		`addInfo` mkStrictnessInfo [WwStrict,WwLazy False] Nothing)
		`addInfo` mkArgUsageInfo [ArgUsage 2,UnknownArgUsage])
	-- cheating, but since _augment never actually exists ...
  where
    -- The type of this strange object is:
    --  \/ a . (\/ b . (a -> b -> b) -> b -> b) -> [a] -> [a]

    augmentTy = mkSigmaTy [alphaTyVar] [] (mkFunTys [aug_ty, mkListTy alphaTy] (mkListTy alphaTy))
	where
	    aug_ty = mkSigmaTy [betaTyVar] []
			(mkFunTys [alphaTy, mkFunTys [betaTy] betaTy, betaTy] betaTy)
\end{code}

\begin{code}
foldrId = pcMiscPrelId foldrIdKey pRELUDE_FB{-not "List"-} SLIT("foldr")
		 foldrTy idInfo
  where
	foldrTy =
	  mkSigmaTy [alphaTyVar, betaTyVar] []
		(mkFunTys [alphaTy, mkFunTys [betaTy] betaTy, betaTy, mkListTy alphaTy] betaTy)

	idInfo = (((((noIdInfo
			`addInfo_UF` mkMagicUnfolding foldrIdKey)
			`addInfo` mkStrictnessInfo [WwLazy False,WwLazy False,WwStrict] Nothing)
			`addInfo` mkArityInfo 3)
			`addInfo` mkUpdateInfo [2,2,1])
			`addInfo` pcGenerateSpecs foldrIdKey foldrId noIdInfo{-ToDo-} foldrTy)

foldlId = pcMiscPrelId foldlIdKey pRELUDE_FB{-not "List"-} SLIT("foldl")
		 foldlTy idInfo
  where
	foldlTy =
	  mkSigmaTy [alphaTyVar, betaTyVar] []
		(mkFunTys [alphaTy, mkFunTys [betaTy] betaTy, alphaTy, mkListTy betaTy] alphaTy)

	idInfo = (((((noIdInfo
			`addInfo_UF` mkMagicUnfolding foldlIdKey)
			`addInfo` mkStrictnessInfo [WwLazy False,WwLazy False,WwStrict] Nothing)
			`addInfo` mkArityInfo 3)
			`addInfo` mkUpdateInfo [2,2,1])
			`addInfo` pcGenerateSpecs foldlIdKey foldlId noIdInfo{-ToDo-} foldlTy)

-- A bit of magic goes no here. We translate appendId into ++,
-- you have to be carefull when you actually compile append:
--	xs ++ ys = augment (\ c n -> foldr c n xs) ys
--		 {- unfold augment -}
--		 = foldr (:) ys xs
--		 {- fold foldr to append -}
--		 = ys `appendId` xs
--		 = ys ++ xs		-- ugg!
-- *BUT* you want (++) and not _append in your interfaces.
--
-- So you have to turn *off* unfolding of foldr inside FoldrBuild.hs inside
-- the prelude.
--

appendId
  = pcMiscPrelId appendIdKey pRELUDE_LIST SLIT("++") appendTy idInfo
  where
    appendTy =
      (mkSigmaTy [alphaTyVar] []
	    (mkFunTys [mkListTy alphaTy, mkListTy alphaTy] (mkListTy alphaTy)))
    idInfo = (((noIdInfo
		`addInfo` mkStrictnessInfo [WwStrict,WwLazy False] Nothing)
		`addInfo` mkArityInfo 2)
		`addInfo` mkUpdateInfo [1,2])
\end{code}

%************************************************************************
%*									*
\subsection[PrelUtils-specialisations]{Specialisations for builtin values}
%*									*
%************************************************************************

The specialisations which exist for the builtin values must be recorded in
their IdInfos.

NOTE: THE USES OF THE pcGenerate... FUNCTIONS MUST CORRESPOND
      TO THE SPECIALISATIONS DECLARED IN THE PRELUDE !!!

HACK: We currently use the same unique for the specialised Ids.

The list @specing_types@ determines the types for which specialised
versions are created. Note: This should correspond with the
types passed to the pre-processor with the -genSPECS arg (see ghc.lprl).

ToDo: Create single mkworld definition which is grabbed here and in ghc.lprl

\begin{code}
pcGenerateSpecs :: Unique -> Id -> IdInfo -> Type -> SpecEnv
pcGenerateSpecs key id info ty
  = nullSpecEnv

{- LATER:

pc_gen_specs True key id info ty

pc_gen_specs is_id key id info ty
 = mkSpecEnv spec_infos
 where
   spec_infos = [ let spec_ty = specialiseTy ty spec_tys 0
		      spec_id = if is_id
				then mkSpecId key {- HACK WARNING: same unique! -}
					      id spec_tys spec_ty info
				else panic "SpecData:SpecInfo:SpecId"
		  in
		  SpecInfo spec_tys (length ctxts) spec_id
		| spec_tys <- specialisations ]

   (tyvars, ctxts, _) = splitSigmaTy ty
   no_tyvars	      = length tyvars

   specialisations    = if no_tyvars == 0
			then []
			else tail (cross_product no_tyvars specing_types)

			-- N.B. tail removes fully polymorphic specialisation

cross_product 0 tys = []
cross_product 1 tys = map (:[]) tys
cross_product n tys = concat [map (:cp) tys | cp <- cross_product (n-1) tys]


specing_types = [Nothing,
		 Just charPrimTy,
		 Just doublePrimTy,
		 Just intPrimTy ]
-}
\end{code}
