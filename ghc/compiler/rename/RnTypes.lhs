%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[RnSource]{Main pass of renamer}

\begin{code}
module RnTypes ( rnHsType, rnContext, 
		 rnHsSigType, rnHsTypeFVs, rnHsSigTypeFVs, 
		 rnPat, rnPats, rnPatsAndThen,	-- Here because it's not part 
		 rnOverLit, litFVs,		-- of any mutual recursion	
		 precParseErr, sectionPrecErr, dupFieldErr, patSigErr, checkTupSize
  ) where

import CmdLineOpts	( DynFlag(Opt_WarnMisc, Opt_WarnUnusedMatches, Opt_GlasgowExts) )

import HsSyn
import RdrHsSyn	( RdrNameContext, RdrNameHsType, RdrNamePat,
		  extractHsTyRdrTyVars, extractHsCtxtRdrTyVars )
import RnHsSyn	( RenamedContext, RenamedHsType, RenamedPat,
		  extractHsTyNames, 
		  parrTyCon_name, tupleTyCon_name, listTyCon_name, charTyCon_name )
import RnEnv	( lookupOccRn, lookupBndrRn, lookupSyntaxName, lookupGlobalOccRn,
		  newIPName, bindTyVarsRn, lookupFixityRn, mapFvRn,
		  bindPatSigTyVarsFV, bindLocalsFV, warnUnusedMatches )
import TcRnMonad

import PrelNames( eqStringName, eqClassName, integralClassName, 
		  negateName, minusName, lengthPName, indexPName, plusIntegerName, fromIntegerName,
		  timesIntegerName, ratioDataConName, fromRationalName )
import Constants	( mAX_TUPLE_SIZE )
import TysWiredIn	( intTyCon )
import TysPrim		( charPrimTyCon, addrPrimTyCon, intPrimTyCon, 
			  floatPrimTyCon, doublePrimTyCon )
import RdrName	( elemRdrEnv )
import Name	( Name, NamedThing(..) )
import NameSet
import Unique	( Uniquable(..) )

import Literal		( inIntRange, inCharRange )
import BasicTypes	( compareFixity, arrowFixity )
import List		( nub )
import ListSetOps	( removeDupsEq, removeDups )
import Outputable

#include "HsVersions.h"
\end{code}

These type renamers are in a separate module, rather than in (say) RnSource,
to break several loop.

%*********************************************************
%*							*
\subsection{Renaming types}
%*							*
%*********************************************************

\begin{code}
rnHsTypeFVs :: SDoc -> RdrNameHsType -> RnM (RenamedHsType, FreeVars)
rnHsTypeFVs doc_str ty 
  = rnHsType doc_str ty		`thenM` \ ty' ->
    returnM (ty', extractHsTyNames ty')

rnHsSigTypeFVs :: SDoc -> RdrNameHsType -> RnM (RenamedHsType, FreeVars)
rnHsSigTypeFVs doc_str ty
  = rnHsSigType doc_str ty	`thenM` \ ty' ->
    returnM (ty', extractHsTyNames ty')

rnHsSigType :: SDoc -> RdrNameHsType -> RnM RenamedHsType
	-- rnHsSigType is used for source-language type signatures,
	-- which use *implicit* universal quantification.
rnHsSigType doc_str ty
  = rnHsType (text "In the type signature for" <+> doc_str) ty
\end{code}

rnHsType is here because we call it from loadInstDecl, and I didn't
want a gratuitous knot.

\begin{code}
rnHsType :: SDoc -> RdrNameHsType -> RnM RenamedHsType

rnHsType doc (HsForAllTy Nothing ctxt ty)
	-- Implicit quantifiction in source code (no kinds on tyvars)
	-- Given the signature  C => T  we universally quantify 
	-- over FV(T) \ {in-scope-tyvars} 
  = getLocalRdrEnv		`thenM` \ name_env ->
    let
	mentioned_in_tau  = extractHsTyRdrTyVars ty
	mentioned_in_ctxt = extractHsCtxtRdrTyVars ctxt
	mentioned	  = nub (mentioned_in_tau ++ mentioned_in_ctxt)

	-- Don't quantify over type variables that are in scope;
	-- when GlasgowExts is off, there usually won't be any, except for
	-- class signatures:
	--	class C a where { op :: a -> a }
	forall_tyvars = filter (not . (`elemRdrEnv` name_env)) mentioned
    in
    rnForAll doc (map UserTyVar forall_tyvars) ctxt ty

rnHsType doc (HsForAllTy (Just forall_tyvars) ctxt tau)
	-- Explicit quantification.
	-- Check that the forall'd tyvars are actually 
	-- mentioned in the type, and produce a warning if not
  = let
	mentioned_in_tau		= extractHsTyRdrTyVars tau
	mentioned_in_ctxt		= extractHsCtxtRdrTyVars ctxt
	mentioned			= nub (mentioned_in_tau ++ mentioned_in_ctxt)
	forall_tyvar_names		= hsTyVarNames forall_tyvars

	-- Explicitly quantified but not mentioned in ctxt or tau
	warn_guys			= filter (`notElem` mentioned) forall_tyvar_names
    in
    mappM_ (forAllWarn doc tau) warn_guys	`thenM_`
    rnForAll doc forall_tyvars ctxt tau

rnHsType doc (HsTyVar tyvar)
  = lookupOccRn tyvar 		`thenM` \ tyvar' ->
    returnM (HsTyVar tyvar')

rnHsType doc (HsOpTy ty1 op ty2)
  = (case op of
	HsArrow  -> returnM HsArrow
	HsTyOp n -> lookupOccRn n    `thenM` \ n' ->
		    returnM (HsTyOp n')
    )				`thenM` \ op' ->
    rnHsType doc ty1		`thenM` \ ty1' ->
    rnHsType doc ty2		`thenM` \ ty2' -> 
    lookupTyFixityRn op'	`thenM` \ fix ->
    mkHsOpTyRn op' fix ty1' ty2'

rnHsType doc (HsParTy ty)
  = rnHsType doc ty 	        `thenM` \ ty' ->
    returnM (HsParTy ty')

rnHsType doc (HsNumTy i)
  | i == 1    = returnM (HsNumTy i)
  | otherwise = addErr err_msg	`thenM_`  returnM (HsNumTy i)
  where
    err_msg = ptext SLIT("Only unit numeric type pattern is valid")
			   

rnHsType doc (HsFunTy ty1 ty2)
  = rnHsType doc ty1	`thenM` \ ty1' ->
	-- Might find a for-all as the arg of a function type
    rnHsType doc ty2	`thenM` \ ty2' ->
	-- Or as the result.  This happens when reading Prelude.hi
	-- when we find return :: forall m. Monad m -> forall a. a -> m a
    returnM (HsFunTy ty1' ty2')

rnHsType doc (HsListTy ty)
  = rnHsType doc ty				`thenM` \ ty' ->
    returnM (HsListTy ty')

rnHsType doc (HsKindSig ty k)
  = rnHsType doc ty				`thenM` \ ty' ->
    returnM (HsKindSig ty' k)

rnHsType doc (HsPArrTy ty)
  = rnHsType doc ty				`thenM` \ ty' ->
    returnM (HsPArrTy ty')

-- Unboxed tuples are allowed to have poly-typed arguments.  These
-- sometimes crop up as a result of CPR worker-wrappering dictionaries.
rnHsType doc (HsTupleTy tup_con tys)
  = mappM (rnHsType doc) tys	  	`thenM` \ tys' ->
    returnM (HsTupleTy tup_con tys')

rnHsType doc (HsAppTy ty1 ty2)
  = rnHsType doc ty1		`thenM` \ ty1' ->
    rnHsType doc ty2		`thenM` \ ty2' ->
    returnM (HsAppTy ty1' ty2')

rnHsType doc (HsPredTy pred)
  = rnPred doc pred	`thenM` \ pred' ->
    returnM (HsPredTy pred')

rnHsTypes doc tys = mappM (rnHsType doc) tys
\end{code}


\begin{code}
rnForAll doc forall_tyvars ctxt ty
  = bindTyVarsRn doc forall_tyvars	$ \ new_tyvars ->
    rnContext doc ctxt			`thenM` \ new_ctxt ->
    rnHsType doc ty			`thenM` \ new_ty ->
    returnM (mkHsForAllTy (Just new_tyvars) new_ctxt new_ty)
\end{code}


%*********************************************************
%*							*
\subsection{Fixities}
%*							*
%*********************************************************

Infix types are read in a *right-associative* way, so that
	a `op` b `op` c
is always read in as
	a `op` (b `op` c)

mkHsOpTyRn rearranges where necessary.  The two arguments
have already been renamed and rearranged.  It's made rather tiresome
by the presence of ->

\begin{code}
lookupTyFixityRn HsArrow    = returnM arrowFixity
lookupTyFixityRn (HsTyOp n) 
  = doptM Opt_GlasgowExts 			`thenM` \ glaExts ->
    warnIf (not glaExts) (infixTyConWarn n)	`thenM_`
    lookupFixityRn n

-- Building (ty1 `op1` (ty21 `op2` ty22))
mkHsOpTyRn :: HsTyOp Name -> Fixity 
	   -> RenamedHsType -> RenamedHsType 
	   -> RnM RenamedHsType

mkHsOpTyRn op1 fix1 ty1 ty2@(HsOpTy ty21 op2 ty22)
  = lookupTyFixityRn op2		`thenM` \ fix2 ->
    let
	(nofix_error, associate_right) = compareFixity fix1 fix2
    in
    if nofix_error then
	addErr (precParseErr (quotes (ppr op1),fix1) 
			       (quotes (ppr op2),fix2))	`thenM_`
	returnM (HsOpTy ty1 op1 ty2)
    else 
    if not associate_right then
	-- Rearrange to ((ty1 `op1` ty21) `op2` ty22)
	mkHsOpTyRn op1 fix1 ty1 ty21		`thenM` \ new_ty ->
	returnM (HsOpTy new_ty op2 ty22)
    else
    returnM (HsOpTy ty1 op1 ty2)

mkHsOpTyRn op fix ty1 ty2 			-- Default case, no rearrangment
  = returnM (HsOpTy ty1 op ty2)

mkHsFunTyRn ty1 ty2			-- Precedence of function arrow is 0
  = returnM (HsFunTy ty1 ty2)		-- so no rearrangement reqd.  Change
					-- this if fixity of -> increases.

not_op_ty (HsOpTy _ _ _) = False
not_op_ty other   	 = True
\end{code}

%*********************************************************
%*							*
\subsection{Contexts and predicates}
%*							*
%*********************************************************

\begin{code}
rnContext :: SDoc -> RdrNameContext -> RnM RenamedContext
rnContext doc ctxt
  = mappM rn_pred ctxt		`thenM` \ theta ->

	-- Check for duplicate assertions
	-- If this isn't an error, then it ought to be:
    ifOptM Opt_WarnMisc (
        let
	    (_, dups) = removeDupsEq theta
		-- We only have equality, not ordering
        in
        mappM_ (addWarn . dupClassAssertWarn theta) dups
    )				`thenM_`

    returnM theta
  where
    rn_pred pred = rnPred doc pred				`thenM` \ pred'->
		   returnM pred'


rnPred doc (HsClassP clas tys)
  = lookupOccRn clas		`thenM` \ clas_name ->
    rnHsTypes doc tys		`thenM` \ tys' ->
    returnM (HsClassP clas_name tys')

rnPred doc (HsIParam n ty)
  = newIPName n			`thenM` \ name ->
    rnHsType doc ty		`thenM` \ ty' ->
    returnM (HsIParam name ty')
\end{code}


*********************************************************
*							*
\subsection{Patterns}
*							*
*********************************************************

\begin{code}
rnPatsAndThen :: HsMatchContext Name
	      -> Bool
	      -> [RdrNamePat] 
	      -> ([RenamedPat] -> RnM (a, FreeVars))
	      -> RnM (a, FreeVars)
-- Bring into scope all the binders and type variables
-- bound by the patterns; then rename the patterns; then
-- do the thing inside.
--
-- Note that we do a single bindLocalsRn for all the
-- matches together, so that we spot the repeated variable in
--	f x x = 1

rnPatsAndThen ctxt repUnused pats thing_inside
  = bindPatSigTyVarsFV pat_sig_tys 	$
    bindLocalsFV doc_pat bndrs		$ \ new_bndrs ->
    rnPats pats				`thenM` \ (pats', pat_fvs) ->
    thing_inside pats'			`thenM` \ (res, res_fvs) ->

    let
	unused_binders = filter (not . (`elemNameSet` res_fvs)) new_bndrs
    in
    (if repUnused
     then warnUnusedMatches unused_binders
     else returnM ())                  `thenM_`
    returnM (res, res_fvs `plusFV` pat_fvs)
  where
    pat_sig_tys = collectSigTysFromPats pats
    bndrs 	= collectPatsBinders    pats
    doc_pat     = ptext SLIT("In") <+> pprMatchContext ctxt

rnPats :: [RdrNamePat] -> RnM ([RenamedPat], FreeVars)
rnPats ps = mapFvRn rnPat ps

rnPat :: RdrNamePat -> RnM (RenamedPat, FreeVars)

rnPat (WildPat _) = returnM (WildPat placeHolderType, emptyFVs)

rnPat (VarPat name)
  = lookupBndrRn  name			`thenM` \ vname ->
    returnM (VarPat vname, emptyFVs)

rnPat (SigPatIn pat ty)
  = doptM Opt_GlasgowExts `thenM` \ glaExts ->
    
    if glaExts
    then rnPat pat		`thenM` \ (pat', fvs1) ->
         rnHsTypeFVs doc ty	`thenM` \ (ty',  fvs2) ->
         returnM (SigPatIn pat' ty', fvs1 `plusFV` fvs2)

    else addErr (patSigErr ty)	`thenM_`
         rnPat pat
  where
    doc = text "In a pattern type-signature"
    
rnPat (LitPat s@(HsString _)) 
  = returnM (LitPat s, unitFV eqStringName)

rnPat (LitPat lit) 
  = litFVs lit		`thenM` \ fvs ->
    returnM (LitPat lit, fvs) 

rnPat (NPatIn lit mb_neg) 
  = rnOverLit lit			`thenM` \ (lit', fvs1) ->
    (case mb_neg of
	Nothing -> returnM (Nothing, emptyFVs)
	Just _  -> lookupSyntaxName negateName	`thenM` \ (neg, fvs) ->
		   returnM (Just neg, fvs)
    )					`thenM` \ (mb_neg', fvs2) ->
    returnM (NPatIn lit' mb_neg', 
	      fvs1 `plusFV` fvs2 `addOneFV` eqClassName)	
	-- Needed to find equality on pattern

rnPat (NPlusKPatIn name lit _)
  = rnOverLit lit			`thenM` \ (lit', fvs1) ->
    lookupBndrRn name			`thenM` \ name' ->
    lookupSyntaxName minusName		`thenM` \ (minus, fvs2) ->
    returnM (NPlusKPatIn name' lit' minus, 
	      fvs1 `plusFV` fvs2 `addOneFV` integralClassName)
	-- The Report says that n+k patterns must be in Integral

rnPat (LazyPat pat)
  = rnPat pat		`thenM` \ (pat', fvs) ->
    returnM (LazyPat pat', fvs)

rnPat (AsPat name pat)
  = rnPat pat		`thenM` \ (pat', fvs) ->
    lookupBndrRn name	`thenM` \ vname ->
    returnM (AsPat vname pat', fvs)

rnPat (ConPatIn con stuff) = rnConPat con stuff


rnPat (ParPat pat)
  = rnPat pat		`thenM` \ (pat', fvs) ->
    returnM (ParPat pat', fvs)

rnPat (ListPat pats _)
  = rnPats pats			`thenM` \ (patslist, fvs) ->
    returnM (ListPat patslist placeHolderType, fvs `addOneFV` listTyCon_name)

rnPat (PArrPat pats _)
  = rnPats pats			`thenM` \ (patslist, fvs) ->
    returnM (PArrPat patslist placeHolderType, 
	      fvs `plusFV` implicit_fvs `addOneFV` parrTyCon_name)
  where
    implicit_fvs = mkFVs [lengthPName, indexPName]

rnPat (TuplePat pats boxed)
  = checkTupSize tup_size	`thenM_`
    rnPats pats			`thenM` \ (patslist, fvs) ->
    returnM (TuplePat patslist boxed, fvs `addOneFV` tycon_name)
  where
    tup_size   = length pats
    tycon_name = tupleTyCon_name boxed tup_size

rnPat (TypePat name) =
    rnHsTypeFVs (text "In a type pattern") name	`thenM` \ (name', fvs) ->
    returnM (TypePat name', fvs)

------------------------------
rnConPat con (PrefixCon pats)
  = lookupOccRn con 	`thenM` \ con' ->
    rnPats pats		`thenM` \ (pats', fvs) ->
    returnM (ConPatIn con' (PrefixCon pats'), fvs `addOneFV` con')

rnConPat con (RecCon rpats)
  = lookupOccRn con 	`thenM` \ con' ->
    rnRpats rpats	`thenM` \ (rpats', fvs) ->
    returnM (ConPatIn con' (RecCon rpats'), fvs `addOneFV` con')

rnConPat con (InfixCon pat1 pat2)
  = lookupOccRn con 	`thenM` \ con' ->
    rnPat pat1		`thenM` \ (pat1', fvs1) ->
    rnPat pat2		`thenM` \ (pat2', fvs2) ->

    getModeRn		`thenM` \ mode ->
	-- See comments with rnExpr (OpApp ...)
    (if isInterfaceMode mode
	then returnM (ConPatIn con' (InfixCon pat1' pat2'))
	else lookupFixityRn con'	`thenM` \ fixity ->
	     mkConOpPatRn con' fixity pat1' pat2'
    )							`thenM` \ pat' ->
    returnM (pat', fvs1 `plusFV` fvs2 `addOneFV` con')

------------------------
rnRpats rpats
  = mappM_ field_dup_err dup_fields 	`thenM_`
    mapFvRn rn_rpat rpats		`thenM` \ (rpats', fvs) ->
    returnM (rpats', fvs)
  where
    (_, dup_fields) = removeDups compare [ f | (f,_) <- rpats ]

    field_dup_err dups = addErr (dupFieldErr "pattern" dups)

    rn_rpat (field, pat)
      = lookupGlobalOccRn field	`thenM` \ fieldname ->
	rnPat pat		`thenM` \ (pat', fvs) ->
	returnM ((fieldname, pat'), fvs `addOneFV` fieldname)
\end{code}

\begin{code}
mkConOpPatRn :: Name -> Fixity -> RenamedPat -> RenamedPat
	     -> RnM RenamedPat

mkConOpPatRn op2 fix2 p1@(ConPatIn op1 (InfixCon p11 p12)) p2
  = lookupFixityRn op1		`thenM` \ fix1 ->
    let
	(nofix_error, associate_right) = compareFixity fix1 fix2
    in
    if nofix_error then
	addErr (precParseErr (ppr_op op1,fix1) (ppr_op op2,fix2))	`thenM_`
	returnM (ConPatIn op2 (InfixCon p1 p2))
    else 
    if associate_right then
	mkConOpPatRn op2 fix2 p12 p2		`thenM` \ new_p ->
	returnM (ConPatIn op1 (InfixCon p11 new_p))
    else
    returnM (ConPatIn op2 (InfixCon p1 p2))

mkConOpPatRn op fix p1 p2 			-- Default case, no rearrangment
  = ASSERT( not_op_pat p2 )
    returnM (ConPatIn op (InfixCon p1 p2))

not_op_pat (ConPatIn _ (InfixCon _ _)) = False
not_op_pat other   	               = True
\end{code}


%************************************************************************
%*									*
\subsubsection{Literals}
%*									*
%************************************************************************

When literals occur we have to make sure
that the types and classes they involve
are made available.

\begin{code}
litFVs (HsChar c)
   = checkErr (inCharRange c) (bogusCharError c) `thenM_`
     returnM (unitFV charTyCon_name)

litFVs (HsCharPrim c)         = returnM (unitFV (getName charPrimTyCon))
litFVs (HsString s)           = returnM (mkFVs [listTyCon_name, charTyCon_name])
litFVs (HsStringPrim s)       = returnM (unitFV (getName addrPrimTyCon))
litFVs (HsInt i)	      = returnM (unitFV (getName intTyCon))
litFVs (HsIntPrim i)          = returnM (unitFV (getName intPrimTyCon))
litFVs (HsFloatPrim f)        = returnM (unitFV (getName floatPrimTyCon))
litFVs (HsDoublePrim d)       = returnM (unitFV (getName doublePrimTyCon))
litFVs lit		      = pprPanic "RnExpr.litFVs" (ppr lit)	-- HsInteger and HsRat only appear 
									-- in post-typechecker translations
bogusCharError c
  = ptext SLIT("character literal out of range: '\\") <> int c <> char '\''

rnOverLit (HsIntegral i _)
  = lookupSyntaxName fromIntegerName	`thenM` \ (from_integer_name, fvs) ->
    if inIntRange i then
	returnM (HsIntegral i from_integer_name, fvs)
    else let
	extra_fvs = mkFVs [plusIntegerName, timesIntegerName]
	-- Big integer literals are built, using + and *, 
	-- out of small integers (DsUtils.mkIntegerLit)
	-- [NB: plusInteger, timesInteger aren't rebindable... 
	--	they are used to construct the argument to fromInteger, 
	--	which is the rebindable one.]
    in
    returnM (HsIntegral i from_integer_name, fvs `plusFV` extra_fvs)

rnOverLit (HsFractional i _)
  = lookupSyntaxName fromRationalName		`thenM` \ (from_rat_name, fvs) ->
    let
	extra_fvs = mkFVs [ratioDataConName, plusIntegerName, timesIntegerName]
	-- We have to make sure that the Ratio type is imported with
	-- its constructor, because literals of type Ratio t are
	-- built with that constructor.
	-- The Rational type is needed too, but that will come in
	-- as part of the type for fromRational.
	-- The plus/times integer operations may be needed to construct the numerator
	-- and denominator (see DsUtils.mkIntegerLit)
    in
    returnM (HsFractional i from_rat_name, fvs `plusFV` extra_fvs)
\end{code}



%*********************************************************
%*							*
\subsection{Errors}
%*							*
%*********************************************************

\begin{code}
checkTupSize :: Int -> RnM ()
checkTupSize tup_size
  | tup_size <= mAX_TUPLE_SIZE 
  = returnM ()
  | otherwise		       
  = addErr (sep [ptext SLIT("A") <+> int tup_size <> ptext SLIT("-tuple is too large for GHC"),
		 nest 2 (parens (ptext SLIT("max size is") <+> int mAX_TUPLE_SIZE)),
		 nest 2 (ptext SLIT("Workaround: use nested tuples or define a data type"))])

forAllWarn doc ty tyvar
  = ifOptM Opt_WarnUnusedMatches 	$
    getModeRn				`thenM` \ mode ->
    case mode of {
#ifndef DEBUG
	     InterfaceMode _ -> returnM () ; -- Don't warn of unused tyvars in interface files
		                            -- unless DEBUG is on, in which case it is slightly
					    -- informative.  They can arise from mkRhsTyLam,
					    -- leading to (say) 	f :: forall a b. [b] -> [b]
#endif
	     other ->
		addWarn (
		   sep [ptext SLIT("The universally quantified type variable") <+> quotes (ppr tyvar),
		   nest 4 (ptext SLIT("does not appear in the type") <+> quotes (ppr ty))]
		   $$
		   doc
                )
          }

dupClassAssertWarn ctxt (assertion : dups)
  = sep [hsep [ptext SLIT("Duplicate class assertion"), 
	       quotes (ppr assertion),
	       ptext SLIT("in the context:")],
	 nest 4 (pprHsContext ctxt <+> ptext SLIT("..."))]

naughtyCCallContextErr (HsClassP clas _)
  = sep [ptext SLIT("Can't use class") <+> quotes (ppr clas), 
	 ptext SLIT("in a context")]

precParseErr op1 op2 
  = hang (ptext SLIT("precedence parsing error"))
      4 (hsep [ptext SLIT("cannot mix"), ppr_opfix op1, ptext SLIT("and"), 
	       ppr_opfix op2,
	       ptext SLIT("in the same infix expression")])

sectionPrecErr op arg_op section
 = vcat [ptext SLIT("The operator") <+> ppr_opfix op <+> ptext SLIT("of a section"),
	 nest 4 (ptext SLIT("must have lower precedence than the operand") <+> ppr_opfix arg_op),
	 nest 4 (ptext SLIT("in the section:") <+> quotes (ppr section))]

infixTyConWarn op
  = ftext FSLIT("Accepting non-standard infix type constructor") <+> quotes (ppr op)

patSigErr ty
  =  (ptext SLIT("Illegal signature in pattern:") <+> ppr ty)
	$$ nest 4 (ptext SLIT("Use -fglasgow-exts to permit it"))

dupFieldErr str (dup:rest)
  = hsep [ptext SLIT("duplicate field name"), 
          quotes (ppr dup),
	  ptext SLIT("in record"), text str]

ppr_op op = quotes (ppr op)	-- Here, op can be a Name or a (Var n), where n is a Name
ppr_opfix (pp_op, fixity) = pp_op <+> brackets (ppr fixity)
\end{code}
