%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[RnSource]{Main pass of renamer}

\begin{code}
module RnTypes ( rnHsType, rnLHsType, rnLHsTypes, rnContext,
		 rnHsSigType, rnHsTypeFVs,
		 rnLPat, rnPat, rnPatsAndThen,		-- Here because it's not part 
		 rnLit, rnOverLit, 			-- of any mutual recursion	
		 precParseErr, sectionPrecErr, dupFieldErr, patSigErr, checkTupSize
  ) where

import DynFlags		( DynFlag(Opt_WarnUnusedMatches, Opt_GlasgowExts) )

import HsSyn
import RdrHsSyn		( extractHsRhoRdrTyVars )
import RnHsSyn		( extractHsTyNames, parrTyCon_name, tupleTyCon_name, 
			  listTyCon_name
			)
import RnEnv		( lookupOccRn, lookupBndrRn, lookupSyntaxName,
			  lookupLocatedOccRn, lookupLocatedBndrRn,
			  lookupLocatedGlobalOccRn, bindTyVarsRn, lookupFixityRn,
			  mapFvRn, warnUnusedMatches,
			  newIPNameRn, bindPatSigTyVarsFV, bindLocatedLocalsFV )
import TcRnMonad
import RdrName		( RdrName, elemLocalRdrEnv )
import PrelNames	( eqClassName, integralClassName, geName, eqName,
		  	  negateName, minusName, lengthPName, indexPName,
			  plusIntegerName, fromIntegerName, timesIntegerName,
			  ratioDataConName, fromRationalName )
import TypeRep		( funTyCon )
import Constants	( mAX_TUPLE_SIZE )
import Name		( Name )
import SrcLoc		( SrcSpan, Located(..), unLoc, noLoc )
import NameSet

import Literal		( inIntRange, inCharRange )
import BasicTypes	( compareFixity, Fixity(..), FixityDirection(..) )
import ListSetOps	( removeDups )
import Outputable
import Monad		( when )

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
rnHsTypeFVs :: SDoc -> LHsType RdrName -> RnM (LHsType Name, FreeVars)
rnHsTypeFVs doc_str ty 
  = rnLHsType doc_str ty	`thenM` \ ty' ->
    returnM (ty', extractHsTyNames ty')

rnHsSigType :: SDoc -> LHsType RdrName -> RnM (LHsType Name)
	-- rnHsSigType is used for source-language type signatures,
	-- which use *implicit* universal quantification.
rnHsSigType doc_str ty
  = rnLHsType (text "In the type signature for" <+> doc_str) ty
\end{code}

rnHsType is here because we call it from loadInstDecl, and I didn't
want a gratuitous knot.

\begin{code}
rnLHsType  :: SDoc -> LHsType RdrName -> RnM (LHsType Name)
rnLHsType doc = wrapLocM (rnHsType doc)

rnHsType :: SDoc -> HsType RdrName -> RnM (HsType Name)

rnHsType doc (HsForAllTy Implicit _ ctxt ty)
	-- Implicit quantifiction in source code (no kinds on tyvars)
	-- Given the signature  C => T  we universally quantify 
	-- over FV(T) \ {in-scope-tyvars} 
  = getLocalRdrEnv		`thenM` \ name_env ->
    let
	mentioned = extractHsRhoRdrTyVars ctxt ty

	-- Don't quantify over type variables that are in scope;
	-- when GlasgowExts is off, there usually won't be any, except for
	-- class signatures:
	--	class C a where { op :: a -> a }
	forall_tyvars = filter (not . (`elemLocalRdrEnv` name_env) . unLoc) mentioned
	tyvar_bndrs = [ L loc (UserTyVar v) | (L loc v) <- forall_tyvars ]
    in
    rnForAll doc Implicit tyvar_bndrs ctxt ty

rnHsType doc (HsForAllTy Explicit forall_tyvars ctxt tau)
	-- Explicit quantification.
	-- Check that the forall'd tyvars are actually 
	-- mentioned in the type, and produce a warning if not
  = let
	mentioned	   = map unLoc (extractHsRhoRdrTyVars ctxt tau)
	forall_tyvar_names = hsLTyVarLocNames forall_tyvars

	-- Explicitly quantified but not mentioned in ctxt or tau
	warn_guys = filter ((`notElem` mentioned) . unLoc) forall_tyvar_names
    in
    mappM_ (forAllWarn doc tau) warn_guys	`thenM_`
    rnForAll doc Explicit forall_tyvars ctxt tau

rnHsType doc (HsTyVar tyvar)
  = lookupOccRn tyvar 		`thenM` \ tyvar' ->
    returnM (HsTyVar tyvar')

rnHsType doc (HsOpTy ty1 (L loc op) ty2)
  = setSrcSpan loc (
      lookupOccRn op			`thenM` \ op' ->
      let
	l_op' = L loc op'
      in
      lookupTyFixityRn l_op'		`thenM` \ fix ->
      rnLHsType doc ty1			`thenM` \ ty1' ->
      rnLHsType doc ty2			`thenM` \ ty2' -> 
      mkHsOpTyRn (\t1 t2 -> HsOpTy t1 l_op' t2) (ppr op') fix ty1' ty2'
   )

rnHsType doc (HsParTy ty)
  = rnLHsType doc ty 	        `thenM` \ ty' ->
    returnM (HsParTy ty')

rnHsType doc (HsBangTy b ty)
  = rnLHsType doc ty 	        `thenM` \ ty' ->
    returnM (HsBangTy b ty')

rnHsType doc (HsNumTy i)
  | i == 1    = returnM (HsNumTy i)
  | otherwise = addErr err_msg	`thenM_`  returnM (HsNumTy i)
  where
    err_msg = ptext SLIT("Only unit numeric type pattern is valid")
			   

rnHsType doc (HsFunTy ty1 ty2)
  = rnLHsType doc ty1	`thenM` \ ty1' ->
	-- Might find a for-all as the arg of a function type
    rnLHsType doc ty2	`thenM` \ ty2' ->
	-- Or as the result.  This happens when reading Prelude.hi
	-- when we find return :: forall m. Monad m -> forall a. a -> m a

	-- Check for fixity rearrangements
    mkHsOpTyRn HsFunTy (ppr funTyCon) funTyFixity ty1' ty2'

rnHsType doc (HsListTy ty)
  = rnLHsType doc ty				`thenM` \ ty' ->
    returnM (HsListTy ty')

rnHsType doc (HsKindSig ty k)
  = rnLHsType doc ty				`thenM` \ ty' ->
    returnM (HsKindSig ty' k)

rnHsType doc (HsPArrTy ty)
  = rnLHsType doc ty				`thenM` \ ty' ->
    returnM (HsPArrTy ty')

-- Unboxed tuples are allowed to have poly-typed arguments.  These
-- sometimes crop up as a result of CPR worker-wrappering dictionaries.
rnHsType doc (HsTupleTy tup_con tys)
  = mappM (rnLHsType doc) tys	  	`thenM` \ tys' ->
    returnM (HsTupleTy tup_con tys')

rnHsType doc (HsAppTy ty1 ty2)
  = rnLHsType doc ty1		`thenM` \ ty1' ->
    rnLHsType doc ty2		`thenM` \ ty2' ->
    returnM (HsAppTy ty1' ty2')

rnHsType doc (HsPredTy pred)
  = rnPred doc pred	`thenM` \ pred' ->
    returnM (HsPredTy pred')

rnLHsTypes doc tys = mappM (rnLHsType doc) tys
\end{code}


\begin{code}
rnForAll :: SDoc -> HsExplicitForAll -> [LHsTyVarBndr RdrName]
	 -> LHsContext RdrName -> LHsType RdrName -> RnM (HsType Name)

rnForAll doc exp [] (L _ []) (L _ ty) = rnHsType doc ty
	-- One reason for this case is that a type like Int#
	-- starts off as (HsForAllTy Nothing [] Int), in case
	-- there is some quantification.  Now that we have quantified
	-- and discovered there are no type variables, it's nicer to turn
	-- it into plain Int.  If it were Int# instead of Int, we'd actually
	-- get an error, because the body of a genuine for-all is
	-- of kind *.

rnForAll doc exp forall_tyvars ctxt ty
  = bindTyVarsRn doc forall_tyvars	$ \ new_tyvars ->
    rnContext doc ctxt			`thenM` \ new_ctxt ->
    rnLHsType doc ty			`thenM` \ new_ty ->
    returnM (HsForAllTy exp new_tyvars new_ctxt new_ty)
	-- Retain the same implicit/explicit flag as before
	-- so that we can later print it correctly
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
by the presence of ->, which is a separate syntactic construct.

\begin{code}
---------------
-- Building (ty1 `op1` (ty21 `op2` ty22))
mkHsOpTyRn :: (LHsType Name -> LHsType Name -> HsType Name)
	   -> SDoc -> Fixity -> LHsType Name -> LHsType Name 
	   -> RnM (HsType Name)

mkHsOpTyRn mk1 pp_op1 fix1 ty1 (L loc2 (HsOpTy ty21 op2 ty22))
  = do  { fix2 <- lookupTyFixityRn op2
	; mk_hs_op_ty mk1 pp_op1 fix1 ty1 
		      (\t1 t2 -> HsOpTy t1 op2 t2)
		      (ppr op2) fix2 ty21 ty22 loc2 }

mkHsOpTyRn mk1 pp_op1 fix1 ty1 ty2@(L loc2 (HsFunTy ty21 ty22))
  = mk_hs_op_ty mk1 pp_op1 fix1 ty1 
		HsFunTy (ppr funTyCon) funTyFixity ty21 ty22 loc2

mkHsOpTyRn mk1 pp_op1 fix1 ty1 ty2 		-- Default case, no rearrangment
  = return (mk1 ty1 ty2)

---------------
mk_hs_op_ty :: (LHsType Name -> LHsType Name -> HsType Name)
	    -> SDoc -> Fixity -> LHsType Name
	    -> (LHsType Name -> LHsType Name -> HsType Name)
	    -> SDoc -> Fixity -> LHsType Name -> LHsType Name -> SrcSpan
	    -> RnM (HsType Name)
mk_hs_op_ty mk1 pp_op1 fix1 ty1 
	    mk2 pp_op2 fix2 ty21 ty22 loc2
  | nofix_error     = do { addErr (precParseErr (quotes pp_op1,fix1) 
			 		        (quotes pp_op2,fix2))
		         ; return (mk1 ty1 (L loc2 (mk2 ty21 ty22))) }
  | associate_right = return (mk1 ty1 (L loc2 (mk2 ty21 ty22)))
  | otherwise	    = do { -- Rearrange to ((ty1 `op1` ty21) `op2` ty22)
			   new_ty <- mkHsOpTyRn mk1 pp_op1 fix1 ty1 ty21
			 ; return (mk2 (noLoc new_ty) ty22) }
  where
    (nofix_error, associate_right) = compareFixity fix1 fix2

---------------
lookupTyFixityRn (L loc n)
  = doptM Opt_GlasgowExts 			`thenM` \ glaExts ->
    when (not glaExts) 
    	(setSrcSpan loc $ addWarn (infixTyConWarn n))	`thenM_`
    lookupFixityRn n

---------------
funTyFixity = Fixity 0 InfixR	-- Fixity of '->'
\end{code}

%*********************************************************
%*							*
\subsection{Contexts and predicates}
%*							*
%*********************************************************

\begin{code}
rnContext :: SDoc -> LHsContext RdrName -> RnM (LHsContext Name)
rnContext doc = wrapLocM (rnContext' doc)

rnContext' :: SDoc -> HsContext RdrName -> RnM (HsContext Name)
rnContext' doc ctxt = mappM (rnLPred doc) ctxt

rnLPred :: SDoc -> LHsPred RdrName -> RnM (LHsPred Name)
rnLPred doc  = wrapLocM (rnPred doc)

rnPred doc (HsClassP clas tys)
  = lookupOccRn clas		`thenM` \ clas_name ->
    rnLHsTypes doc tys		`thenM` \ tys' ->
    returnM (HsClassP clas_name tys')

rnPred doc (HsIParam n ty)
  = newIPNameRn n		`thenM` \ name ->
    rnLHsType doc ty		`thenM` \ ty' ->
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
	      -> [LPat RdrName] 
	      -> ([LPat Name] -> RnM (a, FreeVars))
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
    bindLocatedLocalsFV doc_pat bndrs	$ \ new_bndrs ->
    rnLPats pats			`thenM` \ (pats', pat_fvs) ->
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
    bndrs 	= collectLocatedPatsBinders pats
    doc_pat     = ptext SLIT("In") <+> pprMatchContext ctxt

rnLPats :: [LPat RdrName] -> RnM ([LPat Name], FreeVars)
rnLPats ps = mapFvRn rnLPat ps

rnLPat :: LPat RdrName -> RnM (LPat Name, FreeVars)
rnLPat = wrapLocFstM rnPat

-- -----------------------------------------------------------------------------
-- rnPat

rnPat :: Pat RdrName -> RnM (Pat Name, FreeVars)

rnPat (WildPat _) = returnM (WildPat placeHolderType, emptyFVs)

rnPat (VarPat name)
  = lookupBndrRn  name			`thenM` \ vname ->
    returnM (VarPat vname, emptyFVs)

rnPat (SigPatIn pat ty)
  = doptM Opt_GlasgowExts `thenM` \ glaExts ->
    
    if glaExts
    then rnLPat pat		`thenM` \ (pat', fvs1) ->
         rnHsTypeFVs doc ty	`thenM` \ (ty',  fvs2) ->
         returnM (SigPatIn pat' ty', fvs1 `plusFV` fvs2)

    else addErr (patSigErr ty)	`thenM_`
         rnPat (unLoc pat) -- XXX shouldn't throw away the loc
  where
    doc = text "In a pattern type-signature"
    
rnPat (LitPat lit) 
  = rnLit lit 	`thenM_` 
    returnM (LitPat lit, emptyFVs) 

rnPat (NPat lit mb_neg eq _) 
  = rnOverLit lit			`thenM` \ (lit', fvs1) ->
    (case mb_neg of
	Nothing -> returnM (Nothing, emptyFVs)
	Just _  -> lookupSyntaxName negateName	`thenM` \ (neg, fvs) ->
		   returnM (Just neg, fvs)
    )					`thenM` \ (mb_neg', fvs2) ->
    lookupSyntaxName eqName		`thenM` \ (eq', fvs3) -> 
    returnM (NPat lit' mb_neg' eq' placeHolderType, 
	      fvs1 `plusFV` fvs2 `plusFV` fvs3 `addOneFV` eqClassName)	
	-- Needed to find equality on pattern

rnPat (NPlusKPat name lit _ _)
  = rnOverLit lit			`thenM` \ (lit', fvs1) ->
    lookupLocatedBndrRn name		`thenM` \ name' ->
    lookupSyntaxName minusName		`thenM` \ (minus, fvs2) ->
    lookupSyntaxName geName		`thenM` \ (ge, fvs3) ->
    returnM (NPlusKPat name' lit' ge minus,
	     fvs1 `plusFV` fvs2 `plusFV` fvs3 `addOneFV` integralClassName)
	-- The Report says that n+k patterns must be in Integral

rnPat (LazyPat pat)
  = rnLPat pat		`thenM` \ (pat', fvs) ->
    returnM (LazyPat pat', fvs)

rnPat (AsPat name pat)
  = rnLPat pat			`thenM` \ (pat', fvs) ->
    lookupLocatedBndrRn name	`thenM` \ vname ->
    returnM (AsPat vname pat', fvs)

rnPat (ConPatIn con stuff) = rnConPat con stuff


rnPat (ParPat pat)
  = rnLPat pat		`thenM` \ (pat', fvs) ->
    returnM (ParPat pat', fvs)

rnPat (ListPat pats _)
  = rnLPats pats			`thenM` \ (patslist, fvs) ->
    returnM (ListPat patslist placeHolderType, fvs `addOneFV` listTyCon_name)

rnPat (PArrPat pats _)
  = rnLPats pats			`thenM` \ (patslist, fvs) ->
    returnM (PArrPat patslist placeHolderType, 
	      fvs `plusFV` implicit_fvs `addOneFV` parrTyCon_name)
  where
    implicit_fvs = mkFVs [lengthPName, indexPName]

rnPat (TuplePat pats boxed)
  = checkTupSize tup_size	`thenM_`
    rnLPats pats			`thenM` \ (patslist, fvs) ->
    returnM (TuplePat patslist boxed, fvs `addOneFV` tycon_name)
  where
    tup_size   = length pats
    tycon_name = tupleTyCon_name boxed tup_size

rnPat (TypePat name) =
    rnHsTypeFVs (text "In a type pattern") name	`thenM` \ (name', fvs) ->
    returnM (TypePat name', fvs)

-- -----------------------------------------------------------------------------
-- rnConPat

rnConPat con (PrefixCon pats)
  = lookupLocatedOccRn con 	`thenM` \ con' ->
    rnLPats pats		`thenM` \ (pats', fvs) ->
    returnM (ConPatIn con' (PrefixCon pats'), fvs `addOneFV` unLoc con')

rnConPat con (RecCon rpats)
  = lookupLocatedOccRn con 	`thenM` \ con' ->
    rnRpats rpats		`thenM` \ (rpats', fvs) ->
    returnM (ConPatIn con' (RecCon rpats'), fvs `addOneFV` unLoc con')

rnConPat con (InfixCon pat1 pat2)
  = lookupLocatedOccRn con			`thenM` \ con' ->
    rnLPat pat1					`thenM` \ (pat1', fvs1) ->
    rnLPat pat2					`thenM` \ (pat2', fvs2) ->
    lookupFixityRn (unLoc con')			`thenM` \ fixity ->
    mkConOpPatRn con' fixity pat1' pat2'	`thenM` \ pat' ->
    returnM (pat', fvs1 `plusFV` fvs2 `addOneFV` unLoc con')

-- -----------------------------------------------------------------------------
-- rnRpats

rnRpats :: [(Located RdrName, LPat RdrName)]
        -> RnM ([(Located Name, LPat Name)], FreeVars)
rnRpats rpats
  = mappM_ field_dup_err dup_fields 	`thenM_`
    mapFvRn rn_rpat rpats		`thenM` \ (rpats', fvs) ->
    returnM (rpats', fvs)
  where
    (_, dup_fields) = removeDups compare [ unLoc f | (f,_) <- rpats ]

    field_dup_err dups = addErr (dupFieldErr "pattern" dups)

    rn_rpat (field, pat)
      = lookupLocatedGlobalOccRn field	`thenM` \ fieldname ->
	rnLPat pat			`thenM` \ (pat', fvs) ->
	returnM ((fieldname, pat'), fvs `addOneFV` unLoc fieldname)

-- -----------------------------------------------------------------------------
-- mkConOpPatRn

mkConOpPatRn :: Located Name -> Fixity -> LPat Name -> LPat Name
	     -> RnM (Pat Name)

mkConOpPatRn op2 fix2 p1@(L loc (ConPatIn op1 (InfixCon p11 p12))) p2
  = lookupFixityRn (unLoc op1)	`thenM` \ fix1 ->
    let
	(nofix_error, associate_right) = compareFixity fix1 fix2
    in
    if nofix_error then
	addErr (precParseErr (ppr_op op1,fix1) (ppr_op op2,fix2))	`thenM_`
	returnM (ConPatIn op2 (InfixCon p1 p2))
    else 
    if associate_right then
	mkConOpPatRn op2 fix2 p12 p2		`thenM` \ new_p ->
	returnM (ConPatIn op1 (InfixCon p11 (L loc new_p)))  -- XXX loc right?
    else
    returnM (ConPatIn op2 (InfixCon p1 p2))

mkConOpPatRn op fix p1 p2 			-- Default case, no rearrangment
  = ASSERT( not_op_pat (unLoc p2) )
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
rnLit :: HsLit -> RnM ()
rnLit (HsChar c) = checkErr (inCharRange c) (bogusCharError c)
rnLit other	 = returnM ()

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

forAllWarn doc ty (L loc tyvar)
  = ifOptM Opt_WarnUnusedMatches 	$
    setSrcSpan loc $
    addWarn (sep [ptext SLIT("The universally quantified type variable") <+> quotes (ppr tyvar),
		   nest 4 (ptext SLIT("does not appear in the type") <+> quotes (ppr ty))]
		   $$
		   doc
                )

bogusCharError c
  = ptext SLIT("character literal out of range: '\\") <> char c  <> char '\''

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
  = vcat [ftext FSLIT("Accepting non-standard infix type constructor") <+> quotes (ppr op),
	  ftext FSLIT("Use -fglasgow-exts to avoid this warning")]

patSigErr ty
  =  (ptext SLIT("Illegal signature in pattern:") <+> ppr ty)
	$$ nest 4 (ptext SLIT("Use -fglasgow-exts to permit it"))

dupFieldErr str dup
  = hsep [ptext SLIT("duplicate field name"), 
          quotes (ppr dup),
	  ptext SLIT("in record"), text str]

ppr_op op = quotes (ppr op)	-- Here, op can be a Name or a (Var n), where n is a Name
ppr_opfix (pp_op, fixity) = pp_op <+> brackets (ppr fixity)
\end{code}
