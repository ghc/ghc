%
% (c) The University of Glasgow, 1996-2003

Functions over HsSyn specialised to RdrName.

\begin{code}
module RdrHsSyn (
	extractHsTyRdrTyVars, 
	extractHsRhoRdrTyVars, extractGenericPatTyVars,
 
	mkHsOpApp, 
	mkHsIntegral, mkHsFractional, mkHsIsString,
	mkHsDo, mkHsSplice, mkTopSpliceDecl,
        mkClassDecl, mkTyData, mkTyFamily, mkTySynonym,
        splitCon, mkInlinePragma,	
	mkRecConstrOrUpdate, -- HsExp -> [HsFieldUpdate] -> P HsExp

	cvBindGroup,
        cvBindsAndSigs,
	cvTopDecls,
        placeHolderPunRhs,

	-- Stuff to do with Foreign declarations
	mkImport,
        parseCImport,
	mkExport,
	mkExtName,           -- RdrName -> CLabelString
	mkGadtDecl,          -- [Located RdrName] -> LHsType RdrName -> ConDecl RdrName
	mkSimpleConDecl, 
	mkDeprecatedGadtRecordDecl,

	-- Bunch of functions in the parser monad for 
	-- checking and constructing values
	checkPrecP, 	      -- Int -> P Int
	checkContext,	      -- HsType -> P HsContext
	checkPred,	      -- HsType -> P HsPred
	checkTyVars,          -- [LHsType RdrName] -> P ()
	checkKindSigs,	      -- [LTyClDecl RdrName] -> P ()
	checkInstType,	      -- HsType -> P HsType
	checkPattern,	      -- HsExp -> P HsPat
	bang_RDR,
	checkPatterns,	      -- SrcLoc -> [HsExp] -> P [HsPat]
	checkDo,	      -- [Stmt] -> P [Stmt]
	checkMDo,	      -- [Stmt] -> P [Stmt]
	checkValDef,	      -- (SrcLoc, HsExp, HsRhs, [HsDecl]) -> P HsDecl
	checkValSig,	      -- (SrcLoc, HsExp, HsRhs, [HsDecl]) -> P HsDecl
	checkDoAndIfThenElse,
	parseError,	    
	parseErrorSDoc,	    
    ) where

import HsSyn		-- Lots of it
import Class            ( FunDep )
import TypeRep          ( Kind )
import RdrName		( RdrName, isRdrTyVar, isRdrTc, mkUnqual, rdrNameOcc, 
			  isRdrDataCon, isUnqual, getRdrName, setRdrNameSpace )
import BasicTypes	( maxPrecedence, Activation(..), RuleMatchInfo,
                          InlinePragma(..) )
import Lexer
import TysWiredIn	( unitTyCon ) 
import ForeignCall
import OccName  	( srcDataName, varName, isDataOcc, isTcOcc, 
			  occNameString )
import PrelNames	( forall_tv_RDR )
import DynFlags
import SrcLoc
import OrdList		( OrdList, fromOL )
import Bag		( Bag, emptyBag, consBag, foldrBag )
import Outputable
import FastString
import Maybes

import Control.Applicative ((<$>))       
import Control.Monad
import Text.ParserCombinators.ReadP as ReadP
import Data.List        ( nubBy )
import Data.Char

#include "HsVersions.h"
\end{code}


%************************************************************************
%*									*
\subsection{A few functions over HsSyn at RdrName}
%*                                                                    *
%************************************************************************

extractHsTyRdrNames finds the free variables of a HsType
It's used when making the for-alls explicit.

\begin{code}
extractHsTyRdrTyVars :: LHsType RdrName -> [Located RdrName]
extractHsTyRdrTyVars ty = nubBy eqLocated (extract_lty ty [])

extractHsTysRdrTyVars :: [LHsType RdrName] -> [Located RdrName]
extractHsTysRdrTyVars ty = nubBy eqLocated (extract_ltys ty [])

extractHsRhoRdrTyVars :: LHsContext RdrName -> LHsType RdrName -> [Located RdrName]
-- This one takes the context and tau-part of a 
-- sigma type and returns their free type variables
extractHsRhoRdrTyVars ctxt ty 
 = nubBy eqLocated $ extract_lctxt ctxt (extract_lty ty [])

extract_lctxt :: Located [LHsPred RdrName] -> [Located RdrName] -> [Located RdrName]
extract_lctxt ctxt acc = foldr (extract_pred . unLoc) acc (unLoc ctxt)

extract_pred :: HsPred RdrName -> [Located RdrName] -> [Located RdrName]
extract_pred (HsClassP _   tys) acc = extract_ltys tys acc
extract_pred (HsEqualP ty1 ty2) acc = extract_lty ty1 (extract_lty ty2 acc)
extract_pred (HsIParam _   ty ) acc = extract_lty ty acc

extract_ltys :: [LHsType RdrName] -> [Located RdrName] -> [Located RdrName]
extract_ltys tys acc = foldr extract_lty acc tys

extract_lty :: LHsType RdrName -> [Located RdrName] -> [Located RdrName]
extract_lty (L loc ty) acc 
  = case ty of
      HsTyVar tv 	        -> extract_tv loc tv acc
      HsBangTy _ ty            	-> extract_lty ty acc
      HsRecTy flds            	-> foldr (extract_lty . cd_fld_type) acc flds
      HsAppTy ty1 ty2          	-> extract_lty ty1 (extract_lty ty2 acc)
      HsListTy ty              	-> extract_lty ty acc
      HsPArrTy ty              	-> extract_lty ty acc
      HsTupleTy _ tys          	-> extract_ltys tys acc
      HsFunTy ty1 ty2          	-> extract_lty ty1 (extract_lty ty2 acc)
      HsPredTy p		-> extract_pred p acc
      HsOpTy ty1 (L loc tv) ty2 -> extract_tv loc tv (extract_lty ty1 (extract_lty ty2 acc))
      HsParTy ty               	-> extract_lty ty acc
      HsNumTy _                 -> acc
      HsQuasiQuoteTy {}	        -> acc  -- Quasi quotes mention no type variables
      HsSpliceTy {}           	-> acc	-- Type splices mention no type variables
      HsKindSig ty _            -> extract_lty ty acc
      HsForAllTy _ [] cx ty     -> extract_lctxt cx (extract_lty ty acc)
      HsForAllTy _ tvs cx ty    -> acc ++ (filter ((`notElem` locals) . unLoc) $
					   extract_lctxt cx (extract_lty ty []))
				where
				   locals = hsLTyVarNames tvs
      HsDocTy ty _              -> extract_lty ty acc

extract_tv :: SrcSpan -> RdrName -> [Located RdrName] -> [Located RdrName]
extract_tv loc tv acc | isRdrTyVar tv = L loc tv : acc
		      | otherwise     = acc

extractGenericPatTyVars :: LHsBinds RdrName -> [Located RdrName]
-- Get the type variables out of the type patterns in a bunch of
-- possibly-generic bindings in a class declaration
extractGenericPatTyVars binds
  = nubBy eqLocated (foldrBag get [] binds)
  where
    get (L _ (FunBind { fun_matches = MatchGroup ms _ })) acc = foldr (get_m.unLoc) acc ms
    get _                                                 acc = acc

    get_m (Match (L _ (TypePat ty) : _) _ _) acc = extract_lty ty acc
    get_m _                                        acc = acc
\end{code}


%************************************************************************
%*									*
\subsection{Construction functions for Rdr stuff}
%*                                                                    *
%************************************************************************

mkClassDecl builds a RdrClassDecl, filling in the names for tycon and datacon
by deriving them from the name of the class.  We fill in the names for the
tycon and datacon corresponding to the class, by deriving them from the
name of the class itself.  This saves recording the names in the interface
file (which would be equally good).

Similarly for mkConDecl, mkClassOpSig and default-method names.

	*** See "THE NAMING STORY" in HsDecls ****
  
\begin{code}
mkClassDecl :: SrcSpan
            -> Located (Maybe (LHsContext RdrName), LHsType RdrName)
            -> Located [Located (FunDep RdrName)]
            -> Located (OrdList (LHsDecl RdrName))
	    -> P (LTyClDecl RdrName)

mkClassDecl loc (L _ (mcxt, tycl_hdr)) fds where_cls
  = do { let (binds, sigs, ats, docs) = cvBindsAndSigs (unLoc where_cls)
       ; let cxt = fromMaybe (noLoc []) mcxt
       ; (cls, tparams) <- checkTyClHdr tycl_hdr
       ; tyvars <- checkTyVars tparams      -- Only type vars allowed
       ; checkKindSigs ats
       ; return (L loc (ClassDecl { tcdCtxt = cxt, tcdLName = cls, tcdTyVars = tyvars,
		             	    tcdFDs = unLoc fds, tcdSigs = sigs, tcdMeths = binds,
			     	    tcdATs   = ats, tcdDocs  = docs })) }

mkTyData :: SrcSpan
         -> NewOrData
	 -> Bool		-- True <=> data family instance
         -> Located (Maybe (LHsContext RdrName), LHsType RdrName)
         -> Maybe Kind
         -> [LConDecl RdrName]
         -> Maybe [LHsType RdrName]
         -> P (LTyClDecl RdrName)
mkTyData loc new_or_data is_family (L _ (mcxt, tycl_hdr)) ksig data_cons maybe_deriv
  = do { (tc, tparams) <- checkTyClHdr tycl_hdr

       ; checkDatatypeContext mcxt
       ; let cxt = fromMaybe (noLoc []) mcxt
       ; (tyvars, typats) <- checkTParams is_family tparams
       ; return (L loc (TyData { tcdND = new_or_data, tcdCtxt = cxt, tcdLName = tc,
	                  	 tcdTyVars = tyvars, tcdTyPats = typats, 
                                 tcdCons = data_cons, 
	                  	 tcdKindSig = ksig, tcdDerivs = maybe_deriv })) }

mkTySynonym :: SrcSpan 
            -> Bool	        -- True <=> type family instances
            -> LHsType RdrName  -- LHS
            -> LHsType RdrName	-- RHS
            -> P (LTyClDecl RdrName)
mkTySynonym loc is_family lhs rhs
  = do { (tc, tparams) <- checkTyClHdr lhs
       ; (tyvars, typats) <- checkTParams is_family tparams
       ; return (L loc (TySynonym tc tyvars typats rhs)) }

mkTyFamily :: SrcSpan
           -> FamilyFlavour
	   -> LHsType RdrName   -- LHS
	   -> Maybe Kind        -- Optional kind signature
           -> P (LTyClDecl RdrName)
mkTyFamily loc flavour lhs ksig
  = do { (tc, tparams) <- checkTyClHdr lhs
       ; tyvars <- checkTyVars tparams
       ; return (L loc (TyFamily flavour tc tyvars ksig)) }

mkTopSpliceDecl :: LHsExpr RdrName -> HsDecl RdrName
-- If the user wrote
--      [pads| ... ]   then return a QuasiQuoteD
--	$(e)           then return a SpliceD
-- but if she wrote, say,
--      f x            then behave as if she'd written $(f x)
--		       ie a SpliceD
mkTopSpliceDecl (L _ (HsQuasiQuoteE qq))            = QuasiQuoteD qq
mkTopSpliceDecl (L _ (HsSpliceE (HsSplice _ expr))) = SpliceD (SpliceDecl expr       Explicit)
mkTopSpliceDecl other_expr                          = SpliceD (SpliceDecl other_expr Implicit)
\end{code}

%************************************************************************
%*									*
\subsection[cvBinds-etc]{Converting to @HsBinds@, etc.}
%*									*
%************************************************************************

Function definitions are restructured here. Each is assumed to be recursive
initially, and non recursive definitions are discovered by the dependency
analyser.


\begin{code}
--  | Groups together bindings for a single function
cvTopDecls :: OrdList (LHsDecl RdrName) -> [LHsDecl RdrName]
cvTopDecls decls = go (fromOL decls)
  where
    go :: [LHsDecl RdrName] -> [LHsDecl RdrName]
    go [] 		    = []
    go (L l (ValD b) : ds)  = L l' (ValD b') : go ds'
			    where (L l' b', ds') = getMonoBind (L l b) ds
    go (d : ds)   	    = d : go ds

-- Declaration list may only contain value bindings and signatures.
cvBindGroup :: OrdList (LHsDecl RdrName) -> HsValBinds RdrName
cvBindGroup binding
  = case cvBindsAndSigs binding of
      (mbs, sigs, tydecls, _) -> ASSERT( null tydecls )
      	    	  	         ValBindsIn mbs sigs

cvBindsAndSigs :: OrdList (LHsDecl RdrName)
  -> (Bag (LHsBind RdrName), [LSig RdrName], [LTyClDecl RdrName], [LDocDecl])
-- Input decls contain just value bindings and signatures
-- and in case of class or instance declarations also
-- associated type declarations. They might also contain Haddock comments.
cvBindsAndSigs  fb = go (fromOL fb)
  where
    go [] 		   = (emptyBag, [], [], [])
    go (L l (SigD s) : ds) = (bs, L l s : ss, ts, docs)
			   where (bs, ss, ts, docs) = go ds
    go (L l (ValD b) : ds) = (b' `consBag` bs, ss, ts, docs)
			   where (b', ds')    = getMonoBind (L l b) ds
				 (bs, ss, ts, docs) = go ds'
    go (L l (TyClD t): ds) = (bs, ss, L l t : ts, docs)
			   where (bs, ss, ts, docs) = go ds
    go (L l (DocD d) : ds) =  (bs, ss, ts, (L l d) : docs)
			   where (bs, ss, ts, docs) = go ds
    go (L _ d : _)        = pprPanic "cvBindsAndSigs" (ppr d)

-----------------------------------------------------------------------------
-- Group function bindings into equation groups

getMonoBind :: LHsBind RdrName -> [LHsDecl RdrName]
  -> (LHsBind RdrName, [LHsDecl RdrName])
-- Suppose 	(b',ds') = getMonoBind b ds
-- 	ds is a list of parsed bindings
--	b is a MonoBinds that has just been read off the front

-- Then b' is the result of grouping more equations from ds that
-- belong with b into a single MonoBinds, and ds' is the depleted
-- list of parsed bindings.
--
-- All Haddock comments between equations inside the group are 
-- discarded.
--
-- No AndMonoBinds or EmptyMonoBinds here; just single equations

getMonoBind (L loc1 (FunBind { fun_id = fun_id1@(L _ f1), fun_infix = is_infix1,
                               fun_matches = MatchGroup mtchs1 _ })) binds
  | has_args mtchs1
  = go is_infix1 mtchs1 loc1 binds []
  where
    go is_infix mtchs loc 
       (L loc2 (ValD (FunBind { fun_id = L _ f2, fun_infix = is_infix2,
			        fun_matches = MatchGroup mtchs2 _ })) : binds) _
	| f1 == f2 = go (is_infix || is_infix2) (mtchs2 ++ mtchs) 
		        (combineSrcSpans loc loc2) binds []
    go is_infix mtchs loc (doc_decl@(L loc2 (DocD _)) : binds) doc_decls 
	= let doc_decls' = doc_decl : doc_decls  
          in go is_infix mtchs (combineSrcSpans loc loc2) binds doc_decls'
    go is_infix mtchs loc binds doc_decls
	= (L loc (makeFunBind fun_id1 is_infix (reverse mtchs)), (reverse doc_decls) ++ binds)
	-- Reverse the final matches, to get it back in the right order
        -- Do the same thing with the trailing doc comments

getMonoBind bind binds = (bind, binds)

has_args :: [LMatch RdrName] -> Bool
has_args [] 	    	     	      = panic "RdrHsSyn:has_args"
has_args ((L _ (Match args _ _)) : _) = not (null args)
	-- Don't group together FunBinds if they have
	-- no arguments.  This is necessary now that variable bindings
	-- with no arguments are now treated as FunBinds rather
	-- than pattern bindings (tests/rename/should_fail/rnfail002).
\end{code}

%************************************************************************
%*									*
\subsection[PrefixToHS-utils]{Utilities for conversion}
%*									*
%************************************************************************


\begin{code}
-----------------------------------------------------------------------------
-- splitCon

-- When parsing data declarations, we sometimes inadvertently parse
-- a constructor application as a type (eg. in data T a b = C a b `D` E a b)
-- This function splits up the type application, adds any pending
-- arguments, and converts the type constructor back into a data constructor.

splitCon :: LHsType RdrName
      -> P (Located RdrName, HsConDeclDetails RdrName)
-- This gets given a "type" that should look like
--      C Int Bool
-- or   C { x::Int, y::Bool }
-- and returns the pieces
splitCon ty
 = split ty []
 where
   split (L _ (HsAppTy t u)) ts = split t (u : ts)
   split (L l (HsTyVar tc))  ts = do data_con <- tyConToDataCon l tc
 				     return (data_con, mk_rest ts)
   split (L l _) _ 	        = parseErrorSDoc l (text "parse error in constructor in data/newtype declaration:" <+> ppr ty)

   mk_rest [L _ (HsRecTy flds)] = RecCon flds
   mk_rest ts                   = PrefixCon ts

mkDeprecatedGadtRecordDecl :: SrcSpan 
		     	   -> Located RdrName
		     	   -> [ConDeclField RdrName]
			   -> LHsType RdrName
		     	   ->  P (LConDecl  RdrName)
-- This one uses the deprecated syntax
--    C { x,y ::Int } :: T a b
-- We give it a RecCon details right away
mkDeprecatedGadtRecordDecl loc (L con_loc con) flds res_ty
  = do { data_con <- tyConToDataCon con_loc con
       ; return (L loc (ConDecl { con_old_rec  = True
                                , con_name     = data_con
	    	 		, con_explicit = Implicit
	    	 		, con_qvars    = []
	    	 		, con_cxt      = noLoc []
	    	 		, con_details  = RecCon flds
	    	 		, con_res      = ResTyGADT res_ty
            	 		, con_doc      = Nothing })) }

mkSimpleConDecl :: Located RdrName -> [LHsTyVarBndr RdrName]
		-> LHsContext RdrName -> HsConDeclDetails RdrName
		-> ConDecl RdrName

mkSimpleConDecl name qvars cxt details
  = ConDecl { con_old_rec  = False
            , con_name     = name
	    , con_explicit = Explicit
	    , con_qvars    = qvars
	    , con_cxt      = cxt
	    , con_details  = details
	    , con_res      = ResTyH98
            , con_doc      = Nothing }

mkGadtDecl :: [Located RdrName]
           -> LHsType RdrName     -- Always a HsForAllTy
           -> [ConDecl RdrName]
-- We allow C,D :: ty
-- and expand it as if it had been 
--    C :: ty; D :: ty
-- (Just like type signatures in general.)
mkGadtDecl names (L _ (HsForAllTy imp qvars cxt tau))
  = [mk_gadt_con name | name <- names]
  where
    (details, res_ty)		-- See Note [Sorting out the result type]
      = case tau of
    	  L _ (HsFunTy (L _ (HsRecTy flds)) res_ty) -> (RecCon flds,  res_ty)
	  _other                                    -> (PrefixCon [], tau)

    mk_gadt_con name
       = ConDecl { con_old_rec  = False
                 , con_name     = name
	    	 , con_explicit = imp
	    	 , con_qvars    = qvars
	    	 , con_cxt      = cxt
	    	 , con_details  = details
	    	 , con_res      = ResTyGADT res_ty
            	 , con_doc      = Nothing }
mkGadtDecl _ other_ty = pprPanic "mkGadtDecl" (ppr other_ty)

tyConToDataCon :: SrcSpan -> RdrName -> P (Located RdrName)
tyConToDataCon loc tc
  | isTcOcc (rdrNameOcc tc)
  = return (L loc (setRdrNameSpace tc srcDataName))
  | otherwise
  = parseErrorSDoc loc (msg $$ extra)
  where
    msg = text "Not a data constructor:" <+> quotes (ppr tc)
    extra | tc == forall_tv_RDR
	  = text "Perhaps you intended to use -XExistentialQuantification"
	  | otherwise = empty
\end{code}

Note [Sorting out the result type]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In a GADT declaration which is not a record, we put the whole constr
type into the ResTyGADT for now; the renamer will unravel it once it
has sorted out operator fixities. Consider for example
     C :: a :*: b -> a :*: b -> a :+: b
Initially this type will parse as
      a :*: (b -> (a :*: (b -> (a :+: b))))

so it's hard to split up the arguments until we've done the precedence
resolution (in the renamer) On the other hand, for a record
	{ x,y :: Int } -> a :*: b
there is no doubt.  AND we need to sort records out so that
we can bring x,y into scope.  So:
   * For PrefixCon we keep all the args in the ResTyGADT
   * For RecCon we do not

\begin{code}
----------------------------------------------------------------------------
-- Various Syntactic Checks

checkInstType :: LHsType RdrName -> P (LHsType RdrName)
checkInstType (L l t)
  = case t of
	HsForAllTy exp tvs ctxt ty -> do
		dict_ty <- checkDictTy ty
	      	return (L l (HsForAllTy exp tvs ctxt dict_ty))

        HsParTy ty -> checkInstType ty

	ty ->   do dict_ty <- checkDictTy (L l ty)
	      	   return (L l (HsForAllTy Implicit [] (noLoc []) dict_ty))

checkDictTy :: LHsType RdrName -> P (LHsType RdrName)
checkDictTy (L spn ty) = check ty []
  where
  check (HsTyVar tc)            args | isRdrTc tc = done tc args
  check (HsOpTy t1 (L _ tc) t2) args | isRdrTc tc = done tc (t1:t2:args)
  check (HsAppTy l r) args = check (unLoc l) (r:args)
  check (HsParTy t)   args = check (unLoc t) args
  check _ _ = parseError spn "Malformed instance header"

  done tc args = return (L spn (HsPredTy (HsClassP tc args)))

checkTParams :: Bool	  -- Type/data family
	     -> [LHsType RdrName]
	     -> P ([LHsTyVarBndr RdrName], Maybe [LHsType RdrName])
-- checkTParams checks the type parameters of a data/newtype declaration
-- There are two cases:
--
--  a) Vanilla data/newtype decl. In that case 
--        - the type parameters should all be type variables
--        - they may have a kind annotation
--
--  b) Family data/newtype decl.  In that case
--        - The type parameters may be arbitrary types
--        - We find the type-varaible binders by find the 
--          free type vars of those types
--        - We make them all kind-sig-free binders (UserTyVar)
--          If there are kind sigs in the type parameters, they
--          will fix the binder's kind when we kind-check the 
--          type parameters
checkTParams is_family tparams
  | not is_family        -- Vanilla case (a)
  = do { tyvars <- checkTyVars tparams
       ; return (tyvars, Nothing) }
  | otherwise		 -- Family case (b)
  = do { let tyvars = userHsTyVarBndrs (extractHsTysRdrTyVars tparams)
       ; return (tyvars, Just tparams) }

checkTyVars :: [LHsType RdrName] -> P [LHsTyVarBndr RdrName]
-- Check whether the given list of type parameters are all type variables
-- (possibly with a kind signature).  If the second argument is `False',
-- only type variables are allowed and we raise an error on encountering a
-- non-variable; otherwise, we allow non-variable arguments and return the
-- entire list of parameters.
checkTyVars tparms = mapM chk tparms
  where
	-- Check that the name space is correct!
    chk (L l (HsKindSig (L _ (HsTyVar tv)) k))
	| isRdrTyVar tv    = return (L l (KindedTyVar tv k))
    chk (L l (HsTyVar tv))
        | isRdrTyVar tv    = return (L l (UserTyVar tv placeHolderKind))
    chk (L l _)            =
	  parseError l "Type found where type variable expected"

checkDatatypeContext :: Maybe (LHsContext RdrName) -> P ()
checkDatatypeContext Nothing = return ()
checkDatatypeContext (Just (L loc _))
    = do allowed <- extension datatypeContextsEnabled
         unless allowed $
             parseError loc "Illegal datatype context (use -XDatatypeContexts)"

checkTyClHdr :: LHsType RdrName
             -> P (Located RdrName,	     -- the head symbol (type or class name)
	           [LHsType RdrName])        -- parameters of head symbol
-- Well-formedness check and decomposition of type and class heads.
-- Decomposes   T ty1 .. tyn   into    (T, [ty1, ..., tyn])
-- 		Int :*: Bool   into    (:*:, [Int, Bool])
-- returning the pieces
checkTyClHdr ty
  = goL ty []
  where
    goL (L l ty) acc = go l ty acc

    go l (HsTyVar tc) acc 
	| isRdrTc tc 	     = return (L l tc, acc)
				     
    go _ (HsOpTy t1 ltc@(L _ tc) t2) acc
	| isRdrTc tc	     = return (ltc, t1:t2:acc)
    go _ (HsParTy ty)    acc = goL ty acc
    go _ (HsAppTy t1 t2) acc = goL t1 (t2:acc)
    go l _               _   = parseError l "Malformed head of type or class declaration"

-- Check that associated type declarations of a class are all kind signatures.
--
checkKindSigs :: [LTyClDecl RdrName] -> P ()
checkKindSigs = mapM_ check
  where
    check (L l tydecl) 
      | isFamilyDecl tydecl
        || isSynDecl tydecl  = return ()
      | otherwise	     = 
	parseError l "Type declaration in a class must be a kind signature or synonym default"

checkContext :: LHsType RdrName -> P (LHsContext RdrName)
checkContext (L l t)
  = check t
 where
  check (HsTupleTy _ ts) 	-- (Eq a, Ord b) shows up as a tuple type
    = do ctx <- mapM checkPred ts
	 return (L l ctx)

  check (HsParTy ty)	-- to be sure HsParTy doesn't get into the way
    = check (unLoc ty)

  check (HsTyVar t)	-- Empty context shows up as a unit type ()
    | t == getRdrName unitTyCon = return (L l [])

  check t 
    = do p <- checkPred (L l t)
         return (L l [p])


checkPred :: LHsType RdrName -> P (LHsPred RdrName)
-- Watch out.. in ...deriving( Show )... we use checkPred on 
-- the list of partially applied predicates in the deriving,
-- so there can be zero args.
checkPred (L spn (HsPredTy (HsIParam n ty)))
  = return (L spn (HsIParam n ty))
checkPred (L spn ty)
  = check spn ty []
  where
    checkl (L l ty) args = check l ty args

    check _loc (HsPredTy pred@(HsEqualP _ _)) 
                                       args | null args
					    = return $ L spn pred
    check _loc (HsTyVar t)             args | not (isRdrTyVar t) 
		 		  	    = return (L spn (HsClassP t args))
    check _loc (HsAppTy l r)           args = checkl l (r:args)
    check _loc (HsOpTy l (L loc tc) r) args = check loc (HsTyVar tc) (l:r:args)
    check _loc (HsParTy t)  	       args = checkl t args
    check loc _                        _    = parseError loc  
					        "malformed class assertion"

---------------------------------------------------------------------------
-- Checking statements in a do-expression
-- 	We parse   do { e1 ; e2 ; }
-- 	as [ExprStmt e1, ExprStmt e2]
-- checkDo (a) checks that the last thing is an ExprStmt
--	   (b) returns it separately
-- same comments apply for mdo as well

checkDo, checkMDo :: SrcSpan -> [LStmt RdrName] -> P ([LStmt RdrName], LHsExpr RdrName)

checkDo	 = checkDoMDo "a " "'do'"
checkMDo = checkDoMDo "an " "'mdo'"

checkDoMDo :: String -> String -> SrcSpan -> [LStmt RdrName] -> P ([LStmt RdrName], LHsExpr RdrName)
checkDoMDo _   nm loc []   = parseError loc ("Empty " ++ nm ++ " construct")
checkDoMDo pre nm _   ss   = do
  check ss
  where 
	check  []                     = panic "RdrHsSyn:checkDoMDo"
	check  [L _ (ExprStmt e _ _)] = return ([], e)
	check  [L l _] = parseError l ("The last statement in " ++ pre ++ nm ++
					 " construct must be an expression")
	check (s:ss) = do
	  (ss',e') <-  check ss
	  return ((s:ss'),e')

-- -------------------------------------------------------------------------
-- Checking Patterns.

-- We parse patterns as expressions and check for valid patterns below,
-- converting the expression into a pattern at the same time.

checkPattern :: LHsExpr RdrName -> P (LPat RdrName)
checkPattern e = checkLPat e

checkPatterns :: [LHsExpr RdrName] -> P [LPat RdrName]
checkPatterns es = mapM checkPattern es

checkLPat :: LHsExpr RdrName -> P (LPat RdrName)
checkLPat e@(L l _) = checkPat l e []

checkPat :: SrcSpan -> LHsExpr RdrName -> [LPat RdrName] -> P (LPat RdrName)
checkPat loc (L l (HsVar c)) args
  | isRdrDataCon c = return (L loc (ConPatIn (L l c) (PrefixCon args)))
checkPat loc e args 	-- OK to let this happen even if bang-patterns
			-- are not enabled, because there is no valid
			-- non-bang-pattern parse of (C ! e)
  | Just (e', args') <- splitBang e
  = do	{ args'' <- checkPatterns args'
	; checkPat loc e' (args'' ++ args) }
checkPat loc (L _ (HsApp f x)) args
  = do { x <- checkLPat x; checkPat loc f (x:args) }
checkPat loc (L _ e) []
  = do { pState <- getPState
       ; p <- checkAPat (dflags pState) loc e
       ; return (L loc p) }
checkPat loc _ _
  = patFail loc

checkAPat :: DynFlags -> SrcSpan -> HsExpr RdrName -> P (Pat RdrName)
checkAPat dynflags loc e = case e of
   EWildPat -> return (WildPat placeHolderType)
   HsVar x  -> return (VarPat x)
   HsLit l  -> return (LitPat l)

   -- Overloaded numeric patterns (e.g. f 0 x = x)
   -- Negation is recorded separately, so that the literal is zero or +ve
   -- NB. Negative *primitive* literals are already handled by the lexer
   HsOverLit pos_lit          -> return (mkNPat pos_lit Nothing)
   NegApp (L _ (HsOverLit pos_lit)) _ 
			-> return (mkNPat pos_lit (Just noSyntaxExpr))
   
   SectionR (L _ (HsVar bang)) e 	-- (! x)
 	| bang == bang_RDR 
	-> do { bang_on <- extension bangPatEnabled
	      ; if bang_on then checkLPat e >>= (return . BangPat)
		else parseError loc "Illegal bang-pattern (use -XBangPatterns)" }

   ELazyPat e	      -> checkLPat e >>= (return . LazyPat)
   EAsPat n e	      -> checkLPat e >>= (return . AsPat n)
   -- view pattern is well-formed if the pattern is
   EViewPat expr patE -> checkLPat patE >>= (return . (\p -> ViewPat expr p placeHolderType))
   ExprWithTySig e t  -> do e <- checkLPat e
                            -- Pattern signatures are parsed as sigtypes,
                            -- but they aren't explicit forall points.  Hence
                            -- we have to remove the implicit forall here.
                            let t' = case t of 
                                       L _ (HsForAllTy Implicit _ (L _ []) ty) -> ty
                                       other -> other
                            return (SigPatIn e t')
   
   -- n+k patterns
   OpApp (L nloc (HsVar n)) (L _ (HsVar plus)) _ 
	 (L _ (HsOverLit lit@(OverLit {ol_val = HsIntegral {}})))
   		      | dopt Opt_NPlusKPatterns dynflags && (plus == plus_RDR)
   		      -> return (mkNPlusKPat (L nloc n) lit)
   
   OpApp l op _fix r  -> do l <- checkLPat l
                            r <- checkLPat r
                            case op of
                               L cl (HsVar c) | isDataOcc (rdrNameOcc c)
                                      -> return (ConPatIn (L cl c) (InfixCon l r))
                               _ -> patFail loc
   
   HsPar e	      -> checkLPat e >>= (return . ParPat)
   ExplicitList _ es  -> do ps <- mapM checkLPat es
                            return (ListPat ps placeHolderType)
   ExplicitPArr _ es  -> do ps <- mapM checkLPat es
                            return (PArrPat ps placeHolderType)
   
   ExplicitTuple es b 
     | all tupArgPresent es  -> do ps <- mapM checkLPat [e | Present e <- es]
                                   return (TuplePat ps b placeHolderType)
     | otherwise -> parseError loc "Illegal tuple section in pattern"
   
   RecordCon c _ (HsRecFields fs dd)
                      -> do fs <- mapM checkPatField fs
                            return (ConPatIn c (RecCon (HsRecFields fs dd)))
   HsQuasiQuoteE q    -> return (QuasiQuotePat q)
-- Generics 
   HsType ty          -> return (TypePat ty) 
   _                  -> patFail loc

placeHolderPunRhs :: LHsExpr RdrName
-- The RHS of a punned record field will be filled in by the renamer
-- It's better not to make it an error, in case we want to print it when debugging
placeHolderPunRhs = noLoc (HsVar pun_RDR)

plus_RDR, bang_RDR, pun_RDR :: RdrName
plus_RDR = mkUnqual varName (fsLit "+")	-- Hack
bang_RDR = mkUnqual varName (fsLit "!")	-- Hack
pun_RDR  = mkUnqual varName (fsLit "pun-right-hand-side")

checkPatField :: HsRecField RdrName (LHsExpr RdrName) -> P (HsRecField RdrName (LPat RdrName))
checkPatField fld = do	{ p <- checkLPat (hsRecFieldArg fld)
			; return (fld { hsRecFieldArg = p }) }

patFail :: SrcSpan -> P a
patFail loc = parseError loc "Parse error in pattern"


---------------------------------------------------------------------------
-- Check Equation Syntax

checkValDef :: LHsExpr RdrName
	    -> Maybe (LHsType RdrName)
	    -> Located (GRHSs RdrName)
	    -> P (HsBind RdrName)

checkValDef lhs (Just sig) grhss
   	-- x :: ty = rhs  parses as a *pattern* binding
  = checkPatBind (L (combineLocs lhs sig) (ExprWithTySig lhs sig)) grhss

checkValDef lhs opt_sig grhss
  = do	{ mb_fun <- isFunLhs lhs
	; case mb_fun of
	    Just (fun, is_infix, pats) -> checkFunBind (getLoc lhs)
						fun is_infix pats opt_sig grhss
	    Nothing -> checkPatBind lhs grhss }

checkFunBind :: SrcSpan
             -> Located RdrName
             -> Bool
             -> [LHsExpr RdrName]
             -> Maybe (LHsType RdrName)
             -> Located (GRHSs RdrName)
             -> P (HsBind RdrName)
checkFunBind lhs_loc fun is_infix pats opt_sig (L rhs_span grhss)
  = do	ps <- checkPatterns pats
	let match_span = combineSrcSpans lhs_loc rhs_span
	return (makeFunBind fun is_infix [L match_span (Match ps opt_sig grhss)])
	-- The span of the match covers the entire equation.  
	-- That isn't quite right, but it'll do for now.

makeFunBind :: Located id -> Bool -> [LMatch id] -> HsBind id
-- Like HsUtils.mkFunBind, but we need to be able to set the fixity too
makeFunBind fn is_infix ms 
  = FunBind { fun_id = fn, fun_infix = is_infix, fun_matches = mkMatchGroup ms,
	      fun_co_fn = idHsWrapper, bind_fvs = placeHolderNames, fun_tick = Nothing }

checkPatBind :: LHsExpr RdrName
             -> Located (GRHSs RdrName)
             -> P (HsBind RdrName)
checkPatBind lhs (L _ grhss)
  = do	{ lhs <- checkPattern lhs
	; return (PatBind lhs grhss placeHolderType placeHolderNames) }

checkValSig
	:: LHsExpr RdrName
	-> LHsType RdrName
	-> P (Sig RdrName)
checkValSig (L l (HsVar v)) ty 
  | isUnqual v && not (isDataOcc (rdrNameOcc v))
  = return (TypeSig (L l v) ty)
checkValSig lhs@(L l _) ty
  = parseErrorSDoc l ((text "Invalid type signature:" <+>
                       ppr lhs <+> text "::" <+> ppr ty)
                   $$ text hint)
  where
    hint = if looks_like_foreign lhs
           then "Perhaps you meant to use -XForeignFunctionInterface?"
           else "Should be of form <variable> :: <type>"
    -- A common error is to forget the ForeignFunctionInterface flag
    -- so check for that, and suggest.  cf Trac #3805
    -- Sadly 'foreign import' still barfs 'parse error' because 'import' is a keyword
    looks_like_foreign (L _ (HsVar v))     = v == foreign_RDR
    looks_like_foreign (L _ (HsApp lhs _)) = looks_like_foreign lhs
    looks_like_foreign _                   = False

    foreign_RDR = mkUnqual varName (fsLit "foreign")

checkDoAndIfThenElse :: LHsExpr RdrName
                     -> Bool
                     -> LHsExpr RdrName
                     -> Bool
                     -> LHsExpr RdrName
                     -> P ()
checkDoAndIfThenElse guardExpr semiThen thenExpr semiElse elseExpr
 | semiThen || semiElse
    = do pState <- getPState
         unless (dopt Opt_DoAndIfThenElse (dflags pState)) $ do
             parseErrorSDoc (combineLocs guardExpr elseExpr)
                            (text "Unexpected semi-colons in conditional:"
                          $$ nest 4 expr
                          $$ text "Perhaps you meant to use -XDoAndIfThenElse?")
 | otherwise            = return ()
    where pprOptSemi True  = semi
          pprOptSemi False = empty
          expr = text "if"   <+> ppr guardExpr <> pprOptSemi semiThen <+>
                 text "then" <+> ppr thenExpr  <> pprOptSemi semiElse <+>
                 text "else" <+> ppr elseExpr
\end{code}


\begin{code}
	-- The parser left-associates, so there should 
	-- not be any OpApps inside the e's
splitBang :: LHsExpr RdrName -> Maybe (LHsExpr RdrName, [LHsExpr RdrName])
-- Splits (f ! g a b) into (f, [(! g), a, b])
splitBang (L loc (OpApp l_arg bang@(L _ (HsVar op)) _ r_arg))
  | op == bang_RDR = Just (l_arg, L loc (SectionR bang arg1) : argns)
  where
    (arg1,argns) = split_bang r_arg []
    split_bang (L _ (HsApp f e)) es = split_bang f (e:es)
    split_bang e	   	 es = (e,es)
splitBang _ = Nothing

isFunLhs :: LHsExpr RdrName 
	 -> P (Maybe (Located RdrName, Bool, [LHsExpr RdrName]))
-- A variable binding is parsed as a FunBind.
-- Just (fun, is_infix, arg_pats) if e is a function LHS
--
-- The whole LHS is parsed as a single expression.  
-- Any infix operators on the LHS will parse left-associatively
-- E.g. 	f !x y !z
-- 	will parse (rather strangely) as 
--		(f ! x y) ! z
-- 	It's up to isFunLhs to sort out the mess
--
-- a .!. !b 

isFunLhs e = go e []
 where
   go (L loc (HsVar f)) es 
	| not (isRdrDataCon f)	 = return (Just (L loc f, False, es))
   go (L _ (HsApp f e)) es 	 = go f (e:es)
   go (L _ (HsPar e))   es@(_:_) = go e es

	-- For infix function defns, there should be only one infix *function*
	-- (though there may be infix *datacons* involved too).  So we don't
	-- need fixity info to figure out which function is being defined.
	--	a `K1` b `op` c `K2` d
	-- must parse as
	--	(a `K1` b) `op` (c `K2` d)
	-- The renamer checks later that the precedences would yield such a parse.
	-- 
	-- There is a complication to deal with bang patterns.
	--
	-- ToDo: what about this?
	--		x + 1 `op` y = ...

   go e@(L loc (OpApp l (L loc' (HsVar op)) fix r)) es
	| Just (e',es') <- splitBang e
	= do { bang_on <- extension bangPatEnabled
	     ; if bang_on then go e' (es' ++ es)
	       else return (Just (L loc' op, True, (l:r:es))) }
		-- No bangs; behave just like the next case
	| not (isRdrDataCon op) 	-- We have found the function!
	= return (Just (L loc' op, True, (l:r:es)))
	| otherwise			-- Infix data con; keep going
	= do { mb_l <- go l es
	     ; case mb_l of
		 Just (op', True, j : k : es')
		    -> return (Just (op', True, j : op_app : es'))
		    where
		      op_app = L loc (OpApp k (L loc' (HsVar op)) fix r)
		 _ -> return Nothing }
   go _ _ = return Nothing

---------------------------------------------------------------------------
-- Miscellaneous utilities

checkPrecP :: Located Int -> P Int
checkPrecP (L l i)
 | 0 <= i && i <= maxPrecedence = return i
 | otherwise     	        = parseError l "Precedence out of range"

mkRecConstrOrUpdate 
	:: LHsExpr RdrName 
	-> SrcSpan
	-> ([HsRecField RdrName (LHsExpr RdrName)], Bool)
	-> P (HsExpr RdrName)

mkRecConstrOrUpdate (L l (HsVar c)) _ (fs,dd) | isRdrDataCon c
  = return (RecordCon (L l c) noPostTcExpr (mk_rec_fields fs dd))
mkRecConstrOrUpdate exp loc (fs,dd)
  | null fs   = parseError loc "Empty record update"
  | otherwise = return (RecordUpd exp (mk_rec_fields fs dd) [] [] [])

mk_rec_fields :: [HsRecField id arg] -> Bool -> HsRecFields id arg
mk_rec_fields fs False = HsRecFields { rec_flds = fs, rec_dotdot = Nothing }
mk_rec_fields fs True  = HsRecFields { rec_flds = fs, rec_dotdot = Just (length fs) }

mkInlinePragma :: Maybe Activation -> RuleMatchInfo -> Bool -> InlinePragma
-- The Maybe is because the user can omit the activation spec (and usually does)
mkInlinePragma mb_act match_info inl 
  = InlinePragma { inl_inline = inl
                 , inl_sat    = Nothing
                 , inl_act    = act
                 , inl_rule   = match_info }
  where
    act = case mb_act of
            Just act -> act
            Nothing | inl       -> AlwaysActive
                    | otherwise -> NeverActive
        -- If no specific phase is given then:
	--   NOINLINE => NeverActive
        --   INLINE   => Active

-----------------------------------------------------------------------------
-- utilities for foreign declarations

-- construct a foreign import declaration
--
mkImport :: CCallConv
	 -> Safety 
	 -> (Located FastString, Located RdrName, LHsType RdrName) 
	 -> P (HsDecl RdrName)
mkImport cconv safety (L loc entity, v, ty)
  | cconv == PrimCallConv                      = do
  let funcTarget = CFunction (StaticTarget entity Nothing)
      importSpec = CImport PrimCallConv safety nilFS funcTarget
  return (ForD (ForeignImport v ty importSpec))

  | otherwise = do
    case parseCImport cconv safety (mkExtName (unLoc v)) (unpackFS entity) of
      Nothing         -> parseError loc "Malformed entity string"
      Just importSpec -> return (ForD (ForeignImport v ty importSpec))

-- the string "foo" is ambigous: either a header or a C identifier.  The
-- C identifier case comes first in the alternatives below, so we pick
-- that one.
parseCImport :: CCallConv -> Safety -> FastString -> String
             -> Maybe ForeignImport
parseCImport cconv safety nm str =
 listToMaybe $ map fst $ filter (null.snd) $ 
     readP_to_S parse str
 where
   parse = do
       skipSpaces
       r <- choice [
          string "dynamic" >> return (mk nilFS (CFunction DynamicTarget)),
          string "wrapper" >> return (mk nilFS CWrapper),
          optional (string "static" >> skipSpaces) >> 
           (mk nilFS <$> cimp nm) +++
           (do h <- munch1 hdr_char; skipSpaces; mk (mkFastString h) <$> cimp nm)
         ]
       skipSpaces
       return r

   mk = CImport cconv safety

   hdr_char c = not (isSpace c) -- header files are filenames, which can contain
                                -- pretty much any char (depending on the platform),
                                -- so just accept any non-space character
   id_char  c = isAlphaNum c || c == '_'

   cimp nm = (ReadP.char '&' >> skipSpaces >> CLabel <$> cid)
             +++ ((\c -> CFunction (StaticTarget c Nothing)) <$> cid)
          where 
            cid = return nm +++
                  (do c  <- satisfy (\c -> isAlpha c || c == '_')
                      cs <-  many (satisfy id_char)
                      return (mkFastString (c:cs)))


-- construct a foreign export declaration
--
mkExport :: CCallConv
         -> (Located FastString, Located RdrName, LHsType RdrName) 
	 -> P (HsDecl RdrName)
mkExport cconv (L _ entity, v, ty) = return $
  ForD (ForeignExport v ty (CExport (CExportStatic entity' cconv)))
  where
    entity' | nullFS entity = mkExtName (unLoc v)
	    | otherwise     = entity

-- Supplying the ext_name in a foreign decl is optional; if it
-- isn't there, the Haskell name is assumed. Note that no transformation
-- of the Haskell name is then performed, so if you foreign export (++),
-- it's external name will be "++". Too bad; it's important because we don't
-- want z-encoding (e.g. names with z's in them shouldn't be doubled)
--
mkExtName :: RdrName -> CLabelString
mkExtName rdrNm = mkFastString (occNameString (rdrNameOcc rdrNm))
\end{code}


-----------------------------------------------------------------------------
-- Misc utils

\begin{code}
parseError :: SrcSpan -> String -> P a
parseError span s = parseErrorSDoc span (text s)

parseErrorSDoc :: SrcSpan -> SDoc -> P a
parseErrorSDoc span s = failSpanMsgP span s
\end{code}
