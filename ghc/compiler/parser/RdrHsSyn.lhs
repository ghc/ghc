%
% (c) The AQUA Project, Glasgow University, 1996-1998
%
\section[RdrHsSyn]{Specialisations of the @HsSyn@ syntax for the reader}

(Well, really, for specialisations involving @RdrName@s, even if
they are used somewhat later on in the compiler...)

\begin{code}
module RdrHsSyn (
	RdrNameArithSeqInfo,
	RdrNameBangType,
	RdrNameClassOpSig,
	RdrNameConDecl,
	RdrNameConDetails,
	RdrNameContext,
	RdrNameSpecDataSig,
	RdrNameDefaultDecl,
	RdrNameForeignDecl,
	RdrNameGRHS,
	RdrNameGRHSs,
	RdrNameHsBinds,
	RdrNameHsDecl,
	RdrNameHsExpr,
	RdrNameHsModule,
	RdrNameIE,
	RdrNameImportDecl,
	RdrNameInstDecl,
	RdrNameMatch,
	RdrNameMonoBinds,
	RdrNamePat,
	RdrNameHsType,
	RdrNameHsTyVar,
	RdrNameSig,
	RdrNameStmt,
	RdrNameTyClDecl,
	RdrNameRuleDecl,
	RdrNameRuleBndr,
	RdrNameDeprecation,
	RdrNameHsRecordBinds,
	RdrNameFixitySig,

	RdrBinding(..),
	RdrMatch(..),
	SigConverter,

	RdrNameClassOpPragmas,
	RdrNameClassPragmas,
	RdrNameDataPragmas,
	RdrNameGenPragmas,
	RdrNameInstancePragmas,
	extractHsTyRdrNames, 
	extractHsTyRdrTyVars, extractHsTysRdrTyVars,
	extractPatsTyVars, 
	extractRuleBndrsTyVars,
	extractHsCtxtRdrTyVars, extractGenericPatTyVars,
 
	mkHsOpApp, mkClassDecl, mkClassOpSig, mkConDecl,
	mkHsNegApp, mkHsIntegralLit, mkHsFractionalLit, mkNPlusKPatIn,

	
	-- some built-in names (all :: RdrName)
	unitCon_RDR, unitTyCon_RDR, nilCon_RDR, listTyCon_RDR,
	tupleCon_RDR, tupleTyCon_RDR, ubxTupleCon_RDR, ubxTupleTyCon_RDR,
	funTyCon_RDR,

	cvBinds,
	cvMonoBindsAndSigs,
	cvTopDecls,
	cvValSig, cvClassOpSig, cvInstDeclSig,
        mkTyData
    ) where

#include "HsVersions.h"

import HsSyn		-- Lots of it
import CmdLineOpts	( opt_NoImplicitPrelude )
import HsPat		( collectSigTysFromPats )
import OccName		( mkClassTyConOcc, mkClassDataConOcc, mkWorkerOcc,
                          mkSuperDictSelOcc, mkDefaultMethodOcc, mkGenOcc1,
			  mkGenOcc2, varName, dataName, tcName
                      	)
import PrelNames	( pRELUDE_Name, mkTupNameStr )
import RdrName		( RdrName, isRdrTyVar, mkRdrUnqual, rdrNameOcc,
			  mkSrcUnqual, mkPreludeQual
			)
import HsPragmas	
import List		( nub )
import BasicTypes	( Boxity(..), RecFlag(..) )
import Class            ( DefMeth (..) )
import Outputable
\end{code}

 
%************************************************************************
%*									*
\subsection{Type synonyms}
%*									*
%************************************************************************

\begin{code}
type RdrNameArithSeqInfo	= ArithSeqInfo		RdrName RdrNamePat
type RdrNameBangType		= BangType		RdrName
type RdrNameClassOpSig		= Sig			RdrName
type RdrNameConDecl		= ConDecl		RdrName
type RdrNameConDetails		= ConDetails		RdrName
type RdrNameContext		= HsContext 		RdrName
type RdrNameHsDecl		= HsDecl		RdrName RdrNamePat
type RdrNameSpecDataSig		= SpecDataSig		RdrName
type RdrNameDefaultDecl		= DefaultDecl		RdrName
type RdrNameForeignDecl		= ForeignDecl		RdrName
type RdrNameGRHS		= GRHS			RdrName RdrNamePat
type RdrNameGRHSs		= GRHSs			RdrName RdrNamePat
type RdrNameHsBinds		= HsBinds		RdrName RdrNamePat
type RdrNameHsExpr		= HsExpr		RdrName RdrNamePat
type RdrNameHsModule		= HsModule		RdrName RdrNamePat
type RdrNameIE			= IE			RdrName
type RdrNameImportDecl 		= ImportDecl		RdrName
type RdrNameInstDecl		= InstDecl		RdrName RdrNamePat
type RdrNameMatch		= Match			RdrName RdrNamePat
type RdrNameMonoBinds		= MonoBinds		RdrName RdrNamePat
type RdrNamePat			= InPat			RdrName
type RdrNameHsType		= HsType		RdrName
type RdrNameHsTyVar		= HsTyVarBndr		RdrName
type RdrNameSig			= Sig			RdrName
type RdrNameStmt		= Stmt			RdrName RdrNamePat
type RdrNameTyClDecl		= TyClDecl		RdrName RdrNamePat
type RdrNameRuleBndr            = RuleBndr              RdrName
type RdrNameRuleDecl            = RuleDecl              RdrName RdrNamePat
type RdrNameDeprecation         = DeprecDecl            RdrName
type RdrNameFixitySig		= FixitySig		RdrName

type RdrNameHsRecordBinds	= HsRecordBinds		RdrName RdrNamePat

type RdrNameClassOpPragmas	= ClassOpPragmas	RdrName
type RdrNameClassPragmas	= ClassPragmas		RdrName
type RdrNameDataPragmas		= DataPragmas		RdrName
type RdrNameGenPragmas		= GenPragmas		RdrName
type RdrNameInstancePragmas	= InstancePragmas	RdrName
\end{code}


%************************************************************************
%*									*
\subsection{A few functions over HsSyn at RdrName}
%*                                                                    *
%************************************************************************

@extractHsTyRdrNames@ finds the free variables of a HsType
It's used when making the for-alls explicit.

\begin{code}
extractHsTyRdrNames :: HsType RdrName -> [RdrName]
extractHsTyRdrNames ty = nub (extract_ty ty [])

extractHsTyRdrTyVars	 :: RdrNameHsType -> [RdrName]
extractHsTyRdrTyVars ty =  filter isRdrTyVar (extractHsTyRdrNames ty)

extractHsTysRdrTyVars	  :: [RdrNameHsType] -> [RdrName]
extractHsTysRdrTyVars tys =  filter isRdrTyVar (nub (extract_tys tys))

extractRuleBndrsTyVars :: [RuleBndr RdrName] -> [RdrName]
extractRuleBndrsTyVars bndrs = filter isRdrTyVar (nub (foldr go [] bndrs))
                           where
                             go (RuleBndr _)       acc = acc
                             go (RuleBndrSig _ ty) acc = extract_ty ty acc

extractHsCtxtRdrNames :: HsContext RdrName -> [RdrName]
extractHsCtxtRdrNames ty = nub (extract_ctxt ty [])
extractHsCtxtRdrTyVars :: HsContext RdrName -> [RdrName]
extractHsCtxtRdrTyVars ty = filter isRdrTyVar (extractHsCtxtRdrNames ty)

extract_ctxt ctxt acc = foldr extract_pred acc ctxt

extract_pred (HsPClass cls tys) acc	= foldr extract_ty (cls : acc) tys
extract_pred (HsPIParam n ty) acc	= extract_ty ty acc

extract_tys tys = foldr extract_ty [] tys

extract_ty (HsAppTy ty1 ty2)          acc = extract_ty ty1 (extract_ty ty2 acc)
extract_ty (HsListTy ty)              acc = extract_ty ty acc
extract_ty (HsTupleTy _ tys)          acc = foldr extract_ty acc tys
extract_ty (HsFunTy ty1 ty2)          acc = extract_ty ty1 (extract_ty ty2 acc)
extract_ty (HsPredTy p)		      acc = extract_pred p acc
extract_ty (HsUsgTy usg ty)           acc = extract_ty ty acc
extract_ty (HsUsgForAllTy uv ty)      acc = extract_ty ty acc
extract_ty (HsTyVar tv)               acc = tv : acc
extract_ty (HsForAllTy Nothing ctxt ty) acc = extract_ctxt ctxt (extract_ty ty acc)
-- Generics
extract_ty (HsOpTy ty1 nam ty2)         acc = extract_ty ty1 (extract_ty ty2 acc)
extract_ty (HsNumTy num)              acc = acc
-- Generics
extract_ty (HsForAllTy (Just tvs) ctxt ty) 
                                acc = acc ++
                                      (filter (`notElem` locals) $
				       extract_ctxt ctxt (extract_ty ty []))
				    where
				      locals = hsTyVarNames tvs


extractPatsTyVars :: [RdrNamePat] -> [RdrName]
extractPatsTyVars = filter isRdrTyVar . 
		    nub . 
		    extract_tys .
		    collectSigTysFromPats

extractGenericPatTyVars :: RdrNameMonoBinds -> [RdrName]
-- Get the type variables out of the type patterns in a bunch of
-- possibly-generic bindings in a class declaration
extractGenericPatTyVars binds
  = filter isRdrTyVar (nub (get binds []))
  where
    get (AndMonoBinds b1 b2)   acc = get b1 (get b2 acc)
    get (FunMonoBind _ _ ms _) acc = foldr get_m acc ms
    get other		       acc = acc

    get_m (Match _ (TypePatIn ty : _) _ _) acc = extract_ty ty acc
    get_m other				   acc = acc
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
  
\begin{code}
mkClassDecl cxt cname tyvars fds sigs mbinds prags loc
  = ClassDecl cxt cname tyvars fds sigs mbinds prags new_names loc
  where
    cls_occ  = rdrNameOcc cname
    data_occ = mkClassDataConOcc cls_occ
    dname    = mkRdrUnqual data_occ
    dwname   = mkRdrUnqual (mkWorkerOcc data_occ)
    tname    = mkRdrUnqual (mkClassTyConOcc   cls_occ)
    sc_sel_names = [ mkRdrUnqual (mkSuperDictSelOcc n cls_occ) 
		   | n <- [1..length cxt]]
      -- We number off the superclass selectors, 1, 2, 3 etc so that we 
      -- can construct names for the selectors.  Thus
      --      class (C a, C b) => D a b where ...
      -- gives superclass selectors
      --      D_sc1, D_sc2
      -- (We used to call them D_C, but now we can have two different
      --  superclasses both called C!)
    new_names = toClassDeclNameList (tname, dname, dwname, sc_sel_names)

-- mkTyData :: ??
mkTyData new_or_data context tname list_var list_con i maybe pragmas src =
    let t_occ  = rdrNameOcc tname
        name1 = mkRdrUnqual (mkGenOcc1 t_occ) 
	name2 = mkRdrUnqual (mkGenOcc2 t_occ) 
    in TyData new_or_data context 
         tname list_var list_con i maybe pragmas src name1 name2

mkClassOpSig (DefMeth x) op ty loc
  = ClassOpSig op (Just (DefMeth dm_rn)) ty loc
  where
    dm_rn = mkRdrUnqual (mkDefaultMethodOcc (rdrNameOcc op))
mkClassOpSig x op ty loc =
    ClassOpSig op (Just x) ty loc

mkConDecl cname ex_vars cxt details loc
  = ConDecl cname wkr_name ex_vars cxt details loc
  where
    wkr_name = mkRdrUnqual (mkWorkerOcc (rdrNameOcc cname))
\end{code}

\begin{code}
mkHsNegApp :: RdrNameHsExpr -> RdrNameHsExpr
-- If the type checker sees (negate 3#) it will barf, because negate
-- can't take an unboxed arg.  But that is exactly what it will see when
-- we write "-3#".  So we have to do the negation right now!
-- 
-- We also do the same service for boxed literals, because this function
-- is also used for patterns (which, remember, are parsed as expressions)
-- and pattern don't have negation in them.
-- 
-- Finally, it's important to represent minBound as minBound, and not
-- as (negate (-minBound)), becuase the latter is out of range. 

mkHsNegApp (HsLit (HsIntPrim i))    = HsLit (HsIntPrim (-i))    
mkHsNegApp (HsLit (HsFloatPrim i))  = HsLit (HsFloatPrim (-i))  
mkHsNegApp (HsLit (HsDoublePrim i)) = HsLit (HsDoublePrim (-i)) 

mkHsNegApp (HsOverLit (HsIntegral   i n)) = HsOverLit (HsIntegral   (-i) n)
mkHsNegApp (HsOverLit (HsFractional f n)) = HsOverLit (HsFractional (-f) n)

mkHsNegApp expr = NegApp expr (prelQual varName SLIT("negate"))
\end{code}

\begin{code}
mkHsIntegralLit :: Integer -> HsOverLit RdrName
mkHsIntegralLit i = HsIntegral i (prelQual varName SLIT("fromInteger"))

mkHsFractionalLit :: Rational -> HsOverLit RdrName
mkHsFractionalLit f = HsFractional f (prelQual varName SLIT("fromRational"))

mkNPlusKPatIn :: RdrName -> HsOverLit RdrName -> RdrNamePat
mkNPlusKPatIn n k = NPlusKPatIn n k (prelQual varName SLIT("-"))
\end{code}

A useful function for building @OpApps@.  The operator is always a
variable, and we don't know the fixity yet.

\begin{code}
mkHsOpApp e1 op e2 = OpApp e1 (HsVar op) (error "mkOpApp:fixity") e2
\end{code}

\begin{code}
-----------------------------------------------------------------------------
-- Built-in names
-- Qualified Prelude names are always in scope; so we can just say Prelude.[]
-- for the list type constructor, say.   But it's not so easy when we say
-- -fno-implicit-prelude.   Then you just get whatever "[]" happens to be in scope.

unitCon_RDR, unitTyCon_RDR, nilCon_RDR, listTyCon_RDR :: RdrName
tupleCon_RDR, tupleTyCon_RDR		:: Int -> RdrName
ubxTupleCon_RDR, ubxTupleTyCon_RDR 	:: Int -> RdrName

unitCon_RDR   		= prelQual dataName SLIT("()")
unitTyCon_RDR 		= prelQual tcName   SLIT("()")
nilCon_RDR    		= prelQual dataName SLIT("[]")
listTyCon_RDR 		= prelQual tcName   SLIT("[]")
funTyCon_RDR  		= prelQual tcName   SLIT("(->)")
tupleCon_RDR arity      = prelQual dataName (snd (mkTupNameStr Boxed arity))
tupleTyCon_RDR arity    = prelQual tcName   (snd (mkTupNameStr Boxed arity))
ubxTupleCon_RDR arity   = prelQual dataName (snd (mkTupNameStr Unboxed arity))
ubxTupleTyCon_RDR arity = prelQual tcName   (snd (mkTupNameStr Unboxed arity))

prelQual ns occ | opt_NoImplicitPrelude = mkSrcUnqual   ns occ
		| otherwise		= mkPreludeQual ns pRELUDE_Name occ
\end{code}

%************************************************************************
%*									*
\subsection[rdrBinding]{Bindings straight out of the parser}
%*									*
%************************************************************************

\begin{code}
data RdrBinding
  =   -- On input we use the Empty/And form rather than a list
    RdrNullBind
  | RdrAndBindings    RdrBinding RdrBinding

      -- Value bindings havn't been united with their
      -- signatures yet
  | RdrValBinding     RdrNameMonoBinds

      -- Signatures are mysterious; we can't
      -- tell if its a Sig or a ClassOpSig,
      -- so we just save the pieces:
  | RdrSig            RdrNameSig

      -- The remainder all fit into the main HsDecl form
  | RdrHsDecl         RdrNameHsDecl
  
type SigConverter = RdrNameSig -> RdrNameSig
\end{code}

\begin{code}
data RdrMatch
  = RdrMatch
	     [RdrNamePat]
	     (Maybe RdrNameHsType)
	     RdrNameGRHSs
\end{code}

%************************************************************************
%*									*
\subsection[cvDecls]{Convert various top-level declarations}
%*									*
%************************************************************************

We make a point not to throw any user-pragma ``sigs'' at
these conversion functions:

\begin{code}
cvValSig, cvClassOpSig, cvInstDeclSig :: SigConverter

cvValSig      sig = sig

cvInstDeclSig sig = sig

cvClassOpSig (Sig var poly_ty src_loc) = ClassOpSig var Nothing poly_ty src_loc
cvClassOpSig sig 		       = sig
\end{code}


%************************************************************************
%*									*
\subsection[cvBinds-etc]{Converting to @HsBinds@, @MonoBinds@, etc.}
%*									*
%************************************************************************

Function definitions are restructured here. Each is assumed to be recursive
initially, and non recursive definitions are discovered by the dependency
analyser.

\begin{code}
cvBinds :: SigConverter -> RdrBinding -> RdrNameHsBinds
	-- The mysterious SigConverter converts Sigs to ClassOpSigs
	-- in class declarations.  Mostly it's just an identity function

cvBinds sig_cvtr binding
  = case (cvMonoBindsAndSigs sig_cvtr binding) of { (mbs, sigs) ->
    MonoBind mbs sigs Recursive
    }
\end{code}

\begin{code}
cvMonoBindsAndSigs :: SigConverter
		   -> RdrBinding
		   -> (RdrNameMonoBinds, [RdrNameSig])

cvMonoBindsAndSigs sig_cvtr fb
  = mangle_bind (EmptyMonoBinds, []) fb
  where
    mangle_bind acc RdrNullBind
      = acc

    mangle_bind acc (RdrAndBindings fb1 fb2)
      = mangle_bind (mangle_bind acc fb1) fb2

    mangle_bind (b_acc, s_acc) (RdrSig sig)
      = (b_acc, sig_cvtr sig : s_acc)

    mangle_bind (b_acc, s_acc) (RdrValBinding binding)
      = (b_acc `AndMonoBinds` binding, s_acc)
\end{code}


%************************************************************************
%*									*
\subsection[PrefixToHS-utils]{Utilities for conversion}
%*									*
%************************************************************************

Separate declarations into all the various kinds:

\begin{code}
cvTopDecls :: RdrBinding -> [RdrNameHsDecl]
cvTopDecls bind
  = let
	(top_decls, mono_binds, sigs) = go ([], EmptyMonoBinds, []) bind 
    in
    (ValD (MonoBind mono_binds sigs Recursive) : top_decls)
  where
    go acc		  RdrNullBind		 = acc
    go acc                (RdrAndBindings b1 b2) = go (go acc b1) b2
    go (topds, mbs, sigs) (RdrHsDecl d)		 = (d : topds, mbs, sigs)
    go (topds, mbs, sigs) (RdrSig (FixSig d))    = (FixD d  : topds, mbs, sigs)
    go (topds, mbs, sigs) (RdrSig sig)		 = (topds, mbs, sig:sigs)
    go (topds, mbs, sigs) (RdrValBinding bind)   = (topds, mbs `AndMonoBinds` bind, sigs)
\end{code}
