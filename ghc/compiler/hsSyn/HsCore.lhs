%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%
%************************************************************************
%*									*
\section[HsCore]{Core-syntax unfoldings in Haskell interface files}
%*									*
%************************************************************************

We could either use this, or parameterise @GenCoreExpr@ on @Types@ and
@TyVars@ as well.  Currently trying the former... MEGA SIGH.

\begin{code}
module HsCore (
	UfExpr(..), UfAlt, UfBinder(..), UfNote(..),
	UfBinding(..), UfConAlt(..),
	HsIdInfo(..), pprHsIdInfo,
	
	eq_ufExpr, eq_ufBinders, pprUfExpr, 

	toUfExpr, toUfBndr, ufBinderName
    ) where

#include "HsVersions.h"

-- friends:
import HsTypes		( HsType, pprParendHsType, pprHsTyVarBndr, toHsType,
			  HsTupCon(..), EqHsEnv, hsTupParens,
			  emptyEqHsEnv, extendEqHsEnv,
			  eq_hsType, eq_hsVars
			)

-- others:
import Id		( idArity, idType, isDataConWorkId_maybe, isFCallId_maybe )
import Var		( varType, isId )
import IdInfo		( InlinePragInfo )
import Name		( Name, NamedThing(..), eqNameByOcc )
import RdrName		( RdrName, rdrNameOcc )
import CoreSyn
import CostCentre	( pprCostCentreCore )
import NewDemand	( StrictSig, pprIfaceStrictSig )
import Literal		( Literal )
import ForeignCall	( ForeignCall )
import DataCon		( dataConTyCon, dataConSourceArity )
import TyCon		( isTupleTyCon, tupleTyConBoxity )
import Type		( Kind, eqKind )
import BasicTypes	( Arity )
import FiniteMap	( lookupFM )
import CostCentre
import Util		( eqListBy, lengthIs )
import Outputable
import FastString
\end{code}

%************************************************************************
%*									*
\subsection[HsCore-types]{Types for read/written Core unfoldings}
%*									*
%************************************************************************

\begin{code}
data UfExpr name
  = UfVar 	name
  | UfType      (HsType name)
  | UfTuple 	HsTupCon [UfExpr name]		-- Type arguments omitted
  | UfLam 	(UfBinder name)	(UfExpr name)
  | UfApp 	(UfExpr name)   (UfExpr name)
  | UfCase	(UfExpr name) name [UfAlt name]
  | UfLet	(UfBinding name)  (UfExpr name)
  | UfNote	(UfNote name) (UfExpr name)
  | UfLit	Literal
  | UfFCall	ForeignCall (HsType name)

data UfNote name = UfSCC CostCentre
	         | UfCoerce (HsType name)
	         | UfInlineCall
	         | UfInlineMe
                 | UfCoreNote String

type UfAlt name = (UfConAlt name, [name], UfExpr name)

data UfConAlt name = UfDefault
 		   | UfDataAlt name
		   | UfTupleAlt HsTupCon
		   | UfLitAlt Literal

data UfBinding name
  = UfNonRec	(UfBinder name)
		(UfExpr name)
  | UfRec 	[(UfBinder name, UfExpr name)]

data UfBinder name
  = UfValBinder	name (HsType name)
  | UfTyBinder	name Kind

ufBinderName :: UfBinder name -> name
ufBinderName (UfValBinder n _) = n
ufBinderName (UfTyBinder  n _) = n
\end{code}


%************************************************************************
%*									*
\subsection{Converting from Core to UfCore}
%*									*
%************************************************************************

\begin{code}
toUfExpr :: CoreExpr -> UfExpr Name
toUfExpr (Var v) = toUfVar v
toUfExpr (Lit l) = UfLit l
toUfExpr (Type ty) = UfType (toHsType ty)
toUfExpr (Lam x b) = UfLam (toUfBndr x) (toUfExpr b)
toUfExpr (App f a) = toUfApp f [a]
toUfExpr (Case s x as) = UfCase (toUfExpr s) (getName x) (map toUfAlt as)
toUfExpr (Let b e)     = UfLet (toUfBind b) (toUfExpr e)
toUfExpr (Note n e)    = UfNote (toUfNote n) (toUfExpr e)

---------------------
toUfNote (SCC cc)	= UfSCC cc
toUfNote (Coerce t1 _)	= UfCoerce (toHsType t1)
toUfNote InlineCall	= UfInlineCall
toUfNote InlineMe	= UfInlineMe
toUfNote (CoreNote s)   = UfCoreNote s

---------------------
toUfBind (NonRec b r) = UfNonRec (toUfBndr b) (toUfExpr r)
toUfBind (Rec prs)    = UfRec [(toUfBndr b, toUfExpr r) | (b,r) <- prs]

---------------------
toUfAlt (c,bs,r) = (toUfCon c, map getName bs, toUfExpr r)

---------------------
toUfCon (DataAlt dc) | isTupleTyCon tc = UfTupleAlt (mk_hs_tup_con tc dc)
		     | otherwise       = UfDataAlt (getName dc)
		     where
		       tc = dataConTyCon dc

toUfCon (LitAlt l)   = UfLitAlt l
toUfCon DEFAULT	     = UfDefault

---------------------
mk_hs_tup_con tc dc = HsTupCon (tupleTyConBoxity tc) (dataConSourceArity dc)

---------------------
toUfBndr x | isId x    = UfValBinder (getName x) (toHsType (varType x))
	   | otherwise = UfTyBinder  (getName x) (varType x)

---------------------
toUfApp (App f a) as = toUfApp f (a:as)
toUfApp (Var v) as
  = case isDataConWorkId_maybe v of
	-- We convert the *worker* for tuples into UfTuples
	Just dc |  isTupleTyCon tc && saturated 
		-> UfTuple (mk_hs_tup_con tc dc) tup_args
	  where
	    val_args  = dropWhile isTypeArg as
	    saturated = val_args `lengthIs` idArity v
	    tup_args  = map toUfExpr val_args
	    tc	      = dataConTyCon dc
	;

        other -> mkUfApps (toUfVar v) as

toUfApp e as = mkUfApps (toUfExpr e) as

mkUfApps = foldl (\f a -> UfApp f (toUfExpr a))

---------------------
toUfVar v = case isFCallId_maybe v of
		-- Foreign calls have special syntax
		Just fcall -> UfFCall fcall (toHsType (idType v))
		other	   -> UfVar (getName v)
\end{code}


%************************************************************************
%*									*
\subsection[HsCore-print]{Printing Core unfoldings}
%*									*
%************************************************************************

\begin{code}
instance OutputableBndr name => Outputable (UfExpr name) where
    ppr e = pprUfExpr noParens e


-- Small-hack alert: this instance allows us to do a getOccName on RdrNames.
-- Important because we want to pretty-print UfExprs, and we have to
-- print an '@' before tyvar-binders in a case alternative.
instance NamedThing RdrName where
    getOccName n = rdrNameOcc n
    getName n	 = pprPanic "instance NamedThing RdrName" (ppr n)

noParens :: SDoc -> SDoc
noParens pp = pp

pprUfExpr :: OutputableBndr name => (SDoc -> SDoc) -> UfExpr name -> SDoc
	-- The function adds parens in context that need
	-- an atomic value (e.g. function args)

pprUfExpr add_par (UfVar v)       = ppr v
pprUfExpr add_par (UfLit l)       = ppr l
pprUfExpr add_par (UfFCall cc ty) = braces (ppr cc <+> ppr ty)
pprUfExpr add_par (UfType ty)     = char '@' <+> pprParendHsType ty

pprUfExpr add_par e@(UfLam _ _)   = add_par (char '\\' <+> hsep (map (pprBndr LambdaBind) bndrs)
                                             <+> ptext SLIT("->") <+> pprUfExpr noParens body)
                                  where (bndrs,body) = collectUfBndrs e
pprUfExpr add_par app@(UfApp _ _) = add_par (pprUfApp app)
pprUfExpr add_par (UfTuple c as)  = hsTupParens c (interpp'SP as)

pprUfExpr add_par (UfCase scrut bndr alts)
      = add_par (hsep [ptext SLIT("case"), pprUfExpr noParens scrut, ptext SLIT("of"), ppr bndr,
		       braces (hsep (map pp_alt alts))])
      where
	pp_alt (UfTupleAlt tup_con, bs, rhs) = hsTupParens tup_con (interpp'SP bs) <+> ppr_rhs rhs
	pp_alt (c,		    bs, rhs) = ppr c <+> hsep (map (pprBndr CaseBind) bs) <+> ppr_rhs rhs

        ppr_rhs rhs = ptext SLIT("->") <+> pprUfExpr noParens rhs <> semi

pprUfExpr add_par (UfLet (UfNonRec b rhs) body)
      = add_par (hsep [ptext SLIT("let"), 
		       braces (pprBndr LetBind b <+> equals <+> pprUfExpr noParens rhs), 
		       ptext SLIT("in"), pprUfExpr noParens body])

pprUfExpr add_par (UfLet (UfRec pairs) body)
      = add_par (hsep [ptext SLIT("__letrec"), braces (hsep (map pp_pair pairs)), 
		       ptext SLIT("in"), pprUfExpr noParens body])
      where
	pp_pair (b,rhs) = ppr b <+> equals <+> pprUfExpr noParens rhs <> semi

pprUfExpr add_par (UfNote note body) = add_par (ppr note <+> pprUfExpr parens body)

pprUfApp (UfApp fun arg) = pprUfApp fun <+> pprUfExpr parens arg
pprUfApp fun	         = pprUfExpr parens fun

collectUfBndrs :: UfExpr name -> ([UfBinder name], UfExpr name)
collectUfBndrs expr
  = go [] expr
  where
    go bs (UfLam b e) = go (b:bs) e
    go bs e           = (reverse bs, e)

instance Outputable name => Outputable (UfNote name) where
    ppr (UfSCC cc)    = pprCostCentreCore cc
    ppr (UfCoerce ty) = ptext SLIT("__coerce") <+> pprParendHsType ty
    ppr UfInlineCall  = ptext SLIT("__inline_call")
    ppr UfInlineMe    = ptext SLIT("__inline_me")
    ppr (UfCoreNote s)= ptext SLIT("__core_note") <+> pprHsString (mkFastString s)

instance Outputable name => Outputable (UfConAlt name) where
    ppr UfDefault	   = text "__DEFAULT"
    ppr (UfLitAlt l)       = ppr l
    ppr (UfDataAlt d)	   = ppr d

instance Outputable name => Outputable (UfBinder name) where
    ppr (UfValBinder name ty)  = hsep [ppr name, dcolon, pprParendHsType ty]
    ppr (UfTyBinder name kind) = char '@' <+> pprHsTyVarBndr name kind

instance OutputableBndr name => OutputableBndr (UfBinder name) where
    pprBndr _ (UfValBinder name ty)  = hsep [ppr name, dcolon, pprParendHsType ty]
    pprBndr _ (UfTyBinder name kind) = char '@' <+> pprHsTyVarBndr name kind
\end{code}


%************************************************************************
%*									*
\subsection[HsCore-print]{Equality, for interface file checking
%*									*
%************************************************************************

	----------------------------------------
			HACK ALERT
	----------------------------------------

Whe comparing UfExprs, we compare names by converting to RdrNames and comparing
those.  Reason: this is used when comparing ufoldings in interface files, and the
uniques can differ.  Converting to RdrNames makes it more like comparing the file
contents directly.  But this is bad: version numbers can change when only alpha-conversion
has happened. 

	The hack shows up in eq_ufVar
	There are corresponding getOccName calls in MkIface.diffDecls

	----------------------------------------
			END OF HACK ALERT
	----------------------------------------


\begin{code}
instance (NamedThing name, Ord name) => Eq (UfExpr name) where
  (==) a b = eq_ufExpr emptyEqHsEnv a b

-----------------
eq_ufBinder env (UfValBinder n1 t1) (UfValBinder n2 t2) k
  = eq_hsType env t1 t2 && k (extendEqHsEnv env n1 n2)
eq_ufBinder env (UfTyBinder n1 k1) (UfTyBinder n2 k2) k
  = k1 `eqKind` k2 && k (extendEqHsEnv env n1 n2)
eq_ufBinder _ _ _ _ = False

-----------------
eq_ufBinders env []       []	   k = k env
eq_ufBinders env (b1:bs1) (b2:bs2) k = eq_ufBinder env b1 b2 (\env -> eq_ufBinders env bs1 bs2 k)
eq_ufBinders env _	  _	   _ = False

-----------------
eq_ufVar :: (NamedThing name, Ord name) => EqHsEnv name -> name -> name -> Bool
-- Compare *Rdr* names.  A real hack to avoid gratuitous 
-- differences when comparing interface files
eq_ufVar env n1 n2 = case lookupFM env n1 of
		       Just n1 -> check n1
		       Nothing -> check n1
   where
	check n1 = eqNameByOcc (getName n1) (getName n2)

-----------------
eq_ufExpr :: (NamedThing name, Ord name) => EqHsEnv name -> UfExpr name -> UfExpr name -> Bool
eq_ufExpr env (UfVar v1)	(UfVar v2)	  = eq_ufVar env v1 v2
eq_ufExpr env (UfLit l1)        (UfLit l2) 	  = l1 == l2
eq_ufExpr env (UfFCall c1 ty1)  (UfFCall c2 ty2)  = c1==c2 && eq_hsType env ty1 ty2
eq_ufExpr env (UfType ty1)      (UfType ty2)	  = eq_hsType env ty1 ty2
eq_ufExpr env (UfTuple n1 as1)  (UfTuple n2 as2)  = n1==n2 && eqListBy (eq_ufExpr env) as1 as2
eq_ufExpr env (UfLam b1 body1)  (UfLam b2 body2)  = eq_ufBinder env b1 b2 (\env -> eq_ufExpr env body1 body2)
eq_ufExpr env (UfApp f1 a1)     (UfApp f2 a2)	  = eq_ufExpr env f1 f2 && eq_ufExpr env a1 a2

eq_ufExpr env (UfCase s1 b1 as1) (UfCase s2 b2 as2)
  = eq_ufExpr env s1 s2 && 
    eqListBy (eq_ufAlt (extendEqHsEnv env b1 b2)) as1 as2
  where
    eq_ufAlt env (c1,bs1,r1) (c2,bs2,r2)
	= eq_ufConAlt env c1 c2 && eq_hsVars env bs1 bs2 (\env -> eq_ufExpr env r1 r2)

eq_ufExpr env (UfLet (UfNonRec b1 r1) x1) (UfLet (UfNonRec b2 r2) x2)
  = eq_ufExpr env r1 r2 && eq_ufBinder env b1 b2 (\env -> eq_ufExpr env x1 x2)

eq_ufExpr env (UfLet (UfRec as1) x1) (UfLet (UfRec as2) x2)
  = eq_ufBinders env bs1 bs2 (\env -> eqListBy (eq_ufExpr env) rs1 rs2 && eq_ufExpr env x1 x2)
  where
    (bs1,rs1) = unzip as1
    (bs2,rs2) = unzip as2

eq_ufExpr env (UfNote n1 r1) (UfNote n2 r2)
  = eq_ufNote n1 n2 && eq_ufExpr env r1 r2
  where
    eq_ufNote (UfSCC c1)    (UfSCC c2)    = c1==c2 
    eq_ufNote (UfCoerce t1) (UfCoerce t2) = eq_hsType env t1 t2
    eq_ufNote UfInlineCall  UfInlineCall  = True
    eq_ufNote UfInlineMe    UfInlineMe    = True
    eq_ufNote (UfCoreNote s1) (UfCoreNote s2) = s1==s2
    eq_ufNote _		    _		  = False

eq_ufExpr env _ _ = False

-----------------
eq_ufConAlt env UfDefault	    UfDefault		= True
eq_ufConAlt env (UfDataAlt n1)	    (UfDataAlt n2)	= n1==n2
eq_ufConAlt env (UfTupleAlt c1)	    (UfTupleAlt c2)	= c1==c2
eq_ufConAlt env (UfLitAlt l1)	    (UfLitAlt l2)	= l1==l2
eq_ufConAlt env _ _ = False
\end{code}


%************************************************************************
%*									*
\subsection{Rules in interface files}
%*									*
%************************************************************************

\begin{code}
pprHsIdInfo :: OutputableBndr n => [HsIdInfo n] -> SDoc
pprHsIdInfo []   = empty
pprHsIdInfo info = ptext SLIT("{-##") <+> hsep (map ppr_hs_info info) <+> ptext SLIT("##-}")

data HsIdInfo name
  = HsArity		Arity
  | HsStrictness	StrictSig
  | HsUnfold		InlinePragInfo (UfExpr name)
  | HsNoCafRefs
  | HsWorker		name Arity	-- Worker, if any see IdInfo.WorkerInfo
					-- for why we want arity here.
  deriving( Eq )
-- NB: Specialisations and rules come in separately and are
-- only later attached to the Id.  Partial reason: some are orphans.

ppr_hs_info (HsUnfold prag unf) = ptext SLIT("__U") <> ppr prag <+> parens (pprUfExpr noParens unf)
ppr_hs_info (HsArity arity)     = ptext SLIT("__A") <+> int arity
ppr_hs_info (HsStrictness str)  = ptext SLIT("__S") <+> pprIfaceStrictSig str
ppr_hs_info HsNoCafRefs		= ptext SLIT("__C")
ppr_hs_info (HsWorker w a)	= ptext SLIT("__P") <+> ppr w <+> int a
\end{code}

