%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[Inst]{The @Inst@ type: dictionaries or method instances}

\begin{code}
#include "HsVersions.h"

module Inst (
	Inst(..), InstOrigin(..), OverloadedLit(..),

	mkDict, mkMethod, mkLitInst,
	getInstUniType,
--UNUSED:	getInstLocalName,
	getInstOrigin, getDictClassAndType,
--UNUSED:	instantiateInst,
	applySubstToInst,
	apply_to_Inst,	-- not for general use, please
	extractTyVarsFromInst, extractConstrainedTyVarsFromInst,
	matchesInst,
	isTyVarDict,
--UNUSED: isNullaryTyConDict,
	instBindingRequired, instCanBeGeneralised,
	
	-- and to make the interface self-sufficient...
	Class, ClassOp, ArithSeqInfo, RenamedArithSeqInfo(..),
	Literal, InPat, RenamedPat(..), Expr, RenamedExpr(..),
	Id, Name, SrcLoc, Subst, PrimKind,
	TyVar, TyVarTemplate, TyCon, UniType, Unique, InstTemplate,
	InstanceMapper(..), ClassInstEnv(..), MatchEnv(..)
	
	IF_ATTACK_PRAGMAS(COMMA isTyVarTy)
    ) where

import AbsSyn
import AbsUniType
import Id		( eqId, applySubstToId,
			  getInstNamePieces, getIdUniType,
			  Id
			)
import InstEnv
import ListSetOps
import Maybes		( Maybe(..) )
import Outputable
import Pretty
import SrcLoc		( mkUnknownSrcLoc, SrcLoc )
import Subst		( applySubstToTy, Subst )
import Util
\end{code}


%************************************************************************
%*									*
\subsection[Inst-types]{@Inst@ types}
%*									*
%************************************************************************

An @Inst@ is either a dictionary, an instance of an overloaded
literal, or an instance of an overloaded value.  We call the latter a
``method'' even though it may not correspond to a class operation.
For example, we might have an instance of the @double@ function at
type Int, represented by

	Method 34 doubleId [Int] origin

\begin{code}
data Inst	
  = Dict
	Unique
	Class		-- The type of the dict is (c t), where
	UniType		-- c is the class and t the unitype;
	InstOrigin

  | Method
	Unique
	Id		-- (I expect) be a global, local, or ClassOpId.
			-- Inside instance decls (only) it can also be an InstId!
			-- The id needn't be completely polymorphic,
	[UniType]	-- The types to which its polymorphic tyvars
			--	should be instantiated
			--	These types may not saturate the Id's foralls.
	InstOrigin

  | LitInst
	Unique
	OverloadedLit
	UniType		-- the type at which the literal is used
	InstOrigin	-- always a literal; but more convenient to carry this around

mkDict	 = Dict
mkMethod = Method
mkLitInst= LitInst

data OverloadedLit
  = OverloadedIntegral	 Integer	-- the number
			 Id Id		-- cached fromInt, fromInteger
  | OverloadedFractional Rational	-- the number
			 Id		-- cached fromRational

{- UNUSED:
getInstLocalName (Dict _ clas _ _) = getLocalName clas
getInstLocalName (Method _ id _ _) = getLocalName id
-}

-- this is used for error messages
getDictClassAndType :: Inst -> (Class, UniType)
getDictClassAndType (Dict _ clas ty _)  = (clas, ty)

getInstUniType :: Inst -> UniType
getInstUniType (Dict _ clas ty _)  = mkDictTy clas ty
getInstUniType (LitInst _ _ ty _)  = ty
getInstUniType (Method _ id tys _)
  = instantiateTauTy (tyvars `zip` tys) tau_ty 
  where
    (tyvars, theta, tau_ty) = splitType (getIdUniType id)
	-- Note that we ignore the overloading; this is
	-- an INSTANCE of an overloaded operation
\end{code}

@applySubstToInst@ doesn't make any assumptions, but @instantiateInst@
assumes that the @Id@ in a @Method@ is fully polymorphic (ie has no free
tyvars)

\begin{code}
{- UNUSED:
instantiateInst :: [(TyVarTemplate, UniType)] -> Inst -> Inst

instantiateInst tenv (Dict uniq clas ty orig)  
  = Dict uniq clas (instantiateTy tenv ty) orig

instantiateInst tenv (Method uniq id tys orig) 
  = --False:ASSERT(idHasNoFreeTyVars id)
    Method uniq id (map (instantiateTy tenv) tys) orig

instantiateInst tenv (LitInst u lit ty orig)
  = LitInst u lit (instantiateTy tenv ty) orig
-}

-----------------------------------------------------------------
-- too bad we can't use apply_to_Inst

applySubstToInst subst (Dict uniq clas ty orig) 
  = case (applySubstToTy subst ty) of { (s2, new_ty) ->
    (s2, Dict uniq clas new_ty orig) }

applySubstToInst subst (Method uniq id tys orig) 
  -- NB: *must* zap "id" in the typechecker
  = case (applySubstToId subst id)  	    of { (s2, new_id)  ->
    case (mapAccumL applySubstToTy s2 tys)  of { (s3, new_tys) ->
    (s3, Method uniq new_id new_tys orig) }}

applySubstToInst subst (LitInst u lit ty orig)
  = case (applySubstToTy subst ty) of { (s2, new_ty) ->
    (s2, LitInst u lit new_ty orig) }

-----------------------------------------------------------------
apply_to_Inst :: (UniType -> UniType) -> Inst -> Inst

apply_to_Inst ty_fn (Dict uniq clas ty orig) 
  = Dict uniq clas (ty_fn ty) orig

apply_to_Inst ty_fn (Method uniq id tys orig) 
  = --FALSE: ASSERT(idHasNoFreeTyVars id)
    Method uniq id (map ty_fn tys) orig

apply_to_Inst ty_fn (LitInst u lit ty orig)
  = LitInst u lit (ty_fn ty) orig
\end{code}

\begin{code}
extractTyVarsFromInst, extractConstrainedTyVarsFromInst :: Inst -> [TyVar]

extractTyVarsFromInst (Dict _ _ ty _)    = extractTyVarsFromTy  ty
extractTyVarsFromInst (Method _ _ tys _) = extractTyVarsFromTys tys
extractTyVarsFromInst (LitInst _ _ ty _) = extractTyVarsFromTy  ty

extractConstrainedTyVarsFromInst (Dict _ _ ty _)    = extractTyVarsFromTy  ty
extractConstrainedTyVarsFromInst (LitInst _ _ ty _) = extractTyVarsFromTy  ty

-- `Method' is different!
extractConstrainedTyVarsFromInst (Method _ m tys _)
  = foldr unionLists [] (zipWith xxx tvs tys)
  where
    (tvs,theta,tau_ty) = splitType (getIdUniType m)

    constrained_tvs
      = foldr unionLists [] [extractTyVarTemplatesFromTy t | (_,t) <- theta ]

    xxx tv ty | tv `elem` constrained_tvs = extractTyVarsFromTy ty
	      | otherwise	          = []
\end{code}

@matchesInst@ checks when two @Inst@s are instances of the same
thing at the same type, even if their uniques differ.

\begin{code}
matchesInst :: Inst -> Inst -> Bool
matchesInst (Dict _ clas1 ty1 _) (Dict _ clas2 ty2 _)
  = clas1 == clas2 && ty1 == ty2
matchesInst (Method _ id1 tys1 _) (Method _ id2 tys2 _)
  = id1 `eqId` id2 && tys1 == tys2
matchesInst (LitInst _ lit1 ty1 _) (LitInst _ lit2 ty2 _)
  = lit1 `eq` lit2 && ty1 == ty2
  where
    (OverloadedIntegral   i1 _ _) `eq` (OverloadedIntegral   i2 _ _) = i1 == i2
    (OverloadedFractional f1 _)   `eq` (OverloadedFractional f2 _)   = f1 == f2
    _			          `eq` _			     = False
    
matchesInst other1 other2 = False
\end{code}


\begin{code}
isTyVarDict :: Inst -> Bool
isTyVarDict (Dict _ _ ty _) = isTyVarTy ty
isTyVarDict other 	    = False

{- UNUSED:
isNullaryTyConDict :: Inst -> Bool
isNullaryTyConDict (Dict _ _ ty _)
  = case (getUniDataTyCon_maybe ty) of
      Just (tycon, [], _)   -> True		-- NB null args to tycon
      other 		    -> False
-}
\end{code}

Two predicates which deal with the case where 
class constraints don't necessarily result in bindings.
The first tells whether an @Inst@ must be witnessed by an
actual binding; the second tells whether an @Inst@ can be
generalised over.

\begin{code}
instBindingRequired :: Inst -> Bool
instBindingRequired inst
  = case get_origin_really inst of
	CCallOrigin _ _ _ -> False	-- No binding required
	LitLitOrigin  _ _ -> False
	other             -> True

instCanBeGeneralised :: Inst -> Bool
instCanBeGeneralised inst
  = case get_origin_really inst of
	CCallOrigin _ _ _ -> False	-- Can't be generalised
	LitLitOrigin  _ _ -> False	-- Can't be generalised
	other             -> True
\end{code}

ToDo: improve these pretty-printing things.  The ``origin'' is really only
relevant in error messages.

\begin{code}
-- ToDo: this instance might be nukable (maybe not: used for error msgs)

instance Outputable Inst where
    ppr PprForUser (LitInst _ lit _ _)
      = case lit of
	  OverloadedIntegral   i _ _ -> ppInteger i
#if __GLASGOW_HASKELL__ <= 22
	  OverloadedFractional f _   -> ppDouble (fromRational f) -- ToDo: better
#else
	  OverloadedFractional f _   -> ppRational f
#endif

    ppr sty inst
      = ppIntersperse (ppChar '.') (map ppPStr (getInstNamePieces True inst))
\end{code}
  

%************************************************************************
%*									*
\subsection[Inst-origin]{The @InstOrigin@ type}
%*									*
%************************************************************************

The @InstOrigin@ type gives information about where a dictionary came from.
This is important for decent error message reporting because dictionaries
don't appear in the original source code.  Doubtless this type will evolve...

\begin{code}
data InstOrigin
  = OccurrenceOf	Id	-- Occurrence of an overloaded identifier
			SrcLoc

  | InstanceDeclOrigin	SrcLoc	-- Typechecking an instance decl

  | LiteralOrigin	Literal	-- Occurrence of a literal
			SrcLoc	-- (now redundant? ToDo)

  | ArithSeqOrigin	RenamedArithSeqInfo -- [x..], [x..y] etc
			SrcLoc

  | SignatureOrigin		-- A dict created from a type signature
				-- I don't expect this ever to appear in 
				-- an error message so I can't be bothered
				-- to give it a source location...

  | ClassDeclOrigin	SrcLoc	-- Manufactured during a class decl

  | DerivingOrigin	InstanceMapper
			Class
			Bool	-- True <=> deriving for *functions*;
				-- do *not* look at the TyCon! [WDP 94/09]
			TyCon
			SrcLoc

	-- During "deriving" operations we have an ever changing
	-- mapping of classes to instances, so we record it inside the
	-- origin information.  This is a bit of a hack, but it works
	-- fine.  (Simon is to blame [WDP].)

  | InstanceSpecOrigin	InstanceMapper
			Class	-- in a SPECIALIZE instance pragma
			UniType
			SrcLoc

	-- When specialising instances the instance info attached to
	-- each class is not yet ready, so we record it inside the
	-- origin information.  This is a bit of a hack, but it works
	-- fine.  (Patrick is to blame [WDP].)
  
  | DefaultDeclOrigin	SrcLoc	-- Related to a `default' declaration

  | ValSpecOrigin	Name	-- in a SPECIALIZE pragma for a value
			SrcLoc

	-- Argument or result of a ccall
	-- Dictionaries with this origin aren't actually mentioned in the
	-- translated term, and so need not be bound.  Nor should they
	-- be abstracted over.
  | CCallOrigin		SrcLoc
			String			-- CCall label
			(Maybe RenamedExpr)	-- Nothing if it's the result
						-- Just arg, for an argument

  | LitLitOrigin	SrcLoc
			String	-- the litlit

  | UnknownOrigin	-- Help! I give up...
\end{code}

\begin{code}
get_origin_really (Dict   u clas ty origin) = origin
get_origin_really (Method u clas ty origin) = origin
get_origin_really (LitInst u lit ty origin) = origin

getInstOrigin inst
  = let origin = get_origin_really inst
    in  get_orig origin
  where
    get_orig :: InstOrigin -> (SrcLoc, PprStyle -> Pretty)

    get_orig (OccurrenceOf id loc)
      = (loc, \ sty -> ppBesides [ppPStr SLIT("at a use of an overloaded identifier: `"),
				  ppr sty id, ppChar '\''])
    get_orig (InstanceDeclOrigin loc)
      = (loc, \ sty -> ppStr "in an instance declaration")
    get_orig (LiteralOrigin lit loc) 
      = (loc, \ sty -> ppCat [ppStr "at an overloaded literal:", ppr sty lit])
    get_orig (ArithSeqOrigin seq loc)
      = (loc, \ sty -> ppCat [ppStr "at an arithmetic sequence:", ppr sty seq])
    get_orig SignatureOrigin
      = (mkUnknownSrcLoc, \ sty -> ppStr "in a type signature")
    get_orig (ClassDeclOrigin loc)
      = (loc, \ sty -> ppStr "in a class declaration")
    get_orig (DerivingOrigin _ clas is_function tycon loc)
      = (loc, \ sty -> ppBesides [ppStr "in a `deriving' clause; class \"",
				  ppr sty clas,
				  if is_function
				  then ppStr "\"; type: functions"
				  else ppBeside (ppStr "\"; offending type \"") (ppr sty tycon),
				  ppStr "\""])
    get_orig (InstanceSpecOrigin _ clas ty loc)
      = (loc, \ sty -> ppBesides [ppStr "in a SPECIALIZE instance pragma; class \"",
				  ppr sty clas, ppStr "\" type: ", ppr sty ty])
    get_orig (DefaultDeclOrigin loc)
      = (loc, \ sty -> ppStr "in a `default' declaration")
    get_orig (ValSpecOrigin name loc)
      = (loc, \ sty -> ppBesides [ppStr "in a SPECIALIZE user-pragma for `",
				  ppr sty name, ppStr "'"])
    get_orig (CCallOrigin loc clabel Nothing{-ccall result-})
      = (loc, \ sty -> ppBesides [ppStr "in the result of the _ccall_ to `",
			ppStr clabel, ppStr "'"])
    get_orig (CCallOrigin loc clabel (Just arg_expr))
      = (loc, \ sty -> ppBesides [ppStr "in an argument in the _ccall_ to `",
			ppStr clabel, ppStr "', namely: ", ppr sty arg_expr])
    get_orig (LitLitOrigin loc s)
      = (loc, \ sty -> ppBesides [ppStr "in this ``literal-literal'': ", ppStr s])
    get_orig UnknownOrigin
      = (mkUnknownSrcLoc, \ sty -> ppStr "in... oops -- I don't know where the overloading came from!")
\end{code}
