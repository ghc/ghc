%
% (c) The AQUA Project, Glasgow University, 1994-1995
%
\section[ErrsTc]{Reporting errors from the typechecker}

This is an internal module---access to these functions is through
@Errors@.

DPH errors are in here, too.

\begin{code}
#include "HsVersions.h"

module ErrsTc (
	UnifyErrContext(..), UnifyErrInfo(..),

	ambigErr,
	badMatchErr,
	badSpecialisationErr,
	classCycleErr,
	confusedNameErr,
	dataConArityErr,
	defaultErr,
	derivingEnumErr,
	derivingIxErr,
	derivingWhenInstanceExistsErr,
	dupInstErr,
	genCantGenErr,
	instTypeErr,
	lurkingRank2Err,
	methodTypeLacksTyVarErr,
	naughtyCCallContextErr,
	noInstanceErr,
	nonBoxedPrimCCallErr,
	notAsPolyAsSigErr,
	preludeInstanceErr,
	reduceErr,
	sigContextsErr,
	specCtxtGroundnessErr,
	specDataNoSpecErr,
	specDataUnboxedErr,
	specGroundnessErr,
	specInstUnspecInstNotFoundErr,
	topLevelUnboxedDeclErr,
	tyConArityErr,
	typeCycleErr,
	underAppliedTyErr,
	unifyErr,
	varyingArgsErr
    ) where

import AbsSyn		-- we print a bunch of stuff in here
import UniType		( UniType(..) )		-- Concrete, to make some errors
						-- more informative.
import ErrUtils
import AbsUniType	( extractTyVarsFromTy, pprMaybeTy,
			  TyVar, TyVarTemplate, TyCon,
			  TauType(..), Class, ClassOp
			  IF_ATTACK_PRAGMAS(COMMA pprUniType)
			)
import Bag		( Bag, bagToList )
import GenSpecEtc	( SignatureInfo(..) )
import HsMatches	( pprMatches, pprMatch, pprGRHS )
import Id		( getIdUniType, Id, isSysLocalId )
import Inst		( getInstOrigin, getDictClassAndType, Inst )
import Name		( cmpName )
import Outputable
import Pretty		-- to pretty-print error messages
#ifdef DPH
import PodizeMonad	( PodWarning(..) )
#endif {- Data Parallel Haskell -}
import SrcLoc		( mkUnknownSrcLoc, SrcLoc )
import Util
\end{code}

\begin{code}
ambigErr :: [Inst] -> Error
ambigErr insts@(inst1:_)
  = addErrLoc loc1 "Ambiguous overloading" ( \ sty ->
    ppAboves (map (ppr_inst sty) insts) )
  where
    (loc1, _) = getInstOrigin inst1

ppr_inst sty inst
  = let
	(clas, ty)  = getDictClassAndType inst
	(locn, msg) = getInstOrigin inst
    in
    ppSep [ ppBesides [ppStr "class `", ppr sty clas,
		       ppStr "', type `", ppr sty ty, ppStr "'"],
	    ppBesides [ppStr "(", msg sty, ppStr ")"] ]

----------------------------------------------------------------
badMatchErr :: UniType -> UniType -> UnifyErrContext -> SrcLoc -> Error
badMatchErr sig_ty inferred_ty ctxt locn
  = addErrLoc locn "Type signature mismatch" ( \ sty ->
    let
	thing
	  = case ctxt of 
	      SigCtxt id _     -> ppBesides [ppChar '`', ppr sty id, ppChar '\'']
	      MethodSigCtxt op _ -> ppBesides [ppStr "class method `", ppr sty op, ppStr "'"]
	      ExprSigCtxt _ _  -> ppStr "an expression"
	      Rank2ArgCtxt _ _ -> ppStr "an expression with rank-2 polymorphic type(!)"
	      ctxt  	       -> pprUnifyErrContext sty ctxt
		-- the latter is ugly, but better than a patt-match failure
    in
    ppAboves [ppSep [
		ppStr "Signature for", thing, ppStr "doesn't match its inferred type."
	      ],
	      ppHang (ppStr "Signature:") 4 (ppr sty sig_ty),
	      ppHang (ppStr "Inferred type:") 4 (ppr sty inferred_ty)
    ] )

----------------------------------------------------------------
badSpecialisationErr :: String -> String -> Int -> [Maybe UniType] -> SrcLoc -> Error

badSpecialisationErr flavor messg no_tyvars ty_maybes locn
  = addErrLoc locn ("Bad "++flavor++" specialisation pragma: "++messg)  ( \ sty ->
    ppStr "MSG NOT DONE YET"
    )

----------------------------------------------------------------
confusedNameErr :: String
		-> Name		-- the confused name
		-> SrcLoc
		-> Error
confusedNameErr msg nm locn
  = addErrLoc locn msg ( \ sty ->
    ppr sty nm )
{-
  where
    msg = if flag then "Type constructor used where a class is expected"
		  else "Class used where a type constructor is expected"
-}

----------------------------------------------------------------
typeCycleErr :: [[(Pretty, SrcLoc)]] -> Error
typeCycleErr = cycleErr  "The following type synonyms refer to themselves:"

classCycleErr :: [[(Pretty, SrcLoc)]] -> Error
classCycleErr = cycleErr  "The following classes form a cycle:"

cycleErr :: String -> [[(Pretty, SrcLoc)]] -> Error
cycleErr msg cycles sty
 = ppHang (ppStr msg)
	4 (ppAboves (map pp_cycle cycles))
 where
   pp_cycle things	= ppAboves (map pp_thing things)
   pp_thing (thing,loc) = ppHang (ppBesides [ppr PprForUser loc, ppStr ": "]) 4 thing

----------------------------------------------------------------
defaultErr :: [Inst]{-dicts-} -> [UniType] -> Error
	-- when default-resolution fails...

defaultErr dicts defaulting_tys sty
  = ppHang (ppStr "Ambiguously-overloaded types could not be resolved:")
	 4 (ppAboves [
	     ppHang (ppStr "Conflicting:")
	          4 (ppInterleave ppSemi (map (ppr_inst sty) dicts)),
	     ppHang (ppStr "Defaulting types :")
		  4 (ppr sty defaulting_tys),
	     ppStr "([Int, Double] is the default list of defaulting types.)" ])

----------------------------------------------------------------
derivingEnumErr :: TyCon -> Error
derivingEnumErr tycon
  = addErrLoc (getSrcLoc tycon) "Can't derive an instance of `Enum'" ( \ sty ->
    ppBesides [ppStr "type `", ppr sty tycon, ppStr "'"] )

----------------------------------------------------------------
derivingIxErr :: TyCon -> Error
derivingIxErr tycon
  = addErrLoc (getSrcLoc tycon) "Can't derive an instance of `Ix'" ( \ sty ->
    ppBesides [ppStr "type `", ppr sty tycon, ppStr "'"] )

----------------------------------------------------------------
derivingWhenInstanceExistsErr :: Class -> TyCon -> Error
derivingWhenInstanceExistsErr clas tycon
  = addErrLoc (getSrcLoc tycon) "`deriving' when an instance also exists" ( \ sty ->
    ppBesides [ppStr "class `", ppr sty clas,
	       ppStr "', type `", ppr sty tycon, ppStr "'"] )

----------------------------------------------------------------
{- UNUSED:
derivingNoSuperClassInstanceErr :: Class -> TyCon -> Class -> Error
derivingNoSuperClassInstanceErr clas tycon super_class
  = addErrLoc (getSrcLoc tycon) "No instance for a superclass in a `deriving'" ( \ sty ->
    ppSep [ppBesides [ppStr "the superclass `", ppr sty super_class, ppStr "' has no instance"],
	   ppBesides [ppStr "at the type `", ppr sty tycon, ppStr "';"],
	   ppBesides [ppStr "(the class being \"derived\" is `", ppr sty clas, ppStr "')"]
    	  ])
-}

----------------------------------------------------------------
dupInstErr :: (Class, (UniType, SrcLoc), (UniType, SrcLoc)) -> Error
dupInstErr (clas, info1@(ty1, locn1), info2@(ty2, locn2))
	-- Overlapping/duplicate instances for given class; msg could be more glamourous
  = addErrLoc locn1 "Duplicate/overlapping instances" ( \ sty ->
    ppSep [ ppBesides [ppStr "class `", ppr sty clas, ppStr "',"],
	    showOverlap sty info1 info2] )

----------------------------------------------------------------
{- UNUSED?
extraMethodsErr :: [Id] {-dicts-} -> SrcLoc -> Error
	-- when an instance decl has binds for methods that aren't in the class decl
extraMethodsErr extra_methods locn
  = addErrLoc locn "Extra methods in instance declaration" ( \ sty ->
    interpp'SP sty extra_methods )
-}

----------------------------------------------------------------
genCantGenErr :: [Inst] -> Error
genCantGenErr insts@(inst1:_)
  = addErrLoc loc1 "Cannot generalise these overloadings (in a _ccall_):" ( \ sty ->
    ppAboves (map (ppr_inst sty) insts) )
  where
    (loc1, _) = getInstOrigin inst1

----------------------------------------------------------------
{- UNUSED:
genPrimTyVarErr :: [TyVar] -> SrcLoc -> Error
	-- Attempt to generalise over a primitive type variable

genPrimTyVarErr tyvars locn
  = addErrLoc locn "These primitive type variables can't be made more general" ( \ sty ->
    	ppAbove (interpp'SP sty tyvars)
		(ppStr "(Solution: add a type signature.)") )
-}
----------------------------------------------------------------
noInstanceErr :: Inst -> Error
noInstanceErr inst
  = let (clas, ty)  = getDictClassAndType inst
	(locn, msg) = getInstOrigin inst
    in
    addErrLoc locn "No such instance" ( \ sty ->
    ppSep [ ppBesides [ppStr "class `", ppr sty clas,
		       ppStr "', type `", ppr sty ty, ppStr "'"],
	    ppBesides [ppStr "(", msg sty, ppStr ")"] ]
    )

----------------------------------------------------------------
{- UNUSED:
instOpErr :: Id -> Class -> TyCon -> Error

instOpErr dict clas tycon
	-- no instance of "Class" for "TyCon"
	-- the Id is the offending dictionary; has src location
	-- (and we could get the Class and TyCon from it, but
	-- since we already have it at hand ...)
  = addErrLoc (getSrcLoc dict) "Invalid instance" ( \ sty ->
    ppBesides [ ppStr "There is no instance of `", ppr sty tycon,
		ppStr "' for class `",
		ppr sty clas, ppChar '\'' ] )
-}

----------------------------------------------------------------
instTypeErr :: UniType -> SrcLoc -> Error
instTypeErr ty locn
  = addShortErrLocLine locn (\ sty ->
    let
	rest_of_msg = ppStr "' cannot be used as the instance type\n    in an instance declaration."
    in
    case ty of
      UniSyn tc _ _ -> ppBesides [ppStr "The type synonym `", ppr sty tc, rest_of_msg]
      UniTyVar tv   -> ppBesides [ppStr "The type variable `", ppr sty tv, rest_of_msg]
      other	    -> ppBesides [ppStr "The type `", ppr sty ty, rest_of_msg]
    )

----------------------------------------------------------------
lurkingRank2Err :: Name -> UniType -> SrcLoc -> Error
lurkingRank2Err name ty locn
  = addErrLoc locn "Illegal use of a non-Hindley-Milner variable" ( \ sty ->
    ppAboves [
      ppBesides [ppStr "The variable is `", ppr sty name, ppStr "'."],
      ppStr "Its type does not have all its for-alls at the top",
      ppBesides [ppStr "(the type is `", ppr sty ty, ppStr "'),"],
      ppStr "nor is it a full application of a rank-2-typed variable.",
      ppStr "(Most common cause: `_runST' or `_build' not applied to an argument.)"])

----------------------------------------------------------------
{- UNUSED:
methodInstErr :: (ClassOp, (UniType, SrcLoc), (UniType, SrcLoc)) -> Error
methodInstErr (class_op, info1, info2) sty
	-- Two instances for given class op
  = ppHang (ppBesides [ ppStr "The class method `", ppr sty class_op, ppStr "' has been given more than one definition for"])
	4 (showOverlap sty info1 info2)
-}

showOverlap :: PprStyle -> (UniType, SrcLoc) -> (UniType, SrcLoc) -> Pretty
showOverlap sty (ty1,loc1) (ty2,loc2)
  = ppSep [ppBesides [ppStr "type `", ppr sty ty1, ppStr "'"],
	   ppBeside (ppStr "at ") (ppr sty loc1),
	   ppBeside (ppStr "and ") (ppr sty loc2)]

----------------------------------------------------------------
methodTypeLacksTyVarErr :: TyVarTemplate -> String -> SrcLoc -> Error
methodTypeLacksTyVarErr tyvar method_name locn
  = addErrLoc locn "Method's type doesn't mention the class type variable"  (\ sty ->
    ppAboves [ppBeside (ppStr "Class type variable: ") (ppr sty tyvar),
	      ppBeside (ppStr "Method: ") (ppStr method_name)] )

----------------------------------------------------------------
{- UNUSED:
missingClassOpErr :: Id -> [ClassOp] -> SrcLoc -> Error
missingClassOpErr op classops locn
  = addErrLoc locn "Undefined class method" ( \ sty ->
    ppBesides [ ppr sty op, ppStr "; valid method(s):",
		interpp'SP sty classops ] )
-}

----------------------------------------------------------------
naughtyCCallContextErr :: Name -> SrcLoc -> Error
naughtyCCallContextErr clas_name locn
  = addErrLoc locn "Can't use this class in a context" (\ sty ->
    ppr sty clas_name )

----------------------------------------------------------------
nonBoxedPrimCCallErr :: Class -> UniType -> SrcLoc -> Error
nonBoxedPrimCCallErr clas inst_ty locn
  = addErrLoc locn "Instance isn't for a `boxed-primitive' type" ( \ sty ->
    ppBesides [ ppStr "class `", ppr sty clas, ppStr "'; type `",
    		ppr sty inst_ty, ppStr "'"] )

----------------------------------------------------------------
notAsPolyAsSigErr :: UniType -> [TyVar] -> UnifyErrContext -> SrcLoc -> Error
notAsPolyAsSigErr sig_ty mono_tyvars ctxt locn
  = addErrLoc locn "A type signature is more polymorphic than the inferred type" ( \ sty ->
    ppAboves [  ppStr "(That is, one or more type variables in the inferred type can't be forall'd.)",
	        pprUnifyErrContext sty ctxt,
		ppHang (ppStr "Monomorphic type variable(s):")
		   4 (interpp'SP sty mono_tyvars),
		ppStr "Possible cause: the RHS mentions something subject to the monomorphism restriction"
	] )

----------------------------------------------------------------
{- UNUSED:
patMatchWithPrimErr :: Error
patMatchWithPrimErr
  = dontAddErrLoc
	"Pattern-bindings may not involve primitive types." ( \ sty ->
	ppNil )
-}

----------------------------------------------------------------
preludeInstanceErr :: Class -> UniType -> SrcLoc -> Error
preludeInstanceErr clas ty locn
  = addShortErrLocLine locn ( \ sty ->
    ppHang (ppBesides [ppStr "Illegal instance: for Prelude class `", ppr sty clas,
	      		 ppStr "' and Prelude type `", ppr sty ty, ppStr "'."] )
	 4 (ppStr "(An instance decl must be in the same module as the type decl or the class decl)") )

----------------------------------------------------------------
{- UNUSED:
purelyLocalErr :: Name -> SrcLoc -> Error
purelyLocalErr thing locn
  = addShortErrLocLine locn ( \ sty ->
    ppBesides [ppStr "`", ppr sty thing,
	       ppStr "' cannot be exported -- it would refer to an unexported local entity."] )
-}

----------------------------------------------------------------
reduceErr :: [Inst] -> UnifyErrContext -> Error
	-- Used by tcSimplifyCheckLIE
	-- Could not express required dictionaries in terms of the signature
reduceErr insts ctxt
  = dontAddErrLoc "Type signature lacks context required by inferred type" ( \ sty ->
    ppAboves [
	pprUnifyErrContext sty ctxt,
	ppHang (ppStr "Context reqd: ")
	     4 (ppAboves (map (ppr_inst sty) insts))
    ])
  where
    ppr_inst sty inst
      = let (clas, ty)  = getDictClassAndType inst
	    (locn, msg) = getInstOrigin inst
	in
	ppSep [ ppBesides [ppr sty locn, ppStr ": ", ppr sty clas, ppSP, ppr sty ty],
		ppBesides [ppStr "(", msg sty, ppStr ")"] ]

----------------------------------------------------------------
{-
unexpectedPreludeThingErr :: Outputable a => String -> a -> SrcLoc -> Error

unexpectedPreludeThingErr category thing locn
  = addShortErrLocLine locn ( \ sty ->
    ppBesides [ppStr "Prelude ", ppStr category,
	       ppStr " not expected here: ", ppr sty thing])
-}

----------------------------------------------------------------
specGroundnessErr :: UnifyErrContext -> [UniType] -> Error

specGroundnessErr (ValSpecSigCtxt name spec_ty locn) arg_tys
  = addShortErrLocLine locn ( \ sty ->
    ppHang (
    	ppSep [ppStr "In the SPECIALIZE pragma for `", ppr sty name,
	       ppStr "'... not all type variables were specialised",
	       ppStr "to type variables or ground types (nothing in between, please!):"])
      4 (ppAboves (map (ppr sty) arg_tys))
    )

specGroundnessErr (ValSpecSpecIdCtxt name spec_ty spec locn) arg_tys
  = addShortErrLocLine locn ( \ sty ->
    ppHang (
        ppSep [ppBesides [ppStr "In the SPECIALIZE pragma for `", ppr sty name, ppStr "'"],
	       ppBesides [ppStr "... type of explicit id `", ppr sty spec, ppStr "'"],
	       ppStr "... not all type variables were instantiated",
	       ppStr "to type variables or ground types (nothing in between, please!):"])
      4 (ppAboves (map (ppr sty) arg_tys))
    )

----------------------------------------------------------------
specCtxtGroundnessErr :: UnifyErrContext -> [Inst] -> Error

specCtxtGroundnessErr err_ctxt dicts
  = addShortErrLocLine locn ( \ sty ->
    ppHang (
    	ppSep [ppBesides [ppStr "In the SPECIALIZE pragma for `", ppr sty name, ppStr "'"],
	       ppBesides [ppStr " specialised to the type `", ppr sty spec_ty,  ppStr "'"],
	       pp_spec_id sty,
	       ppStr "... not all overloaded type variables were instantiated",
	       ppStr "to ground types:"])
      4 (ppAboves [ppCat [ppr sty c, ppr sty t]
		  | (c,t) <- map getDictClassAndType dicts])
    )
  where
    (name, spec_ty, locn, pp_spec_id)
      = case err_ctxt of
	  ValSpecSigCtxt    n ty loc      -> (n, ty, loc, \ x -> ppNil)
	  ValSpecSpecIdCtxt n ty spec loc ->
	    (n, ty, loc,
	     \ sty -> ppBesides [ppStr "... type of explicit id `", ppr sty spec, ppStr "'"])

----------------------------------------------------------------
specDataNoSpecErr :: Name -> [UniType] -> SrcLoc -> Error

specDataNoSpecErr name arg_tys locn
  = addShortErrLocLine locn ( \ sty ->
    ppHang (
    	ppSep [ppBesides [ppStr "In the SPECIALIZE pragma for `", ppr sty name, ppStr "'"],
	       ppStr "... no unboxed type arguments in specialisation:"])
     4 (ppAboves (map (ppr sty) arg_tys))
    )

----------------------------------------------------------------
specDataUnboxedErr :: Name -> [UniType] -> SrcLoc -> Error

specDataUnboxedErr name arg_tys locn
  = addShortErrLocLine locn ( \ sty ->
    ppHang (
    	ppSep [ppBesides [ppStr "In the SPECIALIZE pragma for `", ppr sty name, ppStr "'"],
	       ppStr "... not all type arguments were specialised to",
	       ppStr "specific unboxed types or (boxed) type variables:"])
      4 (ppAboves (map (ppr sty) arg_tys))
    )

----------------------------------------------------------------
specInstUnspecInstNotFoundErr :: Class -> UniType -> SrcLoc -> Error

specInstUnspecInstNotFoundErr clas inst_ty locn
  = addErrLoc locn "No local instance to specialise" ( \ sty ->
    ppBesides [ ppStr "class `", ppr sty clas, ppStr "' at the type `",
    		ppr sty inst_ty, ppStr "'"] )

----------------------------------------------------------------
-- The type signatures on a mutually-recursive group of definitions
-- must all have the same context (or none).  For example:
--	f :: Eq a => ...
--	g :: (Eq a, Text a) => ...
-- is illegal if f and g are mutually recursive.  This also
-- applies to variables bound in the same pattern binding.

sigContextsErr :: [SignatureInfo] -> Error

sigContextsErr infos
  = dontAddErrLoc "A group of type signatures have mismatched contexts" ( \ sty ->
    ppAboves (map (ppr_sig_info sty) infos) )
  where
    ppr_sig_info sty (TySigInfo val tyvars insts tau_ty _)
      = ppHang (ppBeside (ppr sty val) (ppStr " :: "))
    	     4 (ppHang (if null insts
			then ppNil
			else ppBesides [ppStr "(", ppInterleave ppComma (map (ppr_inst sty) insts), ppStr ") => "])
		     4 (ppr sty tau_ty))

    ppr_inst sty inst
      = let (clas, ty)  = getDictClassAndType inst
	    (locn, msg) = getInstOrigin inst
	in
	ppCat [ppr sty clas, ppr sty ty]

----------------------------------------------------------------
topLevelUnboxedDeclErr :: Id -> SrcLoc -> Error
	-- Top level decl of something with a primitive type

topLevelUnboxedDeclErr id locn
  = addShortErrLocLine locn ( \ sty ->
	ppBesides [ppStr "The top-level value `", ppr sty id, ppStr "' shouldn't have an unboxed type." ])

----------------------------------------------------------------
dataConArityErr :: Id   -> Int -> Int -> SrcLoc -> Error
tyConArityErr   :: Name -> Int -> Int -> SrcLoc -> Error

tyConArityErr   = arityError "Type"
dataConArityErr = arityError "Constructor"

arityError kind name n m locn = 
    addErrLoc locn errmsg
    (\ sty ->
    ppBesides [ ppStr "`", ppr sty name, ppStr "' should have ",
		n_arguments, ppStr ", but has been given ", ppInt m, ppChar '.'])
    where
	errmsg = kind ++ " has too " ++ quantity ++ " arguments"
	quantity | m < n     = "few"
		 | otherwise = "many"
	n_arguments | n == 0 = ppStr "no arguments"
		    | n == 1 = ppStr "1 argument"
		    | True   = ppCat [ppInt n, ppStr "arguments"]

----------------------------------------------------------------
underAppliedTyErr :: UniType -> SrcLoc -> Error
underAppliedTyErr ty locn
  = addErrLoc locn "A for-all type has been applied to too few arguments" ( \ sty ->
    ppAboves [
      ppBesides [ppStr "The type is `", ppr sty ty, ppStr "';"],
      ppStr "This might be because of a GHC bug; feel free to report",
      ppStr "it to glasgow-haskell-bugs@dcs.glasgow.ac.uk."])

----------------------------------------------------------------
unifyErr :: UnifyErrInfo -> UnifyErrContext -> SrcLoc -> Error

unifyErr unify_err_info unify_err_context locn
  = addShortErrLocLine locn ( \ sty ->
    pprUnifyErrInfo sty unify_err_info unify_err_context)

----------------------------------------------------------------
varyingArgsErr :: Name -> [RenamedMatch] -> Error
	-- Different number of arguments in different equations

varyingArgsErr name matches
  = dontAddErrLoc "Varying number of arguments for function" ( \ sty ->
    ppr sty name )
{-
varyingArgsErr name matches
  = addErrLoc locn "Function Definition Error" ( \ sty ->
	ppBesides [ppStr "Function `", ppr sty name, ppStr "' should have a fixed number of arguments" ])
-}
\end{code}

%************************************************************************
%*									*
\subsection[UnifyErr-types]{@UnifyErrInfo@ and @UnifyErrContext@ datatypes}
%*									*
%************************************************************************

Here are the things that can go wrong during unification:

\begin{code}
data UnifyErrInfo
  = UnifyMisMatch	UniType UniType
  | TypeRec		TyVar   TauType		-- Occurs check failure

  | UnifyListMisMatch	[TauType] [TauType]   	-- Args to unifyList: diff lengths
					      	-- produces system error

  | UnifyUnboxedMisMatch UniType UniType	-- No unboxed specialisation

\end{code}

@UnifyErrContext@ gives some context for unification
errors found in expressions.  Also see the @UnifyErrInfo@ type (above),
as well as the general error-reporting type @Error@ (in @TcErrors@).
\begin{code}
data UnifyErrContext
  = PredCtxt		RenamedExpr
  | AppCtxt		RenamedExpr RenamedExpr

  | TooManyArgsCtxt	RenamedExpr	-- The offending function
					-- We don't want the typechecked expr here,
					-- because that may be full of 
					-- confusing dictionaries

  | FunAppCtxt		RenamedExpr	-- The offending function
			(Maybe Id)	-- same info (probably) in a more convenient form
			RenamedExpr	-- The offending arg
			UniType		-- Expected type of offending arg
			UniType		-- Inferred type for offending arg
			Int		-- Which arg number (first is 1)

  | OpAppCtxt		RenamedExpr RenamedExpr RenamedExpr
  | SectionLAppCtxt	RenamedExpr RenamedExpr
  | SectionRAppCtxt	RenamedExpr RenamedExpr
  | CaseCtxt		RenamedExpr [RenamedMatch]
  | BranchCtxt		RenamedExpr RenamedExpr
  | ListCtxt		[RenamedExpr]
  | PatCtxt		RenamedPat
  | CaseBranchesCtxt	[RenamedMatch]
  | FilterCtxt		RenamedExpr
  | GeneratorCtxt	RenamedPat RenamedExpr
  | GRHSsBranchCtxt	[RenamedGRHS]
  | GRHSsGuardCtxt	RenamedExpr
  | PatMonoBindsCtxt	RenamedPat RenamedGRHSsAndBinds
  | FunMonoBindsCtxt	Name [RenamedMatch]
  | MatchCtxt		UniType UniType
  | ArithSeqCtxt	RenamedExpr
  | CCallCtxt		String [RenamedExpr]
  | AmbigDictCtxt	[Inst]	-- Occurs check when simplifying ambiguous
				-- dictionaries.  Should never happen!
  | SigCtxt		Id UniType
  | MethodSigCtxt	Name UniType
  | ExprSigCtxt		RenamedExpr UniType
  | ValSpecSigCtxt	Name UniType SrcLoc
  | ValSpecSpecIdCtxt	Name UniType Name SrcLoc

	-- The next two contexts are associated only with TcSimplifyAndCheck failures
  | BindSigCtxt		[Id]		-- Signature(s) for a group of bindings
  | SuperClassSigCtxt			-- Superclasses for this instance decl

  | CaseBranchCtxt	RenamedMatch
  | Rank2ArgCtxt	TypecheckedExpr UniType
#ifdef DPH
  | PodCtxt		[RenamedExpr]
  | ParFilterCtxt       RenamedExpr
  | DrawnCtxt		[RenamedPat]  RenamedPat RenamedExpr
  | IndexCtxt		[RenamedExpr] RenamedPat RenamedExpr
  | ParPidPatCtxt	RenamedPat 
  | ParPidExpCtxt	RenamedExpr
  | ParZFlhsCtxt	RenamedExpr
#endif {- Data Parallel Haskell -}
\end{code}

%************************************************************************
%*									*
\subsection[Errors-print-unify]{Printing unification error info}
%*									*
%************************************************************************

\begin{code}
ppUnifyErr :: Pretty -> Pretty -> Pretty
ppUnifyErr head rest = ppSep [head, {-if you want a blank line: ppSP,-} rest]

pprUnifyErrInfo sty (UnifyMisMatch mt1 mt2) err_ctxt
 = ppUnifyErr (ppSep [ppBesides [ppStr "Couldn't match the type `", ppr sty mt1, ppStr "'"],
		       ppBesides [ppStr "against `", ppr sty mt2, ppStr "'."]])
	      (pprUnifyErrContext sty err_ctxt)

pprUnifyErrInfo sty (TypeRec tyvar ty) err_ctxt
 = ppUnifyErr (ppBesides [ppStr "Cannot construct the infinite type `",
				 ppr sty tyvar, 
				 ppStr "' = `",ppr sty ty, ppStr "' (\"occurs check\")."])
	      (pprUnifyErrContext sty err_ctxt)

pprUnifyErrInfo sty (UnifyListMisMatch tys1 tys2) err_ctxt
 = panic "pprUnifyErrInfo: unifying lists of types of different lengths"

pprUnifyErrInfo sty (UnifyUnboxedMisMatch mt1 mt2) err_ctxt
 = ppUnifyErr (ppSep [ppBesides [ppStr "Couldn't match the type variable `", ppr sty mt1, ppStr "'"],
		      ppBesides [ppStr "against unboxed type `", ppr sty mt2, ppStr "'."],
		      ppStr "Try using  -fspecialise-unboxed  ..." ])
	      (pprUnifyErrContext sty err_ctxt)
\end{code}

%************************************************************************
%*									*
\subsection[Errors-print-context]{Printing unification error context}
%*									*
%************************************************************************

\begin{code}
pp_nest_hang :: String -> Pretty -> Pretty
pp_nest_hang label stuff = ppNest 2 (ppHang (ppStr label) 4 stuff)

context = "Error detected when type-checking "

ppContext s = ppStr (context ++ s)

pprUnifyErrContext sty (PredCtxt e)
  = ppHang (ppStr "In a predicate expression:") 4 (ppr sty e)

pprUnifyErrContext sty (AppCtxt f a)
  = ppHang (ppStr "In a function application:") 4 (ppr sty (App f a))

pprUnifyErrContext sty (FunAppCtxt f maybe_id actual_arg expected_arg_ty actual_arg_ty n)
  = let

	(have_extra_info, f_id, f_type)
	   = case maybe_id of
	       Nothing -> (False, bottom, bottom)
	       Just id -> (True,  id, getIdUniType id)

	free_tyvars = extractTyVarsFromTy f_type
	bottom = panic "no maybe_id"
    in
    ppAboves [
	ppHang (ppCat [ ppStr "In the", speakNth n, ppStr "argument of",
			ppBesides [ppChar '`', ppr sty f, ppStr "',"] ])
	4 (ppBesides [ppStr " namely `", ppr sty actual_arg, ppStr "'," ]),

	ppHang  (ppStr "Expected type of the argument: ")
		4 (ppr sty expected_arg_ty),

	ppHang  (ppStr "Inferred type of the argument: ")
		4 (ppr sty actual_arg_ty),

{- OMIT
   I'm not sure this adds anything 

	if have_extra_info
	then ppHang (ppCat [ppStr "The type of",
			    ppBesides [ppChar '`', ppr sty f_id, ppChar '\''],
			    ppStr "is"]) 4
		    (ppBesides [ppChar '`', ppr sty f_type, ppStr "'."])
	else ppNil,
-}
	
	if not have_extra_info || null free_tyvars || isSysLocalId f_id
		-- SysLocals are created for the local (monomorphic) versions
		-- of recursive functions, and the monomorphism suggestion 
		-- below is sometimes positively misleading.  Notably,
		-- if you give an erroneous type sig, you may well end
		-- up with a unification error like this, and it usually ain't due
		-- to monomorphism.
	then ppNil
	else
	   ppAboves [
		ppSep [ppStr "Possible cause of error:",
		       ppBesides [ppChar '`', ppr sty f, ppChar '\''],
		       ppStr "is not polymorphic"],
		ppSep [ppStr "it is monomorphic in the type variable(s):", 
		       interpp'SP sty free_tyvars]
	   ]
    ]

pprUnifyErrContext sty (TooManyArgsCtxt f)
  = ppHang (ppStr "Too many arguments in an application of the function")
	 4 (ppBesides [ ppChar '`', ppr sty f, ppStr "'." ])

pprUnifyErrContext sty (SectionLAppCtxt expr op)
  = ppHang (ppStr "In a left section:")  4 (ppr sty (SectionL expr op))

pprUnifyErrContext sty (SectionRAppCtxt op expr)
  = ppHang (ppStr "In a right section:") 4 (ppr sty (SectionR op expr))

pprUnifyErrContext sty (OpAppCtxt a1 op a2)
  = ppHang (ppStr "In an infix-operator application:") 4 (ppr sty (OpApp a1 op a2))

pprUnifyErrContext sty (CaseCtxt e as)
  = ppHang (ppStr "In a case expression:") 4 (ppr sty (Case e as))

pprUnifyErrContext sty (BranchCtxt b1 b2)
  = ppSep [ppStr "In the branches of a conditional:",
	   pp_nest_hang "`then' branch:" (ppr sty b1),
	   pp_nest_hang "`else' branch:" (ppr sty b2)]

pprUnifyErrContext sty (ListCtxt es)
  = ppHang (ppStr "In a list expression:") 4 (
	      ppBesides [ppLbrack, interpp'SP sty es, ppRbrack])

pprUnifyErrContext sty (PatCtxt (ConPatIn name pats))
  = ppHang (ppStr "In a constructed pattern:")
	 4 (ppCat [ppr sty name, interppSP sty pats])

pprUnifyErrContext sty (PatCtxt (ConOpPatIn pat1 op pat2))
  = ppHang (ppStr "In an infix-operator pattern:")
	 4 (ppCat [ppr sty pat1, ppr sty op, ppr sty pat2])

pprUnifyErrContext sty (PatCtxt (ListPatIn ps))
  = ppHang (ppStr "In an explicit list pattern:")
	 4 (ppBesides [ppLbrack, interpp'SP sty ps, ppRbrack])

pprUnifyErrContext sty (PatCtxt pat@(AsPatIn _ _))
  = ppHang (ppStr "In an as-pattern:") 4 (ppr sty pat)

pprUnifyErrContext sty (CaseBranchesCtxt (m:ms))
  = ppAboves [ppStr "Inside two case alternatives:",
	      ppNest 4 (ppBeside (ppStr "... ") (pprMatches sty (True,ppNil) [m])),
	      ppNest 4 (ppBeside (ppStr "... ") (pprMatches sty (True,ppNil) ms))]

pprUnifyErrContext sty (FilterCtxt e)
  = ppHang (ppStr "In a guard in a list-comprehension:") 4 (ppr sty e)

pprUnifyErrContext sty (GeneratorCtxt p e)
  = ppHang (ppStr "In a generator in a list-comprehension:")
	 4 (ppSep [ppr sty p, ppStr "<-", ppr sty e])

pprUnifyErrContext sty (GRHSsBranchCtxt grhss)
  = ppAboves [ppStr "In some guarded right-hand-sides:",
	      ppNest 4 (ppAboves (map (pprGRHS sty False) grhss))]

pprUnifyErrContext sty (GRHSsGuardCtxt g)
  = ppHang (ppStr "In a guard on an equation:") 4 (ppr sty g)

pprUnifyErrContext sty (PatMonoBindsCtxt pat grhss_and_binds)
  = ppHang (ppStr "In a pattern binding:")
	 4 (ppr sty (PatMonoBind pat grhss_and_binds mkUnknownSrcLoc))

pprUnifyErrContext sty (FunMonoBindsCtxt id matches)
  = ppHang (ppStr "When combining a function's equation(s) & type signature (if applicable):")
	 4 (ppBesides [ppr sty id, ppSP, pprMatches sty (False,ppNil) matches])

pprUnifyErrContext sty (CaseBranchCtxt match)
  = ppHang (ppStr "When combining a \"case\" branch & type signature (if applicable):")
	 4 (pprMatch sty True{-is_case-} match)

pprUnifyErrContext sty (MatchCtxt ty1 ty2)
  = ppAboves [ppStr "In a type signature:",
	      pp_nest_hang "Signature:" (ppr sty ty1),
	      pp_nest_hang "Inferred type:" (ppr sty ty2)]

pprUnifyErrContext sty (ArithSeqCtxt expr)
  = ppHang (ppStr "In an arithmetic sequence:") 4 (ppr sty expr)

pprUnifyErrContext sty (CCallCtxt label args)
  = ppAboves [ppStr "In a _ccall_ or _casm_:",
	      pp_nest_hang "C-calling magic:" (ppStr label),
	      pp_nest_hang "Arguments:" (ppInterleave ppComma (map (ppr sty) args))]

-- OLD: kill
pprUnifyErrContext sty (AmbigDictCtxt dicts)
  = ppStr "Ambiguous dictionary occurs check: should never happen!"

pprUnifyErrContext sty (SigCtxt id tau_ty)
  = ppHang (ppBesides [ppStr "In the type signature for ",
		   ppr sty id,
		   ppStr ":"]
	   ) 4 (ppr sty tau_ty)

pprUnifyErrContext sty (MethodSigCtxt name ty)
  = ppHang (ppBesides [ ppStr "When matching the definition of class method `",
	        ppr sty name, ppStr "' to its signature :" ]
	   ) 4 (ppr sty ty)

pprUnifyErrContext sty (ExprSigCtxt expr ty)
  = ppHang (ppStr "In an expression with a type signature:")
	 4 (ppSep [ppBeside (ppr sty expr) (ppStr " ::"),
		  ppr sty ty])

pprUnifyErrContext sty (BindSigCtxt ids)
  = ppHang (ppStr "When checking type signatures for: ")
	 4 (ppInterleave (ppStr ", ") (map (ppr sty) ids))

pprUnifyErrContext sty SuperClassSigCtxt
  = ppStr "When checking superclass constraints on instance declaration"

pprUnifyErrContext sty (Rank2ArgCtxt expr ty)
  = ppHang (ppStr "In an argument which has rank-2 polymorphic type:")
	 4 (ppSep [ppBeside (ppr sty expr) (ppStr " ::"),
		  ppr sty ty])

pprUnifyErrContext sty (ValSpecSigCtxt v ty src_loc)
  = ppHang (ppStr "In a SPECIALIZE pragma for a value:")
	 4 (ppSep [ppBeside (ppr sty v) (ppStr " ::"),
		  ppr sty ty])

pprUnifyErrContext sty (ValSpecSpecIdCtxt v ty spec src_loc)
  = ppHang (ppStr "When checking type of explicit id in SPECIALIZE pragma:")
	 4 (ppSep [ppBeside (ppr sty v) (ppStr " ::"),
		  ppr sty ty,
		  ppBeside (ppStr " = ") (ppr sty spec)])

#ifdef DPH
pprUnifyErrContext sty (PodCtxt es)
  = ppAboves [ppStr "In a POD expression:",
	      ppBesides [ppStr "<<", interpp'SP sty es, ppStr ">>"]]

pprUnifyErrContext sty (ParFilterCtxt e)
  = ppHang (ppStr "In a guard of a POD comprehension:") 4 
	   (ppr sty e)

pprUnifyErrContext sty (DrawnCtxt ps p e)
  = ppHang (ppStr "In parallel drawn from generator:")
	   4 (ppSep [ppStr "(|" ,interpp'SP sty ps, ppStr ";" , 
		     ppr sty p ,ppStr "|)", ppStr "<<-", ppr sty e])

pprUnifyErrContext sty (IndexCtxt es p e)
  = ppHang (ppStr "In parallel index from generator:")
	   4 (ppSep [ppStr "(|",interpp'SP sty es, ppStr ";" , 
		     ppr sty p ,ppStr "|)" , ppStr "<<=", ppr sty e])

pprUnifyErrContext sty (ParPidPatCtxt p)
  = ppHang (ppStr "In pattern for processor ID has to be in class Pid:")
	   4 (ppr sty p)

pprUnifyErrContext sty (ParPidExpCtxt e)
  = ppHang (ppStr "In expression for processor ID has to be in class Pid:")
	   4 (ppr sty e)

pprUnifyErrContext sty (ParZFlhsCtxt e)
  = ppHang (ppStr "In LHS of a POD comprehension has to be in class Processor")
	   4 (ppr sty e)

#endif {- Data Parallel Haskell -}
\end{code}

\begin{code}
#ifdef DPH
pprPodizedWarning :: PodWarning -> Error
pprPodizedWarning (EntryNotPodized b)
   = addWarningLoc (getSrcLoc b) 			 	(\ sty ->
     ppBeside (ppStr "Unable to parallelise entry: ")
              (ppr sty b)
     )

pprPodizedWarning (NoGoNestedPodized b)
   = addWarningLoc (getSrcLoc b) 			 	(\ sty ->
     ppBeside (ppStr "Sorry no nested parallelism yet: ")
	      (ppr sty b)
   )

pprPodizedWarning (ContextNotAvailable b c)
   = addWarningLoc (getSrcLoc b) 				(\ sty ->
     ppAbove (ppBesides [ppStr "No parallelisation of binding for a ",
			 ppStr (show_context c) , ppStr ": ",ppr sty b])
	     (ppBesides [ppStr "Maybe you should re-compile this module ",
		         ppStr "with the `",ppStr (which_flag c), 
			 ppStr "' flag."])
     )

pprPodizedWarning (ImportNotAvailable b c)
   = addWarningLoc (getSrcLoc b) 				(\ sty ->
     ppAboves [ppBesides [ppStr "No parallelisation of binding for a ",
			  ppStr (show_context c),ppStr ": ", ppr sty b],
	       ppBesides [ppStr "If you re-compile the module `",
			  ppStr (fst (getOrigName b)), ppStr "`"],
	       ppBesides [ppStr "with the `",ppStr (which_flag c),
			  ppStr "' flag I may do a better job :-)"]]
     )


pprPodizedWarning (ArgsInDifferentContexts b)
   = addWarningLoc (getSrcLoc b) 				(\ sty ->
     ppBesides [ppStr "Higher Order argument used in different ",
		ppStr "parallel contexts : ",ppr sty b]
     )

pprPodizedWarning (NoPodization)
   = addWarning 		 				(\ sty ->
     ppStr "Program not podized")

pprPodizedWarning (PodizeStats ci pi vl pl)
   = addWarning 		 				(\ sty ->
     (ppHang (ppStr "Podization Statistics:")
	     5
             (ppAboves [ppCat [ppStr "Info collecting passes =",ppr sty ci],
		        ppCat [ppStr "Podization passes      =",ppr sty pi],
		        ppCat [ppStr "Vanilla's deleted      =",ppr sty vl],
		        ppCat [ppStr "Podized   deleted      =",ppr sty pl]]))
     )

show_context :: Int -> String
show_context 1 = "\"vector\""
show_context 2 = "\"matrix\""
show_context 3 = "\"cube\""
show_context n = "\""++(show n)++"-D Pod\""

which_flag :: Int -> String
which_flag 1 = "-fpodize-vector"
which_flag 2 = "-fpodize-matrix"
which_flag 3 = "-fpodize-cube"
#endif {- Data Parallel Haskell -}
\end{code}


@speakNth@ converts an integer to a verbal index; eg 1 maps to ``first'' etc.
\begin{code}
speakNth :: Int -> Pretty
speakNth 1 = ppStr "first"
speakNth 2 = ppStr "second"
speakNth 3 = ppStr "third"
speakNth 4 = ppStr "fourth"
speakNth 5 = ppStr "fifth"
speakNth 6 = ppStr "sixth"
speakNth n = ppBesides [ ppInt n, ppStr st_nd_rd_th ]
  where
    st_nd_rd_th | n_rem_10 == 1 = "st"
		| n_rem_10 == 2 = "nd"
		| n_rem_10 == 3 = "rd"
		| otherwise     = "th"

    n_rem_10 = n `rem` 10
\end{code}
