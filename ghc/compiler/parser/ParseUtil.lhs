%
% (c) The GRASP/AQUA Project, Glasgow University, 1999
%
\section[ParseUtil]{Parser Utilities}

\begin{code}
module ParseUtil (
	  parseError		-- String -> Pa
	, srcParseErr		-- StringBuffer -> SrcLoc -> Message
	, cbot			-- a
	, splitForConApp        -- RdrNameHsType -> [RdrNameBangType]
				--     -> P (RdrName, [RdrNameBangType])

	, mkRecConstrOrUpdate	-- HsExp -> [HsFieldUpdate] -> P HsExp
	, groupBindings
	
	, mkExtName             -- Maybe ExtName -> RdrName -> ExtName

	, checkPrec 		-- String -> P String
	, checkContext		-- HsType -> P HsContext
	, checkInstType		-- HsType -> P HsType
	, checkAssertion	-- HsType -> P HsAsst
	, checkDataHeader	-- HsQualType -> P (HsContext,HsName,[HsName])
	, checkSimple		-- HsType -> [HsName] -> P ((HsName,[HsName]))
	, checkPattern		-- HsExp -> P HsPat
	, checkPatterns		-- [HsExp] -> P [HsPat]
	-- , checkExpr		-- HsExp -> P HsExp
	, checkValDef		-- (SrcLoc, HsExp, HsRhs, [HsDecl]) -> P HsDecl

	
	-- some built-in names (all :: RdrName)
	, unitCon_RDR, unitTyCon_RDR, nilCon_RDR, listTyCon_RDR
	, tupleCon_RDR, tupleTyCon_RDR, ubxTupleCon_RDR, ubxTupleTyCon_RDR
	, funTyCon_RDR

	-- pseudo-keywords, in var and tyvar forms (all :: RdrName)
	, as_var_RDR, hiding_var_RDR, qualified_var_RDR, forall_var_RDR
	, export_var_RDR, label_var_RDR, dynamic_var_RDR, unsafe_var_RDR
	, stdcall_var_RDR, ccall_var_RDR

	, as_tyvar_RDR, hiding_tyvar_RDR, qualified_tyvar_RDR
	, export_tyvar_RDR, label_tyvar_RDR, dynamic_tyvar_RDR
	, unsafe_tyvar_RDR, stdcall_tyvar_RDR, ccall_tyvar_RDR

	, minus_RDR, pling_RDR, dot_RDR

 ) where

#include "HsVersions.h"

import Lex
import HsSyn
import SrcLoc
import RdrHsSyn
import RdrName
import CallConv
import PrelMods 	( pRELUDE_Name, mkUbxTupNameStr, mkTupNameStr )
import OccName  	( dataName, tcName, varName, tvName, setOccNameSpace, occNameFS )
import CmdLineOpts 	( opt_NoImplicitPrelude )
import StringBuffer 	( lexemeToString )
import FastString	( unpackFS )
import ErrUtils
import UniqFM		( UniqFM, listToUFM, lookupUFM )
import Outputable

-----------------------------------------------------------------------------
-- Misc utils

parseError :: String -> P a
parseError s = 
  getSrcLocP `thenP` \ loc ->
  failMsgP (hcat [ppr loc, text ": ", text s])

srcParseErr :: StringBuffer -> SrcLoc -> Message
srcParseErr s l
  = hcat [ppr l, 
	  if null token 
	     then ptext SLIT(": parse error (possibly incorrect indentation)")
	     else hcat [ptext SLIT(": parse error on input "),
          	  	char '`', text token, char '\'']
    ]
  where 
	token = lexemeToString s

cbot = panic "CCall:result_ty"

-----------------------------------------------------------------------------
-- splitForConApp

-- When parsing data declarations, we sometimes inadvertently parse
-- a constructor application as a type (eg. in data T a b = C a b `D` E a b)
-- This function splits up the type application, adds any pending
-- arguments, and converts the type constructor back into a data constructor.

splitForConApp :: RdrNameHsType -> [RdrNameBangType]
	-> P (RdrName, [RdrNameBangType])

splitForConApp  t ts = split t ts
 where
	split (MonoTyApp t u) ts = split t (Unbanged u : ts)

	split (MonoTyVar t)   ts  = returnP (con, ts)
	   where t_occ = rdrNameOcc t
		 con   = setRdrNameOcc t (setOccNameSpace t_occ dataName)

	split _ _ = parseError "Illegal data/newtype declaration"

----------------------------------------------------------------------------
-- Various Syntactic Checks

callConvFM :: UniqFM CallConv
callConvFM = listToUFM $
      map (\ (x,y) -> (_PK_ x,y))
     [  ("stdcall",  stdCallConv),
	("ccall",    cCallConv)
--	("pascal",   pascalCallConv),
--	("fastcall", fastCallConv)
     ]

checkCallConv :: FAST_STRING -> P CallConv
checkCallConv s = 
  case lookupUFM callConvFM s of
	Nothing -> parseError ("unknown calling convention: `"
				 ++ unpackFS s ++ "'")
	Just conv -> returnP conv

checkInstType :: RdrNameHsType -> P RdrNameHsType
checkInstType t 
  = case t of
	HsForAllTy tvs ctxt ty ->
		checkAssertion ty [] `thenP` \(c,ts)->
	      	returnP (HsForAllTy tvs ctxt (MonoDictTy c ts))

	ty ->   checkAssertion ty [] `thenP` \(c,ts)->
	      	returnP (HsForAllTy Nothing [] (MonoDictTy c ts))

checkContext :: RdrNameHsType -> P RdrNameContext
checkContext (MonoTupleTy ts True) 
  = mapP (\t -> checkAssertion t []) ts `thenP` \cs ->
    returnP cs
checkContext (MonoTyVar t) -- empty contexts are allowed
  | t == unitTyCon_RDR = returnP []
checkContext t 
  = checkAssertion t [] `thenP` \c ->
    returnP [c]

checkAssertion :: RdrNameHsType -> [RdrNameHsType] 
	-> P (ClassAssertion RdrName)
checkAssertion (MonoTyVar t) args@(_:_) | not (isRdrTyVar t) 
  	= returnP (t,args)
checkAssertion (MonoTyApp l r) args = checkAssertion l (r:args)
checkAssertion _ _ = parseError "Illegal class assertion"

checkDataHeader :: RdrNameHsType 
	-> P (RdrNameContext, RdrName, [RdrNameHsTyVar])
checkDataHeader (HsForAllTy Nothing cs t) =
   checkSimple t []	     `thenP` \(c,ts) ->
   returnP (cs,c,map UserTyVar ts)
checkDataHeader t =
   checkSimple t []	     `thenP` \(c,ts) ->
   returnP ([],c,map UserTyVar ts)

checkSimple :: RdrNameHsType -> [RdrName] -> P ((RdrName,[RdrName]))
checkSimple (MonoTyApp l (MonoTyVar a)) xs | isRdrTyVar a 
   = checkSimple l (a:xs)
checkSimple (MonoTyVar t) xs | not (isRdrTyVar t) = returnP (t,xs)
checkSimple t _ = trace (showSDoc (ppr t)) $ parseError "Illegal data/newtype declaration"

---------------------------------------------------------------------------
-- Checking Patterns.

-- We parse patterns as expressions and check for valid patterns below,
-- nverting the expression into a pattern at the same time.

checkPattern :: RdrNameHsExpr -> P RdrNamePat
checkPattern e = checkPat e []

checkPatterns :: [RdrNameHsExpr] -> P [RdrNamePat]
checkPatterns es = mapP checkPattern es

checkPat :: RdrNameHsExpr -> [RdrNamePat] -> P RdrNamePat
checkPat (HsVar c) args | isRdrDataCon c = returnP (ConPatIn c args)
checkPat (HsApp f x) args = 
	checkPat x [] `thenP` \x ->
	checkPat f (x:args)
checkPat e [] = case e of
	EWildPat	   -> returnP WildPatIn
	HsVar x		   -> returnP (VarPatIn x)
	HsLit l            -> returnP (LitPatIn l)
	ELazyPat e	   -> checkPat e [] `thenP` (returnP . LazyPatIn)
	EAsPat n e	   -> checkPat e [] `thenP` (returnP . AsPatIn n)
        ExprWithTySig e t  -> checkPat e [] `thenP` \e ->
			      -- pattern signatures are parsed as sigtypes,
			      -- but they aren't explicit forall points.  Hence
			      -- we have to remove the implicit forall here.
			      let t' = case t of 
					  HsForAllTy Nothing [] ty -> ty
					  other -> other
			      in
			      returnP (SigPatIn e t')

	OpApp (HsVar n) (HsVar plus) _ (HsLit k@(HsInt _)) | plus == plus_RDR
			   -> returnP (NPlusKPatIn n k)

	OpApp l op fix r   -> checkPat l [] `thenP` \l ->
			      checkPat r [] `thenP` \r ->
			      case op of
			   	 HsVar c -> returnP (ConOpPatIn l c fix r)
			   	 _ -> patFail

	NegApp l r	   -> checkPat l [] `thenP` (returnP . NegPatIn)
	HsPar e		   -> checkPat e [] `thenP` (returnP . ParPatIn)
	ExplicitList es	   -> mapP (\e -> checkPat e []) es `thenP` \ps ->
			      returnP (ListPatIn ps)
	ExplicitTuple es b -> mapP (\e -> checkPat e []) es `thenP` \ps ->
			      returnP (TuplePatIn ps b)
	RecordCon c fs     -> mapP checkPatField fs `thenP` \fs ->
			      returnP (RecPatIn c fs)
	_ -> patFail

checkPat _ _ = patFail

checkPatField :: (RdrName, RdrNameHsExpr, Bool) 
	-> P (RdrName, RdrNamePat, Bool)
checkPatField (n,e,b) =
	checkPat e [] `thenP` \p ->
	returnP (n,p,b)

patFail = parseError "Parse error in pattern"

---------------------------------------------------------------------------
-- Check Expression Syntax

{-
We can get away without checkExpr if the renamer generates errors for
pattern syntax used in expressions (wildcards, as patterns and lazy 
patterns).

checkExpr :: RdrNameHsExpr -> P RdrNameHsExpr
checkExpr e = case e of
	HsVar _			  -> returnP e
	HsLit _			  -> returnP e
	HsLam match		  -> checkMatch match `thenP` (returnP.HsLam)
	HsApp e1 e2		  -> check2Exprs e1 e2 HsApp
	OpApp e1 e2 fix e3	  -> checkExpr e1 `thenP` \e1 ->
				     checkExpr e2 `thenP` \e2 ->
				     checkExpr e3 `thenP` \e3 ->
				     returnP (OpApp e1 e2 fix e3)
	NegApp e neg		  -> checkExpr e `thenP` \e ->
				     returnP (NegApp e neg)
	HsPar e			  -> check1Expr e HsPar
	SectionL e1 e2	  	  -> check2Exprs e1 e2 SectionL
	SectionR e1 e2      	  -> check2Exprs e1 e2 SectionR
	HsCase e alts		  -> mapP checkMatch alts `thenP` \alts ->
				     checkExpr e `thenP` \e ->
				     returnP (HsCase e alts)
	HsIf e1 e2 e3		  -> check3Exprs e1 e2 e3 HsIf

	HsLet bs e		  -> check1Expr e (HsLet bs)
	HsDo stmts		  -> mapP checkStmt stmts `thenP` (returnP . HsDo)
	HsTuple es		  -> checkManyExprs es HsTuple
	HsList es		  -> checkManyExprs es HsList
	HsRecConstr c fields	  -> mapP checkField fields `thenP` \fields ->
				     returnP (HsRecConstr c fields)
	HsRecUpdate e fields	  -> mapP checkField fields `thenP` \fields ->
				     checkExpr e `thenP` \e ->
				     returnP (HsRecUpdate e fields)
	HsEnumFrom e		  -> check1Expr e HsEnumFrom
	HsEnumFromTo e1 e2	  -> check2Exprs e1 e2 HsEnumFromTo
	HsEnumFromThen e1 e2      -> check2Exprs e1 e2 HsEnumFromThen
	HsEnumFromThenTo e1 e2 e3 -> check3Exprs e1 e2 e3 HsEnumFromThenTo
	HsListComp e stmts        -> mapP checkStmt stmts `thenP` \stmts ->
				     checkExpr e `thenP` \e ->
				     returnP (HsListComp e stmts)
	RdrNameHsExprTypeSig loc e ty     -> checkExpr e `thenP` \e ->
				     returnP (RdrNameHsExprTypeSig loc e ty)
        _                         -> parseError "parse error in expression"

-- type signature for polymorphic recursion!!
check1Expr :: RdrNameHsExpr -> (RdrNameHsExpr -> a) -> P a
check1Expr e f = checkExpr e `thenP` (returnP . f)

check2Exprs :: RdrNameHsExpr -> RdrNameHsExpr -> (RdrNameHsExpr -> RdrNameHsExpr -> a) -> P a
check2Exprs e1 e2 f = 
	checkExpr e1 `thenP` \e1 ->
	checkExpr e2 `thenP` \e2 ->
	returnP (f e1 e2)

check3Exprs :: RdrNameHsExpr -> RdrNameHsExpr -> RdrNameHsExpr -> (RdrNameHsExpr -> RdrNameHsExpr -> RdrNameHsExpr -> a) -> P a
check3Exprs e1 e2 e3 f = 
	checkExpr e1 `thenP` \e1 ->
	checkExpr e2 `thenP` \e2 ->
	checkExpr e3 `thenP` \e3 ->
	returnP (f e1 e2 e3)

checkManyExprs es f =
	mapP checkExpr es `thenP` \es ->
	returnP (f es) 

checkAlt (HsAlt loc p galts bs) 
	= checkGAlts galts `thenP` \galts -> returnP (HsAlt loc p galts bs)

checkGAlts (HsUnGuardedAlt e) = check1Expr e HsUnGuardedAlt
checkGAlts (HsGuardedAlts galts) 
    = mapP checkGAlt galts `thenP` (returnP . HsGuardedAlts)

checkGAlt (HsGuardedAlt loc e1 e2) = check2Exprs e1 e2 (HsGuardedAlt loc)

checkStmt (HsGenerator p e) = check1Expr e (HsGenerator p)
checkStmt (HsQualifier e)   = check1Expr e HsQualifier
checkStmt s@(HsLetStmt bs)  = returnP s

checkField (HsFieldUpdate n e) = check1Expr e (HsFieldUpdate n)
checkField e = returnP e
-}
---------------------------------------------------------------------------
-- Check Equation Syntax

checkValDef 
	:: RdrNameHsExpr
	-> Maybe RdrNameHsType
	-> RdrNameGRHSs
	-> SrcLoc
	-> P RdrNameMonoBinds

checkValDef lhs opt_sig grhss loc
 = case isFunLhs lhs [] of
	   Just (f,inf,es) -> 
		checkPatterns es `thenP` \ps ->
		returnP (FunMonoBind f inf [Match [] ps opt_sig grhss] loc)

           Nothing ->
		checkPattern lhs `thenP` \lhs ->
		returnP (PatMonoBind lhs grhss loc)

-- A variable binding is parsed as an RdrNamePatBind.

isFunLhs (OpApp l (HsVar op) fix r) []  | not (isRdrDataCon op)
			  	= Just (op, True, [l,r])
isFunLhs (HsVar f) es@(_:_)  | not (isRdrDataCon f)
			 	= Just (f,False,es)
isFunLhs (HsApp f e) es 	= isFunLhs f (e:es)
isFunLhs (HsPar e)   es 	= isFunLhs e es
isFunLhs _ _ 			= Nothing

---------------------------------------------------------------------------
-- Miscellaneous utilities

checkPrec :: Integer -> P ()
checkPrec i | 0 <= i && i <= 9 = returnP ()
	    | otherwise        = parseError "precedence out of range"

mkRecConstrOrUpdate 
	:: RdrNameHsExpr 
	-> RdrNameHsRecordBinds
	-> P RdrNameHsExpr

mkRecConstrOrUpdate (HsVar c) fs | isRdrDataCon c
  = returnP (RecordCon c fs)
mkRecConstrOrUpdate exp fs@(_:_) 
  = returnP (RecordUpd exp fs)
mkRecConstrOrUpdate _ _
  = parseError "Empty record update"

-- supplying the ext_name in a foreign decl is optional ; if it
-- isn't there, the Haskell name is assumed. Note that no transformation
-- of the Haskell name is then performed, so if you foreign export (++),
-- it's external name will be "++". Too bad.
mkExtName :: Maybe ExtName -> RdrName -> ExtName
mkExtName Nothing rdrNm = ExtName (occNameFS (rdrNameOcc rdrNm)) Nothing
mkExtName (Just x) _    = x

-----------------------------------------------------------------------------
-- group function bindings into equation groups

-- we assume the bindings are coming in reverse order, so we take the srcloc
-- from the *last* binding in the group as the srcloc for the whole group.

groupBindings :: [RdrBinding] -> RdrBinding
groupBindings binds = group Nothing binds
  where group :: Maybe RdrNameMonoBinds -> [RdrBinding] -> RdrBinding
	group (Just bind) [] = RdrValBinding bind
	group Nothing [] = RdrNullBind
	group (Just (FunMonoBind f inf1 mtchs ignore_srcloc))
		    (RdrValBinding (FunMonoBind f' _ [mtch] loc) : binds)
	    | f == f' = group (Just (FunMonoBind f inf1 (mtch:mtchs) loc)) binds

	group (Just so_far) binds
	    = RdrValBinding so_far `RdrAndBindings` group Nothing binds
	group Nothing (bind:binds)
	    = case bind of
		RdrValBinding b@(FunMonoBind _ _ _ _) -> group (Just b) binds
		other -> bind `RdrAndBindings` group Nothing binds

-----------------------------------------------------------------------------
-- Built-in names

unitCon_RDR, unitTyCon_RDR, nilCon_RDR, listTyCon_RDR :: RdrName
tupleCon_RDR, tupleTyCon_RDR		:: Int -> RdrName
ubxTupleCon_RDR, ubxTupleTyCon_RDR 	:: Int -> RdrName

unitCon_RDR
	| opt_NoImplicitPrelude = mkSrcUnqual   dataName unitName
	| otherwise		= mkPreludeQual dataName pRELUDE_Name unitName

unitTyCon_RDR
	| opt_NoImplicitPrelude = mkSrcUnqual   tcName unitName
	| otherwise		= mkPreludeQual tcName pRELUDE_Name unitName

nilCon_RDR
	| opt_NoImplicitPrelude = mkSrcUnqual   dataName listName
	| otherwise		= mkPreludeQual dataName pRELUDE_Name listName

listTyCon_RDR
	| opt_NoImplicitPrelude = mkSrcUnqual   tcName listName
	| otherwise		= mkPreludeQual tcName pRELUDE_Name listName

funTyCon_RDR
	| opt_NoImplicitPrelude = mkSrcUnqual   tcName funName
	| otherwise		= mkPreludeQual tcName pRELUDE_Name funName

tupleCon_RDR arity
  | opt_NoImplicitPrelude = mkSrcUnqual   dataName (snd (mkTupNameStr arity))
  | otherwise		  = mkPreludeQual dataName pRELUDE_Name
				(snd (mkTupNameStr arity))

tupleTyCon_RDR arity
  | opt_NoImplicitPrelude = mkSrcUnqual   tcName (snd (mkTupNameStr arity))
  | otherwise		  = mkPreludeQual tcName pRELUDE_Name
				(snd (mkTupNameStr arity))


ubxTupleCon_RDR arity
  | opt_NoImplicitPrelude = mkSrcUnqual   dataName (snd (mkUbxTupNameStr arity))
  | otherwise		  = mkPreludeQual dataName pRELUDE_Name 
				(snd (mkUbxTupNameStr arity))

ubxTupleTyCon_RDR arity
  | opt_NoImplicitPrelude = mkSrcUnqual   tcName (snd (mkUbxTupNameStr arity))
  | otherwise		  = mkPreludeQual tcName pRELUDE_Name 
				(snd (mkUbxTupNameStr arity))

unitName = SLIT("()")
funName  = SLIT("(->)")
listName = SLIT("[]")

asName              = SLIT("as")
hidingName          = SLIT("hiding")
qualifiedName       = SLIT("qualified")
forallName          = SLIT("forall")
exportName	    = SLIT("export")
labelName	    = SLIT("label")
dynamicName	    = SLIT("dynamic")
unsafeName          = SLIT("unsafe")
stdcallName         = SLIT("stdcall")
ccallName           = SLIT("ccall")

as_var_RDR          = mkSrcUnqual varName asName
hiding_var_RDR      = mkSrcUnqual varName hidingName
qualified_var_RDR   = mkSrcUnqual varName qualifiedName
forall_var_RDR      = mkSrcUnqual varName forallName
export_var_RDR      = mkSrcUnqual varName exportName
label_var_RDR       = mkSrcUnqual varName labelName
dynamic_var_RDR     = mkSrcUnqual varName dynamicName
unsafe_var_RDR      = mkSrcUnqual varName unsafeName
stdcall_var_RDR     = mkSrcUnqual varName stdcallName
ccall_var_RDR       = mkSrcUnqual varName ccallName

as_tyvar_RDR        = mkSrcUnqual tvName asName
hiding_tyvar_RDR    = mkSrcUnqual tvName hidingName
qualified_tyvar_RDR = mkSrcUnqual tvName qualifiedName
export_tyvar_RDR    = mkSrcUnqual tvName exportName
label_tyvar_RDR     = mkSrcUnqual tvName labelName
dynamic_tyvar_RDR   = mkSrcUnqual tvName dynamicName
unsafe_tyvar_RDR    = mkSrcUnqual tvName unsafeName
stdcall_tyvar_RDR   = mkSrcUnqual tvName stdcallName
ccall_tyvar_RDR     = mkSrcUnqual tvName ccallName

minus_RDR           = mkSrcUnqual varName SLIT("-")
pling_RDR	    = mkSrcUnqual varName SLIT("!")
dot_RDR		    = mkSrcUnqual varName SLIT(".")

plus_RDR	    = mkSrcUnqual varName SLIT("+")
\end{code}
