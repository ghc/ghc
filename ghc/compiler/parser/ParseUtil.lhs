%
% (c) The GRASP/AQUA Project, Glasgow University, 1999
%
\section[ParseUtil]{Parser Utilities}

\begin{code}
module ParseUtil (
	  parseError		-- String -> Pa
	, cbot			-- a
	, mkVanillaCon, mkRecCon,

	, mkRecConstrOrUpdate	-- HsExp -> [HsFieldUpdate] -> P HsExp
	, groupBindings
	
	, mkExtName             -- Maybe ExtName -> RdrName -> ExtName

	, checkPrec 		-- String -> P String
	, checkContext		-- HsType -> P HsContext
	, checkInstType		-- HsType -> P HsType
	, checkDataHeader	-- HsQualType -> P (HsContext,HsName,[HsName])
	, checkSimple		-- HsType -> [HsName] -> P ((HsName,[HsName]))
	, checkPattern		-- HsExp -> P HsPat
	, checkPatterns		-- SrcLoc -> [HsExp] -> P [HsPat]
	, checkDo		-- [HsStmt] -> P [HsStmt]
	, checkValDef		-- (SrcLoc, HsExp, HsRhs, [HsDecl]) -> P HsDecl
	, checkValSig		-- (SrcLoc, HsExp, HsRhs, [HsDecl]) -> P HsDecl
 ) where

#include "HsVersions.h"

import Lex
import HsSyn		-- Lots of it
import SrcLoc
import RdrHsSyn		( RdrBinding(..),
			  RdrNameHsType, RdrNameBangType, RdrNameContext,
			  RdrNameHsTyVar, RdrNamePat, RdrNameHsExpr, RdrNameGRHSs,
			  RdrNameHsRecordBinds, RdrNameMonoBinds, RdrNameConDetails
			)
import RdrName
import PrelNames	( unitTyCon_RDR )
import CallConv
import OccName  	( dataName, varName, tcClsName,
			  occNameSpace, setOccNameSpace, occNameUserString )
import FastString	( unpackFS )
import UniqFM		( UniqFM, listToUFM )
import Outputable

-----------------------------------------------------------------------------
-- Misc utils

parseError :: String -> P a
parseError s = 
  getSrcLocP `thenP` \ loc ->
  failMsgP (hcat [ppr loc, text ": ", text s])

cbot = panic "CCall:result_ty"

-----------------------------------------------------------------------------
-- mkVanillaCon

-- When parsing data declarations, we sometimes inadvertently parse
-- a constructor application as a type (eg. in data T a b = C a b `D` E a b)
-- This function splits up the type application, adds any pending
-- arguments, and converts the type constructor back into a data constructor.

mkVanillaCon :: RdrNameHsType -> [RdrNameBangType] -> P (RdrName, RdrNameConDetails)

mkVanillaCon ty tys
 = split ty tys
 where
   split (HsAppTy t u)  ts = split t (unbangedType u : ts)
   split (HsTyVar tc)   ts = tyConToDataCon tc	`thenP` \ data_con ->
			     returnP (data_con, VanillaCon ts)
   split _		 _ = parseError "Illegal data/newtype declaration"

mkRecCon :: RdrName -> [([RdrName],RdrNameBangType)] -> P (RdrName, RdrNameConDetails)
mkRecCon con fields
  = tyConToDataCon con	`thenP` \ data_con ->
    returnP (data_con, RecCon fields)

tyConToDataCon :: RdrName -> P RdrName
tyConToDataCon tc
  | occNameSpace tc_occ == tcClsName
  = returnP (setRdrNameOcc tc (setOccNameSpace tc_occ dataName))
  | otherwise
  = parseError (showSDoc (text "not a constructor:" <+> quotes (ppr tc)))
  where 
    tc_occ   = rdrNameOcc tc


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

checkInstType :: RdrNameHsType -> P RdrNameHsType
checkInstType t 
  = case t of
	HsForAllTy tvs ctxt ty ->
		checkDictTy ty [] `thenP` \ dict_ty ->
	      	returnP (HsForAllTy tvs ctxt dict_ty)

	ty ->   checkDictTy ty [] `thenP` \ dict_ty->
	      	returnP (HsForAllTy Nothing [] dict_ty)

checkContext :: RdrNameHsType -> P RdrNameContext
checkContext (HsTupleTy _ ts) 
  = mapP (\t -> checkPred t []) ts `thenP` \ps ->
    returnP ps
checkContext (HsTyVar t) -- empty contexts are allowed
  | t == unitTyCon_RDR = returnP []
checkContext t 
  = checkPred t [] `thenP` \p ->
    returnP [p]

checkPred :: RdrNameHsType -> [RdrNameHsType] 
	-> P (HsPred RdrName)
checkPred (HsTyVar t) args@(_:_) | not (isRdrTyVar t) 
  	= returnP (HsClassP t args)
checkPred (HsAppTy l r) args = checkPred l (r:args)
checkPred (HsPredTy (HsIParam n ty)) [] = returnP (HsIParam n ty)
checkPred _ _ = parseError "Illegal class assertion"

checkDictTy :: RdrNameHsType -> [RdrNameHsType] -> P RdrNameHsType
checkDictTy (HsTyVar t) args@(_:_) | not (isRdrTyVar t) 
  	= returnP (mkHsDictTy t args)
checkDictTy (HsAppTy l r) args = checkDictTy l (r:args)
checkDictTy _ _ = parseError "Malformed context in instance header"

-- Put more comments!
-- Checks that the lhs of a datatype declaration
-- is of the form Context => T a b ... z
checkDataHeader :: RdrNameHsType 
	-> P (RdrNameContext, RdrName, [RdrNameHsTyVar])

checkDataHeader (HsForAllTy Nothing cs t) =
   checkSimple t []	     `thenP` \(c,ts) ->
   returnP (cs,c,map UserTyVar ts)
checkDataHeader t =
   checkSimple t []	     `thenP` \(c,ts) ->
   returnP ([],c,map UserTyVar ts)

-- Checks the type part of the lhs of a datatype declaration
checkSimple :: RdrNameHsType -> [RdrName] -> P ((RdrName,[RdrName]))
checkSimple (HsAppTy l (HsTyVar a)) xs | isRdrTyVar a 
   = checkSimple l (a:xs)
checkSimple (HsTyVar tycon) xs | not (isRdrTyVar tycon) = returnP (tycon,xs)

checkSimple (HsOpTy (HsTyVar t1) tycon (HsTyVar t2)) [] 
  | not (isRdrTyVar tycon) && isRdrTyVar t1 && isRdrTyVar t2
  = returnP (tycon,[t1,t2])

checkSimple t _ = parseError "Illegal left hand side in data/newtype declaration"

---------------------------------------------------------------------------
-- Checking statements in a do-expression
-- 	We parse   do { e1 ; e2 ; }
-- 	as [ExprStmt e1, ExprStmt e2]
-- checkDo (a) checks that the last thing is an ExprStmt
--	   (b) transforms it to a ResultStmt

checkDo []	       = parseError "Empty 'do' construct"
checkDo [ExprStmt e l] = returnP [ResultStmt e l]
checkDo [s] 	       = parseError "The last statment in a 'do' construct must be an expression"
checkDo (s:ss)	       = checkDo ss	`thenP` \ ss' ->
			 returnP (s:ss')

---------------------------------------------------------------------------
-- Checking Patterns.

-- We parse patterns as expressions and check for valid patterns below,
-- converting the expression into a pattern at the same time.

checkPattern :: SrcLoc -> RdrNameHsExpr -> P RdrNamePat
checkPattern loc e = setSrcLocP loc (checkPat e [])

checkPatterns :: SrcLoc -> [RdrNameHsExpr] -> P [RdrNamePat]
checkPatterns loc es = mapP (checkPattern loc) es

checkPat :: RdrNameHsExpr -> [RdrNamePat] -> P RdrNamePat
checkPat (HsVar c) args | isRdrDataCon c = returnP (ConPatIn c args)
checkPat (HsApp f x) args = 
	checkPat x [] `thenP` \x ->
	checkPat f (x:args)
checkPat e [] = case e of
	EWildPat	   -> returnP WildPatIn
	HsVar x		   -> returnP (VarPatIn x)
	HsLit l 	   -> returnP (LitPatIn l)
	HsOverLit l	   -> returnP (NPatIn l)
	ELazyPat e	   -> checkPat e [] `thenP` (returnP . LazyPatIn)
	EAsPat n e	   -> checkPat e [] `thenP` (returnP . AsPatIn n)
        ExprWithTySig e t  -> checkPat e [] `thenP` \e ->
			      -- Pattern signatures are parsed as sigtypes,
			      -- but they aren't explicit forall points.  Hence
			      -- we have to remove the implicit forall here.
			      let t' = case t of 
					  HsForAllTy Nothing [] ty -> ty
					  other -> other
			      in
			      returnP (SigPatIn e t')

	OpApp (HsVar n) (HsVar plus) _ (HsOverLit lit@(HsIntegral k)) 
		  	   | plus == plus_RDR
			   -> returnP (NPlusKPatIn n lit)
			   where
			      plus_RDR = mkUnqual varName SLIT("+")	-- Hack

	OpApp l op fix r   -> checkPat l [] `thenP` \l ->
			      checkPat r [] `thenP` \r ->
			      case op of
			   	 HsVar c -> returnP (ConOpPatIn l c fix r)
			   	 _ -> patFail

	HsPar e		   -> checkPat e [] `thenP` (returnP . ParPatIn)
	ExplicitList es	   -> mapP (\e -> checkPat e []) es `thenP` \ps ->
			      returnP (ListPatIn ps)

	ExplicitTuple es b -> mapP (\e -> checkPat e []) es `thenP` \ps ->
			      returnP (TuplePatIn ps b)

	RecordCon c fs     -> mapP checkPatField fs `thenP` \fs ->
			      returnP (RecPatIn c fs)
-- Generics 
	HsType ty          -> returnP (TypePatIn ty) 
	_ -> patFail

checkPat _ _ = patFail

checkPatField :: (RdrName, RdrNameHsExpr, Bool) 
	-> P (RdrName, RdrNamePat, Bool)
checkPatField (n,e,b) =
	checkPat e [] `thenP` \p ->
	returnP (n,p,b)

patFail = parseError "Parse error in pattern"


---------------------------------------------------------------------------
-- Check Equation Syntax

checkValDef 
	:: RdrNameHsExpr
	-> Maybe RdrNameHsType
	-> RdrNameGRHSs
	-> SrcLoc
	-> P RdrBinding

checkValDef lhs opt_sig grhss loc
 = case isFunLhs lhs [] of
	   Just (f,inf,es) -> 
		checkPatterns loc es `thenP` \ps ->
		returnP (RdrValBinding (FunMonoBind f inf [Match [] ps opt_sig grhss] loc))

           Nothing ->
		checkPattern loc lhs `thenP` \lhs ->
		returnP (RdrValBinding (PatMonoBind lhs grhss loc))

checkValSig
	:: RdrNameHsExpr
	-> RdrNameHsType
	-> SrcLoc
	-> P RdrBinding
checkValSig (HsVar v) ty loc = returnP (RdrSig (Sig v ty loc))
checkValSig other     ty loc = parseError "Type signature given for an expression"


-- A variable binding is parsed as an RdrNameFunMonoBind.
-- See comments with HsBinds.MonoBinds

isFunLhs :: RdrNameHsExpr -> [RdrNameHsExpr] -> Maybe (RdrName, Bool, [RdrNameHsExpr])
isFunLhs (OpApp l (HsVar op) fix r) es  | not (isRdrDataCon op)
			  	= Just (op, True, (l:r:es))
					| otherwise
				= case isFunLhs l es of
				    Just (op', True, j : k : es') ->
				      Just (op', True, j : OpApp k (HsVar op) fix r : es')
				    _ -> Nothing
isFunLhs (HsVar f) es | not (isRdrDataCon f)
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

-- Supplying the ext_name in a foreign decl is optional ; if it
-- isn't there, the Haskell name is assumed. Note that no transformation
-- of the Haskell name is then performed, so if you foreign export (++),
-- it's external name will be "++". Too bad; it's important because we don't
-- want z-encoding (e.g. names with z's in them shouldn't be doubled)
-- (This is why we use occNameUserString.)

mkExtName :: Maybe ExtName -> RdrName -> ExtName
mkExtName Nothing rdrNm = ExtName (_PK_ (occNameUserString (rdrNameOcc rdrNm)))
				  Nothing
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

		-- don't group together FunMonoBinds if they have
		-- no arguments.  This is necessary now that variable bindings
		-- with no arguments are now treated as FunMonoBinds rather
		-- than pattern bindings (tests/rename/should_fail/rnfail002).
	group (Just (FunMonoBind f inf1 mtchs ignore_srcloc))
		    (RdrValBinding (FunMonoBind f' _ 
					[mtch@(Match _ (_:_) _ _)] loc)
			: binds)
	    | f == f' = group (Just (FunMonoBind f inf1 (mtch:mtchs) loc)) binds

	group (Just so_far) binds
	    = RdrValBinding so_far `RdrAndBindings` group Nothing binds
	group Nothing (bind:binds)
	    = case bind of
		RdrValBinding b@(FunMonoBind _ _ _ _) -> group (Just b) binds
		other -> bind `RdrAndBindings` group Nothing binds
\end{code}
