-----------------------------------------------------------------------------
-- $Id: HsParseUtils.lhs,v 1.1 2002/04/04 16:23:43 simonmar Exp $
--
-- (c) The GHC Team 1997-2000
--
-- Utilities for the Haskell parser.
--
-----------------------------------------------------------------------------

ToDo: Polish readInteger, readRational

\begin{code}
module HsParseUtils (
	  parseError		-- String -> Pa
	, splitTyConApp		-- HsType -> P (HsName,[HsType])
	, mkRecConstrOrUpdate	-- HsExp -> [HsFieldUpdate] -> P HsExp
	, checkPrec 		-- String -> P String
	, checkContext		-- HsType -> P HsContext
	, checkAssertion	-- HsType -> P HsAsst
	, checkDataHeader	-- HsType -> P (HsContext,HsName,[HsName])
	, checkSimple		-- HsType -> [HsName] -> P ((HsName,[HsName]))
	, checkPattern		-- HsExp -> P HsPat
	, checkPatterns		-- [HsExp] -> P [HsPat]
	, checkExpr		-- HsExp -> P HsExp
	, checkValDef		-- (SrcLoc, HsExp, HsRhs, [HsDecl]) -> P HsDecl
	, checkUnQual		-- HsQName -> P HsName
	, readInteger 		-- String -> Integer
	, readRational 		-- String -> Rational

	, toVarHsName		-- HsName -> HsName
	, toTyClsHsName		-- HsName -> HsName
 ) where

import HsSyn
import HsParseMonad

import Char(isDigit,isOctDigit,isHexDigit,digitToInt)
import Ratio
\end{code}

\begin{code}
parseError :: String -> P a
parseError s = \r (SrcLoc y x) -> 
    failP (show y ++ ":" ++ show x ++ ": " ++ s) r (SrcLoc y x)

splitTyConApp :: HsType -> P (HsName,[HsType])
splitTyConApp t = split t []
 where
	split :: HsType -> [HsType] -> P (HsName,[HsType])
	split (HsTyApp t u) ts = split t (u:ts)
	split (HsTyCon (UnQual t)) ts = returnP (t,ts)
		-- to cope with data [] = [] | a:[a]
	split (HsTyCon (Qual m t)) ts = returnP (t,ts)
	split _ _ = parseError "Illegal data/newtype declaration"

-----------------------------------------------------------------------------
-- Various Syntactic Checks

checkContext :: HsType -> P HsContext
checkContext (HsTyTuple True ts) = 
     mapP checkAssertion ts `thenP` \cs ->
     returnP cs
checkContext t = 
     checkAssertion t `thenP` \c ->
     returnP [c]

-- Changed for multi-parameter type classes

checkAssertion :: HsType -> P HsAsst
checkAssertion = checkAssertion' []
	where	checkAssertion' ts (HsTyCon c) = returnP (c,ts)
		checkAssertion' ts (HsTyApp a t) = checkAssertion' (t:ts) a
		checkAssertion' _ _ = parseError "Illegal class assertion"


checkDataHeader :: HsType -> P (HsContext,HsName,[HsName])
checkDataHeader (HsForAllType Nothing cs t) =
   checkSimple t []	     `thenP` \(c,ts) ->
   returnP (cs,c,ts)
checkDataHeader t =
   checkSimple t []	     `thenP` \(c,ts) ->
   returnP ([],c,ts)

checkSimple :: HsType -> [HsName] -> P ((HsName,[HsName]))
checkSimple (HsTyApp l (HsTyVar a)) xs = checkSimple l (a:xs)
checkSimple (HsTyCon (UnQual t))    xs = returnP (t,xs)
checkSimple (HsTyCon (Qual m t))    xs = returnP (t,xs)
checkSimple _ _ = parseError "Illegal data/newtype declaration"

-----------------------------------------------------------------------------
-- Checking Patterns.

-- We parse patterns as expressions and check for valid patterns below,
-- converting the expression into a pattern at the same time.

checkPattern :: HsExp -> P HsPat
checkPattern e = checkPat e []

checkPatterns :: [HsExp] -> P [HsPat]
checkPatterns es = mapP checkPattern es

checkPat :: HsExp -> [HsPat] -> P HsPat
checkPat (HsCon c) args = returnP (HsPApp c args)
checkPat (HsApp f x) args = checkPat x [] `thenP` \x -> checkPat f (x:args)
checkPat e [] = case e of
	HsVar (UnQual x)   -> returnP (HsPVar x)
	HsLit l            -> returnP (HsPLit l)
	HsInfixApp l op r  -> checkPat l [] `thenP` \l ->
			      checkPat r [] `thenP` \r ->
			      case op of
				 HsCon c -> returnP (HsPInfixApp l c r)
				 _ -> patFail
	HsTuple b es       -> mapP (\e -> checkPat e []) es `thenP` \ps ->
			      returnP (HsPTuple b ps)
	HsList es	   -> mapP (\e -> checkPat e []) es `thenP` \ps ->
			      returnP (HsPList ps)
	HsParen e	   -> checkPat e [] `thenP` (returnP . HsPParen)
	HsAsPat n e	   -> checkPat e [] `thenP` (returnP . HsPAsPat n)
	HsWildCard	   -> returnP HsPWildCard
	HsIrrPat e	   -> checkPat e [] `thenP` (returnP . HsPIrrPat)
	HsRecConstr c fs   -> mapP checkPatField fs `thenP` \fs ->
			      returnP (HsPRec c fs)
	HsNegApp (HsLit l) -> returnP (HsPNeg (HsPLit l))
	_ -> patFail

checkPat _ _ = patFail

checkPatField :: HsFieldUpdate -> P HsPatField
checkPatField (HsFieldUpdate n e) = 
   checkPat e [] `thenP` \p ->returnP (HsPFieldPat n p)

patFail = parseError "Parse error in pattern"

-----------------------------------------------------------------------------
-- Check Expression Syntax

checkExpr :: HsExp -> P HsExp
checkExpr e = case e of
	HsVar _			  -> returnP e
	HsCon _			  -> returnP e
	HsLit _			  -> returnP e
	HsInfixApp e1 e2 e3	  -> check3Exprs e1 e2 e3 HsInfixApp
	HsApp e1 e2		  -> check2Exprs e1 e2 HsApp
	HsNegApp e		  -> check1Expr e HsNegApp
	HsLambda ps e		  -> check1Expr e (HsLambda ps)
	HsLet bs e		  -> check1Expr e (HsLet bs)
	HsIf e1 e2 e3		  -> check3Exprs e1 e2 e3 HsIf
	HsCase e alts		  -> mapP checkAlt alts `thenP` \alts ->
				     checkExpr e `thenP` \e ->
				     returnP (HsCase e alts)
	HsDo stmts		  -> mapP checkStmt stmts `thenP` (returnP . HsDo)
	HsTuple b es		  -> checkManyExprs es (HsTuple b)
	HsList es		  -> checkManyExprs es HsList
	HsParen e		  -> check1Expr e HsParen
	HsLeftSection e1 e2	  -> check2Exprs e1 e2 HsLeftSection
	HsRightSection e1 e2      -> check2Exprs e1 e2 HsRightSection
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
	HsExpTypeSig loc e ty     -> checkExpr e `thenP` \e ->
				     returnP (HsExpTypeSig loc e ty)
	_                         -> parseError "parse error in expression"

-- type signature for polymorphic recursion!!
check1Expr :: HsExp -> (HsExp -> a) -> P a
check1Expr e f = checkExpr e `thenP` (returnP . f)

check2Exprs :: HsExp -> HsExp -> (HsExp -> HsExp -> a) -> P a
check2Exprs e1 e2 f = 
	checkExpr e1 `thenP` \e1 ->
	checkExpr e2 `thenP` \e2 ->
	returnP (f e1 e2)

check3Exprs :: HsExp -> HsExp -> HsExp -> (HsExp -> HsExp -> HsExp -> a) -> P a
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

checkGAlt (HsGuardedAlt loc stmts e) = 
  mapP checkStmt stmts 	`thenP` \stmts ->
  checkExpr e 		`thenP` \e ->
  returnP (HsGuardedAlt loc stmts e)

checkStmt (HsGenerator p e) = check1Expr e (HsGenerator p)
checkStmt (HsQualifier e)   = check1Expr e HsQualifier
checkStmt s@(HsLetStmt bs)  = returnP s

checkField (HsFieldUpdate n e) = check1Expr e (HsFieldUpdate n)

-----------------------------------------------------------------------------
-- Check Equation Syntax

checkValDef :: (SrcLoc, HsExp, HsRhs, [HsDecl]) -> P HsDecl
checkValDef (srcloc, lhs, rhs, whereBinds) =
    case isFunLhs lhs [] of
 	 Just (f,es) -> checkPatterns es `thenP` \ps ->
	          	returnP (HsFunBind [HsMatch srcloc f ps rhs whereBinds])
         Nothing     -> checkPattern lhs `thenP` \lhs ->
		  	returnP (HsPatBind srcloc lhs rhs whereBinds)

-- A variable binding is parsed as an HsPatBind.

isFunLhs (HsInfixApp l (HsVar op) r) es = Just (op, l:r:es)
isFunLhs (HsApp (HsVar f) e) es = Just (f,e:es)
isFunLhs (HsApp (HsParen f) e) es = isFunLhs f (e:es)
isFunLhs (HsApp f e) es = isFunLhs f (e:es)
isFunLhs _ _ = Nothing

-----------------------------------------------------------------------------
-- Check that an identifier or symbol is unqualified.
-- For occasions when doing this in the grammar would cause conflicts.

checkUnQual :: HsQName -> P HsName
checkUnQual (Qual _ _) = parseError "Illegal qualified name"
checkUnQual (UnQual n) = returnP n

-----------------------------------------------------------------------------
-- Miscellaneous utilities

toVarHsName :: HsName -> HsName
toVarHsName (HsTyClsName n) = HsVarName n
toVarHsName n = n

toTyClsHsName :: HsName -> HsName
toTyClsHsName (HsVarName n) = HsTyClsName n
toTyClsHsName n = n

checkPrec :: Integer -> P ()
checkPrec i | i >= 0 && i <= 9 = returnP ()
checkPrec i                    = parseError ("Illegal precedence: " ++ show i)

-- Stolen from Hugs' Prelude

readInteger :: String -> Integer
readInteger ('0':'o':ds) = readInteger2  8 isOctDigit ds
readInteger ('0':'x':ds) = readInteger2 16 isHexDigit ds
readInteger          ds  = readInteger2 10 isDigit    ds

readInteger2 :: Integer -> (Char -> Bool) -> String -> Integer
readInteger2 radix isDig ds 
  = foldl1 (\n d -> n * radix + d) (map (fromIntegral . digitToInt) ds)

-- Hack...

readRational :: String -> Rational
readRational xs = (readInteger (i++m))%1 * 10^^(case e of {[] -> 0;  ('+':e2) -> read e2; _ -> read e} - length m)
  where (i,r1) = span isDigit xs
        (m,r2) = span isDigit (dropWhile (=='.') r1)
        e      = dropWhile (=='e') r2

mkRecConstrOrUpdate :: HsExp -> [HsFieldUpdate] -> P HsExp
mkRecConstrOrUpdate (HsCon c) fs       = returnP (HsRecConstr c fs)
mkRecConstrOrUpdate exp       fs@(_:_) = returnP (HsRecUpdate exp fs)
mkRecConstrOrUpdate _         _        = parseError "Empty record update"
\end{code}
