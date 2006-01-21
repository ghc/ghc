-----------------------------------------------------------------------------
-- $Id: HsParseUtils.lhs,v 1.6 2003/10/20 17:19:23 sof Exp $
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
	, checkContext		-- HsType -> P HsIPContext
	, checkIPContext        -- HsIPContext -> P HsContext
	, checkAssertion	-- HsType -> P HsAsst
	, checkInstHeader	-- HsType -> P (HsContext, HsAsst)
	, checkClassHeader	-- HsType -> P (HsContext, HsName, [HsType])
	, checkDataHeader	-- HsType -> P (HsContext,HsName,[HsName])
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
import List(partition)
import Ratio
\end{code}

\begin{code}
parseError :: String -> P a
parseError s = \r (SrcLoc y x f) -> 
    failP (show f ++ ": " ++ show y ++ ":" ++ show x ++ ": " ++ s) r (SrcLoc y x f)

splitTyConApp :: HsType -> P (HsName,[HsType])
splitTyConApp t0 = split t0 []
 where
	split :: HsType -> [HsType] -> P (HsName,[HsType])
	split (HsTyApp t u) ts = split t (u:ts)
	split (HsTyCon (UnQual t)) ts = returnP (t,ts)
		-- to cope with data [] = [] | a:[a]
	split (HsTyCon (Qual _ t)) ts = returnP (t,ts)
	split _ _ = parseError "Illegal data/newtype declaration"

-----------------------------------------------------------------------------
-- Various Syntactic Checks

checkContext :: HsType -> P HsIPContext
checkContext (HsTyTuple True ts) = mapP checkCtxt ts
  where
   checkCtxt (HsTyIP n ty) = returnP (HsIP n ty)
   checkCtxt t0 = 
      checkAssertion t0 `thenP` \ c ->
      returnP (HsAssump c)
checkContext (HsTyIP n t) = returnP [HsIP n t]
checkContext t = 
     checkAssertion t `thenP` \c ->
     returnP [HsAssump c]

checkIPContext :: HsIPContext -> P HsContext
checkIPContext ls = 
  case partition isIP ls of
   ([],cs) -> returnP (map (\ (HsAssump c) -> c) cs)
   (_,_)   -> parseError "Unexpected implicit parameter in context"
 where
  isIP HsIP{} = True
  isIP _      = False

-- Changed for multi-parameter type classes

checkAssertion :: HsType -> P HsAsst
checkAssertion = checkAssertion' []
	where	checkAssertion' ts (HsTyCon c) = returnP (c,ts)
		checkAssertion' ts (HsTyApp a t) = checkAssertion' (t:ts) a
		checkAssertion' _ _ = parseError "Illegal class assertion"

checkInstHeader :: HsType -> P (HsContext, HsAsst)
checkInstHeader (HsForAllType Nothing ctxt ty) =
  checkAssertion ty   `thenP` \asst ->
  checkIPContext ctxt `thenP` \ctxt' ->
  returnP (ctxt', asst)
checkInstHeader ty =
  checkAssertion ty `thenP` \asst ->
  returnP ([], asst)

checkDataHeader :: HsType -> P (HsContext,HsName,[HsName])
checkDataHeader (HsForAllType Nothing cs t) =
	checkSimple "data/newtype" t [] `thenP` \(c,ts) ->
        checkIPContext cs `thenP` \cs' ->
	returnP (cs',c,ts)
checkDataHeader ty =
	checkSimple "data/newtype" ty [] `thenP` \(c,ts) ->
	returnP ([],c,ts)

checkClassHeader :: HsType -> P (HsContext,HsName,[HsName])
checkClassHeader (HsForAllType Nothing cs t) = 
	checkSimple "class" t [] `thenP` \(c,ts) ->
        checkIPContext cs        `thenP` \cs' ->
	returnP (cs',c,ts)
checkClassHeader ty = 
	checkSimple "class" ty [] `thenP` \(c,ts) ->
	returnP ([],c,ts)

checkSimple :: String -> HsType -> [HsName] -> P ((HsName,[HsName]))
checkSimple kw (HsTyApp l (HsTyVar a)) xs = checkSimple kw l (a:xs)
checkSimple _kw (HsTyCon (UnQual t))   xs = returnP (t,xs)
checkSimple _ (HsTyCon (Qual m t))    xs
  | m == prelude_mod = returnP (t,xs)	-- for "special" declarations
checkSimple kw _ _ = failP ("Illegal " ++ kw ++ " declaration")

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
checkPat (HsApp f x0) args = checkPat x0 [] `thenP` \x -> checkPat f (x:args)
checkPat e0 [] = case e0 of
	HsVar (UnQual x)     -> returnP (HsPVar x)
	HsLit l              -> returnP (HsPLit l)
	HsInfixApp l0 op r0  -> checkPat l0 [] `thenP` \l ->
			        checkPat r0 [] `thenP` \r ->
			        case op of
				     HsCon c -> returnP (HsPInfixApp l c r)
				     _       -> patFail
	HsTuple b es         -> mapP (\e -> checkPat e []) es `thenP` \ps ->
			        returnP (HsPTuple b ps)
	HsList es	     -> mapP (\e -> checkPat e []) es `thenP` \ps ->
			        returnP (HsPList ps)
	HsParen e	     -> checkPat e [] `thenP` (returnP . HsPParen)
	HsAsPat n e	     -> checkPat e [] `thenP` (returnP . HsPAsPat n)
	HsWildCard	     -> returnP HsPWildCard
	HsIrrPat e	     -> checkPat e [] `thenP` (returnP . HsPIrrPat)
	HsRecConstr c fs0    -> mapP checkPatField fs0 `thenP` \fs ->
			        returnP (HsPRec c fs)
	HsNegApp (HsLit l)   -> returnP (HsPNeg (HsPLit l))
	HsExpTypeSig _ e1 ty -> checkPat e1 [] `thenP` \e -> 
			        returnP (HsPTypeSig e ty)
	_                    -> patFail

checkPat _ _ = patFail

checkPatField :: HsFieldUpdate -> P HsPatField
checkPatField (HsFieldUpdate n e) = 
   checkPat e [] `thenP` \p ->returnP (HsPFieldPat n p)

patFail :: P a
patFail = parseError "Parse error in pattern"

-----------------------------------------------------------------------------
-- Check Expression Syntax

checkExpr :: HsExp -> P HsExp
checkExpr e0 = case e0 of
	HsVar _			  -> returnP e0
	HsIPVar _                 -> returnP e0
	HsCon _			  -> returnP e0
	HsLit _			  -> returnP e0
	HsInfixApp e1 e2 e3	  -> check3Exprs e1 e2 e3 HsInfixApp
	HsApp e1 e2		  -> check2Exprs e1 e2 HsApp
	HsNegApp e		  -> check1Expr e HsNegApp
	HsLambda ps e		  -> check1Expr e (HsLambda ps)
	HsLet bs e		  -> check1Expr e (HsLet bs)
	HsIf e1 e2 e3		  -> check3Exprs e1 e2 e3 HsIf
	HsCase e1 alts0		  -> mapP checkAlt alts0 `thenP` \alts ->
				     checkExpr e1 `thenP` \e ->
				     returnP (HsCase e alts)
	HsDo stmts		  -> mapP checkStmt stmts `thenP` (returnP . HsDo)
	HsTuple b es		  -> checkManyExprs es (HsTuple b)
	HsList es		  -> checkManyExprs es HsList
	HsParen e		  -> check1Expr e HsParen
	HsLeftSection e1 e2	  -> check2Exprs e1 e2 HsLeftSection
	HsRightSection e1 e2      -> check2Exprs e1 e2 HsRightSection
	HsRecConstr c fields0	  -> mapP checkField fields0 `thenP` \fields ->
				     returnP (HsRecConstr c fields)
	HsRecUpdate e1 fields0	  -> mapP checkField fields0 `thenP` \fields ->
				     checkExpr e1 `thenP` \e ->
				     returnP (HsRecUpdate e fields)
	HsEnumFrom e		  -> check1Expr e HsEnumFrom
	HsEnumFromTo e1 e2	  -> check2Exprs e1 e2 HsEnumFromTo
	HsEnumFromThen e1 e2      -> check2Exprs e1 e2 HsEnumFromThen
	HsEnumFromThenTo e1 e2 e3 -> check3Exprs e1 e2 e3 HsEnumFromThenTo
	HsListComp e1 stmts0      -> mapP checkStmt stmts0 `thenP` \stmts ->
				     checkExpr e1 `thenP` \e ->
				     returnP (HsListComp e stmts)
	HsExpTypeSig loc e1 ty    -> checkExpr e1 `thenP` \e ->
				     returnP (HsExpTypeSig loc e ty)
	_                         -> parseError "parse error in expression"

-- type signature for polymorphic recursion!!
check1Expr :: HsExp -> (HsExp -> a) -> P a
check1Expr e f = checkExpr e `thenP` (returnP . f)

check2Exprs :: HsExp -> HsExp -> (HsExp -> HsExp -> a) -> P a
check2Exprs e1 e2 f = 
	checkExpr e1 `thenP` \e1' ->
	checkExpr e2 `thenP` \e2' ->
	returnP (f e1' e2')

check3Exprs :: HsExp -> HsExp -> HsExp -> (HsExp -> HsExp -> HsExp -> a) -> P a
check3Exprs e1 e2 e3 f = 
	checkExpr e1 `thenP` \e1' ->
	checkExpr e2 `thenP` \e2' ->
	checkExpr e3 `thenP` \e3' ->
	returnP (f e1' e2' e3')

checkManyExprs :: [HsExp] -> ([HsExp] -> HsExp) -> P HsExp
checkManyExprs es0 f =
	mapP checkExpr es0 `thenP` \es ->
	returnP (f es) 

checkAlt :: HsAlt -> P HsAlt
checkAlt (HsAlt loc p galts0 bs) 
	= checkGAlts galts0 `thenP` \galts -> returnP (HsAlt loc p galts bs)

checkGAlts :: HsGuardedAlts -> P HsGuardedAlts
checkGAlts (HsUnGuardedAlt e) = check1Expr e HsUnGuardedAlt
checkGAlts (HsGuardedAlts galts) 
    	= mapP checkGAlt galts `thenP` (returnP . HsGuardedAlts)

checkGAlt :: HsGuardedAlt -> P HsGuardedAlt
checkGAlt (HsGuardedAlt loc stmts0 e0) = 
  mapP checkStmt stmts0	`thenP` \stmts ->
  checkExpr e0 		`thenP` \e ->
  returnP (HsGuardedAlt loc stmts e)

checkStmt :: HsStmt -> P HsStmt
checkStmt (HsGenerator p e) = check1Expr e (HsGenerator p)
checkStmt (HsParStmt ss)    = mapP checkStmt ss `thenP` \ ss1 -> returnP (HsParStmt ss1)
checkStmt (HsQualifier e)   = check1Expr e HsQualifier
checkStmt s@(HsLetStmt _)   = returnP s

checkField :: HsFieldUpdate -> P HsFieldUpdate
checkField (HsFieldUpdate n e) = check1Expr e (HsFieldUpdate n)

-----------------------------------------------------------------------------
-- Check Equation Syntax

checkValDef :: (SrcLoc, HsExp, HsRhs, [HsDecl]) -> P HsDecl
checkValDef (srcloc, lhs0, rhs, whereBinds) =
    case isFunLhs lhs0 [] of
 	 Just (f,es) -> checkPatterns es `thenP` \ps ->
	          	returnP (HsFunBind [HsMatch srcloc f ps rhs whereBinds])
         Nothing     -> checkPattern lhs0 `thenP` \lhs ->
		  	returnP (HsPatBind srcloc lhs rhs whereBinds)

-- A variable binding is parsed as an HsPatBind.

isFunLhs :: HsExp -> [HsExp] -> Maybe (HsQName, [HsExp])
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
    | and $ map isDig ds
        = foldl1 (\n d -> n * radix + d) (map (fromIntegral . digitToInt) ds)
    | otherwise = error $ "readInteger2:expected all digits, got " ++ show ds

-- Hack...

readRational :: String -> Rational
readRational xs = (readInteger (i++m))%1 * 10^^(case e of {[] -> 0;  ('+':e2) -> read e2; _ -> read e} - length m)
  where (i,r1) = span isDigit xs
        (m,r2) = span isDigit (dropWhile (=='.') r1)
        e      = dropWhile (=='e') r2

mkRecConstrOrUpdate :: HsExp -> [HsFieldUpdate] -> P HsExp
mkRecConstrOrUpdate (HsCon c) fs        = returnP (HsRecConstr c fs)
mkRecConstrOrUpdate exp0       fs@(_:_) = returnP (HsRecUpdate exp0 fs)
mkRecConstrOrUpdate _         _         = parseError "Empty record update"
\end{code}
