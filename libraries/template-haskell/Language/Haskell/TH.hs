-- The public face of Template Haskell

module Language.Haskell.TH(
	-- The monad and its operations
	Q, runQ, 
	report,		-- :: Bool -> String -> Q ()
	recover, 	-- :: Q a -> Q a -> Q a
	reify, 		-- :: Name -> Q Info
	currentModule, 	-- :: Q String
	runIO, 		-- :: IO a -> Q a

	-- Names
	Name, 
	mkName,  	-- :: String -> Name
	newName, 	-- :: String -> Q Name
	nameBase,	-- :: Name -> String
	nameModule,	-- :: Name -> Maybe String
	tupleTypeName, tupleDataName,	-- Int -> Name
	
	-- The algebraic data types
	Dec(..), Exp(..), Con(..), Type(..), Cxt, Match(..), 
	Clause(..), Body(..), Guard(..), Stmt(..), Range(..),
	Lit(..), Pat(..), FieldExp, FieldPat, 
	Strict(..), Foreign(..), Callconv(..), Safety(..), FunDep(..),
	Info(..), 
	Fixity(..), FixityDirection(..), defaultFixity, maxPrecedence,

	-- Library functions
	InfoQ, ExpQ, DecQ, ConQ, TypeQ, CxtQ, MatchQ, ClauseQ, BodyQ, GuardQ,
	StmtQ, RangeQ, StrictTypeQ, VarStrictTypeQ, PatQ, FieldPatQ,
	intPrimL, floatPrimL, doublePrimL, integerL, charL, stringL, rationalL, 
	litP, varP, tupP, conP, infixP, tildeP, asP, wildP, recP, listP, sigP,
	fieldPat,
	bindS, letS, noBindS, parS, 
	fromR, fromThenR, fromToR, fromThenToR, 
	normalB, guardedB, normalG, normalGE, patG, patGE, match, clause, 
	dyn, global, varE, conE, litE, appE, infixE, infixApp, sectionL, sectionR, 
	lamE, lam1E, tupE, condE, letE, caseE, doE, compE, arithSeqE, appsE,
	fromE, fromThenE, fromToE, fromThenToE,
	listE, sigE, recConE, recUpdE, stringE, fieldExp,
	valD, funD, tySynD, dataD, newtypeD, classD, instanceD, sigD, forImpD,
	cxt, normalC, recC, infixC,
	forallT, varT, conT, appT, arrowT, listT, tupleT,
	isStrict, notStrict, strictType, varStrictType,
	cCall, stdCall, unsafe, safe, threadsafe,

	-- Pretty-printer
	Ppr(..), pprint, pprExp, pprLit, pprPat, pprParendType
	
   ) where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Ppr

