-- The public face of Template Haskell

module Language.Haskell.TH(
	-- The monad and its operations
	Q, runQ, 
	report,		-- :: Bool -> String -> Q ()
	recover, 	-- :: Q a -> Q a -> Q a
	reify, 		-- :: Name -> Q Decl
	currentModule, 	-- :: Q String
	runIO, 		-- :: IO a -> Q a

	-- Names
	Name, 
	mkName,  	-- :: String -> Name
	newName, 	-- :: String -> Q Name
	nameBase,	-- :: Name -> String
	
	-- The algebraic data types
	Dec(..), Exp(..), Con(..), Type(..), Cxt, Match(..), 
	Clause(..), Body(..), Stmt(..), Range(..),
	Lit(..), Pat(..), FieldExp, FieldPat, 
	Strict(..), Foreign(..), Callconv(..), Safety(..),
	Info(..), 
	Fixity(..), FixityDirection(..), defaultFixity, maxPrecedence,

	-- Library functions
	InfoQ, ExpQ, DecQ, ConQ, TypeQ, CxtQ, MatchQ, ClauseQ, BodyQ,
	StmtQ, RangeQ, StrictTypeQ, VarStrictTypeQ,
	intPrimL, floatPrimL, doublePrimL, integerL, charL, stringL, rationalL, 
	litP, varP, tupP, conP, tildeP, asP, wildP, recP, listP, fieldPat, 
	bindS, letS, noBindS, parS, 
	fromR, fromThenR, fromToR, fromThenToR, 
	normalB, guardedB, match, clause, 
	dyn, global, varE, conE, litE, appE, infixE, infixApp, sectionL, sectionR, 
	lamE, lam1E, tupE, condE, letE, caseE, doE, compE, arithSeqE, 
	fromE, fromThenE, fromThenToE, listE, sigE, recConE, recUpdE, stringE, fieldExp,
	valD, funD, tySynD, dataD, newtypeD, classD, instanceD, sigD, forImpD,
	normalC, recC, 
	cxt, varT, conT, appT, arrowT, tupleT, isStrict, notStrict, strictType, varStrictType,
	cCall, stdCall, unsafe, safe, threadsafe,

	-- Pretty-printer
	Ppr(..), pprint, pprExp, pprLit, pprPat, pprParendType
	
   ) where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Ppr

