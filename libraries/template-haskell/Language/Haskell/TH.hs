-- The public face of Template Haskell

module Language.Haskell.TH(
	-- The monad and its operations
	Q, runQ, 
	report,		  -- :: Bool -> String -> Q ()
	recover, 	  -- :: Q a -> Q a -> Q a
	reify, 		  -- :: Name -> Q Info
	location,	  -- :: Q Location
	runIO, 		  -- :: IO a -> Q a

	-- Names
	Name, 
	mkName,  	-- :: String -> Name
	newName, 	-- :: String -> Q Name
	nameBase,	-- :: Name -> String
	nameModule,	-- :: Name -> Maybe String
	tupleTypeName, tupleDataName,	-- Int -> Name
	
	-- The algebraic data types
	Dec(..), Exp(..), Con(..), Type(..), TyVarBndr(..), Kind(..), Cxt,
	Pred(..), Match(..), Clause(..), Body(..), Guard(..), Stmt(..),
	Range(..), Lit(..), Pat(..), FieldExp, FieldPat, 
	Strict(..), Foreign(..), Callconv(..), Safety(..), Pragma(..),
	InlineSpec(..), FunDep(..), FamFlavour(..), Info(..), Loc(..),
	Fixity(..), FixityDirection(..), defaultFixity, maxPrecedence,

	-- Library functions
	InfoQ, ExpQ, DecQ, ConQ, TypeQ, CxtQ, PredQ, MatchQ, ClauseQ, BodyQ,
	GuardQ, StmtQ, RangeQ, StrictTypeQ, VarStrictTypeQ, PatQ, FieldPatQ,
        InlineSpecQ,
	intPrimL, wordPrimL, floatPrimL, doublePrimL, integerL, rationalL,
	charL, stringL,
	litP, varP, tupP, conP, infixP, tildeP, bangP, asP, wildP, recP,
	listP, sigP, 
	fieldPat,
	bindS, letS, noBindS, parS, 
	fromR, fromThenR, fromToR, fromThenToR, 
	normalB, guardedB, normalG, normalGE, patG, patGE, match, clause, 
	dyn, global, varE, conE, litE, appE, infixE, infixApp, sectionL, sectionR, 
	lamE, lam1E, tupE, condE, letE, caseE, doE, compE, arithSeqE, appsE,
	fromE, fromThenE, fromToE, fromThenToE,
	listE, sigE, recConE, recUpdE, stringE, fieldExp,
	valD, funD, tySynD, dataD, newtypeD, classD, instanceD, sigD, forImpD,
        pragInlD, pragSpecD, familyNoKindD, familyKindD, dataInstD,
        newtypeInstD, tySynInstD, 
	cxt, classP, equalP, normalC, recC, infixC,
	forallT, varT, conT, appT, arrowT, listT, tupleT, sigT,
	isStrict, notStrict, strictType, varStrictType,
	cCall, stdCall, unsafe, safe, threadsafe, 
        inlineSpecNoPhase, inlineSpecPhase, typeFam, dataFam,

	-- Pretty-printer
	Ppr(..), pprint, pprExp, pprLit, pprPat, pprParendType
	
   ) where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Ppr

