{- | The public face of Template Haskell

For other documentation, refer to:
<http://www.haskell.org/haskellwiki/Template_Haskell>

-}
module Language.Haskell.TH(
	-- * The monad and its operations
	Q, runQ, 
	report,		  -- :: Bool -> String -> Q ()
	recover, 	  -- :: Q a -> Q a -> Q a
	reify, 		  -- :: Name -> Q Info
	location,	  -- :: Q Location
	runIO, 		  -- :: IO a -> Q a
	lookupTypeName, lookupValueName,
        isInstance, reifyInstances,

	-- * Names
	Name, NameSpace,	-- Abstract
	mkName,  	-- :: String -> Name
	newName, 	-- :: String -> Q Name
	nameBase,	-- :: Name -> String
	nameModule,	-- :: Name -> Maybe String
	tupleTypeName, tupleDataName,	-- Int -> Name
	
    -- * The algebraic data types
    -- | The lowercase versions (/syntax operators/) of these constructors are
    -- preferred to these constructors, since they compose better with
    -- quotations (@[| |]@) and splices (@$( ... )@)
	Dec(..), Exp(..), Con(..), Type(..), TyVarBndr(..), Kind(..), Cxt,
	Pred(..), Match(..), Clause(..), Body(..), Guard(..), Stmt(..),
	Range(..), Lit(..), Pat(..), FieldExp, FieldPat, 
	Strict(..), Foreign(..), Callconv(..), Safety(..), Pragma(..),
	InlineSpec(..), FunDep(..), FamFlavour(..), Info(..), Loc(..),
	Fixity(..), FixityDirection(..), defaultFixity, maxPrecedence,

    -- * Library functions
    -- ** Abbreviations
	InfoQ, ExpQ, DecQ, DecsQ, ConQ, TypeQ, CxtQ, PredQ, MatchQ, ClauseQ, BodyQ,
	GuardQ, StmtQ, RangeQ, StrictTypeQ, VarStrictTypeQ, PatQ, FieldPatQ,
        InlineSpecQ,

    -- ** Constructors lifted to 'Q'
    -- *** Literals
	intPrimL, wordPrimL, floatPrimL, doublePrimL, integerL, rationalL,
	charL, stringL, stringPrimL,
    -- *** Patterns
	litP, varP, tupP, conP, uInfixP, parensP, infixP,
	tildeP, bangP, asP, wildP, recP,
	listP, sigP, viewP,
	fieldPat,

    -- *** Pattern Guards
	normalB, guardedB, normalG, normalGE, patG, patGE, match, clause, 

    -- *** Expressions
	dyn, global, varE, conE, litE, appE, uInfixE, parensE,
	infixE, infixApp, sectionL, sectionR, 
	lamE, lam1E, tupE, condE, letE, caseE, appsE,
	listE, sigE, recConE, recUpdE, stringE, fieldExp,
    -- **** Ranges
    fromE, fromThenE, fromToE, fromThenToE,

    -- ***** Ranges with more indirection
    arithSeqE,
    fromR, fromThenR, fromToR, fromThenToR, 
    -- **** Statements
    doE, compE,
    bindS, letS, noBindS, parS,

    -- *** Types
	forallT, varT, conT, appT, arrowT, listT, tupleT, sigT,
    -- **** Strictness
	isStrict, notStrict, strictType, varStrictType,
    -- **** Class Contexts
    cxt, classP, equalP, normalC, recC, infixC,

    -- *** Top Level Declarations
    -- **** Data
	valD, funD, tySynD, dataD, newtypeD,
    -- **** Class
    classD, instanceD, sigD,
    -- **** Type Family / Data Family
    familyNoKindD, familyKindD, dataInstD,
    newtypeInstD, tySynInstD, 
    typeFam, dataFam,
    -- **** Foreign Function Interface (FFI)
    cCall, stdCall, unsafe, safe, forImpD,
    -- **** Pragmas
    -- | Just inline supported so far
    inlineSpecNoPhase, inlineSpecPhase,
    pragInlD, pragSpecD,

	-- * Pretty-printer
    Ppr(..), pprint, pprExp, pprLit, pprPat, pprParendType
	
   ) where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Ppr

