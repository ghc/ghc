{- | The public face of Template Haskell

For other documentation, refer to:
<http://www.haskell.org/haskellwiki/Template_Haskell>

-}
module Language.Haskell.TH(
	-- * The monad and its operations
	Q,
	runQ,
        -- ** Administration: errors, locations and IO
	reportError,		  -- :: String -> Q ()
	reportWarning,		  -- :: String -> Q ()
	report,			  -- :: Bool -> String -> Q ()
	recover, 	  -- :: Q a -> Q a -> Q a
	location,	  -- :: Q Loc
	Loc(..),
	runIO, 		  -- :: IO a -> Q a
	-- ** Querying the compiler
	-- *** Reify
	reify, 		  -- :: Name -> Q Info
	Info(..),
	InstanceDec,
	ParentName,
	Arity,
	Unlifted,
	-- *** Name lookup
	lookupTypeName,	 -- :: String -> Q (Maybe Name)
	lookupValueName, -- :: String -> Q (Maybe Name)
	-- *** Instance lookup
	reifyInstances,
	isInstance,
        -- *** Roles lookup
        reifyRoles,
        -- *** Annotation lookup
        reifyAnnotations, AnnLookup(..),

	-- * Typed expressions
	TExp, unType,

	-- * Names
	Name, NameSpace,	-- Abstract
	-- ** Constructing names
	mkName,  	-- :: String -> Name
	newName, 	-- :: String -> Q Name
	-- ** Deconstructing names
	nameBase,	-- :: Name -> String
	nameModule,	-- :: Name -> Maybe String
	-- ** Built-in names
	tupleTypeName, tupleDataName,	-- Int -> Name
	unboxedTupleTypeName, unboxedTupleDataName, -- :: Int -> Name

    -- * The algebraic data types
    -- | The lowercase versions (/syntax operators/) of these constructors are
    -- preferred to these constructors, since they compose better with
    -- quotations (@[| |]@) and splices (@$( ... )@)

    -- ** Declarations
	Dec(..), Con(..), Clause(..), 
	Strict(..), Foreign(..), Callconv(..), Safety(..), Pragma(..),
	Inline(..), RuleMatch(..), Phases(..), RuleBndr(..), AnnTarget(..),
	FunDep(..), FamFlavour(..), TySynEqn(..),
	Fixity(..), FixityDirection(..), defaultFixity, maxPrecedence,
    -- ** Expressions
        Exp(..), Match(..), Body(..), Guard(..), Stmt(..), Range(..), Lit(..),
    -- ** Patterns
        Pat(..), FieldExp, FieldPat,
    -- ** Types
        Type(..), TyVarBndr(..), TyLit(..), Kind, Cxt, Pred(..), Syntax.Role(..),

    -- * Library functions
    -- ** Abbreviations
        InfoQ, ExpQ, DecQ, DecsQ, ConQ, TypeQ, TyLitQ, CxtQ, PredQ, MatchQ, ClauseQ,
        BodyQ, GuardQ, StmtQ, RangeQ, StrictTypeQ, VarStrictTypeQ, PatQ, FieldPatQ,
        RuleBndrQ, TySynEqnQ,

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
	lamE, lam1E, lamCaseE, tupE, condE, multiIfE, letE, caseE, appsE,
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
	forallT, varT, conT, appT, arrowT, listT, tupleT, sigT, litT,
    promotedT, promotedTupleT, promotedNilT, promotedConsT,
    -- **** Type literals
    numTyLit, strTyLit,
    -- **** Strictness
	isStrict, notStrict, strictType, varStrictType,
    -- **** Class Contexts
    cxt, classP, equalP, normalC, recC, infixC, forallC,

    -- *** Kinds
  varK, conK, tupleK, arrowK, listK, appK, starK, constraintK,

    -- *** Roles
    nominalR, representationalR, phantomR, inferR,

    -- *** Top Level Declarations
    -- **** Data
	valD, funD, tySynD, dataD, newtypeD,
    -- **** Class
    classD, instanceD, sigD,
    -- **** Role annotations
    roleAnnotD,
    -- **** Type Family / Data Family
    familyNoKindD, familyKindD, dataInstD,
    closedTypeFamilyNoKindD, closedTypeFamilyKindD,
    newtypeInstD, tySynInstD,
    typeFam, dataFam, tySynEqn,
    -- **** Foreign Function Interface (FFI)
    cCall, stdCall, unsafe, safe, forImpD,
    -- **** Pragmas
    ruleVar, typedRuleVar,
    pragInlD, pragSpecD, pragSpecInlD, pragSpecInstD, pragRuleD, pragAnnD,

	-- * Pretty-printer
    Ppr(..), pprint, pprExp, pprLit, pprPat, pprParendType

   ) where

import Language.Haskell.TH.Syntax as Syntax
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Ppr

