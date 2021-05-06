{- | The public face of Template Haskell

For other documentation, refer to:
<http://www.haskell.org/haskellwiki/Template_Haskell>

-}
{-# LANGUAGE Safe #-}
module Language.Haskell.TH(
        -- * The monad and its operations
        Q,
        runQ,
        Quote(..),
        -- ** Administration: errors, locations and IO
        reportError,              -- :: String -> Q ()
        reportWarning,            -- :: String -> Q ()
        report,                   -- :: Bool -> String -> Q ()
        recover,          -- :: Q a -> Q a -> Q a
        location,         -- :: Q Loc
        Loc(..),
        runIO,            -- :: IO a -> Q a
        -- ** Querying the compiler
        -- *** Reify
        reify,            -- :: Name -> Q Info
        reifyModule,
        newDeclarationGroup,
        Info(..), ModuleInfo(..),
        InstanceDec,
        ParentName,
        SumAlt, SumArity,
        Arity,
        Unlifted,
        -- *** Language extension lookup
        Extension(..),
        extsEnabled, isExtEnabled,
        -- *** Name lookup
        lookupTypeName,  -- :: String -> Q (Maybe Name)
        lookupValueName, -- :: String -> Q (Maybe Name)
        -- *** Fixity lookup
        reifyFixity,
        -- *** Type lookup
        reifyType,
        -- *** Instance lookup
        reifyInstances,
        isInstance,
        -- *** Roles lookup
        reifyRoles,
        -- *** Annotation lookup
        reifyAnnotations, AnnLookup(..),
        -- *** Constructor strictness lookup
        reifyConStrictness,

        -- * Typed expressions
        TExp, unType,
        Code(..), unTypeCode, unsafeCodeCoerce, hoistCode, bindCode,
        bindCode_, joinCode, liftCode,

        -- * Names
        Name, NameSpace,        -- Abstract
        -- ** Constructing names
        mkName,         -- :: String -> Name
        -- ** Deconstructing names
        nameBase,       -- :: Name -> String
        nameModule,     -- :: Name -> Maybe String
        namePackage,    -- :: Name -> Maybe String
        nameSpace,      -- :: Name -> Maybe NameSpace
        -- ** Built-in names
        tupleTypeName, tupleDataName,   -- Int -> Name
        unboxedTupleTypeName, unboxedTupleDataName, -- :: Int -> Name
        unboxedSumTypeName, -- :: SumArity -> Name
        unboxedSumDataName, -- :: SumAlt -> SumArity -> Name

    -- * The algebraic data types
    -- | The lowercase versions (/syntax operators/) of these constructors are
    -- preferred to these constructors, since they compose better with
    -- quotations (@[| |]@) and splices (@$( ... )@)

    -- ** Declarations
        Dec(..), Con(..), Clause(..),
        SourceUnpackedness(..), SourceStrictness(..), DecidedStrictness(..),
        Bang(..), Strict, Foreign(..), Callconv(..), Safety(..), Pragma(..),
        Inline(..), RuleMatch(..), Phases(..), RuleBndr(..), AnnTarget(..),
        FunDep(..), TySynEqn(..), TypeFamilyHead(..),
        Fixity(..), FixityDirection(..), defaultFixity, maxPrecedence,
        PatSynDir(..), PatSynArgs(..),
    -- ** Expressions
        Exp(..), Match(..), Body(..), Guard(..), Stmt(..), Range(..), Lit(..),
    -- ** Patterns
        Pat(..), FieldExp, FieldPat,
    -- ** Types
        Type(..), TyVarBndr(..), TyLit(..), Kind, Cxt, Pred, Syntax.Role(..),
        Syntax.Specificity(..),
        FamilyResultSig(..), Syntax.InjectivityAnn(..), PatSynType, BangType, VarBangType,

    -- ** Documentation
        putDoc, getDoc, DocLoc(..),

    -- * Library functions
    module Language.Haskell.TH.Lib,

    -- * Pretty-printer
    Ppr(..), pprint, pprExp, pprLit, pprPat, pprParendType

   ) where

import Language.Haskell.TH.Syntax as Syntax
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Ppr
