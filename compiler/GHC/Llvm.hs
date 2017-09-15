-- ----------------------------------------------------------------------------
-- | This module supplies bindings to generate Llvm IR from Haskell
-- (<http://www.llvm.org/docs/LangRef.html>).
--
-- Note: this module is developed in a demand driven way. It is no complete
-- LLVM binding library in Haskell, but enough to generate code for GHC.
--
-- This code is derived from code taken from the Essential Haskell Compiler
-- (EHC) project.
--

module GHC.Llvm (
        -- * Modules, Functions and Blocks
        LlvmModule(..),

        LlvmFunction(..), LlvmFunctionDecl(..),
        LlvmFunctions, LlvmFunctionDecls,
        LlvmStatement(..), LlvmExpression(..),
        LlvmBlocks, LlvmBlock(..), LlvmBlockId,
        LlvmParamAttr(..), LlvmParameter,

        -- * Atomic operations
        LlvmAtomicOp(..),

        -- * Fence synchronization
        LlvmSyncOrdering(..),

        -- * Call Handling
        LlvmCallConvention(..), LlvmCallType(..), LlvmParameterListType(..),
        LlvmLinkageType(..), LlvmFuncAttr(..),

        -- * Operations and Comparisons
        LlvmCmpOp(..), LlvmMachOp(..), LlvmCastOp(..),

        -- * Variables and Type System
        LlvmVar(..), LlvmStatic(..), LlvmLit(..), LlvmType(..),
        LlvmAlias, LMGlobal(..), LMString, LMSection, LMAlign,
        LMConst(..),

        -- ** Some basic types
        i64, i32, i16, i8, i1, i8Ptr, llvmWord, llvmWordPtr,

        -- ** Metadata types
        MetaExpr(..), MetaAnnot(..), MetaDecl(..), MetaId(..),
        Distinction(..),
        -- *** Module flags
        ModuleFlagBehavior(..),
        ModuleFlag(..),
        moduleFlagToMetaExpr,

        -- ** Operations on the type system.
        isGlobal, getLitType, getVarType,
        getLink, getStatType, pVarLift, pVarLower,
        pLift, pLower, isInt, isFloat, isPointer, isVector, llvmWidthInBits,

        -- * Pretty Printing
        ppVar, ppLit, ppTypeLit, ppName, ppPlainName,
        ppLlvmModule, ppLlvmComments, ppLlvmComment, ppLlvmGlobals,
        ppLlvmGlobal, ppLlvmFunctionDecls, ppLlvmFunctionDecl, ppLlvmFunctions,
        ppLlvmFunction, ppLlvmAlias, ppLlvmAliases, ppLlvmMetas, ppLlvmMeta,

    ) where

import GHC.Llvm.Syntax
import GHC.Llvm.MetaData
import GHC.Llvm.Ppr
import GHC.Llvm.Types

