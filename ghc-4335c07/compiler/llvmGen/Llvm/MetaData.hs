{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Llvm.MetaData where

import GhcPrelude

import Llvm.Types
import Outputable

-- The LLVM Metadata System.
--
-- The LLVM metadata feature is poorly documented but roughly follows the
-- following design:
-- * Metadata can be constructed in a few different ways (See below).
-- * After which it can either be attached to LLVM statements to pass along
-- extra information to the optimizer and code generator OR specifically named
-- metadata has an affect on the whole module (i.e., linking behaviour).
--
--
-- # Constructing metadata
-- Metadata comes largely in three forms:
--
-- * Metadata expressions -- these are the raw metadata values that encode
--   information. They consist of metadata strings, metadata nodes, regular
--   LLVM values (both literals and references to global variables) and
--   metadata expressions (i.e., recursive data type). Some examples:
--     !{ !"hello", !0, i32 0 }
--     !{ !1, !{ i32 0 } }
--
-- * Metadata nodes -- global metadata variables that attach a metadata
--   expression to a number. For example:
--     !0 = !{ [<metadata expressions>] !}
--
-- * Named metadata -- global metadata variables that attach a metadata nodes
--   to a name. Used ONLY to communicated module level information to LLVM
--   through a meaningful name. For example:
--     !llvm.module.linkage = !{ !0, !1 }
--
--
-- # Using Metadata
-- Using metadata depends on the form it is in:
--
-- * Attach to instructions -- metadata can be attached to LLVM instructions
--   using a specific reference as follows:
--     %l = load i32* @glob, !nontemporal !10
--     %m = load i32* @glob, !nontemporal !{ i32 0, !{ i32 0 } }
--   Only metadata nodes or expressions can be attached, named metadata cannot.
--   Refer to LLVM documentation for which instructions take metadata and its
--   meaning.
--
-- * As arguments -- llvm functions can take metadata as arguments, for
--   example:
--     call void @llvm.dbg.value(metadata !{ i32 0 }, i64 0, metadata !1)
--   As with instructions, only metadata nodes or expressions can be attached.
--
-- * As a named metadata -- Here the metadata is simply declared in global
--   scope using a specific name to communicate module level information to LLVM.
--   For example:
--     !llvm.module.linkage = !{ !0, !1 }
--

-- | A reference to an un-named metadata node.
newtype MetaId = MetaId Int
               deriving (Eq, Ord, Enum)

instance Outputable MetaId where
    ppr (MetaId n) = char '!' <> int n

-- | LLVM metadata expressions
data MetaExpr = MetaStr !LMString
              | MetaNode !MetaId
              | MetaVar !LlvmVar
              | MetaStruct [MetaExpr]
              deriving (Eq)

instance Outputable MetaExpr where
  ppr (MetaVar (LMLitVar (LMNullLit _))) = text "null"
  ppr (MetaStr    s ) = char '!' <> doubleQuotes (ftext s)
  ppr (MetaNode   n ) = ppr n
  ppr (MetaVar    v ) = ppr v
  ppr (MetaStruct es) = char '!' <> braces (ppCommaJoin es)

-- | Associates some metadata with a specific label for attaching to an
-- instruction.
data MetaAnnot = MetaAnnot LMString MetaExpr
               deriving (Eq)

-- | Metadata declarations. Metadata can only be declared in global scope.
data MetaDecl
    -- | Named metadata. Only used for communicating module information to
    -- LLVM. ('!name = !{ [!<n>] }' form).
    = MetaNamed !LMString [MetaId]
    -- | Metadata node declaration.
    -- ('!0 = metadata !{ <metadata expression> }' form).
    | MetaUnnamed !MetaId !MetaExpr
