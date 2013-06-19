--------------------------------------------------------------------------------
-- | The LLVM Metadata System.
--
-- The LLVM metadata feature is poorly documented but roughly follows the
-- following design:
-- * Metadata can be constructed in a few different ways (See below).
-- * After which it can either be attached to LLVM statements to pass along
-- extra information to the optimizer and code generator OR specificially named
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
--     !{ metadata !"hello", metadata !0, i32 0 }
--     !{ metadata !1, metadata !{ i32 0 } }
--
-- * Metadata nodes -- global metadata variables that attach a metadata
--   expression to a number. For example:
--     !0 = metadata !{ [<metadata expressions>] !}
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
--     %m = load i32* @glob, !nontemporal !{ i32 0, metadata !{ i32 0 } }
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
module Llvm.MetaData where

import Data.List (intercalate)

import Llvm.Types

import FastString

-- | LLVM metadata expressions ('metadata ...' form).
data MetaExpr = MetaStr LMString
              | MetaNode Int
              | MetaVar LlvmVar
              | MetaExpr [MetaExpr]
              deriving (Eq)

-- | LLVM metadata nodes. See [Note: Metadata encoding].
data MetaVal
    -- | A literal expression as a metadata value ('!{ ..}' form).
    = MetaValExpr MetaExpr
    -- | A metadata node as a metadata value ('!10' form).
    | MetaValNode Int
    deriving (Eq)

instance Show MetaExpr where
  show (MetaStr  s ) = "metadata !\"" ++ unpackFS s ++ "\""
  show (MetaNode n ) = "metadata !" ++ show n
  show (MetaVar  v ) = show v
  show (MetaExpr es) = intercalate ", " $ map show es

instance Show MetaVal where
  show (MetaValExpr  e) = "!{ " ++ show e ++ "}"
  show (MetaValNode  n) = "!" ++ show n

-- | Associated some metadata with a specific label for attaching to an
-- instruction.
type MetaData = (LMString, MetaVal)

-- | Metadata declarations. Metadata can only be declared in global scope.
data MetaDecl
    -- | Named metadata. Only used for communicating module information to
    -- LLVM. ('!name = !{ [!<n>] }' form).
    = MetaNamed LMString [Int]
    -- | Metadata node declaration.
    -- ('!0 = metadata !{ <metadata expression> }' form).
    | MetaUnamed Int MetaExpr

-- | LLVM function call arguments.
data MetaArgs
    = ArgVar  LlvmVar  -- ^ Regular LLVM variable as argument.
    | ArgMeta MetaExpr -- ^ Metadata as argument.
    deriving (Eq)

instance Show MetaArgs where
  show (ArgVar  v) = show v
  show (ArgMeta m) = show m

{-
   Note: Metadata encoding
   ~~~~~~~~~~~~~~~~~~~~~~~
   The encoding use today has some redundancy in the form of 'MetaValNode'.
   Instead of the current encoding where MetaExpr is an independent recursive
   type, the encoding below could be used where MetaExpr and MetaVal are
   co-recursive. The current encoding was chosen instead as it appears easier
   to work with and cleaner to separate the two types.

   -- metadata ...
   data MetaExpr = MetaStr String
                 | MetaVar LlvmVar
                 | MetaVal [MetaVal]

   -- !{ .. } | !10
   data MetaVal = MetaExpr MetaExpr
                | MetaNode Int
 -}

