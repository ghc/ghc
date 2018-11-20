{-# LANGUAGE CPP #-}

--------------------------------------------------------------------------------
-- | Pretty print LLVM IR Code.
--

module Llvm.PpLlvm (

    -- * Top level LLVM objects.
    ppLlvmModule,
    ppLlvmComments,
    ppLlvmComment,
    ppLlvmGlobals,
    ppLlvmGlobal,
    ppLlvmAliases,
    ppLlvmAlias,
    ppLlvmMetas,
    ppLlvmMeta,
    ppLlvmFunctionDecls,
    ppLlvmFunctionDecl,
    ppLlvmFunctions,
    ppLlvmFunction,

    ) where

#include "HsVersions.h"

import GhcPrelude

import Llvm.AbsSyn
import Llvm.MetaData
import Llvm.Types

import Data.List ( intersperse )
import Outputable
import Unique
import FastString ( sLit )

--------------------------------------------------------------------------------
-- * Top Level Print functions
--------------------------------------------------------------------------------

-- | Print out a whole LLVM module.
ppLlvmModule :: LlvmModule -> SDoc
ppLlvmModule (LlvmModule comments aliases meta globals decls funcs)
  = ppLlvmComments comments $+$ newLine
    $+$ ppLlvmAliases aliases $+$ newLine
    $+$ ppLlvmMetas meta $+$ newLine
    $+$ ppLlvmGlobals globals $+$ newLine
    $+$ ppLlvmFunctionDecls decls $+$ newLine
    $+$ ppLlvmFunctions funcs

-- | Print out a multi-line comment, can be inside a function or on its own
ppLlvmComments :: [LMString] -> SDoc
ppLlvmComments comments = vcat $ map ppLlvmComment comments

-- | Print out a comment, can be inside a function or on its own
ppLlvmComment :: LMString -> SDoc
ppLlvmComment com = semi <+> ftext com


-- | Print out a list of global mutable variable definitions
ppLlvmGlobals :: [LMGlobal] -> SDoc
ppLlvmGlobals ls = vcat $ map ppLlvmGlobal ls

-- | Print out a global mutable variable definition
ppLlvmGlobal :: LMGlobal -> SDoc
ppLlvmGlobal (LMGlobal var@(LMGlobalVar _ _ link x a c) dat) =
    let sect = case x of
            Just x' -> text ", section" <+> doubleQuotes (ftext x')
            Nothing -> empty

        align = case a of
            Just a' -> text ", align" <+> int a'
            Nothing -> empty

        rhs = case dat of
            Just stat -> pprSpecialStatic stat
            Nothing   -> ppr (pLower $ getVarType var)

        -- Position of linkage is different for aliases.
        const = case c of
          Global   -> "global"
          Constant -> "constant"
          Alias    -> "alias"

    in ppAssignment var $ ppr link <+> text const <+> rhs <> sect <> align
       $+$ newLine

ppLlvmGlobal (LMGlobal var val) = sdocWithDynFlags $ \dflags ->
  error $ "Non Global var ppr as global! "
          ++ showSDoc dflags (ppr var) ++ " " ++ showSDoc dflags (ppr val)


-- | Print out a list of LLVM type aliases.
ppLlvmAliases :: [LlvmAlias] -> SDoc
ppLlvmAliases tys = vcat $ map ppLlvmAlias tys

-- | Print out an LLVM type alias.
ppLlvmAlias :: LlvmAlias -> SDoc
ppLlvmAlias (name, ty)
  = char '%' <> ftext name <+> equals <+> text "type" <+> ppr ty


-- | Print out a list of LLVM metadata.
ppLlvmMetas :: [MetaDecl] -> SDoc
ppLlvmMetas metas = vcat $ map ppLlvmMeta metas

-- | Print out an LLVM metadata definition.
ppLlvmMeta :: MetaDecl -> SDoc
ppLlvmMeta (MetaUnnamed n m)
  = ppr n <+> equals <+> ppr m

ppLlvmMeta (MetaNamed n m)
  = exclamation <> ftext n <+> equals <+> exclamation <> braces nodes
  where
    nodes = hcat $ intersperse comma $ map ppr m


-- | Print out a list of function definitions.
ppLlvmFunctions :: LlvmFunctions -> SDoc
ppLlvmFunctions funcs = vcat $ map ppLlvmFunction funcs

-- | Print out a function definition.
ppLlvmFunction :: LlvmFunction -> SDoc
ppLlvmFunction fun =
    let attrDoc = ppSpaceJoin (funcAttrs fun)
        secDoc = case funcSect fun of
                      Just s' -> text "section" <+> (doubleQuotes $ ftext s')
                      Nothing -> empty
        prefixDoc = case funcPrefix fun of
                        Just v  -> text "prefix" <+> ppr v
                        Nothing -> empty
    in text "define" <+> ppLlvmFunctionHeader (funcDecl fun) (funcArgs fun)
        <+> attrDoc <+> secDoc <+> prefixDoc
        $+$ lbrace
        $+$ ppLlvmBlocks (funcBody fun)
        $+$ rbrace
        $+$ newLine
        $+$ newLine

-- | Print out a function definition header.
ppLlvmFunctionHeader :: LlvmFunctionDecl -> [LMString] -> SDoc
ppLlvmFunctionHeader (LlvmFunctionDecl n l c r varg p a) args
  = let varg' = case varg of
                      VarArgs | null p    -> sLit "..."
                              | otherwise -> sLit ", ..."
                      _otherwise          -> sLit ""
        align = case a of
                     Just a' -> text " align " <> ppr a'
                     Nothing -> empty
        args' = map (\((ty,p),n) -> ppr ty <+> ppSpaceJoin p <+> char '%'
                                    <> ftext n)
                    (zip p args)
    in ppr l <+> ppr c <+> ppr r <+> char '@' <> ftext n <> lparen <>
        (hsep $ punctuate comma args') <> ptext varg' <> rparen <> align

-- | Print out a list of function declaration.
ppLlvmFunctionDecls :: LlvmFunctionDecls -> SDoc
ppLlvmFunctionDecls decs = vcat $ map ppLlvmFunctionDecl decs

-- | Print out a function declaration.
-- Declarations define the function type but don't define the actual body of
-- the function.
ppLlvmFunctionDecl :: LlvmFunctionDecl -> SDoc
ppLlvmFunctionDecl (LlvmFunctionDecl n l c r varg p a)
  = let varg' = case varg of
                      VarArgs | null p    -> sLit "..."
                              | otherwise -> sLit ", ..."
                      _otherwise          -> sLit ""
        align = case a of
                     Just a' -> text " align" <+> ppr a'
                     Nothing -> empty
        args = hcat $ intersperse (comma <> space) $
                  map (\(t,a) -> ppr t <+> ppSpaceJoin a) p
    in text "declare" <+> ppr l <+> ppr c <+> ppr r <+> char '@' <>
        ftext n <> lparen <> args <> ptext varg' <> rparen <> align $+$ newLine


-- | Print out a list of LLVM blocks.
ppLlvmBlocks :: LlvmBlocks -> SDoc
ppLlvmBlocks blocks = vcat $ map ppLlvmBlock blocks

-- | Print out an LLVM block.
-- It must be part of a function definition.
ppLlvmBlock :: LlvmBlock -> SDoc
ppLlvmBlock (LlvmBlock blockId stmts) =
  let isLabel (MkLabel _) = True
      isLabel _           = False
      (block, rest)       = break isLabel stmts
      ppRest = case rest of
        MkLabel id:xs -> ppLlvmBlock (LlvmBlock id xs)
        _             -> empty
  in ppLlvmBlockLabel blockId
           $+$ (vcat $ map ppLlvmStatement block)
           $+$ newLine
           $+$ ppRest

-- | Print out an LLVM block label.
ppLlvmBlockLabel :: LlvmBlockId -> SDoc
ppLlvmBlockLabel id = pprUniqueAlways id <> colon


-- | Print out an LLVM statement.
ppLlvmStatement :: LlvmStatement -> SDoc
ppLlvmStatement stmt =
  let ind = (text "  " <>)
  in case stmt of
        Assignment  dst expr      -> ind $ ppAssignment dst (ppLlvmExpression expr)
        Fence       st ord        -> ind $ ppFence st ord
        Branch      target        -> ind $ ppBranch target
        BranchIf    cond ifT ifF  -> ind $ ppBranchIf cond ifT ifF
        Comment     comments      -> ind $ ppLlvmComments comments
        MkLabel     label         -> ppLlvmBlockLabel label
        Store       value ptr     -> ind $ ppStore value ptr
        Switch      scrut def tgs -> ind $ ppSwitch scrut def tgs
        Return      result        -> ind $ ppReturn result
        Expr        expr          -> ind $ ppLlvmExpression expr
        Unreachable               -> ind $ text "unreachable"
        Nop                       -> empty
        MetaStmt    meta s        -> ppMetaStatement meta s


-- | Print out an LLVM expression.
ppLlvmExpression :: LlvmExpression -> SDoc
ppLlvmExpression expr
  = case expr of
        Alloca     tp amount        -> ppAlloca tp amount
        LlvmOp     op left right    -> ppMachOp op left right
        Call       tp fp args attrs -> ppCall tp fp (map MetaVar args) attrs
        CallM      tp fp args attrs -> ppCall tp fp args attrs
        Cast       op from to       -> ppCast op from to
        Compare    op left right    -> ppCmpOp op left right
        Extract    vec idx          -> ppExtract vec idx
        ExtractV   struct idx       -> ppExtractV struct idx
        Insert     vec elt idx      -> ppInsert vec elt idx
        GetElemPtr inb ptr indexes  -> ppGetElementPtr inb ptr indexes
        Load       ptr              -> ppLoad ptr
        ALoad      ord st ptr       -> ppALoad ord st ptr
        Malloc     tp amount        -> ppMalloc tp amount
        AtomicRMW  aop tgt src ordering -> ppAtomicRMW aop tgt src ordering
        CmpXChg    addr old new s_ord f_ord -> ppCmpXChg addr old new s_ord f_ord
        Phi        tp predecessors  -> ppPhi tp predecessors
        Asm        asm c ty v se sk -> ppAsm asm c ty v se sk
        MExpr      meta expr        -> ppMetaExpr meta expr


--------------------------------------------------------------------------------
-- * Individual print functions
--------------------------------------------------------------------------------

-- | Should always be a function pointer. So a global var of function type
-- (since globals are always pointers) or a local var of pointer function type.
ppCall :: LlvmCallType -> LlvmVar -> [MetaExpr] -> [LlvmFuncAttr] -> SDoc
ppCall ct fptr args attrs = case fptr of
                           --
    -- if local var function pointer, unwrap
    LMLocalVar _ (LMPointer (LMFunction d)) -> ppCall' d

    -- should be function type otherwise
    LMGlobalVar _ (LMFunction d) _ _ _ _    -> ppCall' d

    -- not pointer or function, so error
    _other -> error $ "ppCall called with non LMFunction type!\nMust be "
                ++ " called with either global var of function type or "
                ++ "local var of pointer function type."

    where
        ppCall' (LlvmFunctionDecl _ _ cc ret argTy params _) =
            let tc = if ct == TailCall then text "tail " else empty
                ppValues = hsep $ punctuate comma $ map ppCallMetaExpr args
                ppArgTy  = (ppCommaJoin $ map fst params) <>
                           (case argTy of
                               VarArgs   -> text ", ..."
                               FixedArgs -> empty)
                fnty = space <> lparen <> ppArgTy <> rparen
                attrDoc = ppSpaceJoin attrs
            in  tc <> text "call" <+> ppr cc <+> ppr ret
                    <> fnty <+> ppName fptr <> lparen <+> ppValues
                    <+> rparen <+> attrDoc

        -- Metadata needs to be marked as having the `metadata` type when used
        -- in a call argument
        ppCallMetaExpr (MetaVar v) = ppr v
        ppCallMetaExpr v           = text "metadata" <+> ppr v

ppMachOp :: LlvmMachOp -> LlvmVar -> LlvmVar -> SDoc
ppMachOp op left right =
  (ppr op) <+> (ppr (getVarType left)) <+> ppName left
        <> comma <+> ppName right


ppCmpOp :: LlvmCmpOp -> LlvmVar -> LlvmVar -> SDoc
ppCmpOp op left right =
  let cmpOp
        | isInt (getVarType left) && isInt (getVarType right) = text "icmp"
        | isFloat (getVarType left) && isFloat (getVarType right) = text "fcmp"
        | otherwise = text "icmp" -- Just continue as its much easier to debug
        {-
        | otherwise = error ("can't compare different types, left = "
                ++ (show $ getVarType left) ++ ", right = "
                ++ (show $ getVarType right))
        -}
  in cmpOp <+> ppr op <+> ppr (getVarType left)
        <+> ppName left <> comma <+> ppName right


ppAssignment :: LlvmVar -> SDoc -> SDoc
ppAssignment var expr = ppName var <+> equals <+> expr

ppFence :: Bool -> LlvmSyncOrdering -> SDoc
ppFence st ord =
  let singleThread = case st of True  -> text "singlethread"
                                False -> empty
  in text "fence" <+> singleThread <+> ppSyncOrdering ord

ppSyncOrdering :: LlvmSyncOrdering -> SDoc
ppSyncOrdering SyncUnord     = text "unordered"
ppSyncOrdering SyncMonotonic = text "monotonic"
ppSyncOrdering SyncAcquire   = text "acquire"
ppSyncOrdering SyncRelease   = text "release"
ppSyncOrdering SyncAcqRel    = text "acq_rel"
ppSyncOrdering SyncSeqCst    = text "seq_cst"

ppAtomicOp :: LlvmAtomicOp -> SDoc
ppAtomicOp LAO_Xchg = text "xchg"
ppAtomicOp LAO_Add  = text "add"
ppAtomicOp LAO_Sub  = text "sub"
ppAtomicOp LAO_And  = text "and"
ppAtomicOp LAO_Nand = text "nand"
ppAtomicOp LAO_Or   = text "or"
ppAtomicOp LAO_Xor  = text "xor"
ppAtomicOp LAO_Max  = text "max"
ppAtomicOp LAO_Min  = text "min"
ppAtomicOp LAO_Umax = text "umax"
ppAtomicOp LAO_Umin = text "umin"

ppAtomicRMW :: LlvmAtomicOp -> LlvmVar -> LlvmVar -> LlvmSyncOrdering -> SDoc
ppAtomicRMW aop tgt src ordering =
  text "atomicrmw" <+> ppAtomicOp aop <+> ppr tgt <> comma
  <+> ppr src <+> ppSyncOrdering ordering

ppCmpXChg :: LlvmVar -> LlvmVar -> LlvmVar
          -> LlvmSyncOrdering -> LlvmSyncOrdering -> SDoc
ppCmpXChg addr old new s_ord f_ord =
  text "cmpxchg" <+> ppr addr <> comma <+> ppr old <> comma <+> ppr new
  <+> ppSyncOrdering s_ord <+> ppSyncOrdering f_ord

-- XXX: On x86, vector types need to be 16-byte aligned for aligned access, but
-- we have no way of guaranteeing that this is true with GHC (we would need to
-- modify the layout of the stack and closures, change the storage manager,
-- etc.). So, we blindly tell LLVM that *any* vector store or load could be
-- unaligned. In the future we may be able to guarantee that certain vector
-- access patterns are aligned, in which case we will need a more granular way
-- of specifying alignment.

ppLoad :: LlvmVar -> SDoc
ppLoad var = text "load" <+> ppr derefType <> comma <+> ppr var <> align
  where
    derefType = pLower $ getVarType var
    align | isVector . pLower . getVarType $ var = text ", align 1"
          | otherwise = empty

ppALoad :: LlvmSyncOrdering -> SingleThreaded -> LlvmVar -> SDoc
ppALoad ord st var = sdocWithDynFlags $ \dflags ->
  let alignment = (llvmWidthInBits dflags $ getVarType var) `quot` 8
      align     = text ", align" <+> ppr alignment
      sThreaded | st        = text " singlethread"
                | otherwise = empty
      derefType = pLower $ getVarType var
  in text "load atomic" <+> ppr derefType <> comma <+> ppr var <> sThreaded
            <+> ppSyncOrdering ord <> align

ppStore :: LlvmVar -> LlvmVar -> SDoc
ppStore val dst
    | isVecPtrVar dst = text "store" <+> ppr val <> comma <+> ppr dst <>
                        comma <+> text "align 1"
    | otherwise       = text "store" <+> ppr val <> comma <+> ppr dst
  where
    isVecPtrVar :: LlvmVar -> Bool
    isVecPtrVar = isVector . pLower . getVarType


ppCast :: LlvmCastOp -> LlvmVar -> LlvmType -> SDoc
ppCast op from to
    =   ppr op
    <+> ppr (getVarType from) <+> ppName from
    <+> text "to"
    <+> ppr to


ppMalloc :: LlvmType -> Int -> SDoc
ppMalloc tp amount =
  let amount' = LMLitVar $ LMIntLit (toInteger amount) i32
  in text "malloc" <+> ppr tp <> comma <+> ppr amount'


ppAlloca :: LlvmType -> Int -> SDoc
ppAlloca tp amount =
  let amount' = LMLitVar $ LMIntLit (toInteger amount) i32
  in text "alloca" <+> ppr tp <> comma <+> ppr amount'


ppGetElementPtr :: Bool -> LlvmVar -> [LlvmVar] -> SDoc
ppGetElementPtr inb ptr idx =
  let indexes = comma <+> ppCommaJoin idx
      inbound = if inb then text "inbounds" else empty
      derefType = pLower $ getVarType ptr
  in text "getelementptr" <+> inbound <+> ppr derefType <> comma <+> ppr ptr
                            <> indexes


ppReturn :: Maybe LlvmVar -> SDoc
ppReturn (Just var) = text "ret" <+> ppr var
ppReturn Nothing    = text "ret" <+> ppr LMVoid


ppBranch :: LlvmVar -> SDoc
ppBranch var = text "br" <+> ppr var


ppBranchIf :: LlvmVar -> LlvmVar -> LlvmVar -> SDoc
ppBranchIf cond trueT falseT
  = text "br" <+> ppr cond <> comma <+> ppr trueT <> comma <+> ppr falseT


ppPhi :: LlvmType -> [(LlvmVar,LlvmVar)] -> SDoc
ppPhi tp preds =
  let ppPreds (val, label) = brackets $ ppName val <> comma <+> ppName label
  in text "phi" <+> ppr tp <+> hsep (punctuate comma $ map ppPreds preds)


ppSwitch :: LlvmVar -> LlvmVar -> [(LlvmVar,LlvmVar)] -> SDoc
ppSwitch scrut dflt targets =
  let ppTarget  (val, lab) = ppr val <> comma <+> ppr lab
      ppTargets  xs        = brackets $ vcat (map ppTarget xs)
  in text "switch" <+> ppr scrut <> comma <+> ppr dflt
        <+> ppTargets targets


ppAsm :: LMString -> LMString -> LlvmType -> [LlvmVar] -> Bool -> Bool -> SDoc
ppAsm asm constraints rty vars sideeffect alignstack =
  let asm'  = doubleQuotes $ ftext asm
      cons  = doubleQuotes $ ftext constraints
      rty'  = ppr rty
      vars' = lparen <+> ppCommaJoin vars <+> rparen
      side  = if sideeffect then text "sideeffect" else empty
      align = if alignstack then text "alignstack" else empty
  in text "call" <+> rty' <+> text "asm" <+> side <+> align <+> asm' <> comma
        <+> cons <> vars'

ppExtract :: LlvmVar -> LlvmVar -> SDoc
ppExtract vec idx =
    text "extractelement"
    <+> ppr (getVarType vec) <+> ppName vec <> comma
    <+> ppr idx

ppExtractV :: LlvmVar -> Int -> SDoc
ppExtractV struct idx =
    text "extractvalue"
    <+> ppr (getVarType struct) <+> ppName struct <> comma
    <+> ppr idx

ppInsert :: LlvmVar -> LlvmVar -> LlvmVar -> SDoc
ppInsert vec elt idx =
    text "insertelement"
    <+> ppr (getVarType vec) <+> ppName vec <> comma
    <+> ppr (getVarType elt) <+> ppName elt <> comma
    <+> ppr idx


ppMetaStatement :: [MetaAnnot] -> LlvmStatement -> SDoc
ppMetaStatement meta stmt = ppLlvmStatement stmt <> ppMetaAnnots meta

ppMetaExpr :: [MetaAnnot] -> LlvmExpression -> SDoc
ppMetaExpr meta expr = ppLlvmExpression expr <> ppMetaAnnots meta

ppMetaAnnots :: [MetaAnnot] -> SDoc
ppMetaAnnots meta = hcat $ map ppMeta meta
  where
    ppMeta (MetaAnnot name e)
        = comma <+> exclamation <> ftext name <+>
          case e of
            MetaNode n    -> ppr n
            MetaStruct ms -> exclamation <> braces (ppCommaJoin ms)
            other         -> exclamation <> braces (ppr other) -- possible?


--------------------------------------------------------------------------------
-- * Misc functions
--------------------------------------------------------------------------------

-- | Blank line.
newLine :: SDoc
newLine = empty

-- | Exclamation point.
exclamation :: SDoc
exclamation = char '!'
