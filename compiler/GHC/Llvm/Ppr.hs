{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

--------------------------------------------------------------------------------
-- | Pretty print LLVM IR Code.
--

module GHC.Llvm.Ppr (

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

    ppVar,
    ppLit,
    ppTypeLit,
    ppName,
    ppPlainName

    ) where

#include "HsVersions.h"

import GHC.Prelude

import GHC.Llvm.Syntax
import GHC.Llvm.MetaData
import GHC.Llvm.Types

import Data.Int
import Data.List ( intersperse )
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Types.Unique
import GHC.Data.FastString

--------------------------------------------------------------------------------
-- * Top Level Print functions
--------------------------------------------------------------------------------

-- | Print out a whole LLVM module.
ppLlvmModule :: LlvmOpts -> LlvmModule -> SDoc
ppLlvmModule opts (LlvmModule comments aliases meta globals decls funcs)
  = ppLlvmComments comments $+$ newLine
    $+$ ppLlvmAliases aliases $+$ newLine
    $+$ ppLlvmMetas opts meta $+$ newLine
    $+$ ppLlvmGlobals opts globals $+$ newLine
    $+$ ppLlvmFunctionDecls decls $+$ newLine
    $+$ ppLlvmFunctions opts funcs

-- | Print out a multi-line comment, can be inside a function or on its own
ppLlvmComments :: [LMString] -> SDoc
ppLlvmComments comments = vcat $ map ppLlvmComment comments

-- | Print out a comment, can be inside a function or on its own
ppLlvmComment :: LMString -> SDoc
ppLlvmComment com = semi <+> ftext com


-- | Print out a list of global mutable variable definitions
ppLlvmGlobals :: LlvmOpts -> [LMGlobal] -> SDoc
ppLlvmGlobals opts ls = vcat $ map (ppLlvmGlobal opts) ls

-- | Print out a global mutable variable definition
ppLlvmGlobal :: LlvmOpts -> LMGlobal -> SDoc
ppLlvmGlobal opts (LMGlobal var@(LMGlobalVar _ _ link x a c) dat) =
    let sect = case x of
            Just x' -> text ", section" <+> doubleQuotes (ftext x')
            Nothing -> empty

        align = case a of
            Just a' -> text ", align" <+> int a'
            Nothing -> empty

        rhs = case dat of
            Just stat -> pprSpecialStatic opts stat
            Nothing   -> ppr (pLower $ getVarType var)

        -- Position of linkage is different for aliases.
        const = case c of
          Global   -> "global"
          Constant -> "constant"
          Alias    -> "alias"

    in ppAssignment opts var $ ppr link <+> text const <+> rhs <> sect <> align
       $+$ newLine

ppLlvmGlobal opts (LMGlobal var val) = pprPanic "ppLlvmGlobal" $
  text "Non Global var ppr as global! " <> ppVar opts var <> text "=" <> ppr (fmap (ppStatic opts) val)


-- | Print out a list of LLVM type aliases.
ppLlvmAliases :: [LlvmAlias] -> SDoc
ppLlvmAliases tys = vcat $ map ppLlvmAlias tys

-- | Print out an LLVM type alias.
ppLlvmAlias :: LlvmAlias -> SDoc
ppLlvmAlias (name, ty)
  = char '%' <> ftext name <+> equals <+> text "type" <+> ppr ty


-- | Print out a list of LLVM metadata.
ppLlvmMetas :: LlvmOpts -> [MetaDecl] -> SDoc
ppLlvmMetas opts metas = vcat $ map (ppLlvmMeta opts) metas

-- | Print out an LLVM metadata definition.
ppLlvmMeta :: LlvmOpts -> MetaDecl -> SDoc
ppLlvmMeta opts (MetaUnnamed n m)
  = ppr n <+> equals <+> ppMetaExpr opts m

ppLlvmMeta _opts (MetaNamed n m)
  = exclamation <> ftext n <+> equals <+> exclamation <> braces nodes
  where
    nodes = hcat $ intersperse comma $ map ppr m


-- | Print out a list of function definitions.
ppLlvmFunctions :: LlvmOpts -> LlvmFunctions -> SDoc
ppLlvmFunctions opts funcs = vcat $ map (ppLlvmFunction opts) funcs

-- | Print out a function definition.
ppLlvmFunction :: LlvmOpts -> LlvmFunction -> SDoc
ppLlvmFunction opts fun =
    let attrDoc = ppSpaceJoin (funcAttrs fun)
        secDoc = case funcSect fun of
                      Just s' -> text "section" <+> (doubleQuotes $ ftext s')
                      Nothing -> empty
        prefixDoc = case funcPrefix fun of
                        Just v  -> text "prefix" <+> ppStatic opts v
                        Nothing -> empty
    in text "define" <+> ppLlvmFunctionHeader (funcDecl fun) (funcArgs fun)
        <+> attrDoc <+> secDoc <+> prefixDoc
        $+$ lbrace
        $+$ ppLlvmBlocks opts (funcBody fun)
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
ppLlvmBlocks :: LlvmOpts -> LlvmBlocks -> SDoc
ppLlvmBlocks opts blocks = vcat $ map (ppLlvmBlock opts) blocks

-- | Print out an LLVM block.
-- It must be part of a function definition.
ppLlvmBlock :: LlvmOpts -> LlvmBlock -> SDoc
ppLlvmBlock opts (LlvmBlock blockId stmts) =
  let isLabel (MkLabel _) = True
      isLabel _           = False
      (block, rest)       = break isLabel stmts
      ppRest = case rest of
        MkLabel id:xs -> ppLlvmBlock opts (LlvmBlock id xs)
        _             -> empty
  in ppLlvmBlockLabel blockId
           $+$ (vcat $ map (ppLlvmStatement opts) block)
           $+$ newLine
           $+$ ppRest

-- | Print out an LLVM block label.
ppLlvmBlockLabel :: LlvmBlockId -> SDoc
ppLlvmBlockLabel id = pprUniqueAlways id <> colon


-- | Print out an LLVM statement.
ppLlvmStatement :: LlvmOpts -> LlvmStatement -> SDoc
ppLlvmStatement opts stmt =
  let ind = (text "  " <>)
  in case stmt of
        Assignment  dst expr      -> ind $ ppAssignment opts dst (ppLlvmExpression opts expr)
        Fence       st ord        -> ind $ ppFence st ord
        Branch      target        -> ind $ ppBranch opts target
        BranchIf    cond ifT ifF  -> ind $ ppBranchIf opts cond ifT ifF
        Comment     comments      -> ind $ ppLlvmComments comments
        MkLabel     label         -> ppLlvmBlockLabel label
        Store       value ptr align
                                  -> ind $ ppStore opts value ptr align
        Switch      scrut def tgs -> ind $ ppSwitch opts scrut def tgs
        Return      result        -> ind $ ppReturn opts result
        Expr        expr          -> ind $ ppLlvmExpression opts expr
        Unreachable               -> ind $ text "unreachable"
        Nop                       -> empty
        MetaStmt    meta s        -> ppMetaStatement opts meta s


-- | Print out an LLVM expression.
ppLlvmExpression :: LlvmOpts -> LlvmExpression -> SDoc
ppLlvmExpression opts expr
  = case expr of
        Alloca     tp amount        -> ppAlloca opts tp amount
        LlvmOp     op left right    -> ppMachOp opts op left right
        Call       tp fp args attrs -> ppCall opts tp fp (map MetaVar args) attrs
        CallM      tp fp args attrs -> ppCall opts tp fp args attrs
        Cast       op from to       -> ppCast opts op from to
        Compare    op left right    -> ppCmpOp opts op left right
        Extract    vec idx          -> ppExtract opts vec idx
        ExtractV   struct idx       -> ppExtractV opts struct idx
        Insert     vec elt idx      -> ppInsert opts vec elt idx
        GetElemPtr inb ptr indexes  -> ppGetElementPtr opts inb ptr indexes
        Load       ptr align        -> ppLoad opts ptr align
        ALoad      ord st ptr       -> ppALoad opts ord st ptr
        Malloc     tp amount        -> ppMalloc opts tp amount
        AtomicRMW  aop tgt src ordering -> ppAtomicRMW opts aop tgt src ordering
        CmpXChg    addr old new s_ord f_ord -> ppCmpXChg opts addr old new s_ord f_ord
        Phi        tp predecessors  -> ppPhi opts tp predecessors
        Asm        asm c ty v se sk -> ppAsm opts asm c ty v se sk
        MExpr      meta expr        -> ppMetaAnnotExpr opts meta expr

ppMetaExpr :: LlvmOpts -> MetaExpr -> SDoc
ppMetaExpr opts = \case
  MetaVar (LMLitVar (LMNullLit _)) -> text "null"
  MetaStr    s                     -> char '!' <> doubleQuotes (ftext s)
  MetaNode   n                     -> ppr n
  MetaVar    v                     -> ppVar opts v
  MetaStruct es                    -> char '!' <> braces (ppCommaJoin (map (ppMetaExpr opts) es))


--------------------------------------------------------------------------------
-- * Individual print functions
--------------------------------------------------------------------------------

-- | Should always be a function pointer. So a global var of function type
-- (since globals are always pointers) or a local var of pointer function type.
ppCall :: LlvmOpts -> LlvmCallType -> LlvmVar -> [MetaExpr] -> [LlvmFuncAttr] -> SDoc
ppCall opts ct fptr args attrs = case fptr of
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
                ppValues = ppCallParams opts (map snd params) args
                ppArgTy  = (ppCommaJoin $ map (ppr . fst) params) <>
                           (case argTy of
                               VarArgs   -> text ", ..."
                               FixedArgs -> empty)
                fnty = space <> lparen <> ppArgTy <> rparen
                attrDoc = ppSpaceJoin attrs
            in  tc <> text "call" <+> ppr cc <+> ppr ret
                    <> fnty <+> ppName opts fptr <> lparen <+> ppValues
                    <+> rparen <+> attrDoc

        ppCallParams :: LlvmOpts -> [[LlvmParamAttr]] -> [MetaExpr] -> SDoc
        ppCallParams opts attrs args = hsep $ punctuate comma $ zipWith ppCallMetaExpr attrs args
         where
          -- Metadata needs to be marked as having the `metadata` type when used
          -- in a call argument
          ppCallMetaExpr attrs (MetaVar v) = ppVar' attrs opts v
          ppCallMetaExpr _ v             = text "metadata" <+> ppMetaExpr opts v


ppMachOp :: LlvmOpts -> LlvmMachOp -> LlvmVar -> LlvmVar -> SDoc
ppMachOp opts op left right =
  (ppr op) <+> (ppr (getVarType left)) <+> ppName opts left
        <> comma <+> ppName opts right


ppCmpOp :: LlvmOpts -> LlvmCmpOp -> LlvmVar -> LlvmVar -> SDoc
ppCmpOp opts op left right =
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
        <+> ppName opts left <> comma <+> ppName opts right


ppAssignment :: LlvmOpts -> LlvmVar -> SDoc -> SDoc
ppAssignment opts var expr = ppName opts var <+> equals <+> expr

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

ppAtomicRMW :: LlvmOpts -> LlvmAtomicOp -> LlvmVar -> LlvmVar -> LlvmSyncOrdering -> SDoc
ppAtomicRMW opts aop tgt src ordering =
  text "atomicrmw" <+> ppAtomicOp aop <+> ppVar opts tgt <> comma
  <+> ppVar opts src <+> ppSyncOrdering ordering

ppCmpXChg :: LlvmOpts -> LlvmVar -> LlvmVar -> LlvmVar
          -> LlvmSyncOrdering -> LlvmSyncOrdering -> SDoc
ppCmpXChg opts addr old new s_ord f_ord =
  text "cmpxchg" <+> ppVar opts addr <> comma <+> ppVar opts old <> comma <+> ppVar opts new
  <+> ppSyncOrdering s_ord <+> ppSyncOrdering f_ord


ppLoad :: LlvmOpts -> LlvmVar -> Maybe Int -> SDoc
ppLoad opts var alignment =
  text "load" <+> ppr derefType <> comma <+> ppVar opts var <> align
  where
    derefType = pLower $ getVarType var
    align =
      case alignment of
        Just n  -> text ", align" <+> ppr n
        Nothing -> empty

ppALoad :: LlvmOpts -> LlvmSyncOrdering -> SingleThreaded -> LlvmVar -> SDoc
ppALoad opts ord st var =
  let alignment = (llvmWidthInBits (llvmOptsPlatform opts) $ getVarType var) `quot` 8
      align     = text ", align" <+> ppr alignment
      sThreaded | st        = text " singlethread"
                | otherwise = empty
      derefType = pLower $ getVarType var
  in text "load atomic" <+> ppr derefType <> comma <+> ppVar opts var <> sThreaded
            <+> ppSyncOrdering ord <> align

ppStore :: LlvmOpts -> LlvmVar -> LlvmVar -> LMAlign -> SDoc
ppStore opts val dst alignment =
    text "store" <+> ppVar opts val <> comma <+> ppVar opts dst <> align
  where
    align =
      case alignment of
        Just n  -> text ", align" <+> ppr n
        Nothing -> empty


ppCast :: LlvmOpts -> LlvmCastOp -> LlvmVar -> LlvmType -> SDoc
ppCast opts op from to
    =   ppr op
    <+> ppr (getVarType from) <+> ppName opts from
    <+> text "to"
    <+> ppr to


ppMalloc :: LlvmOpts -> LlvmType -> Int -> SDoc
ppMalloc opts tp amount =
  let amount' = LMLitVar $ LMIntLit (toInteger amount) i32
  in text "malloc" <+> ppr tp <> comma <+> ppVar opts amount'


ppAlloca :: LlvmOpts -> LlvmType -> Int -> SDoc
ppAlloca opts tp amount =
  let amount' = LMLitVar $ LMIntLit (toInteger amount) i32
  in text "alloca" <+> ppr tp <> comma <+> ppVar opts amount'


ppGetElementPtr :: LlvmOpts -> Bool -> LlvmVar -> [LlvmVar] -> SDoc
ppGetElementPtr opts inb ptr idx =
  let indexes = comma <+> ppCommaJoin (map (ppVar opts) idx)
      inbound = if inb then text "inbounds" else empty
      derefType = pLower $ getVarType ptr
  in text "getelementptr" <+> inbound <+> ppr derefType <> comma <+> ppVar opts ptr
                            <> indexes


ppReturn :: LlvmOpts -> Maybe LlvmVar -> SDoc
ppReturn opts (Just var) = text "ret" <+> ppVar opts var
ppReturn _    Nothing    = text "ret" <+> ppr LMVoid


ppBranch :: LlvmOpts -> LlvmVar -> SDoc
ppBranch opts var = text "br" <+> ppVar opts var


ppBranchIf :: LlvmOpts -> LlvmVar -> LlvmVar -> LlvmVar -> SDoc
ppBranchIf opts cond trueT falseT
  = text "br" <+> ppVar opts cond <> comma <+> ppVar opts trueT <> comma <+> ppVar opts falseT


ppPhi :: LlvmOpts -> LlvmType -> [(LlvmVar,LlvmVar)] -> SDoc
ppPhi opts tp preds =
  let ppPreds (val, label) = brackets $ ppName opts val <> comma <+> ppName opts label
  in text "phi" <+> ppr tp <+> hsep (punctuate comma $ map ppPreds preds)


ppSwitch :: LlvmOpts -> LlvmVar -> LlvmVar -> [(LlvmVar,LlvmVar)] -> SDoc
ppSwitch opts scrut dflt targets =
  let ppTarget  (val, lab) = ppVar opts val <> comma <+> ppVar opts lab
      ppTargets  xs        = brackets $ vcat (map ppTarget xs)
  in text "switch" <+> ppVar opts scrut <> comma <+> ppVar opts dflt
        <+> ppTargets targets


ppAsm :: LlvmOpts -> LMString -> LMString -> LlvmType -> [LlvmVar] -> Bool -> Bool -> SDoc
ppAsm opts asm constraints rty vars sideeffect alignstack =
  let asm'  = doubleQuotes $ ftext asm
      cons  = doubleQuotes $ ftext constraints
      rty'  = ppr rty
      vars' = lparen <+> ppCommaJoin (map (ppVar opts) vars) <+> rparen
      side  = if sideeffect then text "sideeffect" else empty
      align = if alignstack then text "alignstack" else empty
  in text "call" <+> rty' <+> text "asm" <+> side <+> align <+> asm' <> comma
        <+> cons <> vars'

ppExtract :: LlvmOpts -> LlvmVar -> LlvmVar -> SDoc
ppExtract opts vec idx =
    text "extractelement"
    <+> ppr (getVarType vec) <+> ppName opts vec <> comma
    <+> ppVar opts idx

ppExtractV :: LlvmOpts -> LlvmVar -> Int -> SDoc
ppExtractV opts struct idx =
    text "extractvalue"
    <+> ppr (getVarType struct) <+> ppName opts struct <> comma
    <+> ppr idx

ppInsert :: LlvmOpts -> LlvmVar -> LlvmVar -> LlvmVar -> SDoc
ppInsert opts vec elt idx =
    text "insertelement"
    <+> ppr (getVarType vec) <+> ppName opts vec <> comma
    <+> ppr (getVarType elt) <+> ppName opts elt <> comma
    <+> ppVar opts idx


ppMetaStatement :: LlvmOpts -> [MetaAnnot] -> LlvmStatement -> SDoc
ppMetaStatement opts meta stmt =
   ppLlvmStatement opts stmt <> ppMetaAnnots opts meta

ppMetaAnnotExpr :: LlvmOpts -> [MetaAnnot] -> LlvmExpression -> SDoc
ppMetaAnnotExpr opts meta expr =
   ppLlvmExpression opts expr <> ppMetaAnnots opts meta

ppMetaAnnots :: LlvmOpts -> [MetaAnnot] -> SDoc
ppMetaAnnots opts meta = hcat $ map ppMeta meta
  where
    ppMeta (MetaAnnot name e)
        = comma <+> exclamation <> ftext name <+>
          case e of
            MetaNode n    -> ppr n
            MetaStruct ms -> exclamation <> braces (ppCommaJoin (map (ppMetaExpr opts) ms))
            other         -> exclamation <> braces (ppMetaExpr opts other) -- possible?

-- | Return the variable name or value of the 'LlvmVar'
-- in Llvm IR textual representation (e.g. @\@x@, @%y@ or @42@).
ppName :: LlvmOpts -> LlvmVar -> SDoc
ppName opts v = case v of
   LMGlobalVar {} -> char '@' <> ppPlainName opts v
   LMLocalVar  {} -> char '%' <> ppPlainName opts v
   LMNLocalVar {} -> char '%' <> ppPlainName opts v
   LMLitVar    {} ->             ppPlainName opts v

-- | Return the variable name or value of the 'LlvmVar'
-- in a plain textual representation (e.g. @x@, @y@ or @42@).
ppPlainName :: LlvmOpts -> LlvmVar -> SDoc
ppPlainName opts v = case v of
   (LMGlobalVar x _ _ _ _ _) -> ftext x
   (LMLocalVar  x LMLabel  ) -> text (show x)
   (LMLocalVar  x _        ) -> text ('l' : show x)
   (LMNLocalVar x _        ) -> ftext x
   (LMLitVar    x          ) -> ppLit opts x

-- | Print a literal value. No type.
ppLit :: LlvmOpts -> LlvmLit -> SDoc
ppLit opts l = case l of
   (LMIntLit i (LMInt 32))  -> ppr (fromInteger i :: Int32)
   (LMIntLit i (LMInt 64))  -> ppr (fromInteger i :: Int64)
   (LMIntLit   i _       )  -> ppr ((fromInteger i)::Int)
   (LMFloatLit r LMFloat )  -> ppFloat (llvmOptsPlatform opts) $ narrowFp r
   (LMFloatLit r LMDouble)  -> ppDouble (llvmOptsPlatform opts) r
   f@(LMFloatLit _ _)       -> pprPanic "ppLit" (text "Can't print this float literal: " <> ppTypeLit opts f)
   (LMVectorLit ls  )       -> char '<' <+> ppCommaJoin (map (ppTypeLit opts) ls) <+> char '>'
   (LMNullLit _     )       -> text "null"
   -- #11487 was an issue where we passed undef for some arguments
   -- that were actually live. By chance the registers holding those
   -- arguments usually happened to have the right values anyways, but
   -- that was not guaranteed. To find such bugs reliably, we set the
   -- flag below when validating, which replaces undef literals (at
   -- common types) with values that are likely to cause a crash or test
   -- failure.
   (LMUndefLit t    )
      | llvmOptsFillUndefWithGarbage opts
      , Just lit <- garbageLit t   -> ppLit opts lit
      | otherwise                  -> text "undef"

ppVar :: LlvmOpts -> LlvmVar -> SDoc
ppVar = ppVar' []

ppVar' :: [LlvmParamAttr] -> LlvmOpts -> LlvmVar -> SDoc
ppVar' attrs opts v = case v of
  LMLitVar x -> ppTypeLit' attrs opts x
  x          -> ppr (getVarType x) <+> ppSpaceJoin attrs <+> ppName opts x

ppTypeLit :: LlvmOpts -> LlvmLit -> SDoc
ppTypeLit = ppTypeLit' []

ppTypeLit' :: [LlvmParamAttr] -> LlvmOpts -> LlvmLit -> SDoc
ppTypeLit' attrs opts l = case l of
  LMVectorLit {} -> ppLit opts l
  _              -> ppr (getLitType l) <+> ppSpaceJoin attrs <+> ppLit opts l

ppStatic :: LlvmOpts -> LlvmStatic -> SDoc
ppStatic opts st = case st of
  LMComment       s -> text "; " <> ftext s
  LMStaticLit   l   -> ppTypeLit opts l
  LMUninitType    t -> ppr t <> text " undef"
  LMStaticStr   s t -> ppr t <> text " c\"" <> ftext s <> text "\\00\""
  LMStaticArray d t -> ppr t <> text " [" <> ppCommaJoin (map (ppStatic opts) d) <> char ']'
  LMStaticStruc d t -> ppr t <> text "<{" <> ppCommaJoin (map (ppStatic opts) d) <> text "}>"
  LMStaticPointer v -> ppVar opts v
  LMTrunc v t       -> ppr t <> text " trunc (" <> ppStatic opts v <> text " to " <> ppr t <> char ')'
  LMBitc v t        -> ppr t <> text " bitcast (" <> ppStatic opts v <> text " to " <> ppr t <> char ')'
  LMPtoI v t        -> ppr t <> text " ptrtoint (" <> ppStatic opts v <> text " to " <> ppr t <> char ')'
  LMAdd s1 s2       -> pprStaticArith opts s1 s2 (sLit "add") (sLit "fadd") "LMAdd"
  LMSub s1 s2       -> pprStaticArith opts s1 s2 (sLit "sub") (sLit "fsub") "LMSub"


pprSpecialStatic :: LlvmOpts -> LlvmStatic -> SDoc
pprSpecialStatic opts stat = case stat of
   LMBitc v t        -> ppr (pLower t)
                        <> text ", bitcast ("
                        <> ppStatic opts v <> text " to " <> ppr t
                        <> char ')'
   LMStaticPointer x -> ppr (pLower $ getVarType x)
                        <> comma <+> ppStatic opts stat
   _                 -> ppStatic opts stat


pprStaticArith :: LlvmOpts -> LlvmStatic -> LlvmStatic -> PtrString -> PtrString
                  -> String -> SDoc
pprStaticArith opts s1 s2 int_op float_op op_name =
  let ty1 = getStatType s1
      op  = if isFloat ty1 then float_op else int_op
  in if ty1 == getStatType s2
     then ppr ty1 <+> ptext op <+> lparen <> ppStatic opts s1 <> comma <> ppStatic opts s2 <> rparen
     else pprPanic "pprStaticArith" $
            text op_name <> text " with different types! s1: " <> ppStatic opts s1
                         <> text", s2: " <> ppStatic opts s2


--------------------------------------------------------------------------------
-- * Misc functions
--------------------------------------------------------------------------------

-- | Blank line.
newLine :: SDoc
newLine = empty

-- | Exclamation point.
exclamation :: SDoc
exclamation = char '!'
