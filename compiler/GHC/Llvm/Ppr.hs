
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

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

import GHC.Prelude

import GHC.Llvm.Syntax
import GHC.Llvm.MetaData
import GHC.Llvm.Types

import Data.List ( intersperse )
import GHC.Utils.Outputable

import GHC.CmmToLlvm.Config
import GHC.Utils.Panic
import GHC.Types.Unique

--------------------------------------------------------------------------------
-- * Top Level Print functions
--------------------------------------------------------------------------------

-- | Print out a whole LLVM module.
ppLlvmModule :: IsDoc doc => LlvmCgConfig -> LlvmModule ->  doc
ppLlvmModule opts (LlvmModule comments aliases meta globals decls funcs)
  = ppLlvmComments comments $$ newLine
    $$ ppLlvmAliases aliases $$ newLine
    $$ ppLlvmMetas opts meta $$ newLine
    $$ ppLlvmGlobals opts globals $$ newLine
    $$ ppLlvmFunctionDecls decls $$ newLine
    $$ ppLlvmFunctions opts funcs


-- | Print out a multi-line comment, can be inside a function or on its own
ppLlvmComments :: IsDoc doc => [LMString] ->  doc
ppLlvmComments comments = lines_ $ map ppLlvmComment comments

-- | Print out a comment, can be inside a function or on its own
ppLlvmComment :: IsLine doc => LMString ->  doc
ppLlvmComment com = semi <+> ftext com


-- | Print out a list of global mutable variable definitions
ppLlvmGlobals :: IsDoc doc => LlvmCgConfig -> [LMGlobal] ->  doc
ppLlvmGlobals opts ls = lines_ $ map (ppLlvmGlobal opts) ls

-- | Print out a global mutable variable definition
ppLlvmGlobal :: IsLine doc => LlvmCgConfig -> LMGlobal ->  doc
ppLlvmGlobal opts (LMGlobal var@(LMGlobalVar _ _ link x a c) dat) =
    let sect = case x of
            Just x' -> text ", section" <+> doubleQuotes (ftext x')
            Nothing -> empty

        align = case a of
            Just a' -> text ", align" <+> int a'
            Nothing -> empty

        rhs = case dat of
            Just stat -> pprSpecialStatic opts stat
            Nothing   -> ppType (pLower $ getVarType var)

        -- Position of linkage is different for aliases.
        const = case c of
          Global   -> "global"
          Constant -> "constant"
          Alias    -> "alias"

    in ppAssignment opts var $ ppLlvmLinkageType link <+> text const <+> rhs <> sect <> align

ppLlvmGlobal opts (LMGlobal var val) = pprPanic "ppLlvmGlobal" $
  text "Non Global var ppr as global! " <> ppVar opts var <> text "=" <> ppr (fmap (ppStatic @SDoc opts) val)


-- | Print out a list of LLVM type aliases.
ppLlvmAliases :: IsDoc doc => [LlvmAlias] ->  doc
ppLlvmAliases tys = lines_ $ map ppLlvmAlias tys

-- | Print out an LLVM type alias.
ppLlvmAlias :: IsLine doc => LlvmAlias ->  doc
ppLlvmAlias (name, ty)
  = char '%' <> ftext name <+> equals <+> text "type" <+> ppType ty


-- | Print out a list of LLVM metadata.
ppLlvmMetas :: IsDoc doc => LlvmCgConfig -> [MetaDecl] ->  doc
ppLlvmMetas opts metas = lines_ $ map (ppLlvmMeta opts) metas

-- | Print out an LLVM metadata definition.
ppLlvmMeta :: IsLine doc => LlvmCgConfig -> MetaDecl ->  doc
ppLlvmMeta opts (MetaUnnamed n m)
  = ppMetaId n <+> equals <+> ppMetaExpr opts m

ppLlvmMeta _opts (MetaNamed n m)
  = exclamation <> ftext n <+> equals <+> exclamation <> braces nodes
  where
    nodes = hcat $ intersperse comma $ map ppMetaId m


-- | Print out a list of function definitions.
ppLlvmFunctions :: IsDoc doc => LlvmCgConfig -> LlvmFunctions ->  doc
ppLlvmFunctions opts funcs = vcat $ map (ppLlvmFunction opts) funcs

-- | Print out a function definition.
ppLlvmFunction :: IsDoc doc => LlvmCgConfig -> LlvmFunction ->  doc
ppLlvmFunction opts fun =
    let attrDoc = ppSpaceJoin ppLlvmFuncAttr (funcAttrs fun)
        secDoc = case funcSect fun of
                      Just s' -> text "section" <+> (doubleQuotes $ ftext s')
                      Nothing -> empty
        prefixDoc = case funcPrefix fun of
                        Just v  -> text "prefix" <+> ppStatic opts v
                        Nothing -> empty
    in vcat
        [line $ text "define" <+> ppLlvmFunctionHeader (funcDecl fun) (funcArgs fun)
              <+> attrDoc <+> secDoc <+> prefixDoc
        , line lbrace
        , ppLlvmBlocks opts (funcBody fun)
        , line rbrace
        , newLine
        , newLine]

-- | Print out a function definition header.
ppLlvmFunctionHeader :: IsLine doc => LlvmFunctionDecl -> [LMString] ->  doc
ppLlvmFunctionHeader (LlvmFunctionDecl n l c r varg p a) args
  = let varg' = case varg of
                      VarArgs | null p    -> text "..."
                              | otherwise -> text ", ..."
                      _otherwise          -> text ""
        align = case a of
                     Just a' -> text " align " <> int a'
                     Nothing -> empty
        args' = zipWith (\(ty,p) n -> ppType ty <+> ppSpaceJoin ppLlvmParamAttr p <+> char '%'
                                    <> ftext n)
                   p
                   args
    in ppLlvmLinkageType l <+> ppLlvmCallConvention c <+> ppType r <+> char '@' <> ftext n <> lparen <>
        hsep (punctuate comma args') <> varg' <> rparen <> align

-- | Print out a list of function declaration.
ppLlvmFunctionDecls :: IsDoc doc => LlvmFunctionDecls ->  doc
ppLlvmFunctionDecls decs = vcat $ map ppLlvmFunctionDecl decs

-- | Print out a function declaration.
-- Declarations define the function type but don't define the actual body of
-- the function.
ppLlvmFunctionDecl :: IsDoc doc => LlvmFunctionDecl -> doc
ppLlvmFunctionDecl (LlvmFunctionDecl n l c r varg p a)
  = let varg' = case varg of
                      VarArgs | null p    -> text "..."
                              | otherwise -> text ", ..."
                      _otherwise          -> text ""
        align = case a of
                     Just a' -> text " align" <+> int a'
                     Nothing -> empty
        args = hcat $ intersperse (comma <> space) $
                  map (\(t,a) -> ppType t <+> ppSpaceJoin ppLlvmParamAttr a) p
    in lines_
        [ text "declare" <+> ppLlvmLinkageType l <+> ppLlvmCallConvention c
          <+> ppType r <+> char '@' <> ftext n <> lparen <> args <> varg' <> rparen <> align
        , empty]


-- | Print out a list of LLVM blocks.
ppLlvmBlocks :: IsDoc doc => LlvmCgConfig -> LlvmBlocks ->  doc
ppLlvmBlocks opts blocks = vcat $ map (ppLlvmBlock opts) blocks

-- | Print out an LLVM block.
-- It must be part of a function definition.
ppLlvmBlock :: IsDoc doc => LlvmCgConfig -> LlvmBlock ->  doc
ppLlvmBlock opts (LlvmBlock blockId stmts) =
  let isLabel (MkLabel _) = True
      isLabel _           = False
      (block, rest)       = break isLabel stmts
      ppRest = case rest of
        MkLabel id:xs -> ppLlvmBlock opts (LlvmBlock id xs)
        _             -> empty
  in vcat $
      line (ppLlvmBlockLabel blockId)
      : map (ppLlvmStatement opts) block
      ++ [ empty , ppRest ]

-- | Print out an LLVM block label.
ppLlvmBlockLabel :: IsLine doc => LlvmBlockId ->  doc
ppLlvmBlockLabel id = pprUniqueAlways id <> colon


-- | Print out an LLVM statement.
ppLlvmStatement :: forall doc. IsDoc doc => LlvmCgConfig -> LlvmStatement ->  doc
ppLlvmStatement opts stmt =
  let ind = line . (text "  " <>)
  in case stmt of
        Assignment  dst expr      -> ind $ ppAssignment opts dst (ppLlvmExpression opts expr)
        Fence       st ord        -> ind $ ppFence st ord
        Branch      target        -> ind $ ppBranch opts target
        BranchIf    cond ifT ifF  -> ind $ ppBranchIf opts cond ifT ifF
        Comment     comments      -> ppLlvmComments comments
        MkLabel     label         -> line $ ppLlvmBlockLabel label
        Store       value ptr align
                                  -> ind $ ppStore opts value ptr align
        Switch      scrut def tgs -> ppSwitch opts scrut def tgs
        Return      result        -> ind $ ppReturn opts result
        Expr        expr          -> ind $ ppLlvmExpression opts expr
        Unreachable               -> ind $ text "unreachable"
        Nop                       -> empty
        MetaStmt    meta s        -> ppLlvmStatement' opts s (ppMetaAnnots opts meta)


ppLlvmStatement' :: IsDoc doc => LlvmCgConfig -> LlvmStatement -> Line doc ->  doc
ppLlvmStatement' opts stmt lastLineMeta =
  let ind = line . (<+> lastLineMeta) . (text "  " <>)
  in case stmt of
        Assignment  dst expr      -> ind $ ppAssignment opts dst (ppLlvmExpression opts expr) <> lastLineMeta
        Fence       st ord        -> ind $ ppFence st ord
        Branch      target        -> ind $ ppBranch opts target
        BranchIf    cond ifT ifF  -> ind $ ppBranchIf opts cond ifT ifF
        Comment     comments      -> ppLlvmComments comments -- Ignore metadata?
        MkLabel     label         -> line $ ppLlvmBlockLabel label
        Store       value ptr align
                                  -> ind $ ppStore opts value ptr align
        Switch      scrut def tgs -> ppSwitch' opts scrut def tgs lastLineMeta
        Return      result        -> ind $ ppReturn opts result
        Expr        expr          -> ind $ ppLlvmExpression opts expr
        Unreachable               -> ind $ text "unreachable"
        Nop                       -> line $ empty <> lastLineMeta
        -- MetaStmt    meta s        -> ppMetaStatement opts meta s
        MetaStmt    meta s        -> ppLlvmStatement' opts s (lastLineMeta <+> ppMetaAnnots opts meta)

-- | Print out an LLVM expression.
ppLlvmExpression :: IsLine doc => LlvmCgConfig -> LlvmExpression ->  doc
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

ppMetaExpr :: IsLine doc => LlvmCgConfig -> MetaExpr ->  doc
ppMetaExpr opts = \case
  MetaVar (LMLitVar (LMNullLit _)) -> text "null"
  MetaStr    s                     -> char '!' <> doubleQuotes (ftext s)
  MetaNode   n                     -> ppMetaId n
  MetaVar    v                     -> ppVar opts v
  MetaStruct es                    -> char '!' <> braces (ppCommaJoin (ppMetaExpr opts) es)


--------------------------------------------------------------------------------
-- * Individual print functions
--------------------------------------------------------------------------------

-- | Should always be a function pointer. So a global var of function type
-- (since globals are always pointers) or a local var of pointer function type.
ppCall :: forall doc. IsLine doc => LlvmCgConfig -> LlvmCallType -> LlvmVar -> [MetaExpr] -> [LlvmFuncAttr] ->  doc
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
                ppArgTy  = ppCommaJoin (ppType . fst) params <>
                           (case argTy of
                               VarArgs   -> text ", ..."
                               FixedArgs -> empty)
                fnty = space <> lparen <> ppArgTy <> rparen
                attrDoc = ppSpaceJoin ppLlvmFuncAttr attrs
            in  tc <> text "call" <+> ppLlvmCallConvention cc <+> ppType ret
                    <> fnty <+> ppName opts fptr <> lparen <+> ppValues
                    <+> rparen <+> attrDoc

        ppCallParams :: LlvmCgConfig -> [[LlvmParamAttr]] -> [MetaExpr] ->  doc
        ppCallParams opts attrs args = hsep $ punctuate comma $ zipWith ppCallMetaExpr attrs args
         where
          -- Metadata needs to be marked as having the `metadata` type when used
          -- in a call argument
          ppCallMetaExpr attrs (MetaVar v) = ppVar' attrs opts v
          ppCallMetaExpr _ v             = text "metadata" <+> ppMetaExpr opts v


ppMachOp :: IsLine doc => LlvmCgConfig -> LlvmMachOp -> LlvmVar -> LlvmVar ->  doc
ppMachOp opts op left right =
  ppLlvmMachOp op <+> ppType (getVarType left) <+> ppName opts left
        <> comma <+> ppName opts right


ppCmpOp :: IsLine doc => LlvmCgConfig -> LlvmCmpOp -> LlvmVar -> LlvmVar ->  doc
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
  in cmpOp <+> ppLlvmCmpOp op <+> ppType (getVarType left)
        <+> ppName opts left <> comma <+> ppName opts right


ppAssignment :: IsLine doc => IsLine doc => LlvmCgConfig -> LlvmVar ->  doc ->  doc
ppAssignment opts var expr = ppName opts var <+> equals <+> expr

ppFence :: IsLine doc => Bool -> LlvmSyncOrdering ->  doc
ppFence st ord =
  let singleThread = case st of True  -> text "singlethread"
                                False -> empty
  in text "fence" <+> singleThread <+> ppSyncOrdering ord

ppSyncOrdering :: IsLine doc => LlvmSyncOrdering ->  doc
ppSyncOrdering SyncUnord     = text "unordered"
ppSyncOrdering SyncMonotonic = text "monotonic"
ppSyncOrdering SyncAcquire   = text "acquire"
ppSyncOrdering SyncRelease   = text "release"
ppSyncOrdering SyncAcqRel    = text "acq_rel"
ppSyncOrdering SyncSeqCst    = text "seq_cst"

ppAtomicOp :: IsLine doc => LlvmAtomicOp ->  doc
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

ppAtomicRMW :: IsLine doc => LlvmCgConfig -> LlvmAtomicOp -> LlvmVar -> LlvmVar -> LlvmSyncOrdering ->  doc
ppAtomicRMW opts aop tgt src ordering =
  text "atomicrmw" <+> ppAtomicOp aop <+> ppVar opts tgt <> comma
  <+> ppVar opts src <+> ppSyncOrdering ordering

ppCmpXChg :: IsLine doc => LlvmCgConfig -> LlvmVar -> LlvmVar -> LlvmVar
          -> LlvmSyncOrdering -> LlvmSyncOrdering -> doc
ppCmpXChg opts addr old new s_ord f_ord =
  text "cmpxchg" <+> ppVar opts addr <> comma <+> ppVar opts old <> comma <+> ppVar opts new
  <+> ppSyncOrdering s_ord <+> ppSyncOrdering f_ord


ppLoad :: IsLine doc => LlvmCgConfig -> LlvmVar -> LMAlign ->  doc
ppLoad opts var alignment =
  text "load" <+> ppType derefType <> comma <+> ppVar opts var <> align
  where
    derefType = pLower $ getVarType var
    align =
      case alignment of
        Just n  -> text ", align" <+> int n
        Nothing -> empty

ppALoad :: IsLine doc => LlvmCgConfig -> LlvmSyncOrdering -> SingleThreaded -> LlvmVar ->  doc
ppALoad opts ord st var =
  let alignment = llvmWidthInBits (llvmCgPlatform opts) (getVarType var) `quot` 8
      align     = text ", align" <+> int alignment
      sThreaded | st        = text " singlethread"
                | otherwise = empty
      derefType = pLower $ getVarType var
  in text "load atomic" <+> ppType derefType <> comma <+> ppVar opts var <> sThreaded
            <+> ppSyncOrdering ord <> align

ppStore :: IsLine doc => LlvmCgConfig -> LlvmVar -> LlvmVar -> LMAlign ->  doc
ppStore opts val dst alignment =
    text "store" <+> ppVar opts val <> comma <+> ppVar opts dst <> align
  where
    align =
      case alignment of
        Just n  -> text ", align" <+> int n
        Nothing -> empty


ppCast :: IsLine doc => LlvmCgConfig -> LlvmCastOp -> LlvmVar -> LlvmType ->  doc
ppCast opts op from to
    =   ppLlvmCastOp op
    <+> ppType (getVarType from) <+> ppName opts from
    <+> text "to"
    <+> ppType to


ppMalloc :: IsLine doc => LlvmCgConfig -> LlvmType -> Int ->  doc
ppMalloc opts tp amount =
  let amount' = LMLitVar $ LMIntLit (toInteger amount) i32
  in text "malloc" <+> ppType tp <> comma <+> ppVar opts amount'


ppAlloca :: IsLine doc => LlvmCgConfig -> LlvmType -> Int ->  doc
ppAlloca opts tp amount =
  let amount' = LMLitVar $ LMIntLit (toInteger amount) i32
  in text "alloca" <+> ppType tp <> comma <+> ppVar opts amount'


ppGetElementPtr :: IsLine doc => LlvmCgConfig -> Bool -> LlvmVar -> [LlvmVar] ->  doc
ppGetElementPtr opts inb ptr idx =
  let indexes = comma <+> ppCommaJoin (ppVar opts) idx
      inbound = if inb then text "inbounds" else empty
      derefType = pLower $ getVarType ptr
  in text "getelementptr" <+> inbound <+> ppType derefType <> comma <+> ppVar opts ptr
                            <> indexes


ppReturn :: IsLine doc => LlvmCgConfig -> Maybe LlvmVar ->  doc
ppReturn opts (Just var) = text "ret" <+> ppVar opts var
ppReturn _    Nothing    = text "ret" <+> ppType LMVoid


ppBranch :: IsLine doc => LlvmCgConfig -> LlvmVar ->  doc
ppBranch opts var = text "br" <+> ppVar opts var


ppBranchIf :: IsLine doc => LlvmCgConfig -> LlvmVar -> LlvmVar -> LlvmVar ->  doc
ppBranchIf opts cond trueT falseT
  = text "br" <+> ppVar opts cond <> comma <+> ppVar opts trueT <> comma <+> ppVar opts falseT


ppPhi :: IsLine doc => LlvmCgConfig -> LlvmType -> [(LlvmVar,LlvmVar)] ->  doc
ppPhi opts tp preds =
  let ppPreds (val, label) = brackets $ ppName opts val <> comma <+> ppName opts label
  in text "phi" <+> ppType tp <+> hsep (punctuate comma $ map ppPreds preds)


ppSwitch :: IsDoc doc => LlvmCgConfig -> LlvmVar -> LlvmVar -> [(LlvmVar,LlvmVar)] ->  doc
ppSwitch opts scrut dflt targets =
  let ppTarget  (val, lab) = text "  " <> ppVar opts val <> comma <+> ppVar opts lab
  in lines_ $ concat
      [ [text "switch" <+> ppVar opts scrut <> comma <+> ppVar opts dflt <+> char '[']
      , map ppTarget targets
      , [char ']']
      ]

ppSwitch' :: IsDoc doc => LlvmCgConfig -> LlvmVar -> LlvmVar -> [(LlvmVar,LlvmVar)] -> Line doc ->  doc
ppSwitch' opts scrut dflt targets lastLineMeta =
  let ppTarget  (val, lab) = text "  " <> ppVar opts val <> comma <+> ppVar opts lab
  in lines_ $ concat
      [ [text "switch" <+> ppVar opts scrut <> comma <+> ppVar opts dflt <+> char '[']
      , map ppTarget targets
      , [char ']' <> lastLineMeta]
      ]


ppAsm :: IsLine doc => LlvmCgConfig -> LMString -> LMString -> LlvmType -> [LlvmVar] -> Bool -> Bool ->  doc
ppAsm opts asm constraints rty vars sideeffect alignstack =
  let asm'  = doubleQuotes $ ftext asm
      cons  = doubleQuotes $ ftext constraints
      rty'  = ppType rty
      vars' = lparen <+> ppCommaJoin (ppVar opts) vars <+> rparen
      side  = if sideeffect then text "sideeffect" else empty
      align = if alignstack then text "alignstack" else empty
  in text "call" <+> rty' <+> text "asm" <+> side <+> align <+> asm' <> comma
        <+> cons <> vars'

ppExtract :: IsLine doc => LlvmCgConfig -> LlvmVar -> LlvmVar ->  doc
ppExtract opts vec idx =
    text "extractelement"
    <+> ppType (getVarType vec) <+> ppName opts vec <> comma
    <+> ppVar opts idx

ppExtractV :: IsLine doc => LlvmCgConfig -> LlvmVar -> Int ->  doc
ppExtractV opts struct idx =
    text "extractvalue"
    <+> ppType (getVarType struct) <+> ppName opts struct <> comma
    <+> int idx

ppInsert :: IsLine doc => LlvmCgConfig -> LlvmVar -> LlvmVar -> LlvmVar ->  doc
ppInsert opts vec elt idx =
    text "insertelement"
    <+> ppType (getVarType vec) <+> ppName opts vec <> comma
    <+> ppType (getVarType elt) <+> ppName opts elt <> comma
    <+> ppVar opts idx


-- ppMetaStatement :: IsDoc doc => LlvmCgConfig -> [MetaAnnot] -> LlvmStatement ->  doc
-- ppMetaStatement opts meta stmt =
--    ppLlvmStatement opts stmt <> line (ppMetaAnnots opts meta)

ppMetaAnnotExpr :: IsLine doc => LlvmCgConfig -> [MetaAnnot] -> LlvmExpression-> doc
{-# SPECIALIZE ppMetaAnnotExpr :: LlvmCgConfig -> [MetaAnnot] -> LlvmExpression -> SDoc #-}
{-# SPECIALIZE ppMetaAnnotExpr :: LlvmCgConfig -> [MetaAnnot] -> LlvmExpression -> HLine #-}
ppMetaAnnotExpr opts meta expr =
   ppLlvmExpression opts expr <> ppMetaAnnots opts meta

ppMetaAnnots :: IsLine doc => LlvmCgConfig -> [MetaAnnot] ->  doc
ppMetaAnnots opts meta = hcat $ map ppMeta meta
  where
    ppMeta (MetaAnnot name e)
        = comma <+> exclamation <> ftext name <+>
          case e of
            MetaNode n    -> ppMetaId n
            MetaStruct ms -> exclamation <> braces (ppCommaJoin (ppMetaExpr opts) ms)
            other         -> exclamation <> braces (ppMetaExpr opts other) -- possible?

-- | Return the variable name or value of the 'LlvmVar'
-- in Llvm IR textual representation (e.g. @\@x@, @%y@ or @42@).
ppName :: IsLine doc => LlvmCgConfig -> LlvmVar ->  doc
ppName opts v = case v of
   LMGlobalVar {} -> char '@' <> ppPlainName opts v
   LMLocalVar  {} -> char '%' <> ppPlainName opts v
   LMNLocalVar {} -> char '%' <> ppPlainName opts v
   LMLitVar    {} ->             ppPlainName opts v

-- | Return the variable name or value of the 'LlvmVar'
-- in a plain textual representation (e.g. @x@, @y@ or @42@).
ppPlainName :: IsLine doc => LlvmCgConfig -> LlvmVar ->  doc
ppPlainName opts v = case v of
   (LMGlobalVar x _ _ _ _ _) -> ftext x
   (LMLocalVar  x LMLabel  ) -> pprUniqueAlways x
   (LMLocalVar  x _        ) -> char 'l' <> pprUniqueAlways x
   (LMNLocalVar x _        ) -> ftext x
   (LMLitVar    x          ) -> ppLit opts x

-- | Print a literal value. No type.
ppLit :: IsLine doc => LlvmCgConfig -> LlvmLit ->  doc
ppLit opts l = case l of
   (LMIntLit i (LMInt 32))  -> integer (fromInteger i) -- TODO: ew
   (LMIntLit i (LMInt 64))  -> integer (fromInteger i)
   (LMIntLit   i _       )  -> integer (fromInteger i)
   (LMFloatLit r LMFloat )  -> ppFloat (llvmCgPlatform opts) $ narrowFp r
   (LMFloatLit r LMDouble)  -> ppDouble (llvmCgPlatform opts) r
   f@(LMFloatLit _ _)       -> pprPanic "ppLit" (text "Can't print this float literal: " <> ppTypeLit opts f)
   (LMVectorLit ls  )       -> char '<' <+> ppCommaJoin (ppTypeLit opts) ls <+> char '>'
   (LMNullLit _     )       -> text "null"
   -- #11487 was an issue where we passed undef for some arguments
   -- that were actually live. By chance the registers holding those
   -- arguments usually happened to have the right values anyways, but
   -- that was not guaranteed. To find such bugs reliably, we set the
   -- flag below when validating, which replaces undef literals (at
   -- common types) with values that are likely to cause a crash or test
   -- failure.
   (LMUndefLit t    )
      | llvmCgFillUndefWithGarbage opts
      , Just lit <- garbageLit t   -> ppLit opts lit
      | otherwise                  -> text "undef"

ppVar :: IsLine doc => LlvmCgConfig -> LlvmVar ->  doc
ppVar = ppVar' []

ppVar' :: IsLine doc => [LlvmParamAttr] -> LlvmCgConfig -> LlvmVar ->  doc
ppVar' attrs opts v = case v of
  LMLitVar x -> ppTypeLit' attrs opts x
  x          -> ppType (getVarType x) <+> ppSpaceJoin ppLlvmParamAttr attrs <+> ppName opts x

ppTypeLit :: IsLine doc => LlvmCgConfig -> LlvmLit ->  doc
ppTypeLit = ppTypeLit' []

ppTypeLit' :: IsLine doc => [LlvmParamAttr] -> LlvmCgConfig -> LlvmLit ->  doc
ppTypeLit' attrs opts l = case l of
  LMVectorLit {} -> ppLit opts l
  _              -> ppType (getLitType l) <+> ppSpaceJoin ppLlvmParamAttr attrs <+> ppLit opts l

ppStatic :: IsLine doc => LlvmCgConfig -> LlvmStatic ->  doc
ppStatic opts st = case st of
  LMComment       s -> text "; " <> ftext s
  LMStaticLit   l   -> ppTypeLit opts l
  LMUninitType    t -> ppType t <> text " undef"
  LMStaticStr   s t -> ppType t <> text " c\"" <> ftext s <> text "\\00\""
  LMStaticArray d t -> ppType t <> text " [" <> ppCommaJoin (ppStatic opts) d <> char ']'
  LMStaticStruc d t -> ppType t <> text "<{" <> ppCommaJoin (ppStatic opts) d <> text "}>"
  LMStaticStrucU d t -> ppType t <> text "{" <> ppCommaJoin (ppStatic opts) d <> text "}"
  LMStaticPointer v -> ppVar opts v
  LMTrunc v t       -> ppType t <> text " trunc (" <> ppStatic opts v <> text " to " <> ppType t <> char ')'
  LMBitc v t        -> ppType t <> text " bitcast (" <> ppStatic opts v <> text " to " <> ppType t <> char ')'
  LMPtoI v t        -> ppType t <> text " ptrtoint (" <> ppStatic opts v <> text " to " <> ppType t <> char ')'
  LMAdd s1 s2       -> pprStaticArith opts s1 s2 (text "add") (text "fadd") (text "LMAdd")
  LMSub s1 s2       -> pprStaticArith opts s1 s2 (text "sub") (text "fsub") (text "LMSub")


pprSpecialStatic :: IsLine doc => LlvmCgConfig -> LlvmStatic ->  doc
pprSpecialStatic opts stat = case stat of
   LMBitc v t        -> ppType (pLower t)
                        <> text ", bitcast ("
                        <> ppStatic opts v <> text " to " <> ppType t
                        <> char ')'
   LMStaticPointer x -> ppType (pLower $ getVarType x)
                        <> comma <+> ppStatic opts stat
   _                 -> ppStatic opts stat


pprStaticArith :: IsLine doc => LlvmCgConfig -> LlvmStatic -> LlvmStatic -> doc ->  doc
                  -> SDoc -> doc
pprStaticArith opts s1 s2 int_op float_op op_name =
  let ty1 = getStatType s1
      op  = if isFloat ty1 then float_op else int_op
  in if ty1 == getStatType s2
     then ppType ty1 <+> op <+> lparen <> ppStatic opts s1 <> comma <> ppStatic opts s2 <> rparen
     else pprPanic "pprStaticArith" $
                 op_name <> text " with different types! s1: " <> ppStatic opts s1
                         <> text", s2: " <> ppStatic opts s2


--------------------------------------------------------------------------------
-- * Misc functions
--------------------------------------------------------------------------------

-- | Blank line.
newLine :: IsDoc doc => doc
newLine = empty

-- | Exclamation point.
exclamation :: IsLine doc => doc
exclamation = char '!'
