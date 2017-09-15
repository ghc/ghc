
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

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
    ppPlainName,

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
ppLlvmModule :: IsDoc doc => LlvmCgConfig -> LlvmModule -> doc
ppLlvmModule opts (LlvmModule comments aliases meta globals decls funcs)
  = ppLlvmComments comments $$ newLine
    $$ ppLlvmAliases aliases $$ newLine
    $$ ppLlvmMetas opts meta $$ newLine
    $$ ppLlvmGlobals opts globals $$ newLine
    $$ ppLlvmFunctionDecls decls $$ newLine
    $$ ppLlvmFunctions opts funcs
{-# SPECIALIZE ppLlvmModule :: LlvmCgConfig -> LlvmModule -> SDoc #-}
{-# SPECIALIZE ppLlvmModule :: LlvmCgConfig -> LlvmModule -> HDoc #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable


-- | Print out a multi-line comment, can be inside a function or on its own
ppLlvmComments :: IsDoc doc => [LMString] -> doc
ppLlvmComments comments = lines_ $ map ppLlvmComment comments
{-# SPECIALIZE ppLlvmComments :: [LMString] -> SDoc #-}
{-# SPECIALIZE ppLlvmComments :: [LMString] -> HDoc #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

-- | Print out a comment, can be inside a function or on its own
ppLlvmComment :: IsLine doc => LMString -> doc
ppLlvmComment com = semi <+> ftext com
{-# SPECIALIZE ppLlvmComment :: LMString -> SDoc #-}
{-# SPECIALIZE ppLlvmComment :: LMString -> HLine #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable


-- | Print out a list of global mutable variable definitions
ppLlvmGlobals :: IsDoc doc => LlvmCgConfig -> [LMGlobal] -> doc
ppLlvmGlobals opts ls = lines_ $ map (ppLlvmGlobal opts) ls
{-# SPECIALIZE ppLlvmGlobals :: LlvmCgConfig -> [LMGlobal] -> SDoc #-}
{-# SPECIALIZE ppLlvmGlobals :: LlvmCgConfig -> [LMGlobal] -> HDoc #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

-- | Print out a global mutable variable definition
ppLlvmGlobal :: IsLine doc => LlvmCgConfig -> LMGlobal -> doc
ppLlvmGlobal opts (LMGlobal var@(LMGlobalVar _ _ link x a c) dat) =
    let sect = case x of
            Just x' -> text ", section" <+> doubleQuotes (ftext x')
            Nothing -> empty

        align = case a of
            Just a' -> text ", align" <+> int a'
            Nothing -> empty

        rhs = case dat of
            Just stat -> pprSpecialStatic opts stat
            Nothing   -> ppLlvmType (pLower $ getVarType var)

        -- Position of linkage is different for aliases.
        const = case c of
          Global   -> "global"
          Constant -> "constant"
          Alias    -> "alias"

    in ppAssignment opts var $ ppLlvmLinkageType link <+> text const <+> rhs <> sect <> align

ppLlvmGlobal opts (LMGlobal var val) = pprPanic "ppLlvmGlobal" $
  text "Non Global var ppr as global! " <> ppVar opts var <> text "=" <> ppr (fmap (ppStatic @SDoc opts) val)
{-# SPECIALIZE ppLlvmGlobal :: LlvmCgConfig -> LMGlobal -> SDoc #-}
{-# SPECIALIZE ppLlvmGlobal :: LlvmCgConfig -> LMGlobal -> HLine #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable


-- | Print out a list of LLVM type aliases.
ppLlvmAliases :: IsDoc doc => [LlvmAlias] -> doc
ppLlvmAliases tys = lines_ $ map ppLlvmAlias tys
{-# SPECIALIZE ppLlvmAliases :: [LlvmAlias] -> SDoc #-}
{-# SPECIALIZE ppLlvmAliases :: [LlvmAlias] -> HDoc #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

-- | Print out an LLVM type alias.
ppLlvmAlias :: IsLine doc => LlvmAlias -> doc
ppLlvmAlias (name, ty)
  = char '%' <> ftext name <+> equals <+> text "type" <+> ppLlvmType ty
{-# SPECIALIZE ppLlvmAlias :: LlvmAlias -> SDoc #-}
{-# SPECIALIZE ppLlvmAlias :: LlvmAlias -> HLine #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable


-- | Print out a list of LLVM metadata.
ppLlvmMetas :: IsDoc doc => LlvmCgConfig -> [MetaDecl] -> doc
ppLlvmMetas opts metas = lines_ $ map (ppLlvmMeta opts) metas
{-# SPECIALIZE ppLlvmMetas :: LlvmCgConfig -> [MetaDecl] -> SDoc #-}
{-# SPECIALIZE ppLlvmMetas :: LlvmCgConfig -> [MetaDecl] -> HDoc #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

-- | Print out an LLVM metadata definition.
ppLlvmMeta :: IsLine doc => LlvmCgConfig -> MetaDecl -> doc
ppLlvmMeta opts (MetaUnnamed n m)
  = ppMetaId n <+> equals <+> ppMetaExpr opts m

ppLlvmMeta _opts (MetaNamed n m)
  = exclamation <> ftext n <+> equals <+> exclamation <> braces nodes
  where
    nodes = hcat $ intersperse comma $ map ppMetaId m
{-# SPECIALIZE ppLlvmMeta :: LlvmCgConfig -> MetaDecl -> SDoc #-}
{-# SPECIALIZE ppLlvmMeta :: LlvmCgConfig -> MetaDecl -> HLine #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable


-- | Print out a list of function definitions.
ppLlvmFunctions :: IsDoc doc => LlvmCgConfig -> LlvmFunctions -> doc
ppLlvmFunctions opts funcs = vcat $ map (ppLlvmFunction opts) funcs
{-# SPECIALIZE ppLlvmFunctions :: LlvmCgConfig -> LlvmFunctions -> SDoc #-}
{-# SPECIALIZE ppLlvmFunctions :: LlvmCgConfig -> LlvmFunctions -> HDoc #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

-- | Print out a function definition.
ppLlvmFunction :: IsDoc doc => LlvmCgConfig -> LlvmFunction -> doc
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
              <+> attrDoc <+> secDoc <+> prefixDoc <+> ppMetaAnnots opts (funcMetadata fun)
        , line lbrace
        , ppLlvmBlocks opts (funcBody fun)
        , line rbrace
        , newLine
        , newLine]
{-# SPECIALIZE ppLlvmFunction :: LlvmCgConfig -> LlvmFunction -> SDoc #-}
{-# SPECIALIZE ppLlvmFunction :: LlvmCgConfig -> LlvmFunction -> HDoc #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

-- | Print out a function definition header.
ppLlvmFunctionHeader :: IsLine doc => LlvmFunctionDecl -> [LMString] -> doc
ppLlvmFunctionHeader (LlvmFunctionDecl n l c r varg p a) args
  = let varg' = case varg of
                      VarArgs | null p    -> text "..."
                              | otherwise -> text ", ..."
                      _otherwise          -> text ""
        align = case a of
                     Just a' -> text " align " <> int a'
                     Nothing -> empty
        args' = zipWith (\(ty,p) n -> ppLlvmType ty <+> ppSpaceJoin ppLlvmParamAttr p <+> char '%'
                                    <> ftext n)
                   p
                   args
    in ppLlvmLinkageType l <+> ppLlvmCallConvention c <+> ppLlvmType r <+> char '@' <> ftext n <> lparen <>
        hsep (punctuate comma args') <> varg' <> rparen <> align
{-# SPECIALIZE ppLlvmFunctionHeader :: LlvmFunctionDecl -> [LMString] -> SDoc #-}
{-# SPECIALIZE ppLlvmFunctionHeader :: LlvmFunctionDecl -> [LMString] -> HLine #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

-- | Print out a list of function declaration.
ppLlvmFunctionDecls :: IsDoc doc => LlvmFunctionDecls -> doc
ppLlvmFunctionDecls decs = vcat $ map ppLlvmFunctionDecl decs
{-# SPECIALIZE ppLlvmFunctionDecls :: LlvmFunctionDecls -> SDoc #-}
{-# SPECIALIZE ppLlvmFunctionDecls :: LlvmFunctionDecls -> HDoc #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

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
                  map (\(t,a) -> ppLlvmType t <+> ppSpaceJoin ppLlvmParamAttr a) p
    in lines_
        [ text "declare" <+> ppLlvmLinkageType l <+> ppLlvmCallConvention c
          <+> ppLlvmType r <+> char '@' <> ftext n <> lparen <> args <> varg' <> rparen <> align
        , empty]
{-# SPECIALIZE ppLlvmFunctionDecl :: LlvmFunctionDecl -> SDoc #-}
{-# SPECIALIZE ppLlvmFunctionDecl :: LlvmFunctionDecl -> HDoc #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable


-- | Print out a list of LLVM blocks.
ppLlvmBlocks :: IsDoc doc => LlvmCgConfig -> LlvmBlocks -> doc
ppLlvmBlocks opts blocks = vcat $ map (ppLlvmBlock opts) blocks
{-# SPECIALIZE ppLlvmBlocks :: LlvmCgConfig -> LlvmBlocks -> SDoc #-}
{-# SPECIALIZE ppLlvmBlocks :: LlvmCgConfig -> LlvmBlocks -> HDoc #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

-- | Print out an LLVM block.
-- It must be part of a function definition.
ppLlvmBlock :: IsDoc doc => LlvmCgConfig -> LlvmBlock -> doc
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
{-# SPECIALIZE ppLlvmBlock :: LlvmCgConfig -> LlvmBlock -> SDoc #-}
{-# SPECIALIZE ppLlvmBlock :: LlvmCgConfig -> LlvmBlock -> HDoc #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

-- | Print out an LLVM block label.
ppLlvmBlockLabel :: IsLine doc => LlvmBlockId -> doc
ppLlvmBlockLabel id = pprUniqueAlways id <> colon
{-# SPECIALIZE ppLlvmBlockLabel :: LlvmBlockId -> SDoc #-}
{-# SPECIALIZE ppLlvmBlockLabel :: LlvmBlockId -> HLine #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable


-- | Print out an LLVM statement, with any metadata to append to the statement.
ppLlvmStatement :: IsDoc doc => LlvmCgConfig -> LlvmStatement -> doc
ppLlvmStatement opts stmt =
  let ind = line . (text "  " <>)
  in case stmt of
        Assignment  dst expr      -> ind $ ppAssignment opts dst (ppLlvmExpression opts expr)
        Fence       st ord        -> ind $ ppFence st ord
        Branch      target        -> ind $ ppBranch opts target
        BranchIf    cond ifT ifF  -> ind $ ppBranchIf opts cond ifT ifF
        Comment     comments      -> ppLlvmComments comments
        MkLabel     label         -> line $ ppLlvmBlockLabel label
        Store       value ptr align metas
                                  -> ind $ ppStore opts value ptr align metas
        Switch      scrut def tgs -> ppSwitch opts scrut def tgs
        Return      result        -> ind $ ppReturn opts result
        Expr        expr          -> ind $ ppLlvmExpression opts expr
        Unreachable               -> ind $ text "unreachable"
        Nop                       -> line empty

{-# SPECIALIZE ppLlvmStatement :: LlvmCgConfig -> LlvmStatement -> SDoc #-}
{-# SPECIALIZE ppLlvmStatement :: LlvmCgConfig -> LlvmStatement -> HDoc #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

-- | Print out an LLVM expression.
ppLlvmExpression :: IsLine doc => LlvmCgConfig -> LlvmExpression -> doc
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
        Shuffle    v1 v2 idxs       -> ppShuffle opts v1 v2 idxs
        GetElemPtr inb ptr indexes  -> ppGetElementPtr opts inb ptr indexes
        Load       ptr align        -> ppLoad opts ptr align
        ALoad      ord st ptr       -> ppALoad opts ord st ptr
        Malloc     tp amount        -> ppMalloc opts tp amount
        AtomicRMW  aop tgt src ordering -> ppAtomicRMW opts aop tgt src ordering
        CmpXChg    addr old new s_ord f_ord -> ppCmpXChg opts addr old new s_ord f_ord
        Phi        tp predecessors  -> ppPhi opts tp predecessors
        Asm        asm c ty v se sk -> ppAsm opts asm c ty v se sk
        MExpr      meta expr        -> ppMetaAnnotExpr opts meta expr
{-# SPECIALIZE ppLlvmExpression :: LlvmCgConfig -> LlvmExpression -> SDoc #-}
{-# SPECIALIZE ppLlvmExpression :: LlvmCgConfig -> LlvmExpression -> HLine #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

ppMetaExpr :: IsLine doc => LlvmCgConfig -> MetaExpr -> doc
ppMetaExpr opts = \case
  MetaVar (LMLitVar (LMNullLit _)) -> text "null"
  MetaStr    s                     -> char '!' <> doubleQuotes (ftext s)
  MetaLit    l                     -> ppTypeLit opts l
  MetaNode   n                     -> ppMetaId n
  MetaVar    v                     -> ppVar opts v
  MetaStruct es                    -> char '!' <> braces (ppCommaJoin (ppMetaExpr opts) es)
  MetaDIFile {..} ->
      specialMetadata "DIFile"
      [ ("filename" , doubleQuotes $ ftext difFilename)
      , ("directory", doubleQuotes $ ftext difDirectory)
      ]
  MetaDISubroutineType {..} ->
      specialMetadata "DISubroutineType"
      [ ("types", ppMetaExpr opts $ MetaStruct distType ) ]
  MetaDICompileUnit {..} ->
      specialMetadata "DICompileUnit"
      [ ("language"   , ftext dicuLanguage)
      , ("file"       , ppMetaId dicuFile)
      , ("producer"   , doubleQuotes $ ftext dicuProducer)
      , ("isOptimized", if dicuIsOptimized
                            then text "true"
                            else text "false")
      , ("subprograms", ppMetaExpr opts $ dicuSubprograms)
      ]
  MetaDISubprogram {..} ->
      specialMetadata "DISubprogram"
      [ ("name"        , doubleQuotes $ ftext disName)
      , ("linkageName" , doubleQuotes $ ftext disLinkageName)
      , ("scope"       , ppMetaId disScope)
      , ("file"        , ppMetaId disFile)
      , ("line"        , int disLine)
      , ("type"        , ppMetaId disType)
      , ("isDefinition", if disIsDefinition
                              then text "true"
                              else text "false")
      ]
  where
    specialMetadata :: IsLine doc => String -> [(String, doc)] -> doc
    specialMetadata nodeName fields =
        char '!'
        <> text nodeName
        <> parens (hsep $ punctuate comma $ map (\(k,v) -> text k <> colon <+> v) fields)


{-# SPECIALIZE ppMetaExpr :: LlvmCgConfig -> MetaExpr -> SDoc #-}
{-# SPECIALIZE ppMetaExpr :: LlvmCgConfig -> MetaExpr -> HLine #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable


--------------------------------------------------------------------------------
-- * Individual print functions
--------------------------------------------------------------------------------

-- | Should always be a function pointer. So a global var of function type
-- (since globals are always pointers) or a local var of pointer function type.
ppCall :: forall doc. IsLine doc => LlvmCgConfig -> LlvmCallType -> LlvmVar -> [MetaExpr]
       -> [LlvmFuncAttr] -> doc
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
                ppArgTy  = ppCommaJoin (ppLlvmType . fst) params <>
                           (case argTy of
                               VarArgs   -> text ", ..."
                               FixedArgs -> empty)
                fnty = space <> lparen <> ppArgTy <> rparen
                attrDoc = ppSpaceJoin ppLlvmFuncAttr attrs
            in  tc <> text "call" <+> ppLlvmCallConvention cc <+> ppLlvmType ret
                    <> fnty <+> ppName opts fptr <> lparen <+> ppValues
                    <+> rparen <+> attrDoc

        ppCallParams :: LlvmCgConfig -> [[LlvmParamAttr]] -> [MetaExpr] -> doc
        ppCallParams opts attrs args = hsep $ punctuate comma $ zipWith ppCallMetaExpr attrs args
         where
          -- Metadata needs to be marked as having the `metadata` type when used
          -- in a call argument
          ppCallMetaExpr attrs (MetaVar v) = ppVar' attrs opts v
          ppCallMetaExpr _ v             = text "metadata" <+> ppMetaExpr opts v
{-# SPECIALIZE ppCall :: LlvmCgConfig -> LlvmCallType -> LlvmVar -> [MetaExpr] -> [LlvmFuncAttr] -> SDoc #-}
{-# SPECIALIZE ppCall :: LlvmCgConfig -> LlvmCallType -> LlvmVar -> [MetaExpr] -> [LlvmFuncAttr] -> HLine #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable


ppMachOp :: IsLine doc => LlvmCgConfig -> LlvmMachOp -> LlvmVar -> LlvmVar -> doc
ppMachOp opts op left right =
  ppLlvmMachOp op <+> ppLlvmType (getVarType left) <+> ppName opts left
        <> comma <+> ppName opts right
{-# SPECIALIZE ppMachOp :: LlvmCgConfig -> LlvmMachOp -> LlvmVar -> LlvmVar -> SDoc #-}
{-# SPECIALIZE ppMachOp :: LlvmCgConfig -> LlvmMachOp -> LlvmVar -> LlvmVar -> HLine #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable


ppCmpOp :: IsLine doc => LlvmCgConfig -> LlvmCmpOp -> LlvmVar -> LlvmVar -> doc
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
  in cmpOp <+> ppLlvmCmpOp op <+> ppLlvmType (getVarType left)
        <+> ppName opts left <> comma <+> ppName opts right
{-# SPECIALIZE ppCmpOp :: LlvmCgConfig -> LlvmCmpOp -> LlvmVar -> LlvmVar -> SDoc #-}
{-# SPECIALIZE ppCmpOp :: LlvmCgConfig -> LlvmCmpOp -> LlvmVar -> LlvmVar -> HLine #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

ppAssignment :: IsLine doc => LlvmCgConfig -> LlvmVar -> doc -> doc
ppAssignment opts var expr = ppName opts var <+> equals <+> expr
{-# SPECIALIZE ppAssignment :: LlvmCgConfig -> LlvmVar -> SDoc -> SDoc #-}
{-# SPECIALIZE ppAssignment :: LlvmCgConfig -> LlvmVar -> HLine -> HLine #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

ppFence :: IsLine doc => Bool -> LlvmSyncOrdering -> doc
ppFence st ord =
  let singleThread = case st of True  -> text "singlethread"
                                False -> empty
  in text "fence" <+> singleThread <+> ppSyncOrdering ord
{-# SPECIALIZE ppFence :: Bool -> LlvmSyncOrdering -> SDoc #-}
{-# SPECIALIZE ppFence :: Bool -> LlvmSyncOrdering -> HLine #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

ppSyncOrdering :: IsLine doc => LlvmSyncOrdering -> doc
ppSyncOrdering SyncUnord     = text "unordered"
ppSyncOrdering SyncMonotonic = text "monotonic"
ppSyncOrdering SyncAcquire   = text "acquire"
ppSyncOrdering SyncRelease   = text "release"
ppSyncOrdering SyncAcqRel    = text "acq_rel"
ppSyncOrdering SyncSeqCst    = text "seq_cst"
{-# SPECIALIZE ppSyncOrdering :: LlvmSyncOrdering -> SDoc #-}
{-# SPECIALIZE ppSyncOrdering :: LlvmSyncOrdering -> HLine #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

ppAtomicOp :: IsLine doc => LlvmAtomicOp -> doc
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
{-# SPECIALIZE ppAtomicOp :: LlvmAtomicOp -> SDoc #-}
{-# SPECIALIZE ppAtomicOp :: LlvmAtomicOp -> HLine #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

ppAtomicRMW :: IsLine doc => LlvmCgConfig -> LlvmAtomicOp -> LlvmVar -> LlvmVar -> LlvmSyncOrdering -> doc
ppAtomicRMW opts aop tgt src ordering =
  text "atomicrmw" <+> ppAtomicOp aop <+> ppVar opts tgt <> comma
  <+> ppVar opts src <+> ppSyncOrdering ordering
{-# SPECIALIZE ppAtomicRMW :: LlvmCgConfig -> LlvmAtomicOp -> LlvmVar -> LlvmVar -> LlvmSyncOrdering -> SDoc #-}
{-# SPECIALIZE ppAtomicRMW :: LlvmCgConfig -> LlvmAtomicOp -> LlvmVar -> LlvmVar -> LlvmSyncOrdering -> HLine #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

ppCmpXChg :: IsLine doc => LlvmCgConfig -> LlvmVar -> LlvmVar -> LlvmVar
          -> LlvmSyncOrdering -> LlvmSyncOrdering -> doc
ppCmpXChg opts addr old new s_ord f_ord =
  text "cmpxchg" <+> ppVar opts addr <> comma <+> ppVar opts old <> comma <+> ppVar opts new
  <+> ppSyncOrdering s_ord <+> ppSyncOrdering f_ord
{-# SPECIALIZE ppCmpXChg :: LlvmCgConfig -> LlvmVar -> LlvmVar -> LlvmVar -> LlvmSyncOrdering -> LlvmSyncOrdering -> SDoc #-}
{-# SPECIALIZE ppCmpXChg :: LlvmCgConfig -> LlvmVar -> LlvmVar -> LlvmVar -> LlvmSyncOrdering -> LlvmSyncOrdering -> HLine #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable


ppLoad :: IsLine doc => LlvmCgConfig -> LlvmVar -> LMAlign -> doc
ppLoad opts var alignment =
  text "load" <+> ppLlvmType derefType <> comma <+> ppVar opts var <> align
  where
    derefType = pLower $ getVarType var
    align =
      case alignment of
        Just n  -> text ", align" <+> int n
        Nothing -> empty
{-# SPECIALIZE ppLoad :: LlvmCgConfig -> LlvmVar -> LMAlign -> SDoc #-}
{-# SPECIALIZE ppLoad :: LlvmCgConfig -> LlvmVar -> LMAlign -> HLine #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

ppALoad :: IsLine doc => LlvmCgConfig -> LlvmSyncOrdering -> SingleThreaded -> LlvmVar -> doc
ppALoad opts ord st var =
  let alignment = llvmWidthInBits (llvmCgPlatform opts) (getVarType var) `quot` 8
      align     = text ", align" <+> int alignment
      sThreaded | st        = text " singlethread"
                | otherwise = empty
      derefType = pLower $ getVarType var
  in text "load atomic" <+> ppLlvmType derefType <> comma <+> ppVar opts var <> sThreaded
            <+> ppSyncOrdering ord <> align
{-# SPECIALIZE ppALoad :: LlvmCgConfig -> LlvmSyncOrdering -> SingleThreaded -> LlvmVar -> SDoc #-}
{-# SPECIALIZE ppALoad :: LlvmCgConfig -> LlvmSyncOrdering -> SingleThreaded -> LlvmVar -> HLine #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

ppStore :: IsLine doc => LlvmCgConfig -> LlvmVar -> LlvmVar -> LMAlign -> [MetaAnnot] -> doc
ppStore opts val dst alignment metas =
    text "store" <+> ppVar opts val <> comma <+> ppVar opts dst <> align <+> ppMetaAnnots opts metas
  where
    align =
      case alignment of
        Just n  -> text ", align" <+> int n
        Nothing -> empty
{-# SPECIALIZE ppStore :: LlvmCgConfig -> LlvmVar -> LlvmVar -> LMAlign -> [MetaAnnot] -> SDoc #-}
{-# SPECIALIZE ppStore :: LlvmCgConfig -> LlvmVar -> LlvmVar -> LMAlign -> [MetaAnnot] -> HLine #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable


ppCast :: IsLine doc => LlvmCgConfig -> LlvmCastOp -> LlvmVar -> LlvmType -> doc
ppCast opts op from to
    =   ppLlvmCastOp op
    <+> ppLlvmType (getVarType from) <+> ppName opts from
    <+> text "to"
    <+> ppLlvmType to
{-# SPECIALIZE ppCast :: LlvmCgConfig -> LlvmCastOp -> LlvmVar -> LlvmType -> SDoc #-}
{-# SPECIALIZE ppCast :: LlvmCgConfig -> LlvmCastOp -> LlvmVar -> LlvmType -> HLine #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable


ppMalloc :: IsLine doc => LlvmCgConfig -> LlvmType -> Int -> doc
ppMalloc opts tp amount =
  let amount' = LMLitVar $ LMIntLit (toInteger amount) i32
  in text "malloc" <+> ppLlvmType tp <> comma <+> ppVar opts amount'
{-# SPECIALIZE ppMalloc :: LlvmCgConfig -> LlvmType -> Int -> SDoc #-}
{-# SPECIALIZE ppMalloc :: LlvmCgConfig -> LlvmType -> Int -> HLine #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

ppAlloca :: IsLine doc => LlvmCgConfig -> LlvmType -> Int -> doc
ppAlloca opts tp amount =
  let amount' = LMLitVar $ LMIntLit (toInteger amount) i32
  in text "alloca" <+> ppLlvmType tp <> comma <+> ppVar opts amount'
{-# SPECIALIZE ppAlloca :: LlvmCgConfig -> LlvmType -> Int -> SDoc #-}
{-# SPECIALIZE ppAlloca :: LlvmCgConfig -> LlvmType -> Int -> HLine #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

ppGetElementPtr :: IsLine doc => LlvmCgConfig -> Bool -> LlvmVar -> [LlvmVar] -> doc
ppGetElementPtr opts inb ptr idx =
  let indexes = comma <+> ppCommaJoin (ppVar opts) idx
      inbound = if inb then text "inbounds" else empty
      derefType = pLower $ getVarType ptr
  in text "getelementptr" <+> inbound <+> ppLlvmType derefType <> comma <+> ppVar opts ptr
                            <> indexes
{-# SPECIALIZE ppGetElementPtr :: LlvmCgConfig -> Bool -> LlvmVar -> [LlvmVar] -> SDoc #-}
{-# SPECIALIZE ppGetElementPtr :: LlvmCgConfig -> Bool -> LlvmVar -> [LlvmVar] -> HLine #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable


ppReturn :: IsLine doc => LlvmCgConfig -> Maybe LlvmVar -> doc
ppReturn opts (Just var) = text "ret" <+> ppVar opts var
ppReturn _    Nothing    = text "ret" <+> ppLlvmType LMVoid
{-# SPECIALIZE ppReturn :: LlvmCgConfig -> Maybe LlvmVar -> SDoc #-}
{-# SPECIALIZE ppReturn :: LlvmCgConfig -> Maybe LlvmVar -> HLine #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

ppBranch :: IsLine doc => LlvmCgConfig -> LlvmVar -> doc
ppBranch opts var = text "br" <+> ppVar opts var
{-# SPECIALIZE ppBranch :: LlvmCgConfig -> LlvmVar -> SDoc #-}
{-# SPECIALIZE ppBranch :: LlvmCgConfig -> LlvmVar -> HLine #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable


ppBranchIf :: IsLine doc => LlvmCgConfig -> LlvmVar -> LlvmVar -> LlvmVar -> doc
ppBranchIf opts cond trueT falseT
  = text "br" <+> ppVar opts cond <> comma <+> ppVar opts trueT <> comma <+> ppVar opts falseT
{-# SPECIALIZE ppBranchIf :: LlvmCgConfig -> LlvmVar -> LlvmVar -> LlvmVar -> SDoc #-}
{-# SPECIALIZE ppBranchIf :: LlvmCgConfig -> LlvmVar -> LlvmVar -> LlvmVar -> HLine #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable


ppPhi :: IsLine doc => LlvmCgConfig -> LlvmType -> [(LlvmVar,LlvmVar)] -> doc
ppPhi opts tp preds =
  let ppPreds (val, label) = brackets $ ppName opts val <> comma <+> ppName opts label
  in text "phi" <+> ppLlvmType tp <+> hsep (punctuate comma $ map ppPreds preds)
{-# SPECIALIZE ppPhi :: LlvmCgConfig -> LlvmType -> [(LlvmVar,LlvmVar)] -> SDoc #-}
{-# SPECIALIZE ppPhi :: LlvmCgConfig -> LlvmType -> [(LlvmVar,LlvmVar)] -> HLine #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable


ppSwitch :: IsDoc doc => LlvmCgConfig -> LlvmVar -> LlvmVar -> [(LlvmVar,LlvmVar)] -> doc
ppSwitch opts scrut dflt targets =
  let ppTarget  (val, lab) = text "  " <> ppVar opts val <> comma <+> ppVar opts lab
  in lines_ $ concat
      [ [text "switch" <+> ppVar opts scrut <> comma <+> ppVar opts dflt <+> char '[']
      , map ppTarget targets
      , [char ']']
      ]
{-# SPECIALIZE ppSwitch :: LlvmCgConfig -> LlvmVar -> LlvmVar -> [(LlvmVar,LlvmVar)] -> SDoc #-}
{-# SPECIALIZE ppSwitch :: LlvmCgConfig -> LlvmVar -> LlvmVar -> [(LlvmVar,LlvmVar)] -> HDoc #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable


ppAsm :: IsLine doc => LlvmCgConfig -> LMString -> LMString -> LlvmType -> [LlvmVar] -> Bool -> Bool -> doc
ppAsm opts asm constraints rty vars sideeffect alignstack =
  let asm'  = doubleQuotes $ ftext asm
      cons  = doubleQuotes $ ftext constraints
      rty'  = ppLlvmType rty
      vars' = lparen <+> ppCommaJoin (ppVar opts) vars <+> rparen
      side  = if sideeffect then text "sideeffect" else empty
      align = if alignstack then text "alignstack" else empty
  in text "call" <+> rty' <+> text "asm" <+> side <+> align <+> asm' <> comma
        <+> cons <> vars'
{-# SPECIALIZE ppAsm :: LlvmCgConfig -> LMString -> LMString -> LlvmType -> [LlvmVar] -> Bool -> Bool -> SDoc #-}
{-# SPECIALIZE ppAsm :: LlvmCgConfig -> LMString -> LMString -> LlvmType -> [LlvmVar] -> Bool -> Bool -> HLine #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

ppExtract :: IsLine doc => LlvmCgConfig -> LlvmVar -> LlvmVar -> doc
ppExtract opts vec idx =
    text "extractelement"
    <+> ppLlvmType (getVarType vec) <+> ppName opts vec <> comma
    <+> ppVar opts idx
{-# SPECIALIZE ppExtract :: LlvmCgConfig -> LlvmVar -> LlvmVar -> SDoc #-}
{-# SPECIALIZE ppExtract :: LlvmCgConfig -> LlvmVar -> LlvmVar -> HLine #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

ppExtractV :: IsLine doc => LlvmCgConfig -> LlvmVar -> Int -> doc
ppExtractV opts struct idx =
    text "extractvalue"
    <+> ppLlvmType (getVarType struct) <+> ppName opts struct <> comma
    <+> int idx
{-# SPECIALIZE ppExtractV :: LlvmCgConfig -> LlvmVar -> Int -> SDoc #-}
{-# SPECIALIZE ppExtractV :: LlvmCgConfig -> LlvmVar -> Int -> HLine #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

ppInsert :: IsLine doc => LlvmCgConfig -> LlvmVar -> LlvmVar -> LlvmVar -> doc
ppInsert opts vec elt idx =
    text "insertelement"
    <+> ppLlvmType (getVarType vec) <+> ppName opts vec <> comma
    <+> ppLlvmType (getVarType elt) <+> ppName opts elt <> comma
    <+> ppVar opts idx
{-# SPECIALIZE ppInsert :: LlvmCgConfig -> LlvmVar -> LlvmVar -> LlvmVar -> SDoc #-}
{-# SPECIALIZE ppInsert :: LlvmCgConfig -> LlvmVar -> LlvmVar -> LlvmVar -> HLine #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

ppShuffle :: IsLine doc => LlvmCgConfig -> LlvmVar -> LlvmVar -> [Int] -> doc
ppShuffle opts v1 v2 idxs =
    text "shufflevector"
    <+> ppLlvmType (getVarType v1) <+> ppName opts v1 <> comma
    <+> ppLlvmType (getVarType v2) <+> ppName opts v2 <> comma
    <+> ppLlvmType (LMVector (length idxs) (LMInt 32)) <+> ppLit opts (LMVectorLit $ map ((`LMIntLit` (LMInt 32)) . fromIntegral) idxs)
{-# SPECIALIZE ppShuffle :: LlvmCgConfig -> LlvmVar -> LlvmVar -> [Int] -> SDoc #-}
{-# SPECIALIZE ppShuffle :: LlvmCgConfig -> LlvmVar -> LlvmVar -> [Int] -> HLine #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

ppMetaAnnotExpr :: IsLine doc => LlvmCgConfig -> [MetaAnnot] -> LlvmExpression -> doc
ppMetaAnnotExpr opts meta expr =
   ppLlvmExpression opts expr <> comma <+> ppMetaAnnots opts meta
{-# SPECIALIZE ppMetaAnnotExpr :: LlvmCgConfig -> [MetaAnnot] -> LlvmExpression -> SDoc #-}
{-# SPECIALIZE ppMetaAnnotExpr :: LlvmCgConfig -> [MetaAnnot] -> LlvmExpression -> HLine #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

ppMetaAnnots :: IsLine doc => LlvmCgConfig -> [MetaAnnot] -> doc
ppMetaAnnots opts meta = hcat $ punctuate comma $ map ppMeta meta
  where
    ppMeta (MetaAnnot name e)
        = exclamation <> ftext name <+>
          case e of
            MetaNode n    -> ppMetaId n
            MetaStruct ms -> exclamation <> braces (ppCommaJoin (ppMetaExpr opts) ms)
            other         -> exclamation <> braces (ppMetaExpr opts other) -- possible?
{-# SPECIALIZE ppMetaAnnots :: LlvmCgConfig -> [MetaAnnot] -> SDoc #-}
{-# SPECIALIZE ppMetaAnnots :: LlvmCgConfig -> [MetaAnnot] -> HLine #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

-- | Return the variable name or value of the 'LlvmVar'
-- in Llvm IR textual representation (e.g. @\@x@, @%y@ or @42@).
ppName :: IsLine doc => LlvmCgConfig -> LlvmVar -> doc
ppName opts v = case v of
   LMGlobalVar {} -> char '@' <> ppPlainName opts v
   LMLocalVar  {} -> char '%' <> ppPlainName opts v
   LMNLocalVar {} -> char '%' <> ppPlainName opts v
   LMLitVar    {} ->             ppPlainName opts v
{-# SPECIALIZE ppName :: LlvmCgConfig -> LlvmVar -> SDoc #-}
{-# SPECIALIZE ppName :: LlvmCgConfig -> LlvmVar -> HLine #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

-- | Return the variable name or value of the 'LlvmVar'
-- in a plain textual representation (e.g. @x@, @y@ or @42@).
ppPlainName :: IsLine doc => LlvmCgConfig -> LlvmVar -> doc
ppPlainName opts v = case v of
   (LMGlobalVar x _ _ _ _ _) -> ftext x
   (LMLocalVar  x LMLabel  ) -> pprUniqueAlways x
   (LMLocalVar  x _        ) -> char 'l' <> pprUniqueAlways x
   (LMNLocalVar x _        ) -> ftext x
   (LMLitVar    x          ) -> ppLit opts x
{-# SPECIALIZE ppPlainName :: LlvmCgConfig -> LlvmVar -> SDoc #-}
{-# SPECIALIZE ppPlainName :: LlvmCgConfig -> LlvmVar -> HLine #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

-- | Print a literal value. No type.
ppLit :: IsLine doc => LlvmCgConfig -> LlvmLit -> doc
ppLit opts l = case l of
   (LMIntLit   i _       )  -> integer i
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
{-# SPECIALIZE ppLit :: LlvmCgConfig -> LlvmLit -> SDoc #-}
{-# SPECIALIZE ppLit :: LlvmCgConfig -> LlvmLit -> HLine #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

ppVar :: IsLine doc => LlvmCgConfig -> LlvmVar -> doc
ppVar = ppVar' []
{-# SPECIALIZE ppVar :: LlvmCgConfig -> LlvmVar -> SDoc #-}
{-# SPECIALIZE ppVar :: LlvmCgConfig -> LlvmVar -> HLine #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

ppVar' :: IsLine doc => [LlvmParamAttr] -> LlvmCgConfig -> LlvmVar -> doc
ppVar' attrs opts v = case v of
  LMLitVar x -> ppTypeLit' attrs opts x
  x          -> ppLlvmType (getVarType x) <+> ppSpaceJoin ppLlvmParamAttr attrs <+> ppName opts x
{-# SPECIALIZE ppVar' :: [LlvmParamAttr] -> LlvmCgConfig -> LlvmVar -> SDoc #-}
{-# SPECIALIZE ppVar' :: [LlvmParamAttr] -> LlvmCgConfig -> LlvmVar -> HLine #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

ppTypeLit :: IsLine doc => LlvmCgConfig -> LlvmLit -> doc
ppTypeLit = ppTypeLit' []
{-# SPECIALIZE ppTypeLit :: LlvmCgConfig -> LlvmLit -> SDoc #-}
{-# SPECIALIZE ppTypeLit :: LlvmCgConfig -> LlvmLit -> HLine #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

ppTypeLit' :: IsLine doc => [LlvmParamAttr] -> LlvmCgConfig -> LlvmLit -> doc
ppTypeLit' attrs opts l = ppLlvmType (getLitType l) <+> ppSpaceJoin ppLlvmParamAttr attrs <+> ppLit opts l
{-# SPECIALIZE ppTypeLit' :: [LlvmParamAttr] -> LlvmCgConfig -> LlvmLit -> SDoc #-}
{-# SPECIALIZE ppTypeLit' :: [LlvmParamAttr] -> LlvmCgConfig -> LlvmLit -> HLine #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable

ppStatic :: IsLine doc => LlvmCgConfig -> LlvmStatic -> doc
ppStatic opts st = case st of
  LMComment       s -> text "; " <> ftext s
  LMStaticLit   l   -> ppTypeLit opts l
  LMUninitType    t -> ppLlvmType t <> text " undef"
  LMStaticStr   s t -> ppLlvmType t <> text " c\"" <> ftext s <> text "\\00\""
  LMStaticArray d t -> ppLlvmType t <> text " [" <> ppCommaJoin (ppStatic opts) d <> char ']'
  LMStaticStruc d t -> ppLlvmType t <> text "<{" <> ppCommaJoin (ppStatic opts) d <> text "}>"
  LMStaticStrucU d t -> ppLlvmType t <> text "{" <> ppCommaJoin (ppStatic opts) d <> text "}"
  LMStaticPointer v -> ppVar opts v
  LMTrunc v t       -> ppLlvmType t <> text " trunc (" <> ppStatic opts v <> text " to " <> ppLlvmType t <> char ')'
  LMBitc v t        -> ppLlvmType t <> text " bitcast (" <> ppStatic opts v <> text " to " <> ppLlvmType t <> char ')'
  LMPtoI v t        -> ppLlvmType t <> text " ptrtoint (" <> ppStatic opts v <> text " to " <> ppLlvmType t <> char ')'
  LMAdd s1 s2       -> pprStaticArith opts s1 s2 (text "add") (text "fadd") (text "LMAdd")
  LMSub s1 s2       -> pprStaticArith opts s1 s2 (text "sub") (text "fsub") (text "LMSub")
{-# SPECIALIZE ppStatic :: LlvmCgConfig -> LlvmStatic -> SDoc #-}
{-# SPECIALIZE ppStatic :: LlvmCgConfig -> LlvmStatic -> HLine #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable


pprSpecialStatic :: IsLine doc => LlvmCgConfig -> LlvmStatic -> doc
pprSpecialStatic opts stat = case stat of
   LMBitc v t        -> ppLlvmType (pLower t)
                        <> text ", bitcast ("
                        <> ppStatic opts v <> text " to " <> ppLlvmType t
                        <> char ')'
   LMStaticPointer x -> ppLlvmType (pLower $ getVarType x)
                        <> comma <+> ppStatic opts stat
   _                 -> ppStatic opts stat
{-# SPECIALIZE pprSpecialStatic :: LlvmCgConfig -> LlvmStatic -> SDoc #-}
{-# SPECIALIZE pprSpecialStatic :: LlvmCgConfig -> LlvmStatic -> HLine #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable


pprStaticArith :: IsLine doc => LlvmCgConfig -> LlvmStatic -> LlvmStatic -> doc -> doc -> SDoc -> doc
pprStaticArith opts s1 s2 int_op float_op op_name =
  let ty1 = getStatType s1
      op  = if isFloat ty1 then float_op else int_op
  in if ty1 == getStatType s2
     then ppLlvmType ty1 <+> op <+> lparen <> ppStatic opts s1 <> comma <> ppStatic opts s2 <> rparen
     else pprPanic "pprStaticArith" $
                 op_name <> text " with different types! s1: " <> ppStatic opts s1
                         <> text", s2: " <> ppStatic opts s2
{-# SPECIALIZE pprStaticArith :: LlvmCgConfig -> LlvmStatic -> LlvmStatic -> SDoc -> SDoc -> SDoc -> SDoc #-}
{-# SPECIALIZE pprStaticArith :: LlvmCgConfig -> LlvmStatic -> LlvmStatic -> HLine -> HLine -> SDoc -> HLine #-} -- see Note [SPECIALIZE to HDoc] in GHC.Utils.Outputable


--------------------------------------------------------------------------------
-- * Misc functions
--------------------------------------------------------------------------------

-- | Blank line.
newLine :: IsDoc doc => doc
newLine = empty
{-# SPECIALIZE newLine :: SDoc #-}
{-# SPECIALIZE newLine :: HDoc #-}

-- | Exclamation point.
exclamation :: IsLine doc => doc
exclamation = char '!'
{-# SPECIALIZE exclamation :: SDoc #-}
{-# SPECIALIZE exclamation :: HLine #-}
