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

import Llvm.AbsSyn
import Llvm.MetaData
import Llvm.Types

import Data.List ( intersperse )
import Outputable
import Unique

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
ppLlvmGlobal (var@(LMGlobalVar _ _ link x a c), dat) =
    let sect = case x of
            Just x' -> text ", section" <+> doubleQuotes (ftext x')
            Nothing -> empty

        align = case a of
            Just a' -> text ", align" <+> int a'
            Nothing -> empty

        rhs = case dat of
            Just stat -> texts stat
            Nothing   -> texts (pLower $ getVarType var)

        const' = if c then text "constant" else text "global"

    in ppAssignment var $ texts link <+> const' <+> rhs <> sect <> align
       $+$ newLine

ppLlvmGlobal oth = error $ "Non Global var ppr as global! " ++ show oth


-- | Print out a list of LLVM type aliases.
ppLlvmAliases :: [LlvmAlias] -> SDoc
ppLlvmAliases tys = vcat $ map ppLlvmAlias tys

-- | Print out an LLVM type alias.
ppLlvmAlias :: LlvmAlias -> SDoc
ppLlvmAlias (name, ty)
  = text "%" <> ftext name <+> equals <+> text "type" <+> texts ty


-- | Print out a list of LLVM metadata.
ppLlvmMetas :: [MetaDecl] -> SDoc
ppLlvmMetas metas = vcat $ map ppLlvmMeta metas

-- | Print out an LLVM metadata definition.
ppLlvmMeta :: MetaDecl -> SDoc
ppLlvmMeta (MetaUnamed n m)
  = exclamation <> int n <> text " = metadata !" <> braces (ppLlvmMetaExpr m)

ppLlvmMeta (MetaNamed n m)
  = exclamation <> ftext n <> text " = !" <> braces nodes
  where
    nodes = hcat $ intersperse comma $ map pprNode m
    pprNode n = exclamation <> int n

-- | Print out an LLVM metadata value.
ppLlvmMetaExpr :: MetaExpr -> SDoc
ppLlvmMetaExpr (MetaStr  s ) = text "metadata !" <> doubleQuotes (ftext s)
ppLlvmMetaExpr (MetaNode n ) = text "metadata !" <> int n
ppLlvmMetaExpr (MetaVar  v ) = texts v
ppLlvmMetaExpr (MetaExpr es) =
    hcat $ intersperse (text ", ") $ map ppLlvmMetaExpr es


-- | Print out a list of function definitions.
ppLlvmFunctions :: LlvmFunctions -> SDoc
ppLlvmFunctions funcs = vcat $ map ppLlvmFunction funcs

-- | Print out a function definition.
ppLlvmFunction :: LlvmFunction -> SDoc
ppLlvmFunction (LlvmFunction dec args attrs sec body) =
    let attrDoc = ppSpaceJoin attrs
        secDoc = case sec of
                      Just s' -> text "section" <+> (doubleQuotes $ ftext s')
                      Nothing -> empty
    in text "define" <+> ppLlvmFunctionHeader dec args
        <+> attrDoc <+> secDoc
        $+$ lbrace
        $+$ ppLlvmBlocks body
        $+$ rbrace
        $+$ newLine
        $+$ newLine

-- | Print out a function defenition header.
ppLlvmFunctionHeader :: LlvmFunctionDecl -> [LMString] -> SDoc
ppLlvmFunctionHeader (LlvmFunctionDecl n l c r varg p a) args
  = let varg' = case varg of
                      VarArgs | null p    -> text "..."
                              | otherwise -> text ", ..."
                      _otherwise          -> empty
        align = case a of
                     Just a' -> text " align" <+> texts a'
                     Nothing -> empty
        args' = map (\((ty,p),n) -> texts ty <+> ppSpaceJoin p <+> text "%"
                                    <> ftext n)
                    (zip p args)
    in texts l <+> texts c <+> texts r <+> text "@" <> ftext n <> lparen <>
        (hcat $ intersperse (comma <> space) args') <> varg' <> rparen <> align

-- | Print out a list of function declaration.
ppLlvmFunctionDecls :: LlvmFunctionDecls -> SDoc
ppLlvmFunctionDecls decs = vcat $ map ppLlvmFunctionDecl decs

-- | Print out a function declaration.
-- Declarations define the function type but don't define the actual body of
-- the function.
ppLlvmFunctionDecl :: LlvmFunctionDecl -> SDoc
ppLlvmFunctionDecl (LlvmFunctionDecl n l c r varg p a)
  = let varg' = case varg of
                      VarArgs | null p    -> text "..."
                              | otherwise -> text ", ..."
                      _otherwise          -> empty
        align = case a of
                     Just a' -> text " align" <+> texts a'
                     Nothing -> empty
        args = hcat $ intersperse (comma <> space) $
                  map (\(t,a) -> texts t <+> ppSpaceJoin a) p
    in text "declare" <+> texts l <+> texts c <+> texts r <+> text "@" <>
        ftext n <> lparen <> args <> varg' <> rparen <> align $+$ newLine


-- | Print out a list of LLVM blocks.
ppLlvmBlocks :: LlvmBlocks -> SDoc
ppLlvmBlocks blocks = vcat $ map ppLlvmBlock blocks

-- | Print out an LLVM block.
-- It must be part of a function definition.
ppLlvmBlock :: LlvmBlock -> SDoc
ppLlvmBlock (LlvmBlock blockId stmts)
  = go blockId stmts
  where
    lbreak acc []              = (Nothing, reverse acc, [])
    lbreak acc (MkLabel id:xs) = (Just id, reverse acc, xs)
    lbreak acc (x:xs)          = lbreak (x:acc) xs

    go id code =
        let (id2, block, rest) = lbreak [] code
            ppRest = case id2 of
                         Just id2' -> go id2' rest
                         Nothing   -> empty
        in ppLlvmBlockLabel id
           $+$ (vcat $ map ppLlvmStatement block)
           $+$ newLine
           $+$ ppRest

-- | Print out an LLVM block label.
ppLlvmBlockLabel :: LlvmBlockId -> SDoc
ppLlvmBlockLabel id = pprUnique id <> colon


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
        Call       tp fp args attrs -> ppCall tp fp args attrs
        Cast       op from to       -> ppCast op from to
        Compare    op left right    -> ppCmpOp op left right
        Extract    vec idx          -> ppExtract vec idx
        Insert     vec elt idx      -> ppInsert vec elt idx
        GetElemPtr inb ptr indexes  -> ppGetElementPtr inb ptr indexes
        Load       ptr              -> ppLoad ptr
        Malloc     tp amount        -> ppMalloc tp amount
        Phi        tp precessors    -> ppPhi tp precessors
        Asm        asm c ty v se sk -> ppAsm asm c ty v se sk
        MExpr      meta expr        -> ppMetaExpr meta expr


--------------------------------------------------------------------------------
-- * Individual print functions
--------------------------------------------------------------------------------

-- | Should always be a function pointer. So a global var of function type
-- (since globals are always pointers) or a local var of pointer function type.
ppCall :: LlvmCallType -> LlvmVar -> [LlvmVar] -> [LlvmFuncAttr] -> SDoc
ppCall ct fptr vals attrs = case fptr of
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
                ppValues = ppCommaJoin vals
                ppParams = map (texts . fst) params
                ppArgTy  = (hcat $ intersperse comma ppParams) <>
                           (case argTy of
                               VarArgs   -> text ", ..."
                               FixedArgs -> empty)
                fnty = space <> lparen <> ppArgTy <> rparen <> text "*"
                attrDoc = ppSpaceJoin attrs
            in  tc <> text "call" <+> texts cc <+> texts ret
                    <> fnty <+> (text $ getName fptr) <> lparen <+> ppValues
                    <+> rparen <+> attrDoc


ppMachOp :: LlvmMachOp -> LlvmVar -> LlvmVar -> SDoc
ppMachOp op left right =
  (texts op) <+> (texts (getVarType left)) <+> (text $ getName left)
        <> comma <+> (text $ getName right)


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
  in cmpOp <+> texts op <+> texts (getVarType left)
        <+> (text $ getName left) <> comma <+> (text $ getName right)


ppAssignment :: LlvmVar -> SDoc -> SDoc
ppAssignment var expr = (text $ getName var) <+> equals <+> expr

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

-- XXX: On x86, vector types need to be 16-byte aligned for aligned access, but
-- we have no way of guaranteeing that this is true with GHC (we would need to
-- modify the layout of the stack and closures, change the storage manager,
-- etc.). So, we blindly tell LLVM that *any* vector store or load could be
-- unaligned. In the future we may be able to guarantee that certain vector
-- access patterns are aligned, in which case we will need a more granular way
-- of specifying alignment.

ppLoad :: LlvmVar -> SDoc
ppLoad var
    | isVecPtrVar var = text "load" <+> texts var <>
                        comma <+> text "align 1"
    | otherwise       = text "load" <+> texts var
  where
    isVecPtrVar :: LlvmVar -> Bool
    isVecPtrVar = isVector . pLower . getVarType

ppStore :: LlvmVar -> LlvmVar -> SDoc
ppStore val dst
    | isVecPtrVar dst = text "store" <+> texts val <> comma <+> texts dst <>
                        comma <+> text "align 1"
    | otherwise       = text "store" <+> texts val <> comma <+> texts dst
  where
    isVecPtrVar :: LlvmVar -> Bool
    isVecPtrVar = isVector . pLower . getVarType


ppCast :: LlvmCastOp -> LlvmVar -> LlvmType -> SDoc
ppCast op from to = texts op <+> texts from <+> text "to" <+> texts to


ppMalloc :: LlvmType -> Int -> SDoc
ppMalloc tp amount =
  let amount' = LMLitVar $ LMIntLit (toInteger amount) i32
  in text "malloc" <+> texts tp <> comma <+> texts amount'


ppAlloca :: LlvmType -> Int -> SDoc
ppAlloca tp amount =
  let amount' = LMLitVar $ LMIntLit (toInteger amount) i32
  in text "alloca" <+> texts tp <> comma <+> texts amount'


ppGetElementPtr :: Bool -> LlvmVar -> [LlvmVar] -> SDoc
ppGetElementPtr inb ptr idx =
  let indexes = comma <+> ppCommaJoin idx
      inbound = if inb then text "inbounds" else empty
  in text "getelementptr" <+> inbound <+> texts ptr <> indexes


ppReturn :: Maybe LlvmVar -> SDoc
ppReturn (Just var) = text "ret" <+> texts var
ppReturn Nothing    = text "ret" <+> texts LMVoid


ppBranch :: LlvmVar -> SDoc
ppBranch var = text "br" <+> texts var


ppBranchIf :: LlvmVar -> LlvmVar -> LlvmVar -> SDoc
ppBranchIf cond trueT falseT
  = text "br" <+> texts cond <> comma <+> texts trueT <> comma <+> texts falseT


ppPhi :: LlvmType -> [(LlvmVar,LlvmVar)] -> SDoc
ppPhi tp preds =
  let ppPreds (val, label) = brackets $ (text $ getName val) <> comma
        <+> (text $ getName label)
  in text "phi" <+> texts tp <+> hcat (intersperse comma $ map ppPreds preds)


ppSwitch :: LlvmVar -> LlvmVar -> [(LlvmVar,LlvmVar)] -> SDoc
ppSwitch scrut dflt targets =
  let ppTarget  (val, lab) = texts val <> comma <+> texts lab
      ppTargets  xs        = brackets $ vcat (map ppTarget xs)
  in text "switch" <+> texts scrut <> comma <+> texts dflt
        <+> ppTargets targets


ppAsm :: LMString -> LMString -> LlvmType -> [LlvmVar] -> Bool -> Bool -> SDoc
ppAsm asm constraints rty vars sideeffect alignstack =
  let asm'  = doubleQuotes $ ftext asm
      cons  = doubleQuotes $ ftext constraints
      rty'  = texts rty 
      vars' = lparen <+> ppCommaJoin vars <+> rparen
      side  = if sideeffect then text "sideeffect" else empty
      align = if alignstack then text "alignstack" else empty
  in text "call" <+> rty' <+> text "asm" <+> side <+> align <+> asm' <> comma
        <+> cons <> vars'

ppExtract :: LlvmVar -> LlvmVar -> SDoc
ppExtract vec idx =
    text "extractelement"
    <+> texts (getVarType vec) <+> text (getName vec) <> comma
    <+> texts idx

ppInsert :: LlvmVar -> LlvmVar -> LlvmVar -> SDoc
ppInsert vec elt idx =
    text "insertelement"
    <+> texts (getVarType vec) <+> text (getName vec) <> comma
    <+> texts (getVarType elt) <+> text (getName elt) <> comma
    <+> texts idx


ppMetaStatement :: [MetaData] -> LlvmStatement -> SDoc
ppMetaStatement meta stmt = ppLlvmStatement stmt <> ppMetas meta

ppMetaExpr :: [MetaData] -> LlvmExpression -> SDoc
ppMetaExpr meta expr = ppLlvmExpression expr <> ppMetas meta

ppMetas :: [MetaData] -> SDoc
ppMetas meta = hcat $ map ppMeta meta
  where
    ppMeta (name, MetaValExpr e)
        = comma <+> exclamation <> ftext name <+> text "!" <>
            braces (ppLlvmMetaExpr e)
    ppMeta (name, MetaValNode n)
        = comma <+> exclamation <> ftext name <+> exclamation <> int n


--------------------------------------------------------------------------------
-- * Misc functions
--------------------------------------------------------------------------------
ppCommaJoin :: (Show a) => [a] -> SDoc
ppCommaJoin strs = hcat $ intersperse (comma <> space) (map texts strs)

ppSpaceJoin :: (Show a) => [a] -> SDoc
ppSpaceJoin strs = hcat $ intersperse space (map texts strs)

-- | Showable to SDoc
texts :: (Show a) => a -> SDoc
texts = (text . show)

-- | Blank line.
newLine :: SDoc
newLine = text ""

-- | Exclamation point.
exclamation :: SDoc
exclamation = text "!"

