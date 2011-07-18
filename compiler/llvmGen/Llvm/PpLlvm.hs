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
    ppLlvmAlias,
    ppLlvmAliases,
    ppLlvmFunctionDecls,
    ppLlvmFunctionDecl,
    ppLlvmFunctions,
    ppLlvmFunction,

    -- * Utility functions
    llvmSDoc

    ) where

#include "HsVersions.h"

import Llvm.AbsSyn
import Llvm.Types

import Data.List ( intersperse )
import Pretty
import qualified Outputable as Out
import Unique

--------------------------------------------------------------------------------
-- * Top Level Print functions
--------------------------------------------------------------------------------

-- | Print out a whole LLVM module.
ppLlvmModule :: LlvmModule -> Doc
ppLlvmModule (LlvmModule comments aliases globals decls funcs)
  = ppLlvmComments comments
    $+$ empty
    $+$ ppLlvmAliases aliases
    $+$ empty
    $+$ ppLlvmGlobals globals
    $+$ empty
    $+$ ppLlvmFunctionDecls decls
    $+$ empty
    $+$ ppLlvmFunctions funcs

-- | Print out a multi-line comment, can be inside a function or on its own
ppLlvmComments :: [LMString] -> Doc
ppLlvmComments comments = vcat $ map ppLlvmComment comments

-- | Print out a comment, can be inside a function or on its own
ppLlvmComment :: LMString -> Doc
ppLlvmComment com = semi <+> ftext com


-- | Print out a list of global mutable variable definitions
ppLlvmGlobals :: [LMGlobal] -> Doc
ppLlvmGlobals ls = vcat $ map ppLlvmGlobal ls

-- | Print out a global mutable variable definition
ppLlvmGlobal :: LMGlobal -> Doc
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

ppLlvmGlobal oth = error $ "Non Global var ppr as global! " ++ show oth


-- | Print out a list of LLVM type aliases.
ppLlvmAliases :: [LlvmAlias] -> Doc
ppLlvmAliases tys = vcat $ map ppLlvmAlias tys

-- | Print out an LLVM type alias.
ppLlvmAlias :: LlvmAlias -> Doc
ppLlvmAlias (name, ty) = text "%" <> ftext name <+> equals <+> text "type" <+> texts ty


-- | Print out a list of function definitions.
ppLlvmFunctions :: LlvmFunctions -> Doc
ppLlvmFunctions funcs = vcat $ map ppLlvmFunction funcs

-- | Print out a function definition.
ppLlvmFunction :: LlvmFunction -> Doc
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

-- | Print out a function defenition header.
ppLlvmFunctionHeader :: LlvmFunctionDecl -> [LMString] -> Doc
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
ppLlvmFunctionDecls :: LlvmFunctionDecls -> Doc
ppLlvmFunctionDecls decs = vcat $ map ppLlvmFunctionDecl decs

-- | Print out a function declaration.
-- Declarations define the function type but don't define the actual body of
-- the function.
ppLlvmFunctionDecl :: LlvmFunctionDecl -> Doc
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
        ftext n <> lparen <> args <> varg' <> rparen <> align


-- | Print out a list of LLVM blocks.
ppLlvmBlocks :: LlvmBlocks -> Doc
ppLlvmBlocks blocks = vcat $ map ppLlvmBlock blocks

-- | Print out an LLVM block.
-- It must be part of a function definition.
ppLlvmBlock :: LlvmBlock -> Doc
ppLlvmBlock (LlvmBlock blockId stmts)
  = ppLlvmStatement (MkLabel blockId)
        $+$ nest 4 (vcat $ map ppLlvmStatement stmts)


-- | Print out an LLVM statement.
ppLlvmStatement :: LlvmStatement -> Doc
ppLlvmStatement stmt
  = case stmt of
        Assignment  dst expr      -> ppAssignment dst (ppLlvmExpression expr)
        Branch      target        -> ppBranch target
        BranchIf    cond ifT ifF  -> ppBranchIf cond ifT ifF
        Comment     comments      -> ppLlvmComments comments
        MkLabel     label         -> (llvmSDoc $ pprUnique label) <> colon
        Store       value ptr     -> ppStore value ptr
        Switch      scrut def tgs -> ppSwitch scrut def tgs
        Return      result        -> ppReturn result
        Expr        expr          -> ppLlvmExpression expr
        Unreachable               -> text "unreachable"
        Nop                       -> empty


-- | Print out an LLVM expression.
ppLlvmExpression :: LlvmExpression -> Doc
ppLlvmExpression expr
  = case expr of
        Alloca     tp amount        -> ppAlloca tp amount
        LlvmOp     op left right    -> ppMachOp op left right
        Call       tp fp args attrs -> ppCall tp fp args attrs
        Cast       op from to       -> ppCast op from to
        Compare    op left right    -> ppCmpOp op left right
        GetElemPtr inb ptr indexes  -> ppGetElementPtr inb ptr indexes
        Load       ptr              -> ppLoad ptr
        Malloc     tp amount        -> ppMalloc tp amount
        Phi        tp precessors    -> ppPhi tp precessors
        Asm        asm c ty v se sk -> ppAsm asm c ty v se sk


--------------------------------------------------------------------------------
-- * Individual print functions
--------------------------------------------------------------------------------

-- | Should always be a function pointer. So a global var of function type
-- (since globals are always pointers) or a local var of pointer function type.
ppCall :: LlvmCallType -> LlvmVar -> [LlvmVar] -> [LlvmFuncAttr] -> Doc
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


ppMachOp :: LlvmMachOp -> LlvmVar -> LlvmVar -> Doc
ppMachOp op left right =
  (texts op) <+> (texts (getVarType left)) <+> (text $ getName left)
        <> comma <+> (text $ getName right)


ppCmpOp :: LlvmCmpOp -> LlvmVar -> LlvmVar -> Doc
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


ppAssignment :: LlvmVar -> Doc -> Doc
ppAssignment var expr = (text $ getName var) <+> equals <+> expr


ppLoad :: LlvmVar -> Doc
ppLoad var = text "load" <+> texts var


ppStore :: LlvmVar -> LlvmVar -> Doc
ppStore val dst = text "store" <+> texts val <> comma <+> texts dst


ppCast :: LlvmCastOp -> LlvmVar -> LlvmType -> Doc
ppCast op from to = texts op <+> texts from <+> text "to" <+> texts to


ppMalloc :: LlvmType -> Int -> Doc
ppMalloc tp amount =
  let amount' = LMLitVar $ LMIntLit (toInteger amount) i32
  in text "malloc" <+> texts tp <> comma <+> texts amount'


ppAlloca :: LlvmType -> Int -> Doc
ppAlloca tp amount =
  let amount' = LMLitVar $ LMIntLit (toInteger amount) i32
  in text "alloca" <+> texts tp <> comma <+> texts amount'


ppGetElementPtr :: Bool -> LlvmVar -> [LlvmVar] -> Doc
ppGetElementPtr inb ptr idx =
  let indexes = comma <+> ppCommaJoin idx
      inbound = if inb then text "inbounds" else empty
  in text "getelementptr" <+> inbound <+> texts ptr <> indexes


ppReturn :: Maybe LlvmVar -> Doc
ppReturn (Just var) = text "ret" <+> texts var
ppReturn Nothing    = text "ret" <+> texts LMVoid


ppBranch :: LlvmVar -> Doc
ppBranch var = text "br" <+> texts var


ppBranchIf :: LlvmVar -> LlvmVar -> LlvmVar -> Doc
ppBranchIf cond trueT falseT
  = text "br" <+> texts cond <> comma <+> texts trueT <> comma <+> texts falseT


ppPhi :: LlvmType -> [(LlvmVar,LlvmVar)] -> Doc
ppPhi tp preds =
  let ppPreds (val, label) = brackets $ (text $ getName val) <> comma
        <+> (text $ getName label)
  in text "phi" <+> texts tp <+> hcat (intersperse comma $ map ppPreds preds)


ppSwitch :: LlvmVar -> LlvmVar -> [(LlvmVar,LlvmVar)] -> Doc
ppSwitch scrut dflt targets =
  let ppTarget  (val, lab) = texts val <> comma <+> texts lab
      ppTargets  xs        = brackets $ vcat (map ppTarget xs)
  in text "switch" <+> texts scrut <> comma <+> texts dflt
        <+> ppTargets targets


ppAsm :: LMString -> LMString -> LlvmType -> [LlvmVar] -> Bool -> Bool -> Doc
ppAsm asm constraints rty vars sideeffect alignstack =
  let asm'  = doubleQuotes $ ftext asm
      cons  = doubleQuotes $ ftext constraints
      rty'  = texts rty 
      vars' = lparen <+> ppCommaJoin vars <+> rparen
      side  = if sideeffect then text "sideeffect" else empty
      align = if alignstack then text "alignstack" else empty
  in text "call" <+> rty' <+> text "asm" <+> side <+> align <+> asm' <> comma
        <+> cons <> vars'


--------------------------------------------------------------------------------
-- * Misc functions
--------------------------------------------------------------------------------
ppCommaJoin :: (Show a) => [a] -> Doc
ppCommaJoin strs = hcat $ intersperse (comma <> space) (map texts strs)

ppSpaceJoin :: (Show a) => [a] -> Doc
ppSpaceJoin strs = hcat $ intersperse space (map texts strs)

-- | Convert SDoc to Doc
llvmSDoc :: Out.SDoc -> Doc
llvmSDoc d = Out.withPprStyleDoc (Out.mkCodeStyle Out.CStyle) d

-- | Showable to Doc
texts :: (Show a) => a -> Doc
texts = (text . show)

