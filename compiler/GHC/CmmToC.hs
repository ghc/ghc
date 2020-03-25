{-# LANGUAGE CPP, DeriveFunctor, GADTs, PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}

-----------------------------------------------------------------------------
--
-- Pretty-printing of Cmm as C, suitable for feeding gcc
--
-- (c) The University of Glasgow 2004-2006
--
-- Print Cmm as real C, for -fvia-C
--
-- See wiki:commentary/compiler/backends/ppr-c
--
-- This is simpler than the old PprAbsC, because Cmm is "macro-expanded"
-- relative to the old AbstractC, and many oddities/decorations have
-- disappeared from the data type.
--
-- This code generator is only supported in unregisterised mode.
--
-----------------------------------------------------------------------------

module GHC.CmmToC (
        writeC
  ) where

#include "HsVersions.h"

-- Cmm stuff
import GhcPrelude

import GHC.Cmm.BlockId
import GHC.Cmm.CLabel
import GHC.Types.ForeignCall
import GHC.Cmm hiding (pprBBlock)
import GHC.Cmm.Ppr () -- For Outputable instances
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Collections
import GHC.Cmm.Dataflow.Graph
import GHC.Cmm.Utils
import GHC.Cmm.Switch

-- Utils
import GHC.CmmToAsm.CPrim
import GHC.Driver.Session
import FastString
import Outputable
import GHC.Platform
import GHC.Types.Unique.Set
import GHC.Types.Unique.FM
import GHC.Types.Unique
import Util

-- The rest
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Control.Monad.ST
import Data.Bits
import Data.Char
import Data.List (intersperse)
import Data.Map (Map)
import Data.Word
import System.IO
import qualified Data.Map as Map
import Control.Monad (ap)
import qualified Data.Array.Unsafe as U ( castSTUArray )
import Data.Array.ST

-- --------------------------------------------------------------------------
-- Top level

writeC :: DynFlags -> Handle -> RawCmmGroup -> IO ()
writeC dflags handle cmm = printForC dflags handle (pprC dflags cmm $$ blankLine)

-- --------------------------------------------------------------------------
-- Now do some real work
--
-- for fun, we could call cmmToCmm over the tops...
--

pprC :: DynFlags -> RawCmmGroup -> SDoc
pprC dflags tops = vcat $ intersperse blankLine $ map (pprTop dflags) tops

--
-- top level procs
--
pprTop :: DynFlags -> RawCmmDecl -> SDoc
pprTop dflags = \case
  (CmmProc infos clbl _in_live_regs graph) ->
    (case mapLookup (g_entry graph) infos of
       Nothing -> empty
       Just (RawCmmStatics info_clbl info_dat) ->
           pprDataExterns platform info_dat $$
           pprWordArray dflags info_is_in_rodata info_clbl info_dat) $$
    (vcat [
           blankLine,
           extern_decls,
           (if (externallyVisibleCLabel clbl)
                    then mkFN_ else mkIF_) (ppr clbl) <+> lbrace,
           nest 8 temp_decls,
           vcat (map (pprBBlock dflags) blocks),
           rbrace ]
    )
    where
        -- info tables are always in .rodata
        info_is_in_rodata = True
        blocks = toBlockListEntryFirst graph
        (temp_decls, extern_decls) = pprTempAndExternDecls platform blocks


  -- Chunks of static data.

  -- We only handle (a) arrays of word-sized things and (b) strings.

  (CmmData section (RawCmmStatics lbl [CmmString str])) ->
    pprExternDecl platform lbl $$
    hcat [
      pprLocalness lbl, pprConstness (isSecConstant section), text "char ", ppr lbl,
      text "[] = ", pprStringInCStyle str, semi
    ]

  (CmmData section (RawCmmStatics lbl [CmmUninitialised size])) ->
    pprExternDecl platform lbl $$
    hcat [
      pprLocalness lbl, pprConstness (isSecConstant section), text "char ", ppr lbl,
      brackets (int size), semi
    ]

  (CmmData section (RawCmmStatics lbl lits)) ->
    pprDataExterns platform lits $$
    pprWordArray dflags (isSecConstant section) lbl lits
  where
    platform = targetPlatform dflags

-- --------------------------------------------------------------------------
-- BasicBlocks are self-contained entities: they always end in a jump.
--
-- Like nativeGen/AsmCodeGen, we could probably reorder blocks to turn
-- as many jumps as possible into fall throughs.
--

pprBBlock :: DynFlags -> CmmBlock -> SDoc
pprBBlock dflags block =
  nest 4 (pprBlockId (entryLabel block) <> colon) $$
  nest 8 (vcat (map (pprStmt dflags) (blockToList nodes)) $$ pprStmt dflags last)
 where
  (_, nodes, last)  = blockSplit block

-- --------------------------------------------------------------------------
-- Info tables. Just arrays of words.
-- See codeGen/ClosureInfo, and nativeGen/PprMach

pprWordArray :: DynFlags -> Bool -> CLabel -> [CmmStatic] -> SDoc
pprWordArray dflags is_ro lbl ds
  = -- TODO: align closures only
    pprExternDecl platform lbl $$
    hcat [ pprLocalness lbl, pprConstness is_ro, text "StgWord"
         , space, ppr lbl, text "[]"
         -- See Note [StgWord alignment]
         , pprAlignment (wordWidth platform)
         , text "= {" ]
    $$ nest 8 (commafy (pprStatics dflags ds))
    $$ text "};"
  where
    platform = targetPlatform dflags

pprAlignment :: Width -> SDoc
pprAlignment words =
     text "__attribute__((aligned(" <> int (widthInBytes words) <> text ")))"

-- Note [StgWord alignment]
-- C codegen builds static closures as StgWord C arrays (pprWordArray).
-- Their real C type is 'StgClosure'. Macros like UNTAG_CLOSURE assume
-- pointers to 'StgClosure' are aligned at pointer size boundary:
--  4 byte boundary on 32 systems
--  and 8 bytes on 64-bit systems
-- see TAG_MASK and TAG_BITS definition and usage.
--
-- It's a reasonable assumption also known as natural alignment.
-- Although some architectures have different alignment rules.
-- One of known exceptions is m68k (#11395, comment:16) where:
--   __alignof__(StgWord) == 2, sizeof(StgWord) == 4
--
-- Thus we explicitly increase alignment by using
--    __attribute__((aligned(4)))
-- declaration.

--
-- has to be static, if it isn't globally visible
--
pprLocalness :: CLabel -> SDoc
pprLocalness lbl | not $ externallyVisibleCLabel lbl = text "static "
                 | otherwise = empty

pprConstness :: Bool -> SDoc
pprConstness is_ro | is_ro = text "const "
                   | otherwise = empty

-- --------------------------------------------------------------------------
-- Statements.
--

pprStmt :: DynFlags -> CmmNode e x -> SDoc

pprStmt dflags stmt =
    case stmt of
    CmmEntry{}   -> empty
    CmmComment _ -> empty -- (hang (text "/*") 3 (ftext s)) $$ ptext (sLit "*/")
                          -- XXX if the string contains "*/", we need to fix it
                          -- XXX we probably want to emit these comments when
                          -- some debugging option is on.  They can get quite
                          -- large.

    CmmTick _ -> empty
    CmmUnwind{} -> empty

    CmmAssign dest src -> pprAssign dflags dest src

    CmmStore  dest src
        | typeWidth rep == W64 && wordWidth platform /= W64
        -> (if isFloatType rep then text "ASSIGN_DBL"
                               else ptext (sLit ("ASSIGN_Word64"))) <>
           parens (mkP_ <> pprExpr1 dflags dest <> comma <> pprExpr dflags src) <> semi

        | otherwise
        -> hsep [ pprExpr dflags (CmmLoad dest rep), equals, pprExpr dflags src <> semi ]
        where
          rep = cmmExprType platform src
          platform = targetPlatform dflags

    CmmUnsafeForeignCall target@(ForeignTarget fn conv) results args ->
        fnCall
        where
        (res_hints, arg_hints) = foreignTargetHints target
        hresults = zip results res_hints
        hargs    = zip args arg_hints
        platform = targetPlatform dflags

        ForeignConvention cconv _ _ ret = conv

        cast_fn = parens (cCast dflags (pprCFunType platform (char '*') cconv hresults hargs) fn)

        -- See wiki:commentary/compiler/backends/ppr-c#prototypes
        fnCall =
            case fn of
              CmmLit (CmmLabel lbl)
                | StdCallConv <- cconv ->
                    pprCall dflags (ppr lbl) cconv hresults hargs
                        -- stdcall functions must be declared with
                        -- a function type, otherwise the C compiler
                        -- doesn't add the @n suffix to the label.  We
                        -- can't add the @n suffix ourselves, because
                        -- it isn't valid C.
                | CmmNeverReturns <- ret ->
                    pprCall dflags cast_fn cconv hresults hargs <> semi
                | not (isMathFun lbl) ->
                    pprForeignCall dflags (ppr lbl) cconv hresults hargs
              _ ->
                    pprCall dflags cast_fn cconv hresults hargs <> semi
                        -- for a dynamic call, no declaration is necessary.

    CmmUnsafeForeignCall (PrimTarget MO_Touch) _results _args -> empty
    CmmUnsafeForeignCall (PrimTarget (MO_Prefetch_Data _)) _results _args -> empty

    CmmUnsafeForeignCall target@(PrimTarget op) results args ->
        fn_call
      where
        cconv = CCallConv
        fn = pprCallishMachOp_for_C op

        (res_hints, arg_hints) = foreignTargetHints target
        hresults = zip results res_hints
        hargs    = zip args arg_hints

        fn_call
          -- The mem primops carry an extra alignment arg.
          -- We could maybe emit an alignment directive using this info.
          -- We also need to cast mem primops to prevent conflicts with GCC
          -- builtins (see bug #5967).
          | Just _align <- machOpMemcpyishAlign op
          = (text ";EFF_(" <> fn <> char ')' <> semi) $$
            pprForeignCall dflags fn cconv hresults hargs
          | otherwise
          = pprCall dflags fn cconv hresults hargs

    CmmBranch ident               -> pprBranch ident
    CmmCondBranch expr yes no _   -> pprCondBranch dflags expr yes no
    CmmCall { cml_target = expr } -> mkJMP_ (pprExpr dflags expr) <> semi
    CmmSwitch arg ids             -> pprSwitch dflags arg ids

    _other -> pprPanic "PprC.pprStmt" (ppr stmt)

type Hinted a = (a, ForeignHint)

pprForeignCall :: DynFlags -> SDoc -> CCallConv -> [Hinted CmmFormal] -> [Hinted CmmActual]
               -> SDoc
pprForeignCall dflags fn cconv results args = fn_call
  where
    platform = targetPlatform dflags
    fn_call = braces (
                 pprCFunType platform (char '*' <> text "ghcFunPtr") cconv results args <> semi
              $$ text "ghcFunPtr" <+> equals <+> cast_fn <> semi
              $$ pprCall dflags (text "ghcFunPtr") cconv results args <> semi
             )
    cast_fn = parens (parens (pprCFunType platform (char '*') cconv results args) <> fn)

pprCFunType :: Platform -> SDoc -> CCallConv -> [Hinted CmmFormal] -> [Hinted CmmActual] -> SDoc
pprCFunType platform ppr_fn cconv ress args
  = let res_type [] = text "void"
        res_type [(one, hint)] = machRepHintCType platform (localRegType one) hint
        res_type _ = panic "pprCFunType: only void or 1 return value supported"

        arg_type (expr, hint) = machRepHintCType platform (cmmExprType platform expr) hint
    in res_type ress <+>
       parens (ccallConvAttribute cconv <> ppr_fn) <>
       parens (commafy (map arg_type args))

-- ---------------------------------------------------------------------
-- unconditional branches
pprBranch :: BlockId -> SDoc
pprBranch ident = text "goto" <+> pprBlockId ident <> semi


-- ---------------------------------------------------------------------
-- conditional branches to local labels
pprCondBranch :: DynFlags -> CmmExpr -> BlockId -> BlockId -> SDoc
pprCondBranch dflags expr yes no
        = hsep [ text "if" , parens(pprExpr dflags expr) ,
                        text "goto", pprBlockId yes <> semi,
                        text "else goto", pprBlockId no <> semi ]

-- ---------------------------------------------------------------------
-- a local table branch
--
-- we find the fall-through cases
--
pprSwitch :: DynFlags -> CmmExpr -> SwitchTargets -> SDoc
pprSwitch dflags e ids
  = (hang (text "switch" <+> parens ( pprExpr dflags e ) <+> lbrace)
                4 (vcat ( map caseify pairs ) $$ def)) $$ rbrace
  where
    (pairs, mbdef) = switchTargetsFallThrough ids
    platform = targetPlatform dflags

    -- fall through case
    caseify (ix:ixs, ident) = vcat (map do_fallthrough ixs) $$ final_branch ix
        where
        do_fallthrough ix =
                 hsep [ text "case" , pprHexVal dflags ix (wordWidth platform) <> colon ,
                        text "/* fall through */" ]

        final_branch ix =
                hsep [ text "case" , pprHexVal dflags ix (wordWidth platform) <> colon ,
                       text "goto" , (pprBlockId ident) <> semi ]

    caseify (_     , _    ) = panic "pprSwitch: switch with no cases!"

    def | Just l <- mbdef = text "default: goto" <+> pprBlockId l <> semi
        | otherwise       = empty

-- ---------------------------------------------------------------------
-- Expressions.
--

-- C Types: the invariant is that the C expression generated by
--
--      pprExpr e
--
-- has a type in C which is also given by
--
--      machRepCType (cmmExprType e)
--
-- (similar invariants apply to the rest of the pretty printer).

pprExpr :: DynFlags -> CmmExpr -> SDoc
pprExpr dflags e = case e of
    CmmLit lit      -> pprLit dflags lit
    CmmLoad e ty    -> pprLoad dflags e ty
    CmmReg reg      -> pprCastReg reg
    CmmRegOff reg 0 -> pprCastReg reg

    -- CmmRegOff is an alias of MO_Add
    CmmRegOff reg i -> pprCastReg reg <> char '+' <>
                       pprHexVal dflags (fromIntegral i) (wordWidth platform)

    CmmMachOp mop args -> pprMachOpApp dflags mop args

    CmmStackSlot _ _   -> panic "pprExpr: CmmStackSlot not supported!"
  where
    platform = targetPlatform dflags


pprLoad :: DynFlags -> CmmExpr -> CmmType -> SDoc
pprLoad dflags e ty
  | width == W64, wordWidth platform /= W64
  = (if isFloatType ty then text "PK_DBL"
                       else text "PK_Word64")
    <> parens (mkP_ <> pprExpr1 dflags e)

  | otherwise
  = case e of
        CmmReg r | isPtrReg r && width == wordWidth platform && not (isFloatType ty)
                 -> char '*' <> pprAsPtrReg r

        CmmRegOff r 0 | isPtrReg r && width == wordWidth platform && not (isFloatType ty)
                      -> char '*' <> pprAsPtrReg r

        CmmRegOff r off | isPtrReg r && width == wordWidth platform
                        , off `rem` platformWordSizeInBytes platform == 0 && not (isFloatType ty)
        -- ToDo: check that the offset is a word multiple?
        --       (For tagging to work, I had to avoid unaligned loads. --ARY)
                        -> pprAsPtrReg r <> brackets (ppr (off `shiftR` wordShift platform))

        _other -> cLoad dflags e ty
  where
    width = typeWidth ty
    platform = targetPlatform dflags

pprExpr1 :: DynFlags -> CmmExpr -> SDoc
pprExpr1 dflags e = case e of
   CmmLit lit  -> pprLit1 dflags lit
   CmmReg _reg -> pprExpr dflags e
   _           -> parens (pprExpr dflags e)

-- --------------------------------------------------------------------------
-- MachOp applications

pprMachOpApp :: DynFlags -> MachOp -> [CmmExpr] -> SDoc

pprMachOpApp dflags op args
  | isMulMayOfloOp op
  = text "mulIntMayOflo" <> parens (commafy (map (pprExpr dflags) args))
  where isMulMayOfloOp (MO_U_MulMayOflo _) = True
        isMulMayOfloOp (MO_S_MulMayOflo _) = True
        isMulMayOfloOp _ = False

pprMachOpApp dflags mop args
  | Just ty <- machOpNeedsCast mop
  = ty <> parens (pprMachOpApp' dflags mop args)
  | otherwise
  = pprMachOpApp' dflags mop args

-- Comparisons in C have type 'int', but we want type W_ (this is what
-- resultRepOfMachOp says).  The other C operations inherit their type
-- from their operands, so no casting is required.
machOpNeedsCast :: MachOp -> Maybe SDoc
machOpNeedsCast mop
  | isComparisonMachOp mop = Just mkW_
  | otherwise              = Nothing

pprMachOpApp' :: DynFlags -> MachOp -> [CmmExpr] -> SDoc
pprMachOpApp' dflags mop args
 = case args of
    -- dyadic
    [x,y] -> pprArg x <+> pprMachOp_for_C platform mop <+> pprArg y

    -- unary
    [x]   -> pprMachOp_for_C platform mop <> parens (pprArg x)

    _     -> panic "PprC.pprMachOp : machop with wrong number of args"

  where
    platform = targetPlatform dflags
        -- Cast needed for signed integer ops
    pprArg e | signedOp    mop = cCast dflags (machRep_S_CType platform (typeWidth (cmmExprType platform e))) e
             | needsFCasts mop = cCast dflags (machRep_F_CType (typeWidth (cmmExprType platform e))) e
             | otherwise       = pprExpr1 dflags e
    needsFCasts (MO_F_Eq _)   = False
    needsFCasts (MO_F_Ne _)   = False
    needsFCasts (MO_F_Neg _)  = True
    needsFCasts (MO_F_Quot _) = True
    needsFCasts mop  = floatComparison mop

-- --------------------------------------------------------------------------
-- Literals

pprLit :: DynFlags -> CmmLit -> SDoc
pprLit dflags lit = case lit of
    CmmInt i rep      -> pprHexVal dflags i rep

    CmmFloat f w       -> parens (machRep_F_CType w) <> str
        where d = fromRational f :: Double
              str | isInfinite d && d < 0 = text "-INFINITY"
                  | isInfinite d          = text "INFINITY"
                  | isNaN d               = text "NAN"
                  | otherwise             = text (show d)
                -- these constants come from <math.h>
                -- see #1861

    CmmVec {} -> panic "PprC printing vector literal"

    CmmBlock bid       -> mkW_ <> pprCLabelAddr (infoTblLbl bid)
    CmmHighStackMark   -> panic "PprC printing high stack mark"
    CmmLabel clbl      -> mkW_ <> pprCLabelAddr clbl
    CmmLabelOff clbl i -> mkW_ <> pprCLabelAddr clbl <> char '+' <> int i
    CmmLabelDiffOff clbl1 _ i _   -- non-word widths not supported via C
        -- WARNING:
        --  * the lit must occur in the info table clbl2
        --  * clbl1 must be an SRT, a slow entry point or a large bitmap
        -> mkW_ <> pprCLabelAddr clbl1 <> char '+' <> int i

    where
        pprCLabelAddr lbl = char '&' <> ppr lbl

pprLit1 :: DynFlags -> CmmLit -> SDoc
pprLit1 dflags lit = case lit of
   (CmmLabelOff _ _)         -> parens (pprLit dflags lit)
   (CmmLabelDiffOff _ _ _ _) -> parens (pprLit dflags lit)
   (CmmFloat _ _)            -> parens (pprLit dflags lit)
   _                         -> pprLit dflags lit

-- ---------------------------------------------------------------------------
-- Static data

pprStatics :: DynFlags -> [CmmStatic] -> [SDoc]
pprStatics dflags = pprStatics'
  where
    platform    = targetPlatform dflags
    pprStatics' = \case
      [] -> []
      (CmmStaticLit (CmmFloat f W32) : rest)
        -- odd numbers of floats are padded to a word by mkVirtHeapOffsetsWithPadding
        | wordWidth platform == W64, CmmStaticLit (CmmInt 0 W32) : rest' <- rest
        -> pprLit1 dflags (floatToWord platform f) : pprStatics' rest'
        -- adjacent floats aren't padded but combined into a single word
        | wordWidth platform == W64, CmmStaticLit (CmmFloat g W32) : rest' <- rest
        -> pprLit1 dflags (floatPairToWord platform f g) : pprStatics' rest'
        | wordWidth platform == W32
        -> pprLit1 dflags (floatToWord platform f) : pprStatics' rest
        | otherwise
        -> pprPanic "pprStatics: float" (vcat (map ppr' rest))
          where ppr' (CmmStaticLit l) = ppr (cmmLitType platform l)
                ppr' _other           = text "bad static!"

      (CmmStaticLit (CmmFloat f W64) : rest)
        -> map (pprLit1 dflags) (doubleToWords platform f) ++ pprStatics' rest

      (CmmStaticLit (CmmInt i W64) : rest)
        | wordWidth platform == W32
        -> case platformByteOrder platform of
            BigEndian    -> pprStatics' (CmmStaticLit (CmmInt q W32) :
                                         CmmStaticLit (CmmInt r W32) : rest)
            LittleEndian -> pprStatics' (CmmStaticLit (CmmInt r W32) :
                                         CmmStaticLit (CmmInt q W32) : rest)
           where r = i .&. 0xffffffff
                 q = i `shiftR` 32

      (CmmStaticLit (CmmInt a W32) : CmmStaticLit (CmmInt b W32) : rest)
        | wordWidth platform == W64
        -> case platformByteOrder platform of
             BigEndian    -> pprStatics' (CmmStaticLit (CmmInt ((shiftL a 32) .|. b) W64) : rest)
             LittleEndian -> pprStatics' (CmmStaticLit (CmmInt ((shiftL b 32) .|. a) W64) : rest)

      (CmmStaticLit (CmmInt a W16) : CmmStaticLit (CmmInt b W16) : rest)
        | wordWidth platform == W32
        -> case platformByteOrder platform of
             BigEndian    -> pprStatics' (CmmStaticLit (CmmInt ((shiftL a 16) .|. b) W32) : rest)
             LittleEndian -> pprStatics' (CmmStaticLit (CmmInt ((shiftL b 16) .|. a) W32) : rest)

      (CmmStaticLit (CmmInt _ w) : _)
        | w /= wordWidth platform
        -> pprPanic "pprStatics: cannot emit a non-word-sized static literal" (ppr w)

      (CmmStaticLit lit : rest)
        -> pprLit1 dflags lit : pprStatics' rest

      (other : _)
        -> pprPanic "pprStatics: other" (pprStatic dflags other)

pprStatic :: DynFlags -> CmmStatic -> SDoc
pprStatic dflags s = case s of

    CmmStaticLit lit   -> nest 4 (pprLit dflags lit)
    CmmUninitialised i -> nest 4 (mkC_ <> brackets (int i))

    -- these should be inlined, like the old .hc
    CmmString s'       -> nest 4 (mkW_ <> parens(pprStringInCStyle s'))
    CmmFileEmbed {}    -> panic "Unexpected CmmFileEmbed literal"


-- ---------------------------------------------------------------------------
-- Block Ids

pprBlockId :: BlockId -> SDoc
pprBlockId b = char '_' <> ppr (getUnique b)

-- --------------------------------------------------------------------------
-- Print a MachOp in a way suitable for emitting via C.
--

pprMachOp_for_C :: Platform -> MachOp -> SDoc

pprMachOp_for_C platform mop = case mop of

        -- Integer operations
        MO_Add          _ -> char '+'
        MO_Sub          _ -> char '-'
        MO_Eq           _ -> text "=="
        MO_Ne           _ -> text "!="
        MO_Mul          _ -> char '*'

        MO_S_Quot       _ -> char '/'
        MO_S_Rem        _ -> char '%'
        MO_S_Neg        _ -> char '-'

        MO_U_Quot       _ -> char '/'
        MO_U_Rem        _ -> char '%'

        -- & Floating-point operations
        MO_F_Add        _ -> char '+'
        MO_F_Sub        _ -> char '-'
        MO_F_Neg        _ -> char '-'
        MO_F_Mul        _ -> char '*'
        MO_F_Quot       _ -> char '/'

        -- Signed comparisons
        MO_S_Ge         _ -> text ">="
        MO_S_Le         _ -> text "<="
        MO_S_Gt         _ -> char '>'
        MO_S_Lt         _ -> char '<'

        -- & Unsigned comparisons
        MO_U_Ge         _ -> text ">="
        MO_U_Le         _ -> text "<="
        MO_U_Gt         _ -> char '>'
        MO_U_Lt         _ -> char '<'

        -- & Floating-point comparisons
        MO_F_Eq         _ -> text "=="
        MO_F_Ne         _ -> text "!="
        MO_F_Ge         _ -> text ">="
        MO_F_Le         _ -> text "<="
        MO_F_Gt         _ -> char '>'
        MO_F_Lt         _ -> char '<'

        -- Bitwise operations.  Not all of these may be supported at all
        -- sizes, and only integral MachReps are valid.
        MO_And          _ -> char '&'
        MO_Or           _ -> char '|'
        MO_Xor          _ -> char '^'
        MO_Not          _ -> char '~'
        MO_Shl          _ -> text "<<"
        MO_U_Shr        _ -> text ">>" -- unsigned shift right
        MO_S_Shr        _ -> text ">>" -- signed shift right

-- Conversions.  Some of these will be NOPs, but never those that convert
-- between ints and floats.
-- Floating-point conversions use the signed variant.
-- We won't know to generate (void*) casts here, but maybe from
-- context elsewhere

-- noop casts
        MO_UU_Conv from to | from == to -> empty
        MO_UU_Conv _from to -> parens (machRep_U_CType platform to)

        MO_SS_Conv from to | from == to -> empty
        MO_SS_Conv _from to -> parens (machRep_S_CType platform to)

        MO_XX_Conv from to | from == to -> empty
        MO_XX_Conv _from to -> parens (machRep_U_CType platform to)

        MO_FF_Conv from to | from == to -> empty
        MO_FF_Conv _from to -> parens (machRep_F_CType to)

        MO_SF_Conv _from to -> parens (machRep_F_CType to)
        MO_FS_Conv _from to -> parens (machRep_S_CType platform to)

        MO_S_MulMayOflo _ -> pprTrace "offending mop:"
                                (text "MO_S_MulMayOflo")
                                (panic $ "PprC.pprMachOp_for_C: MO_S_MulMayOflo"
                                      ++ " should have been handled earlier!")
        MO_U_MulMayOflo _ -> pprTrace "offending mop:"
                                (text "MO_U_MulMayOflo")
                                (panic $ "PprC.pprMachOp_for_C: MO_U_MulMayOflo"
                                      ++ " should have been handled earlier!")

        MO_V_Insert {}    -> pprTrace "offending mop:"
                                (text "MO_V_Insert")
                                (panic $ "PprC.pprMachOp_for_C: MO_V_Insert"
                                      ++ " should have been handled earlier!")
        MO_V_Extract {}   -> pprTrace "offending mop:"
                                (text "MO_V_Extract")
                                (panic $ "PprC.pprMachOp_for_C: MO_V_Extract"
                                      ++ " should have been handled earlier!")

        MO_V_Add {}       -> pprTrace "offending mop:"
                                (text "MO_V_Add")
                                (panic $ "PprC.pprMachOp_for_C: MO_V_Add"
                                      ++ " should have been handled earlier!")
        MO_V_Sub {}       -> pprTrace "offending mop:"
                                (text "MO_V_Sub")
                                (panic $ "PprC.pprMachOp_for_C: MO_V_Sub"
                                      ++ " should have been handled earlier!")
        MO_V_Mul {}       -> pprTrace "offending mop:"
                                (text "MO_V_Mul")
                                (panic $ "PprC.pprMachOp_for_C: MO_V_Mul"
                                      ++ " should have been handled earlier!")

        MO_VS_Quot {}     -> pprTrace "offending mop:"
                                (text "MO_VS_Quot")
                                (panic $ "PprC.pprMachOp_for_C: MO_VS_Quot"
                                      ++ " should have been handled earlier!")
        MO_VS_Rem {}      -> pprTrace "offending mop:"
                                (text "MO_VS_Rem")
                                (panic $ "PprC.pprMachOp_for_C: MO_VS_Rem"
                                      ++ " should have been handled earlier!")
        MO_VS_Neg {}      -> pprTrace "offending mop:"
                                (text "MO_VS_Neg")
                                (panic $ "PprC.pprMachOp_for_C: MO_VS_Neg"
                                      ++ " should have been handled earlier!")

        MO_VU_Quot {}     -> pprTrace "offending mop:"
                                (text "MO_VU_Quot")
                                (panic $ "PprC.pprMachOp_for_C: MO_VU_Quot"
                                      ++ " should have been handled earlier!")
        MO_VU_Rem {}      -> pprTrace "offending mop:"
                                (text "MO_VU_Rem")
                                (panic $ "PprC.pprMachOp_for_C: MO_VU_Rem"
                                      ++ " should have been handled earlier!")

        MO_VF_Insert {}   -> pprTrace "offending mop:"
                                (text "MO_VF_Insert")
                                (panic $ "PprC.pprMachOp_for_C: MO_VF_Insert"
                                      ++ " should have been handled earlier!")
        MO_VF_Extract {}  -> pprTrace "offending mop:"
                                (text "MO_VF_Extract")
                                (panic $ "PprC.pprMachOp_for_C: MO_VF_Extract"
                                      ++ " should have been handled earlier!")

        MO_VF_Add {}      -> pprTrace "offending mop:"
                                (text "MO_VF_Add")
                                (panic $ "PprC.pprMachOp_for_C: MO_VF_Add"
                                      ++ " should have been handled earlier!")
        MO_VF_Sub {}      -> pprTrace "offending mop:"
                                (text "MO_VF_Sub")
                                (panic $ "PprC.pprMachOp_for_C: MO_VF_Sub"
                                      ++ " should have been handled earlier!")
        MO_VF_Neg {}      -> pprTrace "offending mop:"
                                (text "MO_VF_Neg")
                                (panic $ "PprC.pprMachOp_for_C: MO_VF_Neg"
                                      ++ " should have been handled earlier!")
        MO_VF_Mul {}      -> pprTrace "offending mop:"
                                (text "MO_VF_Mul")
                                (panic $ "PprC.pprMachOp_for_C: MO_VF_Mul"
                                      ++ " should have been handled earlier!")
        MO_VF_Quot {}     -> pprTrace "offending mop:"
                                (text "MO_VF_Quot")
                                (panic $ "PprC.pprMachOp_for_C: MO_VF_Quot"
                                      ++ " should have been handled earlier!")

        MO_AlignmentCheck {} -> panic "-falignment-santisation not supported by unregisterised backend"

signedOp :: MachOp -> Bool      -- Argument type(s) are signed ints
signedOp (MO_S_Quot _)    = True
signedOp (MO_S_Rem  _)    = True
signedOp (MO_S_Neg  _)    = True
signedOp (MO_S_Ge   _)    = True
signedOp (MO_S_Le   _)    = True
signedOp (MO_S_Gt   _)    = True
signedOp (MO_S_Lt   _)    = True
signedOp (MO_S_Shr  _)    = True
signedOp (MO_SS_Conv _ _) = True
signedOp (MO_SF_Conv _ _) = True
signedOp _                = False

floatComparison :: MachOp -> Bool  -- comparison between float args
floatComparison (MO_F_Eq   _) = True
floatComparison (MO_F_Ne   _) = True
floatComparison (MO_F_Ge   _) = True
floatComparison (MO_F_Le   _) = True
floatComparison (MO_F_Gt   _) = True
floatComparison (MO_F_Lt   _) = True
floatComparison _             = False

-- ---------------------------------------------------------------------
-- tend to be implemented by foreign calls

pprCallishMachOp_for_C :: CallishMachOp -> SDoc

pprCallishMachOp_for_C mop
    = case mop of
        MO_F64_Pwr      -> text "pow"
        MO_F64_Sin      -> text "sin"
        MO_F64_Cos      -> text "cos"
        MO_F64_Tan      -> text "tan"
        MO_F64_Sinh     -> text "sinh"
        MO_F64_Cosh     -> text "cosh"
        MO_F64_Tanh     -> text "tanh"
        MO_F64_Asin     -> text "asin"
        MO_F64_Acos     -> text "acos"
        MO_F64_Atanh    -> text "atanh"
        MO_F64_Asinh    -> text "asinh"
        MO_F64_Acosh    -> text "acosh"
        MO_F64_Atan     -> text "atan"
        MO_F64_Log      -> text "log"
        MO_F64_Log1P    -> text "log1p"
        MO_F64_Exp      -> text "exp"
        MO_F64_ExpM1    -> text "expm1"
        MO_F64_Sqrt     -> text "sqrt"
        MO_F64_Fabs     -> text "fabs"
        MO_F32_Pwr      -> text "powf"
        MO_F32_Sin      -> text "sinf"
        MO_F32_Cos      -> text "cosf"
        MO_F32_Tan      -> text "tanf"
        MO_F32_Sinh     -> text "sinhf"
        MO_F32_Cosh     -> text "coshf"
        MO_F32_Tanh     -> text "tanhf"
        MO_F32_Asin     -> text "asinf"
        MO_F32_Acos     -> text "acosf"
        MO_F32_Atan     -> text "atanf"
        MO_F32_Asinh    -> text "asinhf"
        MO_F32_Acosh    -> text "acoshf"
        MO_F32_Atanh    -> text "atanhf"
        MO_F32_Log      -> text "logf"
        MO_F32_Log1P    -> text "log1pf"
        MO_F32_Exp      -> text "expf"
        MO_F32_ExpM1    -> text "expm1f"
        MO_F32_Sqrt     -> text "sqrtf"
        MO_F32_Fabs     -> text "fabsf"
        MO_ReadBarrier  -> text "load_load_barrier"
        MO_WriteBarrier -> text "write_barrier"
        MO_Memcpy _     -> text "memcpy"
        MO_Memset _     -> text "memset"
        MO_Memmove _    -> text "memmove"
        MO_Memcmp _     -> text "memcmp"
        (MO_BSwap w)    -> ptext (sLit $ bSwapLabel w)
        (MO_BRev w)     -> ptext (sLit $ bRevLabel w)
        (MO_PopCnt w)   -> ptext (sLit $ popCntLabel w)
        (MO_Pext w)     -> ptext (sLit $ pextLabel w)
        (MO_Pdep w)     -> ptext (sLit $ pdepLabel w)
        (MO_Clz w)      -> ptext (sLit $ clzLabel w)
        (MO_Ctz w)      -> ptext (sLit $ ctzLabel w)
        (MO_AtomicRMW w amop) -> ptext (sLit $ atomicRMWLabel w amop)
        (MO_Cmpxchg w)  -> ptext (sLit $ cmpxchgLabel w)
        (MO_AtomicRead w)  -> ptext (sLit $ atomicReadLabel w)
        (MO_AtomicWrite w) -> ptext (sLit $ atomicWriteLabel w)
        (MO_UF_Conv w)  -> ptext (sLit $ word2FloatLabel w)

        MO_S_Mul2     {} -> unsupported
        MO_S_QuotRem  {} -> unsupported
        MO_U_QuotRem  {} -> unsupported
        MO_U_QuotRem2 {} -> unsupported
        MO_Add2       {} -> unsupported
        MO_AddWordC   {} -> unsupported
        MO_SubWordC   {} -> unsupported
        MO_AddIntC    {} -> unsupported
        MO_SubIntC    {} -> unsupported
        MO_U_Mul2     {} -> unsupported
        MO_Touch         -> unsupported
        (MO_Prefetch_Data _ ) -> unsupported
        --- we could support prefetch via "__builtin_prefetch"
        --- Not adding it for now
    where unsupported = panic ("pprCallishMachOp_for_C: " ++ show mop
                            ++ " not supported!")

-- ---------------------------------------------------------------------
-- Useful #defines
--

mkJMP_, mkFN_, mkIF_ :: SDoc -> SDoc

mkJMP_ i = text "JMP_" <> parens i
mkFN_  i = text "FN_"  <> parens i -- externally visible function
mkIF_  i = text "IF_"  <> parens i -- locally visible

-- from includes/Stg.h
--
mkC_,mkW_,mkP_ :: SDoc

mkC_  = text "(C_)"        -- StgChar
mkW_  = text "(W_)"        -- StgWord
mkP_  = text "(P_)"        -- StgWord*

-- ---------------------------------------------------------------------
--
-- Assignments
--
-- Generating assignments is what we're all about, here
--
pprAssign :: DynFlags -> CmmReg -> CmmExpr -> SDoc

-- dest is a reg, rhs is a reg
pprAssign _ r1 (CmmReg r2)
   | isPtrReg r1 && isPtrReg r2
   = hcat [ pprAsPtrReg r1, equals, pprAsPtrReg r2, semi ]

-- dest is a reg, rhs is a CmmRegOff
pprAssign dflags r1 (CmmRegOff r2 off)
   | isPtrReg r1 && isPtrReg r2 && (off `rem` platformWordSizeInBytes platform == 0)
   = hcat [ pprAsPtrReg r1, equals, pprAsPtrReg r2, op, int off', semi ]
  where
        platform = targetPlatform dflags
        off1 = off `shiftR` wordShift platform

        (op,off') | off >= 0  = (char '+', off1)
                  | otherwise = (char '-', -off1)

-- dest is a reg, rhs is anything.
-- We can't cast the lvalue, so we have to cast the rhs if necessary.  Casting
-- the lvalue elicits a warning from new GCC versions (3.4+).
pprAssign dflags r1 r2
  | isFixedPtrReg r1             = mkAssign (mkP_ <> pprExpr1 dflags r2)
  | Just ty <- strangeRegType r1 = mkAssign (parens ty <> pprExpr1 dflags r2)
  | otherwise                    = mkAssign (pprExpr dflags r2)
    where mkAssign x = if r1 == CmmGlobal BaseReg
                       then text "ASSIGN_BaseReg" <> parens x <> semi
                       else pprReg r1 <> text " = " <> x <> semi

-- ---------------------------------------------------------------------
-- Registers

pprCastReg :: CmmReg -> SDoc
pprCastReg reg
   | isStrangeTypeReg reg = mkW_ <> pprReg reg
   | otherwise            = pprReg reg

-- True if (pprReg reg) will give an expression with type StgPtr.  We
-- need to take care with pointer arithmetic on registers with type
-- StgPtr.
isFixedPtrReg :: CmmReg -> Bool
isFixedPtrReg (CmmLocal _) = False
isFixedPtrReg (CmmGlobal r) = isFixedPtrGlobalReg r

-- True if (pprAsPtrReg reg) will give an expression with type StgPtr
-- JD: THIS IS HORRIBLE AND SHOULD BE RENAMED, AT THE VERY LEAST.
-- THE GARBAGE WITH THE VNonGcPtr HELPS MATCH THE OLD CODE GENERATOR'S OUTPUT;
-- I'M NOT SURE IF IT SHOULD REALLY STAY THAT WAY.
isPtrReg :: CmmReg -> Bool
isPtrReg (CmmLocal _)                         = False
isPtrReg (CmmGlobal (VanillaReg _ VGcPtr))    = True  -- if we print via pprAsPtrReg
isPtrReg (CmmGlobal (VanillaReg _ VNonGcPtr)) = False -- if we print via pprAsPtrReg
isPtrReg (CmmGlobal reg)                      = isFixedPtrGlobalReg reg

-- True if this global reg has type StgPtr
isFixedPtrGlobalReg :: GlobalReg -> Bool
isFixedPtrGlobalReg Sp    = True
isFixedPtrGlobalReg Hp    = True
isFixedPtrGlobalReg HpLim = True
isFixedPtrGlobalReg SpLim = True
isFixedPtrGlobalReg _     = False

-- True if in C this register doesn't have the type given by
-- (machRepCType (cmmRegType reg)), so it has to be cast.
isStrangeTypeReg :: CmmReg -> Bool
isStrangeTypeReg (CmmLocal _)   = False
isStrangeTypeReg (CmmGlobal g)  = isStrangeTypeGlobal g

isStrangeTypeGlobal :: GlobalReg -> Bool
isStrangeTypeGlobal CCCS                = True
isStrangeTypeGlobal CurrentTSO          = True
isStrangeTypeGlobal CurrentNursery      = True
isStrangeTypeGlobal BaseReg             = True
isStrangeTypeGlobal r                   = isFixedPtrGlobalReg r

strangeRegType :: CmmReg -> Maybe SDoc
strangeRegType (CmmGlobal CCCS) = Just (text "struct CostCentreStack_ *")
strangeRegType (CmmGlobal CurrentTSO) = Just (text "struct StgTSO_ *")
strangeRegType (CmmGlobal CurrentNursery) = Just (text "struct bdescr_ *")
strangeRegType (CmmGlobal BaseReg) = Just (text "struct StgRegTable_ *")
strangeRegType _ = Nothing

-- pprReg just prints the register name.
--
pprReg :: CmmReg -> SDoc
pprReg r = case r of
        CmmLocal  local  -> pprLocalReg local
        CmmGlobal global -> pprGlobalReg global

pprAsPtrReg :: CmmReg -> SDoc
pprAsPtrReg (CmmGlobal (VanillaReg n gcp))
  = WARN( gcp /= VGcPtr, ppr n ) char 'R' <> int n <> text ".p"
pprAsPtrReg other_reg = pprReg other_reg

pprGlobalReg :: GlobalReg -> SDoc
pprGlobalReg gr = case gr of
    VanillaReg n _ -> char 'R' <> int n  <> text ".w"
        -- pprGlobalReg prints a VanillaReg as a .w regardless
        -- Example:     R1.w = R1.w & (-0x8UL);
        --              JMP_(*R1.p);
    FloatReg   n   -> char 'F' <> int n
    DoubleReg  n   -> char 'D' <> int n
    LongReg    n   -> char 'L' <> int n
    Sp             -> text "Sp"
    SpLim          -> text "SpLim"
    Hp             -> text "Hp"
    HpLim          -> text "HpLim"
    CCCS           -> text "CCCS"
    CurrentTSO     -> text "CurrentTSO"
    CurrentNursery -> text "CurrentNursery"
    HpAlloc        -> text "HpAlloc"
    BaseReg        -> text "BaseReg"
    EagerBlackholeInfo -> text "stg_EAGER_BLACKHOLE_info"
    GCEnter1       -> text "stg_gc_enter_1"
    GCFun          -> text "stg_gc_fun"
    other          -> panic $ "pprGlobalReg: Unsupported register: " ++ show other

pprLocalReg :: LocalReg -> SDoc
pprLocalReg (LocalReg uniq _) = char '_' <> ppr uniq

-- -----------------------------------------------------------------------------
-- Foreign Calls

pprCall :: DynFlags -> SDoc -> CCallConv -> [Hinted CmmFormal] -> [Hinted CmmActual] -> SDoc
pprCall dflags ppr_fn cconv results args
  | not (is_cishCC cconv)
  = panic $ "pprCall: unknown calling convention"

  | otherwise
  =
    ppr_assign results (ppr_fn <> parens (commafy (map pprArg args))) <> semi
  where
     platform = targetPlatform dflags

     ppr_assign []           rhs = rhs
     ppr_assign [(one,hint)] rhs
         = pprLocalReg one <> text " = "
                 <> pprUnHint hint (localRegType one) <> rhs
     ppr_assign _other _rhs = panic "pprCall: multiple results"

     pprArg (expr, AddrHint)
        = cCast dflags (text "void *") expr
        -- see comment by machRepHintCType below
     pprArg (expr, SignedHint)
        = cCast dflags (machRep_S_CType platform $ typeWidth $ cmmExprType platform expr) expr
     pprArg (expr, _other)
        = pprExpr dflags expr

     pprUnHint AddrHint   rep = parens (machRepCType platform rep)
     pprUnHint SignedHint rep = parens (machRepCType platform rep)
     pprUnHint _          _   = empty

-- Currently we only have these two calling conventions, but this might
-- change in the future...
is_cishCC :: CCallConv -> Bool
is_cishCC CCallConv    = True
is_cishCC CApiConv     = True
is_cishCC StdCallConv  = True
is_cishCC PrimCallConv = False
is_cishCC JavaScriptCallConv = False

-- ---------------------------------------------------------------------
-- Find and print local and external declarations for a list of
-- Cmm statements.
--
pprTempAndExternDecls :: Platform -> [CmmBlock] -> (SDoc{-temps-}, SDoc{-externs-})
pprTempAndExternDecls platform stmts
  = (pprUFM (getUniqSet temps) (vcat . map (pprTempDecl platform)),
     vcat (map (pprExternDecl platform) (Map.keys lbls)))
  where (temps, lbls) = runTE (mapM_ te_BB stmts)

pprDataExterns :: Platform -> [CmmStatic] -> SDoc
pprDataExterns platform statics
  = vcat (map (pprExternDecl platform) (Map.keys lbls))
  where (_, lbls) = runTE (mapM_ te_Static statics)

pprTempDecl :: Platform -> LocalReg -> SDoc
pprTempDecl platform l@(LocalReg _ rep)
  = hcat [ machRepCType platform rep, space, pprLocalReg l, semi ]

pprExternDecl :: Platform -> CLabel -> SDoc
pprExternDecl platform lbl
  -- do not print anything for "known external" things
  | not (needsCDecl lbl) = empty
  | Just sz <- foreignLabelStdcallInfo lbl = stdcall_decl sz
  | otherwise =
        hcat [ visibility, label_type lbl , lparen, ppr lbl, text ");"
             -- occasionally useful to see label type
             -- , text "/* ", pprDebugCLabel lbl, text " */"
             ]
 where
  label_type lbl | isBytesLabel lbl         = text "B_"
                 | isForeignLabel lbl && isCFunctionLabel lbl
                                            = text "FF_"
                 | isCFunctionLabel lbl     = text "F_"
                 | isStaticClosureLabel lbl = text "C_"
                 -- generic .rodata labels
                 | isSomeRODataLabel lbl    = text "RO_"
                 -- generic .data labels (common case)
                 | otherwise                = text "RW_"

  visibility
     | externallyVisibleCLabel lbl = char 'E'
     | otherwise                   = char 'I'

  -- If the label we want to refer to is a stdcall function (on Windows) then
  -- we must generate an appropriate prototype for it, so that the C compiler will
  -- add the @n suffix to the label (#2276)
  stdcall_decl sz =
        text "extern __attribute__((stdcall)) void " <> ppr lbl
        <> parens (commafy (replicate (sz `quot` platformWordSizeInBytes platform) (machRep_U_CType platform (wordWidth platform))))
        <> semi

type TEState = (UniqSet LocalReg, Map CLabel ())
newtype TE a = TE { unTE :: TEState -> (a, TEState) } deriving (Functor)

instance Applicative TE where
      pure a = TE $ \s -> (a, s)
      (<*>) = ap

instance Monad TE where
   TE m >>= k  = TE $ \s -> case m s of (a, s') -> unTE (k a) s'

te_lbl :: CLabel -> TE ()
te_lbl lbl = TE $ \(temps,lbls) -> ((), (temps, Map.insert lbl () lbls))

te_temp :: LocalReg -> TE ()
te_temp r = TE $ \(temps,lbls) -> ((), (addOneToUniqSet temps r, lbls))

runTE :: TE () -> TEState
runTE (TE m) = snd (m (emptyUniqSet, Map.empty))

te_Static :: CmmStatic -> TE ()
te_Static (CmmStaticLit lit) = te_Lit lit
te_Static _ = return ()

te_BB :: CmmBlock -> TE ()
te_BB block = mapM_ te_Stmt (blockToList mid) >> te_Stmt last
  where (_, mid, last) = blockSplit block

te_Lit :: CmmLit -> TE ()
te_Lit (CmmLabel l) = te_lbl l
te_Lit (CmmLabelOff l _) = te_lbl l
te_Lit (CmmLabelDiffOff l1 _ _ _) = te_lbl l1
te_Lit _ = return ()

te_Stmt :: CmmNode e x -> TE ()
te_Stmt (CmmAssign r e)         = te_Reg r >> te_Expr e
te_Stmt (CmmStore l r)          = te_Expr l >> te_Expr r
te_Stmt (CmmUnsafeForeignCall target rs es)
  = do  te_Target target
        mapM_ te_temp rs
        mapM_ te_Expr es
te_Stmt (CmmCondBranch e _ _ _) = te_Expr e
te_Stmt (CmmSwitch e _)         = te_Expr e
te_Stmt (CmmCall { cml_target = e }) = te_Expr e
te_Stmt _                       = return ()

te_Target :: ForeignTarget -> TE ()
te_Target (ForeignTarget e _)      = te_Expr e
te_Target (PrimTarget{})           = return ()

te_Expr :: CmmExpr -> TE ()
te_Expr (CmmLit lit)            = te_Lit lit
te_Expr (CmmLoad e _)           = te_Expr e
te_Expr (CmmReg r)              = te_Reg r
te_Expr (CmmMachOp _ es)        = mapM_ te_Expr es
te_Expr (CmmRegOff r _)         = te_Reg r
te_Expr (CmmStackSlot _ _)      = panic "te_Expr: CmmStackSlot not supported!"

te_Reg :: CmmReg -> TE ()
te_Reg (CmmLocal l) = te_temp l
te_Reg _            = return ()


-- ---------------------------------------------------------------------
-- C types for MachReps

cCast :: DynFlags -> SDoc -> CmmExpr -> SDoc
cCast dflags ty expr = parens ty <> pprExpr1 dflags expr

cLoad :: DynFlags -> CmmExpr -> CmmType -> SDoc
cLoad dflags expr rep
    = if bewareLoadStoreAlignment (platformArch platform)
      then let decl = machRepCType platform rep <+> text "x" <> semi
               struct = text "struct" <+> braces (decl)
               packed_attr = text "__attribute__((packed))"
               cast = parens (struct <+> packed_attr <> char '*')
           in parens (cast <+> pprExpr1 dflags expr) <> text "->x"
      else char '*' <> parens (cCast dflags (machRepPtrCType platform rep) expr)
    where -- On these platforms, unaligned loads are known to cause problems
          bewareLoadStoreAlignment ArchAlpha    = True
          bewareLoadStoreAlignment ArchMipseb   = True
          bewareLoadStoreAlignment ArchMipsel   = True
          bewareLoadStoreAlignment (ArchARM {}) = True
          bewareLoadStoreAlignment ArchARM64    = True
          bewareLoadStoreAlignment ArchSPARC    = True
          bewareLoadStoreAlignment ArchSPARC64  = True
          -- Pessimistically assume that they will also cause problems
          -- on unknown arches
          bewareLoadStoreAlignment ArchUnknown  = True
          bewareLoadStoreAlignment _            = False
          platform = targetPlatform dflags

isCmmWordType :: Platform -> CmmType -> Bool
-- True of GcPtrReg/NonGcReg of native word size
isCmmWordType platform ty = not (isFloatType ty)
                            && typeWidth ty == wordWidth platform

-- This is for finding the types of foreign call arguments.  For a pointer
-- argument, we always cast the argument to (void *), to avoid warnings from
-- the C compiler.
machRepHintCType :: Platform -> CmmType -> ForeignHint -> SDoc
machRepHintCType platform rep = \case
   AddrHint   -> text "void *"
   SignedHint -> machRep_S_CType platform (typeWidth rep)
   _other     -> machRepCType platform rep

machRepPtrCType :: Platform -> CmmType -> SDoc
machRepPtrCType platform r
 = if isCmmWordType platform r
      then text "P_"
      else machRepCType platform r <> char '*'

machRepCType :: Platform -> CmmType -> SDoc
machRepCType platform ty
   | isFloatType ty = machRep_F_CType w
   | otherwise      = machRep_U_CType platform w
   where
      w = typeWidth ty

machRep_F_CType :: Width -> SDoc
machRep_F_CType W32 = text "StgFloat" -- ToDo: correct?
machRep_F_CType W64 = text "StgDouble"
machRep_F_CType _   = panic "machRep_F_CType"

machRep_U_CType :: Platform -> Width -> SDoc
machRep_U_CType platform w
 = case w of
   _ | w == wordWidth platform -> text "W_"
   W8  -> text "StgWord8"
   W16 -> text "StgWord16"
   W32 -> text "StgWord32"
   W64 -> text "StgWord64"
   _   -> panic "machRep_U_CType"

machRep_S_CType :: Platform -> Width -> SDoc
machRep_S_CType platform w
 = case w of
   _ | w == wordWidth platform -> text "I_"
   W8  -> text "StgInt8"
   W16 -> text "StgInt16"
   W32 -> text "StgInt32"
   W64 -> text "StgInt64"
   _   -> panic "machRep_S_CType"


-- ---------------------------------------------------------------------
-- print strings as valid C strings

pprStringInCStyle :: ByteString -> SDoc
pprStringInCStyle s = doubleQuotes (text (concatMap charToC (BS.unpack s)))

-- ---------------------------------------------------------------------------
-- Initialising static objects with floating-point numbers.  We can't
-- just emit the floating point number, because C will cast it to an int
-- by rounding it.  We want the actual bit-representation of the float.
--
-- Consider a concrete C example:
--    double d = 2.5e-10;
--    float f  = 2.5e-10f;
--
--    int * i2 = &d;      printf ("i2: %08X %08X\n", i2[0], i2[1]);
--    long long * l = &d; printf (" l: %016llX\n",   l[0]);
--    int * i = &f;       printf (" i: %08X\n",      i[0]);
-- Result on 64-bit LE (x86_64):
--     i2: E826D695 3DF12E0B
--      l: 3DF12E0BE826D695
--      i: 2F89705F
-- Result on 32-bit BE (m68k):
--     i2: 3DF12E0B E826D695
--      l: 3DF12E0BE826D695
--      i: 2F89705F
--
-- The trick here is to notice that binary representation does not
-- change much: only Word32 values get swapped on LE hosts / targets.

-- This is a hack to turn the floating point numbers into ints that we
-- can safely initialise to static locations.

castFloatToWord32Array :: STUArray s Int Float -> ST s (STUArray s Int Word32)
castFloatToWord32Array = U.castSTUArray

castDoubleToWord64Array :: STUArray s Int Double -> ST s (STUArray s Int Word64)
castDoubleToWord64Array = U.castSTUArray

floatToWord :: Platform -> Rational -> CmmLit
floatToWord platform r
  = runST (do
        arr <- newArray_ ((0::Int),0)
        writeArray arr 0 (fromRational r)
        arr' <- castFloatToWord32Array arr
        w32 <- readArray arr' 0
        return (CmmInt (toInteger w32 `shiftL` wo) (wordWidth platform))
    )
    where wo | wordWidth platform == W64
             , BigEndian <- platformByteOrder platform
             = 32
             | otherwise
             = 0

floatPairToWord :: Platform -> Rational -> Rational -> CmmLit
floatPairToWord platform r1 r2
  = runST (do
        arr <- newArray_ ((0::Int),1)
        writeArray arr 0 (fromRational r1)
        writeArray arr 1 (fromRational r2)
        arr' <- castFloatToWord32Array arr
        w32_1 <- readArray arr' 0
        w32_2 <- readArray arr' 1
        return (pprWord32Pair w32_1 w32_2)
    )
    where pprWord32Pair w32_1 w32_2
              | BigEndian <- platformByteOrder platform =
                  CmmInt ((shiftL i1 32) .|. i2) W64
              | otherwise =
                  CmmInt ((shiftL i2 32) .|. i1) W64
              where i1 = toInteger w32_1
                    i2 = toInteger w32_2

doubleToWords :: Platform -> Rational -> [CmmLit]
doubleToWords platform r
  = runST (do
        arr <- newArray_ ((0::Int),1)
        writeArray arr 0 (fromRational r)
        arr' <- castDoubleToWord64Array arr
        w64 <- readArray arr' 0
        return (pprWord64 w64)
    )
    where targetWidth = wordWidth platform
          pprWord64 w64
              | targetWidth == W64 =
                  [ CmmInt (toInteger w64) targetWidth ]
              | targetWidth == W32 =
                  [ CmmInt (toInteger targetW1) targetWidth
                  , CmmInt (toInteger targetW2) targetWidth
                  ]
              | otherwise = panic "doubleToWords.pprWord64"
              where (targetW1, targetW2) = case platformByteOrder platform of
                        BigEndian    -> (wHi, wLo)
                        LittleEndian -> (wLo, wHi)
                    wHi = w64 `shiftR` 32
                    wLo = w64 .&. 0xFFFFffff

-- ---------------------------------------------------------------------------
-- Utils

wordShift :: Platform -> Int
wordShift platform = widthInLog (wordWidth platform)

commafy :: [SDoc] -> SDoc
commafy xs = hsep $ punctuate comma xs

-- Print in C hex format: 0x13fa
pprHexVal :: DynFlags -> Integer -> Width -> SDoc
pprHexVal dflags w rep
  | w < 0     = parens (char '-' <>
                    text "0x" <> intToDoc (-w) <> repsuffix rep)
  | otherwise =     text "0x" <> intToDoc   w  <> repsuffix rep
  where
        -- type suffix for literals:
        -- Integer literals are unsigned in Cmm/C.  We explicitly cast to
        -- signed values for doing signed operations, but at all other
        -- times values are unsigned.  This also helps eliminate occasional
        -- warnings about integer overflow from gcc.

      repsuffix W64 =
               if cINT_SIZE       dflags == 8 then char 'U'
          else if cLONG_SIZE      dflags == 8 then text "UL"
          else if cLONG_LONG_SIZE dflags == 8 then text "ULL"
          else panic "pprHexVal: Can't find a 64-bit type"
      repsuffix _ = char 'U'

      intToDoc :: Integer -> SDoc
      intToDoc i = case truncInt i of
                       0 -> char '0'
                       v -> go v

      -- We need to truncate value as Cmm backend does not drop
      -- redundant bits to ease handling of negative values.
      -- Thus the following Cmm code on 64-bit arch, like amd64:
      --     CInt v;
      --     v = {something};
      --     if (v == %lobits32(-1)) { ...
      -- leads to the following C code:
      --     StgWord64 v = (StgWord32)({something});
      --     if (v == 0xFFFFffffFFFFffffU) { ...
      -- Such code is incorrect as it promotes both operands to StgWord64
      -- and the whole condition is always false.
      truncInt :: Integer -> Integer
      truncInt i =
          case rep of
              W8  -> i `rem` (2^(8 :: Int))
              W16 -> i `rem` (2^(16 :: Int))
              W32 -> i `rem` (2^(32 :: Int))
              W64 -> i `rem` (2^(64 :: Int))
              _   -> panic ("pprHexVal/truncInt: C backend can't encode "
                            ++ show rep ++ " literals")

      go 0 = empty
      go w' = go q <> dig
           where
             (q,r) = w' `quotRem` 16
             dig | r < 10    = char (chr (fromInteger r + ord '0'))
                 | otherwise = char (chr (fromInteger r - 10 + ord 'a'))
