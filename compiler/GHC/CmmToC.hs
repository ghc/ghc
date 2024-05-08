{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia   #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

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

module GHC.CmmToC
   ( cmmToC
   )
where

import GHC.Prelude

import GHC.Platform

import GHC.CmmToAsm.CPrim

import GHC.Cmm.BlockId
import GHC.Cmm.CLabel
import GHC.Cmm hiding (pprBBlock, pprStatic)
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Graph
import GHC.Cmm.Dataflow.Label
import GHC.Cmm.Utils
import GHC.Cmm.Switch
import GHC.Cmm.InitFini

import GHC.Types.ForeignCall
import GHC.Types.Unique.Set
import GHC.Types.Unique.FM
import GHC.Types.Unique

import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Monad.State.Strict (State (..), runState, state)
import GHC.Utils.Misc

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Float

-- --------------------------------------------------------------------------
-- Now do some real work
--
-- for fun, we could call cmmToCmm over the tops...
--

cmmToC :: Platform -> RawCmmGroup -> SDoc
cmmToC platform tops = (vcat $ intersperse blankLine $ map (pprTop platform) tops) $$ blankLine

--
-- top level procs
--
pprTop :: Platform -> RawCmmDecl -> SDoc
pprTop platform = \case
  (CmmProc infos clbl _in_live_regs graph) ->
    (case mapLookup (g_entry graph) infos of
       Nothing -> empty
       Just (CmmStaticsRaw info_clbl info_dat) ->
           pprDataExterns platform info_dat $$
           pprWordArray platform info_is_in_rodata info_clbl info_dat) $$
    (vcat [
           blankLine,
           extern_decls,
           (if (externallyVisibleCLabel clbl)
                    then mkFN_ else mkIF_) (pprCLabel platform clbl) <+> lbrace,
           nest 8 temp_decls,
           vcat (map (pprBBlock platform) blocks),
           rbrace ]
    )
    where
        -- info tables are always in .rodata
        info_is_in_rodata = True
        blocks = toBlockListEntryFirst graph
        (temp_decls, extern_decls) = pprTempAndExternDecls platform blocks


  -- Chunks of static data.

  -- We only handle (a) arrays of word-sized things and (b) strings.

  cmm_data | Just (initOrFini, clbls) <- isInitOrFiniArray cmm_data ->
    pprCtorArray platform initOrFini clbls

  (CmmData section (CmmStaticsRaw lbl [CmmString str])) ->
    pprExternDecl platform lbl $$
    hcat [
      pprLocalness lbl, pprConstness (isSecConstant section), text "char ", pprCLabel platform lbl,
      text "[] = ", pprStringInCStyle str, semi
    ]

  (CmmData section (CmmStaticsRaw lbl [CmmUninitialised size])) ->
    pprExternDecl platform lbl $$
    hcat [
      pprLocalness lbl, pprConstness (isSecConstant section), text "char ", pprCLabel platform lbl,
      brackets (int size), semi
    ]

  (CmmData section (CmmStaticsRaw lbl lits)) ->
    pprDataExterns platform lits $$
    pprWordArray platform (isSecConstant section) lbl lits
  where
    isSecConstant section = case sectionProtection section of
      ReadOnlySection -> True
      WriteProtectedSection -> True
      _ -> False

-- --------------------------------------------------------------------------
-- BasicBlocks are self-contained entities: they always end in a jump.
--
-- Like nativeGen/AsmCodeGen, we could probably reorder blocks to turn
-- as many jumps as possible into fallthroughs.
--

pprBBlock :: Platform -> CmmBlock -> SDoc
pprBBlock platform block =
  nest 4 (pprBlockId (entryLabel block) <> colon) $$
  nest 8 (vcat (map (pprStmt platform) (blockToList nodes)) $$ pprStmt platform last)
 where
  (_, nodes, last)  = blockSplit block

-- --------------------------------------------------------------------------
-- Info tables. Just arrays of words.
-- See codeGen/ClosureInfo, and nativeGen/PprMach

pprWordArray :: Platform -> Bool -> CLabel -> [CmmStatic] -> SDoc
pprWordArray platform is_ro lbl ds
  = -- TODO: align closures only
    pprExternDecl platform lbl $$
    hcat [ pprLocalness lbl, pprConstness is_ro, text "StgWord"
         , space, pprCLabel platform lbl, text "[]"
         -- See Note [StgWord alignment]
         , pprAlignment (wordWidth platform)
         , text "= {" ]
    $$ nest 8 (commafy (staticLitsToWords platform $ toLits ds))
    $$ text "};"
  where
    toLits :: [CmmStatic] -> [CmmLit]
    toLits = map f
      where
        f (CmmStaticLit lit) = lit
        f static             = pprPanic "pprWordArray: Unexpected literal"  (pprStatic platform static)

pprAlignment :: Width -> SDoc
pprAlignment words =
     text "__attribute__((aligned(" <> int (widthInBytes words) <> text ")))"

-- Note [StgWord alignment]
-- ~~~~~~~~~~~~~~~~~~~~~~~~
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

pprStmt :: Platform -> CmmNode e x -> SDoc
pprStmt platform stmt =
    case stmt of
    CmmEntry{}   -> empty
    CmmComment _ -> empty -- (hang (text "/*") 3 (ftext s)) $$ text "*/"
                          -- XXX if the string contains "*/", we need to fix it
                          -- XXX we probably want to emit these comments when
                          -- some debugging option is on.  They can get quite
                          -- large.

    CmmTick _ -> empty
    CmmUnwind{} -> empty

    CmmAssign dest src -> pprAssign platform dest src

    CmmStore  dest src align
        | typeWidth rep == W64 && wordWidth platform /= W64
        -> (if isFloatType rep then text "ASSIGN_DBL"
                               else text "ASSIGN_Word64") <>
           parens (mkP_ <> pprExpr1 platform dest <> comma <> pprExpr platform src) <> semi

        | otherwise
        -> hsep [ pprExpr platform (CmmLoad dest rep align), equals, pprExpr platform src <> semi ]
        where
          rep = cmmExprType platform src

    CmmUnsafeForeignCall target@(ForeignTarget fn conv) results args ->
        fnCall
        where
        (res_hints, arg_hints) = foreignTargetHints target
        hresults = zip results res_hints
        hargs    = zip args arg_hints

        ForeignConvention cconv _ _ ret = conv

        cast_fn = parens (cCast platform (pprCFunType platform (char '*') cconv hresults hargs) fn)

        -- See wiki:commentary/compiler/backends/ppr-c#prototypes
        fnCall =
            case fn of
              CmmLit (CmmLabel lbl)
                | StdCallConv <- cconv ->
                    pprCall platform (pprCLabel platform lbl) cconv hresults hargs
                        -- stdcall functions must be declared with
                        -- a function type, otherwise the C compiler
                        -- doesn't add the @n suffix to the label.  We
                        -- can't add the @n suffix ourselves, because
                        -- it isn't valid C.
                | CmmNeverReturns <- ret ->
                    pprCall platform cast_fn cconv hresults hargs <> semi <> text "__builtin_unreachable();"
                | not (isMathFun lbl) ->
                    pprForeignCall platform (pprCLabel platform lbl) cconv hresults hargs
              _ ->
                    pprCall platform cast_fn cconv hresults hargs <> semi
                        -- for a dynamic call, no declaration is necessary.

    CmmUnsafeForeignCall (PrimTarget MO_Touch) _results _args -> empty
    CmmUnsafeForeignCall (PrimTarget (MO_Prefetch_Data _)) _results _args -> empty

    CmmUnsafeForeignCall (PrimTarget MO_ReleaseFence) [] [] ->
        text "__atomic_thread_fence(__ATOMIC_RELEASE);"
    CmmUnsafeForeignCall (PrimTarget MO_AcquireFence) [] [] ->
        text "__atomic_thread_fence(__ATOMIC_ACQUIRE);"
    CmmUnsafeForeignCall (PrimTarget MO_SeqCstFence) [] [] ->
        text "__atomic_thread_fence(__ATOMIC_SEQ_CST);"

    CmmUnsafeForeignCall target@(PrimTarget op) results args ->
        fn_call
      where
        cconv = CCallConv
        fn = pprCallishMachOp_for_C op

        (res_hints, arg_hints) = foreignTargetHints target
        hresults = zip results res_hints
        hargs    = zip args arg_hints

        need_cdecl
          | MO_ResumeThread  <- op                 = True
          | MO_SuspendThread <- op                 = True
          | otherwise                              = False

        fn_call
          -- The mem primops carry an extra alignment arg.
          -- We could maybe emit an alignment directive using this info.
          -- We also need to cast mem primops to prevent conflicts with GCC
          -- builtins (see bug #5967).
          | need_cdecl
          = (text ";EFF_(" <> fn <> char ')' <> semi) $$
            pprForeignCall platform fn cconv hresults hargs
          | otherwise
          = pprCall platform fn cconv hresults hargs

    CmmBranch ident               -> pprBranch ident
    CmmCondBranch expr yes no _   -> pprCondBranch platform expr yes no
    CmmCall { cml_target = expr } -> mkJMP_ (pprExpr platform expr) <> semi
    CmmSwitch arg ids             -> pprSwitch platform arg ids

    _other -> pprPanic "PprC.pprStmt" (pdoc platform stmt)

type Hinted a = (a, ForeignHint)

pprForeignCall :: Platform -> SDoc -> CCallConv -> [Hinted CmmFormal] -> [Hinted CmmActual]
               -> SDoc
pprForeignCall platform fn cconv results args = fn_call
  where
    fn_call = braces (
                 pprCFunType platform (char '*' <> text "ghcFunPtr") cconv results args <> semi
              $$ text "ghcFunPtr" <+> equals <+> cast_fn <> semi
              $$ pprCall platform (text "ghcFunPtr") cconv results args <> semi
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
pprCondBranch :: Platform -> CmmExpr -> BlockId -> BlockId -> SDoc
pprCondBranch platform expr yes no
        = hsep [ text "if" , parens (pprExpr platform expr) ,
                        text "goto", pprBlockId yes <> semi,
                        text "else goto", pprBlockId no <> semi ]

-- ---------------------------------------------------------------------
-- a local table branch
--
-- we find the fall-through cases
--
pprSwitch :: Platform -> CmmExpr -> SwitchTargets -> SDoc
pprSwitch platform e ids
  = (hang (text "switch" <+> parens ( pprExpr platform e ) <+> lbrace)
                4 (vcat ( map caseify pairs ) $$ def)) $$ rbrace
  where
    (pairs, mbdef) = switchTargetsFallThrough ids

    rep = typeWidth (cmmExprType platform e)

    -- fall through case
    caseify (ix:|ixs, ident) = vcat (map do_fallthrough ixs) $$ final_branch ix
        where
        do_fallthrough ix =
                 hsep [ text "case" , pprHexVal platform ix rep <> colon ,
                        text "/* fall through */" ]

        final_branch ix =
                hsep [ text "case" , pprHexVal platform ix rep <> colon ,
                       text "goto" , (pprBlockId ident) <> semi ]

    def | Just l <- mbdef = text "default: goto" <+> pprBlockId l <> semi
        | otherwise       = text "default: __builtin_unreachable();"

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

pprExpr :: Platform -> CmmExpr -> SDoc
pprExpr platform e = case e of
    CmmLit lit         -> pprLit platform lit
    CmmLoad e ty align -> pprLoad platform e ty align
    CmmReg reg         -> pprCastReg reg
    CmmRegOff reg 0    -> pprCastReg reg

    -- CmmRegOff is an alias of MO_Add
    CmmRegOff reg i    -> pprExpr platform $ CmmMachOp (MO_Add w) [CmmReg reg, CmmLit $ CmmInt (toInteger i) w]
      where w = cmmRegWidth reg

    CmmMachOp mop args -> pprMachOpApp platform mop args

    CmmStackSlot _ _   -> panic "pprExpr: CmmStackSlot not supported!"


pprLoad :: Platform -> CmmExpr -> CmmType -> AlignmentSpec -> SDoc
pprLoad platform e ty _align
  | width == W64, wordWidth platform /= W64
  = (if isFloatType ty then text "PK_DBL"
                       else text "PK_Word64")
    <> parens (mkP_ <> pprExpr1 platform e)

  -- TODO: exploit natural-alignment where possible
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

        _other -> cLoad platform e ty
  where
    width = typeWidth ty

pprExpr1 :: Platform -> CmmExpr -> SDoc
pprExpr1 platform e = case e of
   CmmLit lit  -> pprLit1 platform lit
   CmmReg _reg -> pprExpr platform e
   _           -> parens (pprExpr platform e)

-- --------------------------------------------------------------------------
-- MachOp applications

pprMachOpApp :: Platform -> MachOp -> [CmmExpr] -> SDoc

pprMachOpApp platform op args
  | isMulMayOfloOp op
  = text "mulIntMayOflo" <> parens (commafy (map (pprExpr platform) args))
  where isMulMayOfloOp (MO_S_MulMayOflo _) = True
        isMulMayOfloOp _ = False

pprMachOpApp platform (MO_RelaxedRead w) [x]
  = pprExpr platform (CmmLoad x (cmmBits w) NaturallyAligned)

pprMachOpApp platform mop args
  | Just ty <- machOpNeedsCast platform mop (map (cmmExprType platform) args)
  = ty <> parens (pprMachOpApp' platform mop args)
  | otherwise
  = pprMachOpApp' platform mop args

{-
Note [Zero-extending sub-word signed results]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider a program like (from #20634):

    test() {
        bits64 ret;
        bits8 a,b;
        a = 0xe1 :: bits8;       // == -31 signed
        b = %quot(a, 3::bits8);  // == -10 signed
        ret = %zx64(a);          // == 0xf6 unsigned
        return (ret);
    }

This program should return 0xf6 == 246. However, we need to be very careful
with when dealing with the result of the %quot. For instance, one might be
tempted produce code like:

    StgWord8 a = 0xe1U;
    StgInt8  b = (StgInt8) a / (StgInt8) 0x3U;
    StgWord ret = (W_) b;

However, this would be wrong; by widening `b` directly from `StgInt8` to
`StgWord` we will get sign-extension semantics: rather than 0xf6 we will get
0xfffffffffffffff6. To avoid this we must first cast `b` back to `StgWord8`,
ensuring that we get zero-extension semantics when we widen up to `StgWord`.

Note [When in doubt, cast arguments as unsigned]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In general C's signed-ness behavior can lead to surprising results and
consequently we are very explicit about ensuring that arguments have the
correct signedness. For instance, consider a program like

    test() {
        bits64 ret, a, b;
        a = %neg(43 :: bits64);
        b = %neg(0x443c70fa3e465120 :: bits64);
        ret = %modu(a, b);
        return (ret);
    }

In this case both `a` and `b` will be StgInts in the generated C (since
`MO_Neg` is a signed operation). However, we want to ensure that we perform an
*unsigned* modulus operation, therefore we must be careful to cast both arguments
to StgWord. We do this for any operation where the signedness of the argument
may affect the operation's semantics.
-}

-- | The result type of most operations is determined by the operands. However,
-- there are a few exceptions: particularly operations which might get promoted
-- to a signed result. For these we explicitly cast the result.
machOpNeedsCast :: Platform -> MachOp -> [CmmType] -> Maybe SDoc
machOpNeedsCast platform mop args
    -- Comparisons in C have type 'int', but we want type W_ (this is what
    -- resultRepOfMachOp says).
  | isComparisonMachOp mop = Just mkW_

    -- See Note [Zero-extending sub-word signed results]
  | signedOp mop
  , res_ty <- machOpResultType platform mop args
  , not $ isFloatType res_ty -- only integer operations, not MO_SF_Conv
  , let w = typeWidth res_ty
  , w < wordWidth platform
  = cast_it w

    -- A shift operation like (a >> b) where a::Word8 and b::Word has type Word
    -- in C yet we want a Word8
  | Just w <- shiftOp mop  = cast_it w

    -- The results of these operations may be promoted to signed values
    -- due to C11 section 6.3.1.1.
  | MO_Add w <- mop        = cast_it w
  | MO_Sub w <- mop        = cast_it w
  | MO_Mul w <- mop        = cast_it w
  | MO_U_Quot w <- mop     = cast_it w
  | MO_U_Rem  w <- mop     = cast_it w
  | MO_And w <- mop        = cast_it w
  | MO_Or  w <- mop        = cast_it w
  | MO_Xor w <- mop        = cast_it w
  | MO_Not w <- mop        = cast_it w

  | otherwise              = Nothing
  where
    cast_it w =
      let ty = machRep_U_CType platform w
      in Just $ parens ty

pprMachOpApp' :: Platform -> MachOp -> [CmmExpr] -> SDoc
pprMachOpApp' platform mop args
 = case args of

    -- ternary
    args@[_,_,_] ->
      pprMachOp_for_C platform mop <> parens (pprWithCommas pprArg args)

    -- dyadic
    [x,y] -> pprArg x <+> pprMachOp_for_C platform mop <+> pprArg y

    -- unary
    [x]   -> pprMachOp_for_C platform mop <> parens (pprArg x)

    _     -> panic "PprC.pprMachOp : machop with wrong number of args"

  where
    pprArg e
      | needsFCasts mop = cCast platform (machRep_F_CType width) e
        -- Cast needed for signed integer ops
      | signedOp    mop = cCast platform (machRep_S_CType platform width) e
        -- See Note [When in doubt, cast arguments as unsigned]
      | needsUnsignedCast mop
                        = cCast platform (machRep_U_CType platform width) e
      | otherwise       = pprExpr1 platform e
      where
        width = typeWidth (cmmExprType platform e)

    needsFCasts (MO_F_Neg _)  = True
    needsFCasts (MO_F_Quot _) = True
    needsFCasts mop  = floatComparison mop

    -- See Note [When in doubt, cast arguments as unsigned]
    needsUnsignedCast (MO_Mul    _) = True
    needsUnsignedCast (MO_U_Shr  _) = True
    needsUnsignedCast (MO_U_Quot _) = True
    needsUnsignedCast (MO_U_Rem  _) = True
    needsUnsignedCast (MO_U_Ge   _) = True
    needsUnsignedCast (MO_U_Le   _) = True
    needsUnsignedCast (MO_U_Gt   _) = True
    needsUnsignedCast (MO_U_Lt   _) = True
    needsUnsignedCast _             = False

-- --------------------------------------------------------------------------
-- Literals

pprLit :: Platform -> CmmLit -> SDoc
pprLit platform lit = case lit of
    CmmInt i rep      -> pprHexVal platform i rep

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
        pprCLabelAddr lbl = char '&' <> pprCLabel platform lbl

pprLit1 :: Platform -> CmmLit -> SDoc
pprLit1 platform lit = case lit of
   (CmmLabelOff _ _)         -> parens (pprLit platform lit)
   (CmmLabelDiffOff _ _ _ _) -> parens (pprLit platform lit)
   (CmmFloat _ _)            -> parens (pprLit platform lit)
   _                         -> pprLit platform lit

-- ---------------------------------------------------------------------------
-- Static data

-- | Produce a list of word sized literals encoding the given list of 'CmmLit's.
staticLitsToWords :: Platform -> [CmmLit] -> [SDoc]
staticLitsToWords platform = go . foldMap decomposeMultiWord
  where
    -- rem_bytes is how many bytes remain in the word we are currently filling.
    -- accum is the word we are filling.
    go :: [CmmLit] -> [SDoc]
    go [] = []
    go lits@(lit : _)
      | Just _ <- isSubWordLit lit
      = goSubWord wordWidthBytes 0 lits
    go (lit : rest)
      = pprLit1 platform lit : go rest

    goSubWord :: Int -> Integer -> [CmmLit] -> [SDoc]
    goSubWord rem_bytes accum (lit : rest)
      | Just (bytes, w) <- isSubWordLit lit
      , rem_bytes >= widthInBytes w
      = let accum' = (accum `shiftL` widthInBits w) .|. fixEndian w bytes
        in goSubWord (rem_bytes - widthInBytes w) accum' rest
    goSubWord rem_bytes accum rest
      = pprWord (fixEndian (wordWidth platform) $ accum `shiftL` (8*rem_bytes)) : go rest

    fixEndian :: Width -> Integer -> Integer
    fixEndian w = case platformByteOrder platform of
      BigEndian    -> id
      LittleEndian -> byteSwap w

    -- Decompose multi-word or floating-point literals into multiple
    -- single-word (or smaller) literals.
    decomposeMultiWord :: CmmLit -> [CmmLit]
    decomposeMultiWord (CmmFloat n W64)
      | W32 <- wordWidth platform = decomposeMultiWord (doubleToWord64 n)
      | otherwise = [doubleToWord64 n]
    decomposeMultiWord (CmmFloat n W32)
      = [floatToWord32 n]
    decomposeMultiWord (CmmInt n W64)
      | W32 <- wordWidth platform
      = case platformByteOrder platform of
          BigEndian -> [CmmInt hi W32, CmmInt lo W32]
          LittleEndian -> [CmmInt lo W32, CmmInt hi W32]
      where
        hi = n `shiftR` 32
        lo = n .&. 0xffffffff
    decomposeMultiWord lit = [lit]

    -- Decompose a sub-word-sized literal into the integer value and its
    -- (sub-word-sized) width.
    isSubWordLit :: CmmLit -> Maybe (Integer, Width)
    isSubWordLit lit =
      case lit of
        CmmInt n w
          | w < wordWidth platform   -> Just (n, w)
        _                            -> Nothing

    wordWidthBytes = widthInBytes $ wordWidth platform

    pprWord :: Integer -> SDoc
    pprWord n = pprHexVal platform n (wordWidth platform)

byteSwap :: Width -> Integer -> Integer
byteSwap width n = foldl' f 0 bytes
  where
    f acc m = (acc `shiftL` 8) .|. m
    bytes = [ byte i | i <- [0..widthInBytes width - 1] ]
    byte i = (n `shiftR` (i*8)) .&. 0xff

pprStatic :: Platform -> CmmStatic -> SDoc
pprStatic platform s = case s of

    CmmStaticLit lit   -> nest 4 (pprLit platform lit)
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

        -- Floating-point operations
        MO_F_Add        _ -> char '+'
        MO_F_Sub        _ -> char '-'
        MO_F_Neg        _ -> char '-'
        MO_F_Mul        _ -> char '*'
        MO_F_Quot       _ -> char '/'

        -- Floating-point fused multiply-add operations
        MO_FMA FMAdd w ->
          case w of
            W32 -> text "fmaf"
            W64 -> text "fma"
            _   ->
              pprTrace "offending mop:"
                (text "FMAdd")
                (panic $ "PprC.pprMachOp_for_C: FMAdd unsupported"
                       ++ "at width " ++ show w)
        MO_FMA var _width  ->
          pprTrace "offending mop:"
            (text $ "FMA " ++ show var)
            (panic $ "PprC.pprMachOp_for_C: should have been handled earlier!")

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

-- bitcasts, in the C backend these are performed with __builtin_memcpy.
-- See rts/include/stg/Prim.h

        MO_FW_Bitcast W32 -> text "hs_bitcastfloat2word"
        MO_FW_Bitcast W64 -> text "hs_bitcastdouble2word64"

        MO_WF_Bitcast W32 -> text "hs_bitcastword2float"
        MO_WF_Bitcast W64 -> text "hs_bitcastword642double"

        MO_FW_Bitcast w -> pprTrace "offending mop:"
                                (text "MO_FW_Bitcast")
                                (panic $ "PprC.pprMachOp_for_C: MO_FW_Bitcast"
                                      ++ " called with improper width!"
                                      ++ show w)

        MO_WF_Bitcast w -> pprTrace "offending mop:"
                                (text "MO_WF_Bitcast")
                                (panic $ "PprC.pprMachOp_for_C: MO_WF_Bitcast"
                                      ++ " called with improper width!"
                                      ++ show w)


-- noop casts
        MO_UU_Conv from to | from == to -> empty
        MO_UU_Conv _from to -> parens (machRep_U_CType platform to)

        MO_SS_Conv from to | from == to -> empty
        MO_SS_Conv _from to -> parens (machRep_S_CType platform to)

        MO_XX_Conv from to | from == to -> empty
        MO_XX_Conv _from to -> parens (machRep_U_CType platform to)

        MO_FF_Conv from to | from == to -> empty
        MO_FF_Conv _from to -> parens (machRep_F_CType to)

        MO_SF_Round    _from to -> parens (machRep_F_CType to)
        MO_FS_Truncate _from to -> parens (machRep_S_CType platform to)

        MO_RelaxedRead _ -> pprTrace "offending mop:"
                                (text "MO_RelaxedRead")
                                (panic $ "PprC.pprMachOp_for_C: MO_S_MulMayOflo"
                                      ++ " should have been handled earlier!")

        MO_S_MulMayOflo _ -> pprTrace "offending mop:"
                                (text "MO_S_MulMayOflo")
                                (panic $ "PprC.pprMachOp_for_C: MO_S_MulMayOflo"
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

        MO_AlignmentCheck {} -> panic "-falignment-sanitisation not supported by unregisterised backend"

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
signedOp (MO_SF_Round _ _) = True
signedOp _                = False

shiftOp :: MachOp -> Maybe Width
shiftOp (MO_Shl w)        = Just w
shiftOp (MO_U_Shr w)      = Just w
shiftOp (MO_S_Shr w)      = Just w
shiftOp _                 = Nothing

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
        MO_AcquireFence -> unsupported
        MO_ReleaseFence -> unsupported
        MO_SeqCstFence  -> unsupported
        MO_Memcpy _     -> text "__builtin_memcpy"
        MO_Memset _     -> text "__builtin_memset"
        MO_Memmove _    -> text "__builtin_memmove"
        MO_Memcmp _     -> text "__builtin_memcmp"

        MO_SuspendThread -> text "suspendThread"
        MO_ResumeThread  -> text "resumeThread"

        MO_BSwap w          -> ftext (bSwapLabel w)
        MO_BRev w           -> ftext (bRevLabel w)
        MO_PopCnt w         -> ftext (popCntLabel w)
        MO_Pext w           -> ftext (pextLabel w)
        MO_Pdep w           -> ftext (pdepLabel w)
        MO_Clz w            -> ftext (clzLabel w)
        MO_Ctz w            -> ftext (ctzLabel w)
        MO_AtomicRMW w amop -> ftext (atomicRMWLabel w amop)
        MO_Cmpxchg w        -> ftext (cmpxchgLabel w)
        MO_Xchg w           -> ftext (xchgLabel w)
        -- TODO: handle orderings
        MO_AtomicRead w _   -> ftext (atomicReadLabel w)
        MO_AtomicWrite w _  -> ftext (atomicWriteLabel w)
        MO_UF_Conv w        -> ftext (word2FloatLabel w)

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
        -- we could support prefetch via "__builtin_prefetch"
        -- Not adding it for now
        (MO_Prefetch_Data _ ) -> unsupported

        MO_I64_ToI   -> text "hs_int64ToInt"
        MO_I64_FromI -> text "hs_intToInt64"
        MO_W64_ToW   -> text "hs_word64ToWord"
        MO_W64_FromW -> text "hs_wordToWord64"
        MO_x64_Neg   -> text "hs_neg64"
        MO_x64_Add   -> text "hs_add64"
        MO_x64_Sub   -> text "hs_sub64"
        MO_x64_Mul   -> text "hs_mul64"
        MO_I64_Quot  -> text "hs_quotInt64"
        MO_I64_Rem   -> text "hs_remInt64"
        MO_W64_Quot  -> text "hs_quotWord64"
        MO_W64_Rem   -> text "hs_remWord64"
        MO_x64_And   -> text "hs_and64"
        MO_x64_Or    -> text "hs_or64"
        MO_x64_Xor   -> text "hs_xor64"
        MO_x64_Not   -> text "hs_not64"
        MO_x64_Shl   -> text "hs_uncheckedShiftL64"
        MO_I64_Shr   -> text "hs_uncheckedIShiftRA64"
        MO_W64_Shr   -> text "hs_uncheckedShiftRL64"
        MO_x64_Eq    -> text "hs_eq64"
        MO_x64_Ne    -> text "hs_ne64"
        MO_I64_Ge    -> text "hs_geInt64"
        MO_I64_Gt    -> text "hs_gtInt64"
        MO_I64_Le    -> text "hs_leInt64"
        MO_I64_Lt    -> text "hs_ltInt64"
        MO_W64_Ge    -> text "hs_geWord64"
        MO_W64_Gt    -> text "hs_gtWord64"
        MO_W64_Le    -> text "hs_leWord64"
        MO_W64_Lt    -> text "hs_ltWord64"
    where unsupported = panic ("pprCallishMachOp_for_C: " ++ show mop
                            ++ " not supported!")

-- ---------------------------------------------------------------------
-- Useful #defines
--

mkJMP_, mkFN_, mkIF_ :: SDoc -> SDoc

mkJMP_ i = text "JMP_" <> parens i
mkFN_  i = text "FN_"  <> parens i -- externally visible function
mkIF_  i = text "IF_"  <> parens i -- locally visible

-- from rts/include/Stg.h
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
pprAssign :: Platform -> CmmReg -> CmmExpr -> SDoc

-- dest is a reg, rhs is a reg
pprAssign _ r1 (CmmReg r2)
   | isPtrReg r1 && isPtrReg r2
   = hcat [ pprAsPtrReg r1, equals, pprAsPtrReg r2, semi ]

-- dest is a reg, rhs is a CmmRegOff
pprAssign platform r1 (CmmRegOff r2 off)
   | isPtrReg r1 && isPtrReg r2 && (off `rem` platformWordSizeInBytes platform == 0)
   = hcat [ pprAsPtrReg r1, equals, pprAsPtrReg r2, op, int off', semi ]
  where
        off1 = off `shiftR` wordShift platform

        (op,off') | off >= 0  = (char '+', off1)
                  | otherwise = (char '-', -off1)

-- dest is a reg, rhs is anything.
-- We can't cast the lvalue, so we have to cast the rhs if necessary.  Casting
-- the lvalue elicits a warning from new GCC versions (3.4+).
pprAssign platform r1 r2
  | isFixedPtrReg r1             = mkAssign (mkP_ <> pprExpr1 platform r2)
  | Just ty <- strangeRegType r1 = mkAssign (parens ty <> pprExpr1 platform r2)
  | otherwise                    = mkAssign (pprExpr platform r2)
    where mkAssign x =
            case r1 of
              CmmGlobal (GlobalRegUse BaseReg _) ->
                text "ASSIGN_BaseReg" <> parens x <> semi
              _ -> pprReg r1 <> text " = " <> x <> semi

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
isFixedPtrReg (CmmGlobal (GlobalRegUse r _)) = isFixedPtrGlobalReg r

-- True if (pprAsPtrReg reg) will give an expression with type StgPtr
-- JD: THIS IS HORRIBLE AND SHOULD BE RENAMED, AT THE VERY LEAST.
-- THE GARBAGE WITH THE VNonGcPtr HELPS MATCH THE OLD CODE GENERATOR'S OUTPUT;
-- I'M NOT SURE IF IT SHOULD REALLY STAY THAT WAY.
isPtrReg :: CmmReg -> Bool
isPtrReg (CmmLocal _)                                 = False
isPtrReg (CmmGlobal (GlobalRegUse (VanillaReg _) ty)) = isGcPtrType ty -- if we print via pprAsPtrReg
isPtrReg (CmmGlobal (GlobalRegUse reg _))             = isFixedPtrGlobalReg reg

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
isStrangeTypeReg (CmmGlobal (GlobalRegUse g _)) = isStrangeTypeGlobal g

isStrangeTypeGlobal :: GlobalReg -> Bool
isStrangeTypeGlobal CCCS                = True
isStrangeTypeGlobal CurrentTSO          = True
isStrangeTypeGlobal CurrentNursery      = True
isStrangeTypeGlobal BaseReg             = True
isStrangeTypeGlobal r                   = isFixedPtrGlobalReg r

strangeRegType :: CmmReg -> Maybe SDoc
strangeRegType (CmmGlobal (GlobalRegUse CCCS _)) = Just (text "struct CostCentreStack_ *")
strangeRegType (CmmGlobal (GlobalRegUse CurrentTSO _)) = Just (text "struct StgTSO_ *")
strangeRegType (CmmGlobal (GlobalRegUse CurrentNursery _)) = Just (text "struct bdescr_ *")
strangeRegType (CmmGlobal (GlobalRegUse BaseReg _)) = Just (text "struct StgRegTable_ *")
strangeRegType _ = Nothing

-- pprReg just prints the register name.
--
pprReg :: CmmReg -> SDoc
pprReg r = case r of
        CmmLocal  local  -> pprLocalReg local
        CmmGlobal (GlobalRegUse global _ ) -> pprGlobalReg global

pprAsPtrReg :: CmmReg -> SDoc
pprAsPtrReg (CmmGlobal (GlobalRegUse (VanillaReg n) ty))
  = warnPprTrace (not $ isGcPtrType ty) "pprAsPtrReg" (ppr n) $ char 'R' <> int n <> text ".p"
pprAsPtrReg other_reg = pprReg other_reg

pprGlobalReg :: GlobalReg -> SDoc
pprGlobalReg gr = case gr of
    VanillaReg n   -> char 'R' <> int n  <> text ".w"
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

pprCall :: Platform -> SDoc -> CCallConv -> [Hinted CmmFormal] -> [Hinted CmmActual] -> SDoc
pprCall platform ppr_fn cconv results args
  | not (is_cishCC cconv)
  = panic $ "pprCall: unknown calling convention"

  | otherwise
  =
    ppr_assign results (ppr_fn <> parens (commafy (map pprArg args))) <> semi
  where
     ppr_assign []           rhs = rhs
     ppr_assign [(one,hint)] rhs
         = pprLocalReg one <> text " = "
                 <> pprUnHint hint (localRegType one) <> rhs
     ppr_assign _other _rhs = panic "pprCall: multiple results"

     pprArg (expr, AddrHint)
        = cCast platform (text "void *") expr
        -- see comment by machRepHintCType below
     pprArg (expr, SignedHint)
        = cCast platform (machRep_S_CType platform $ typeWidth $ cmmExprType platform expr) expr
     pprArg (expr, _other)
        = pprExpr platform expr

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
        hcat [ visibility, label_type lbl , lparen, pprCLabel platform lbl, text ");"
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
        text "extern __attribute__((stdcall)) void " <> pprCLabel platform lbl
        <> parens (commafy (replicate (sz `quot` platformWordSizeInBytes platform) (machRep_U_CType platform (wordWidth platform))))
        <> semi

type TEState = (UniqSet LocalReg, Map CLabel ())
newtype TE a = TE' (State TEState a)
  deriving stock (Functor)
  deriving (Applicative, Monad) via State TEState

pattern TE :: (TEState -> (a, TEState)) -> TE a
pattern TE f <- TE' (runState -> f)
  where TE f  = TE' (state f)
{-# COMPLETE TE #-}

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
te_Stmt (CmmStore l r _)        = te_Expr l >> te_Expr r
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
te_Expr (CmmLoad e _ _)         = te_Expr e
te_Expr (CmmReg r)              = te_Reg r
te_Expr (CmmMachOp _ es)        = mapM_ te_Expr es
te_Expr (CmmRegOff r _)         = te_Reg r
te_Expr (CmmStackSlot _ _)      = panic "te_Expr: CmmStackSlot not supported!"

te_Reg :: CmmReg -> TE ()
te_Reg (CmmLocal l) = te_temp l
te_Reg _            = return ()


-- ---------------------------------------------------------------------
-- C types for MachReps

cCast :: Platform -> SDoc -> CmmExpr -> SDoc
cCast platform ty expr = parens ty <> pprExpr1 platform expr

cLoad :: Platform -> CmmExpr -> CmmType -> SDoc
cLoad platform expr rep
    = if bewareLoadStoreAlignment (platformArch platform)
      then let decl = machRepCType platform rep <+> text "x" <> semi
               struct = text "struct" <+> braces (decl)
               packed_attr = text "__attribute__((packed))"
               cast = parens (struct <+> packed_attr <> char '*')
           in parens (cast <+> pprExpr1 platform expr) <> text "->x"
      else char '*' <> parens (cCast platform (machRepPtrCType platform rep) expr)
    where -- On these platforms, unaligned loads are known to cause problems
          bewareLoadStoreAlignment ArchAlpha    = True
          bewareLoadStoreAlignment ArchMipseb   = True
          bewareLoadStoreAlignment ArchMipsel   = True
          bewareLoadStoreAlignment (ArchARM {}) = True
          bewareLoadStoreAlignment ArchAArch64  = True
          -- Pessimistically assume that they will also cause problems
          -- on unknown arches
          bewareLoadStoreAlignment ArchUnknown  = True
          bewareLoadStoreAlignment _            = False

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

floatToWord32 :: Rational -> CmmLit
floatToWord32 r = CmmInt (toInteger (castFloatToWord32 (fromRational r))) W32

doubleToWord64 :: Rational -> CmmLit
doubleToWord64 r = CmmInt (toInteger (castDoubleToWord64 (fromRational r))) W64

-- ---------------------------------------------------------------------------
-- Utils

wordShift :: Platform -> Int
wordShift platform = widthInLog (wordWidth platform)

commafy :: [SDoc] -> SDoc
commafy xs = hsep $ punctuate comma xs

-- | Print in C hex format
--
-- Examples:
--
--   5114    :: W32  ===>  ((StgWord32)0x13faU)
--   (-5114) :: W32  ===>  ((StgWord32)(-0x13faU))
--
-- We use casts to support types smaller than `unsigned int`; C literal
-- suffixes support longer but not shorter types.
pprHexVal :: Platform -> Integer -> Width -> SDoc
pprHexVal platform w rep = parens ctype <> rawlit
  where
      rawlit
        | w < 0     = parens (char '-' <>
                          text "0x" <> intToDoc (-w) <> repsuffix rep)
        | otherwise =     text "0x" <> intToDoc   w  <> repsuffix rep
      ctype = machRep_U_CType platform rep

        -- type suffix for literals:
        -- Integer literals are unsigned in Cmm/C.  We explicitly cast to
        -- signed values for doing signed operations, but at all other
        -- times values are unsigned.  This also helps eliminate occasional
        -- warnings about integer overflow from gcc.

      constants = platformConstants platform

      repsuffix W64 =
               if pc_CINT_SIZE       constants == 8 then char 'U'
          else if pc_CLONG_SIZE      constants == 8 then text "UL"
          else if pc_CLONG_LONG_SIZE constants == 8 then text "ULL"
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

-- | Construct a constructor/finalizer function. Instead of emitting a
-- initializer/finalizer array we rather just emit a single function, annotated
-- with the appropriate C attribute, which then calls each of the initializers.
pprCtorArray :: Platform -> InitOrFini -> [CLabel] -> SDoc
pprCtorArray platform initOrFini lbls =
       decls
    <> text "static __attribute__((" <> text attribute <> text "))"
    <> text "void _hs_" <> text suffix <> text "()"
    <> braces body
  where
    body = vcat [ pprCLabel platform lbl <> text " ();" | lbl <- lbls ]
    decls = vcat [ text "void" <+> pprCLabel platform lbl <> text " (void);" | lbl <- lbls ]
    (attribute, suffix) = case initOrFini of
                  IsInitArray
                    -- See Note [JSFFI initialization] for details
                    | ArchWasm32 <- platformArch platform -> ("constructor(101)", "constructor")
                    | otherwise -> ("constructor", "constructor")
                  IsFiniArray -> ("destructor", "destructor")
