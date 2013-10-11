-----------------------------------------------------------------------------
--
-- Pretty-printing of Cmm as C, suitable for feeding gcc
--
-- (c) The University of Glasgow 2004-2006
--
-- Print Cmm as real C, for -fvia-C
--
-- See wiki:Commentary/Compiler/Backends/PprC
--
-- This is simpler than the old PprAbsC, because Cmm is "macro-expanded"
-- relative to the old AbstractC, and many oddities/decorations have
-- disappeared from the data type.
--
-- This code generator is only supported in unregisterised mode.
--
-----------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}
module PprC (
        writeCs,
        pprStringInCStyle
  ) where

#include "HsVersions.h"

-- Cmm stuff
import BlockId
import CLabel
import ForeignCall
import Cmm hiding (pprBBlock)
import PprCmm ()
import Hoopl
import CmmUtils

-- Utils
import CPrim
import DynFlags
import FastString
import Outputable
import Platform
import UniqSet
import Unique
import Util

-- The rest
import Control.Monad.ST
import Data.Bits
import Data.Char
import Data.List
import Data.Map (Map)
import Data.Word
import System.IO
import qualified Data.Map as Map
import Control.Monad (liftM, ap)
import Control.Applicative (Applicative(..))

import qualified Data.Array.Unsafe as U ( castSTUArray )
import Data.Array.ST

-- --------------------------------------------------------------------------
-- Top level

pprCs :: DynFlags -> [RawCmmGroup] -> SDoc
pprCs dflags cmms
 = pprCode CStyle (vcat $ map (\c -> split_marker $$ pprC c) cmms)
 where
   split_marker
     | gopt Opt_SplitObjs dflags = ptext (sLit "__STG_SPLIT_MARKER")
     | otherwise                 = empty

writeCs :: DynFlags -> Handle -> [RawCmmGroup] -> IO ()
writeCs dflags handle cmms
  = printForC dflags handle (pprCs dflags cmms)

-- --------------------------------------------------------------------------
-- Now do some real work
--
-- for fun, we could call cmmToCmm over the tops...
--

pprC :: RawCmmGroup -> SDoc
pprC tops = vcat $ intersperse blankLine $ map pprTop tops

--
-- top level procs
--
pprTop :: RawCmmDecl -> SDoc
pprTop (CmmProc infos clbl _ graph) =

    (case mapLookup (g_entry graph) infos of
       Nothing -> empty
       Just (Statics info_clbl info_dat) -> pprDataExterns info_dat $$
                                            pprWordArray info_clbl info_dat) $$
    (vcat [
           blankLine,
           extern_decls,
           (if (externallyVisibleCLabel clbl)
                    then mkFN_ else mkIF_) (ppr clbl) <+> lbrace,
           nest 8 temp_decls,
           nest 8 mkFB_,
           vcat (map pprBBlock blocks),
           nest 8 mkFE_,
           rbrace ]
    )
  where
        blocks = toBlockListEntryFirst graph
        (temp_decls, extern_decls) = pprTempAndExternDecls blocks


-- Chunks of static data.

-- We only handle (a) arrays of word-sized things and (b) strings.

pprTop (CmmData _section (Statics lbl [CmmString str])) =
  hcat [
    pprLocalness lbl, ptext (sLit "char "), ppr lbl,
    ptext (sLit "[] = "), pprStringInCStyle str, semi
  ]

pprTop (CmmData _section (Statics lbl [CmmUninitialised size])) =
  hcat [
    pprLocalness lbl, ptext (sLit "char "), ppr lbl,
    brackets (int size), semi
  ]

pprTop (CmmData _section (Statics lbl lits)) =
  pprDataExterns lits $$
  pprWordArray lbl lits

-- --------------------------------------------------------------------------
-- BasicBlocks are self-contained entities: they always end in a jump.
--
-- Like nativeGen/AsmCodeGen, we could probably reorder blocks to turn
-- as many jumps as possible into fall throughs.
--

pprBBlock :: CmmBlock -> SDoc
pprBBlock block =
  nest 4 (pprBlockId lbl <> colon) $$
  nest 8 (vcat (map pprStmt (blockToList nodes)) $$ pprStmt last)
 where
  (CmmEntry lbl, nodes, last)  = blockSplit block

-- --------------------------------------------------------------------------
-- Info tables. Just arrays of words.
-- See codeGen/ClosureInfo, and nativeGen/PprMach

pprWordArray :: CLabel -> [CmmStatic] -> SDoc
pprWordArray lbl ds
  = sdocWithDynFlags $ \dflags ->
    hcat [ pprLocalness lbl, ptext (sLit "StgWord")
         , space, ppr lbl, ptext (sLit "[] = {") ]
    $$ nest 8 (commafy (pprStatics dflags ds))
    $$ ptext (sLit "};")

--
-- has to be static, if it isn't globally visible
--
pprLocalness :: CLabel -> SDoc
pprLocalness lbl | not $ externallyVisibleCLabel lbl = ptext (sLit "static ")
                 | otherwise = empty

-- --------------------------------------------------------------------------
-- Statements.
--

pprStmt :: CmmNode e x -> SDoc

pprStmt stmt =
    sdocWithDynFlags $ \dflags ->
    case stmt of
    CmmEntry _ -> empty
    CmmComment _ -> empty -- (hang (ptext (sLit "/*")) 3 (ftext s)) $$ ptext (sLit "*/")
                          -- XXX if the string contains "*/", we need to fix it
                          -- XXX we probably want to emit these comments when
                          -- some debugging option is on.  They can get quite
                          -- large.

    CmmAssign dest src -> pprAssign dflags dest src

    CmmStore  dest src
        | typeWidth rep == W64 && wordWidth dflags /= W64
        -> (if isFloatType rep then ptext (sLit "ASSIGN_DBL")
                               else ptext (sLit ("ASSIGN_Word64"))) <>
           parens (mkP_ <> pprExpr1 dest <> comma <> pprExpr src) <> semi

        | otherwise
        -> hsep [ pprExpr (CmmLoad dest rep), equals, pprExpr src <> semi ]
        where
          rep = cmmExprType dflags src

    CmmUnsafeForeignCall target@(ForeignTarget fn conv) results args ->
        fnCall
        where
        (res_hints, arg_hints) = foreignTargetHints target
        hresults = zip results res_hints
        hargs    = zip args arg_hints

        ForeignConvention cconv _ _ ret = conv

        cast_fn = parens (cCast (pprCFunType (char '*') cconv hresults hargs) fn)

        -- See wiki:Commentary/Compiler/Backends/PprC#Prototypes
        fnCall =
            case fn of
              CmmLit (CmmLabel lbl)
                | StdCallConv <- cconv ->
                    pprCall (ppr lbl) cconv hresults hargs
                        -- stdcall functions must be declared with
                        -- a function type, otherwise the C compiler
                        -- doesn't add the @n suffix to the label.  We
                        -- can't add the @n suffix ourselves, because
                        -- it isn't valid C.
                | CmmNeverReturns <- ret ->
                    pprCall cast_fn cconv hresults hargs <> semi
                | not (isMathFun lbl) ->
                    pprForeignCall (ppr lbl) cconv hresults hargs
              _ ->
                    pprCall cast_fn cconv hresults hargs <> semi
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
          -- The mem primops carry an extra alignment arg, must drop it.
          -- We could maybe emit an alignment directive using this info.
          -- We also need to cast mem primops to prevent conflicts with GCC
          -- builtins (see bug #5967).
          | op `elem` [MO_Memcpy, MO_Memset, MO_Memmove]
          = (ptext (sLit ";EF_(") <> fn <> char ')' <> semi) $$
            pprForeignCall fn cconv hresults (init hargs)
          | otherwise
          = pprCall fn cconv hresults hargs

    CmmBranch ident          -> pprBranch ident
    CmmCondBranch expr yes no -> pprCondBranch expr yes no
    CmmCall { cml_target = expr } -> mkJMP_ (pprExpr expr) <> semi
    CmmSwitch arg ids        -> sdocWithDynFlags $ \dflags ->
                                pprSwitch dflags arg ids

    _other -> pprPanic "PprC.pprStmt" (ppr stmt)

type Hinted a = (a, ForeignHint)

pprForeignCall :: SDoc -> CCallConv -> [Hinted CmmFormal] -> [Hinted CmmActual]
               -> SDoc
pprForeignCall fn cconv results args = fn_call
  where
    fn_call = braces (
                 pprCFunType (char '*' <> text "ghcFunPtr") cconv results args <> semi
              $$ text "ghcFunPtr" <+> equals <+> cast_fn <> semi
              $$ pprCall (text "ghcFunPtr") cconv results args <> semi
             )
    cast_fn = parens (parens (pprCFunType (char '*') cconv results args) <> fn)

pprCFunType :: SDoc -> CCallConv -> [Hinted CmmFormal] -> [Hinted CmmActual] -> SDoc
pprCFunType ppr_fn cconv ress args
  = sdocWithDynFlags $ \dflags ->
    let res_type [] = ptext (sLit "void")
        res_type [(one, hint)] = machRepHintCType (localRegType one) hint
        res_type _ = panic "pprCFunType: only void or 1 return value supported"

        arg_type (expr, hint) = machRepHintCType (cmmExprType dflags expr) hint
    in res_type ress <+>
       parens (ccallConvAttribute cconv <> ppr_fn) <>
       parens (commafy (map arg_type args))

-- ---------------------------------------------------------------------
-- unconditional branches
pprBranch :: BlockId -> SDoc
pprBranch ident = ptext (sLit "goto") <+> pprBlockId ident <> semi


-- ---------------------------------------------------------------------
-- conditional branches to local labels
pprCondBranch :: CmmExpr -> BlockId -> BlockId -> SDoc
pprCondBranch expr yes no
        = hsep [ ptext (sLit "if") , parens(pprExpr expr) ,
                        ptext (sLit "goto"), pprBlockId yes <> semi,
                        ptext (sLit "else goto"), pprBlockId no <> semi ]

-- ---------------------------------------------------------------------
-- a local table branch
--
-- we find the fall-through cases
--
-- N.B. we remove Nothing's from the list of branches, as they are
-- 'undefined'. However, they may be defined one day, so we better
-- document this behaviour.
--
pprSwitch :: DynFlags -> CmmExpr -> [ Maybe BlockId ] -> SDoc
pprSwitch dflags e maybe_ids
  = let pairs  = [ (ix, ident) | (ix,Just ident) <- zip [0..] maybe_ids ]
        pairs2 = [ (map fst as, snd (head as)) | as <- groupBy sndEq pairs ]
    in
        (hang (ptext (sLit "switch") <+> parens ( pprExpr e ) <+> lbrace)
                4 (vcat ( map caseify pairs2 )))
        $$ rbrace

  where
    sndEq (_,x) (_,y) = x == y

    -- fall through case
    caseify (ix:ixs, ident) = vcat (map do_fallthrough ixs) $$ final_branch ix
        where
        do_fallthrough ix =
                 hsep [ ptext (sLit "case") , pprHexVal ix (wordWidth dflags) <> colon ,
                        ptext (sLit "/* fall through */") ]

        final_branch ix =
                hsep [ ptext (sLit "case") , pprHexVal ix (wordWidth dflags) <> colon ,
                       ptext (sLit "goto") , (pprBlockId ident) <> semi ]

    caseify (_     , _    ) = panic "pprSwtich: swtich with no cases!"

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

pprExpr :: CmmExpr -> SDoc
pprExpr e = case e of
    CmmLit lit -> pprLit lit


    CmmLoad e ty -> sdocWithDynFlags $ \dflags -> pprLoad dflags e ty
    CmmReg reg      -> pprCastReg reg
    CmmRegOff reg 0 -> pprCastReg reg

    CmmRegOff reg i
        | i < 0 && negate_ok -> pprRegOff (char '-') (-i)
        | otherwise          -> pprRegOff (char '+') i
      where
        pprRegOff op i' = pprCastReg reg <> op <> int i'
        negate_ok = negate (fromIntegral i :: Integer) <
                    fromIntegral (maxBound::Int)
                     -- overflow is undefined; see #7620

    CmmMachOp mop args -> pprMachOpApp mop args

    CmmStackSlot _ _   -> panic "pprExpr: CmmStackSlot not supported!"


pprLoad :: DynFlags -> CmmExpr -> CmmType -> SDoc
pprLoad dflags e ty
  | width == W64, wordWidth dflags /= W64
  = (if isFloatType ty then ptext (sLit "PK_DBL")
                       else ptext (sLit "PK_Word64"))
    <> parens (mkP_ <> pprExpr1 e)

  | otherwise
  = case e of
        CmmReg r | isPtrReg r && width == wordWidth dflags && not (isFloatType ty)
                 -> char '*' <> pprAsPtrReg r

        CmmRegOff r 0 | isPtrReg r && width == wordWidth dflags && not (isFloatType ty)
                      -> char '*' <> pprAsPtrReg r

        CmmRegOff r off | isPtrReg r && width == wordWidth dflags
                        , off `rem` wORD_SIZE dflags == 0 && not (isFloatType ty)
        -- ToDo: check that the offset is a word multiple?
        --       (For tagging to work, I had to avoid unaligned loads. --ARY)
                        -> pprAsPtrReg r <> brackets (ppr (off `shiftR` wordShift dflags))

        _other -> cLoad e ty
  where
    width = typeWidth ty

pprExpr1 :: CmmExpr -> SDoc
pprExpr1 (CmmLit lit)     = pprLit1 lit
pprExpr1 e@(CmmReg _reg)  = pprExpr e
pprExpr1 other            = parens (pprExpr other)

-- --------------------------------------------------------------------------
-- MachOp applications

pprMachOpApp :: MachOp -> [CmmExpr] -> SDoc

pprMachOpApp op args
  | isMulMayOfloOp op
  = ptext (sLit "mulIntMayOflo") <> parens (commafy (map pprExpr args))
  where isMulMayOfloOp (MO_U_MulMayOflo _) = True
        isMulMayOfloOp (MO_S_MulMayOflo _) = True
        isMulMayOfloOp _ = False

pprMachOpApp mop args
  | Just ty <- machOpNeedsCast mop
  = ty <> parens (pprMachOpApp' mop args)
  | otherwise
  = pprMachOpApp' mop args

-- Comparisons in C have type 'int', but we want type W_ (this is what
-- resultRepOfMachOp says).  The other C operations inherit their type
-- from their operands, so no casting is required.
machOpNeedsCast :: MachOp -> Maybe SDoc
machOpNeedsCast mop
  | isComparisonMachOp mop = Just mkW_
  | otherwise              = Nothing

pprMachOpApp' :: MachOp -> [CmmExpr] -> SDoc
pprMachOpApp' mop args
 = case args of
    -- dyadic
    [x,y] -> pprArg x <+> pprMachOp_for_C mop <+> pprArg y

    -- unary
    [x]   -> pprMachOp_for_C mop <> parens (pprArg x)

    _     -> panic "PprC.pprMachOp : machop with wrong number of args"

  where
        -- Cast needed for signed integer ops
    pprArg e | signedOp    mop = sdocWithDynFlags $ \dflags ->
                                 cCast (machRep_S_CType (typeWidth (cmmExprType dflags e))) e
             | needsFCasts mop = sdocWithDynFlags $ \dflags ->
                                 cCast (machRep_F_CType (typeWidth (cmmExprType dflags e))) e
             | otherwise    = pprExpr1 e
    needsFCasts (MO_F_Eq _)   = False
    needsFCasts (MO_F_Ne _)   = False
    needsFCasts (MO_F_Neg _)  = True
    needsFCasts (MO_F_Quot _) = True
    needsFCasts mop  = floatComparison mop

-- --------------------------------------------------------------------------
-- Literals

pprLit :: CmmLit -> SDoc
pprLit lit = case lit of
    CmmInt i rep      -> pprHexVal i rep

    CmmFloat f w       -> parens (machRep_F_CType w) <> str
        where d = fromRational f :: Double
              str | isInfinite d && d < 0 = ptext (sLit "-INFINITY")
                  | isInfinite d          = ptext (sLit "INFINITY")
                  | isNaN d               = ptext (sLit "NAN")
                  | otherwise             = text (show d)
                -- these constants come from <math.h>
                -- see #1861

    CmmVec {} -> panic "PprC printing vector literal"

    CmmBlock bid       -> mkW_ <> pprCLabelAddr (infoTblLbl bid)
    CmmHighStackMark   -> panic "PprC printing high stack mark"
    CmmLabel clbl      -> mkW_ <> pprCLabelAddr clbl
    CmmLabelOff clbl i -> mkW_ <> pprCLabelAddr clbl <> char '+' <> int i
    CmmLabelDiffOff clbl1 _ i
        -- WARNING:
        --  * the lit must occur in the info table clbl2
        --  * clbl1 must be an SRT, a slow entry point or a large bitmap
        -> mkW_ <> pprCLabelAddr clbl1 <> char '+' <> int i

    where
        pprCLabelAddr lbl = char '&' <> ppr lbl

pprLit1 :: CmmLit -> SDoc
pprLit1 lit@(CmmLabelOff _ _) = parens (pprLit lit)
pprLit1 lit@(CmmLabelDiffOff _ _ _) = parens (pprLit lit)
pprLit1 lit@(CmmFloat _ _)    = parens (pprLit lit)
pprLit1 other = pprLit other

-- ---------------------------------------------------------------------------
-- Static data

pprStatics :: DynFlags -> [CmmStatic] -> [SDoc]
pprStatics _ [] = []
pprStatics dflags (CmmStaticLit (CmmFloat f W32) : rest)
  -- floats are padded to a word, see #1852
  | wORD_SIZE dflags == 8, CmmStaticLit (CmmInt 0 W32) : rest' <- rest
  = pprLit1 (floatToWord dflags f) : pprStatics dflags rest'
  | wORD_SIZE dflags == 4
  = pprLit1 (floatToWord dflags f) : pprStatics dflags rest
  | otherwise
  = pprPanic "pprStatics: float" (vcat (map ppr' rest))
    where ppr' (CmmStaticLit l) = sdocWithDynFlags $ \dflags ->
                                  ppr (cmmLitType dflags l)
          ppr' _other           = ptext (sLit "bad static!")
pprStatics dflags (CmmStaticLit (CmmFloat f W64) : rest)
  = map pprLit1 (doubleToWords dflags f) ++ pprStatics dflags rest
pprStatics dflags (CmmStaticLit (CmmInt i W64) : rest)
  | wordWidth dflags == W32
  = if wORDS_BIGENDIAN dflags
    then pprStatics dflags (CmmStaticLit (CmmInt q W32) :
                            CmmStaticLit (CmmInt r W32) : rest)
    else pprStatics dflags (CmmStaticLit (CmmInt r W32) :
                            CmmStaticLit (CmmInt q W32) : rest)
  where r = i .&. 0xffffffff
        q = i `shiftR` 32
pprStatics dflags (CmmStaticLit (CmmInt _ w) : _)
  | w /= wordWidth dflags
  = panic "pprStatics: cannot emit a non-word-sized static literal"
pprStatics dflags (CmmStaticLit lit : rest)
  = pprLit1 lit : pprStatics dflags rest
pprStatics _ (other : _)
  = pprPanic "pprWord" (pprStatic other)

pprStatic :: CmmStatic -> SDoc
pprStatic s = case s of

    CmmStaticLit lit   -> nest 4 (pprLit lit)
    CmmUninitialised i -> nest 4 (mkC_ <> brackets (int i))

    -- these should be inlined, like the old .hc
    CmmString s'       -> nest 4 (mkW_ <> parens(pprStringInCStyle s'))


-- ---------------------------------------------------------------------------
-- Block Ids

pprBlockId :: BlockId -> SDoc
pprBlockId b = char '_' <> ppr (getUnique b)

-- --------------------------------------------------------------------------
-- Print a MachOp in a way suitable for emitting via C.
--

pprMachOp_for_C :: MachOp -> SDoc

pprMachOp_for_C mop = case mop of

        -- Integer operations
        MO_Add          _ -> char '+'
        MO_Sub          _ -> char '-'
        MO_Eq           _ -> ptext (sLit "==")
        MO_Ne           _ -> ptext (sLit "!=")
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
        MO_S_Ge         _ -> ptext (sLit ">=")
        MO_S_Le         _ -> ptext (sLit "<=")
        MO_S_Gt         _ -> char '>'
        MO_S_Lt         _ -> char '<'

        -- & Unsigned comparisons
        MO_U_Ge         _ -> ptext (sLit ">=")
        MO_U_Le         _ -> ptext (sLit "<=")
        MO_U_Gt         _ -> char '>'
        MO_U_Lt         _ -> char '<'

        -- & Floating-point comparisons
        MO_F_Eq         _ -> ptext (sLit "==")
        MO_F_Ne         _ -> ptext (sLit "!=")
        MO_F_Ge         _ -> ptext (sLit ">=")
        MO_F_Le         _ -> ptext (sLit "<=")
        MO_F_Gt         _ -> char '>'
        MO_F_Lt         _ -> char '<'

        -- Bitwise operations.  Not all of these may be supported at all
        -- sizes, and only integral MachReps are valid.
        MO_And          _ -> char '&'
        MO_Or           _ -> char '|'
        MO_Xor          _ -> char '^'
        MO_Not          _ -> char '~'
        MO_Shl          _ -> ptext (sLit "<<")
        MO_U_Shr        _ -> ptext (sLit ">>") -- unsigned shift right
        MO_S_Shr        _ -> ptext (sLit ">>") -- signed shift right

-- Conversions.  Some of these will be NOPs, but never those that convert
-- between ints and floats.
-- Floating-point conversions use the signed variant.
-- We won't know to generate (void*) casts here, but maybe from
-- context elsewhere

-- noop casts
        MO_UU_Conv from to | from == to -> empty
        MO_UU_Conv _from to -> parens (machRep_U_CType to)

        MO_SS_Conv from to | from == to -> empty
        MO_SS_Conv _from to -> parens (machRep_S_CType to)

        MO_FF_Conv from to | from == to -> empty
        MO_FF_Conv _from to -> parens (machRep_F_CType to)

        MO_SF_Conv _from to -> parens (machRep_F_CType to)
        MO_FS_Conv _from to -> parens (machRep_S_CType to)
        
        MO_S_MulMayOflo _ -> pprTrace "offending mop:"
                                (ptext $ sLit "MO_S_MulMayOflo")
                                (panic $ "PprC.pprMachOp_for_C: MO_S_MulMayOflo"
                                      ++ " should have been handled earlier!")
        MO_U_MulMayOflo _ -> pprTrace "offending mop:"
                                (ptext $ sLit "MO_U_MulMayOflo")
                                (panic $ "PprC.pprMachOp_for_C: MO_U_MulMayOflo"
                                      ++ " should have been handled earlier!")

        MO_V_Insert {}    -> pprTrace "offending mop:"
                                (ptext $ sLit "MO_V_Insert")
                                (panic $ "PprC.pprMachOp_for_C: MO_V_Insert"
                                      ++ " should have been handled earlier!")
        MO_V_Extract {}   -> pprTrace "offending mop:"
                                (ptext $ sLit "MO_V_Extract")
                                (panic $ "PprC.pprMachOp_for_C: MO_V_Extract"
                                      ++ " should have been handled earlier!")

        MO_V_Add {}       -> pprTrace "offending mop:"
                                (ptext $ sLit "MO_V_Add")
                                (panic $ "PprC.pprMachOp_for_C: MO_V_Add"
                                      ++ " should have been handled earlier!")
        MO_V_Sub {}       -> pprTrace "offending mop:"
                                (ptext $ sLit "MO_V_Sub")
                                (panic $ "PprC.pprMachOp_for_C: MO_V_Sub"
                                      ++ " should have been handled earlier!")
        MO_V_Mul {}       -> pprTrace "offending mop:"
                                (ptext $ sLit "MO_V_Mul")
                                (panic $ "PprC.pprMachOp_for_C: MO_V_Mul"
                                      ++ " should have been handled earlier!")

        MO_VS_Quot {}     -> pprTrace "offending mop:"
                                (ptext $ sLit "MO_VS_Quot")
                                (panic $ "PprC.pprMachOp_for_C: MO_VS_Quot"
                                      ++ " should have been handled earlier!")
        MO_VS_Rem {}      -> pprTrace "offending mop:"
                                (ptext $ sLit "MO_VS_Rem")
                                (panic $ "PprC.pprMachOp_for_C: MO_VS_Rem"
                                      ++ " should have been handled earlier!")
        MO_VS_Neg {}      -> pprTrace "offending mop:"
                                (ptext $ sLit "MO_VS_Neg")
                                (panic $ "PprC.pprMachOp_for_C: MO_VS_Neg"
                                      ++ " should have been handled earlier!")

        MO_VU_Quot {}     -> pprTrace "offending mop:"
                                (ptext $ sLit "MO_VU_Quot")
                                (panic $ "PprC.pprMachOp_for_C: MO_VU_Quot"
                                      ++ " should have been handled earlier!")
        MO_VU_Rem {}      -> pprTrace "offending mop:"
                                (ptext $ sLit "MO_VU_Rem")
                                (panic $ "PprC.pprMachOp_for_C: MO_VU_Rem"
                                      ++ " should have been handled earlier!")

        MO_VF_Insert {}   -> pprTrace "offending mop:"
                                (ptext $ sLit "MO_VF_Insert")
                                (panic $ "PprC.pprMachOp_for_C: MO_VF_Insert"
                                      ++ " should have been handled earlier!")
        MO_VF_Extract {}  -> pprTrace "offending mop:"
                                (ptext $ sLit "MO_VF_Extract")
                                (panic $ "PprC.pprMachOp_for_C: MO_VF_Extract"
                                      ++ " should have been handled earlier!")

        MO_VF_Add {}      -> pprTrace "offending mop:"
                                (ptext $ sLit "MO_VF_Add")
                                (panic $ "PprC.pprMachOp_for_C: MO_VF_Add"
                                      ++ " should have been handled earlier!")
        MO_VF_Sub {}      -> pprTrace "offending mop:"
                                (ptext $ sLit "MO_VF_Sub")
                                (panic $ "PprC.pprMachOp_for_C: MO_VF_Sub"
                                      ++ " should have been handled earlier!")
        MO_VF_Neg {}      -> pprTrace "offending mop:"
                                (ptext $ sLit "MO_VF_Neg")
                                (panic $ "PprC.pprMachOp_for_C: MO_VF_Neg"
                                      ++ " should have been handled earlier!")
        MO_VF_Mul {}      -> pprTrace "offending mop:"
                                (ptext $ sLit "MO_VF_Mul")
                                (panic $ "PprC.pprMachOp_for_C: MO_VF_Mul"
                                      ++ " should have been handled earlier!")
        MO_VF_Quot {}     -> pprTrace "offending mop:"
                                (ptext $ sLit "MO_VF_Quot")
                                (panic $ "PprC.pprMachOp_for_C: MO_VF_Quot"
                                      ++ " should have been handled earlier!")

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
        MO_F64_Pwr      -> ptext (sLit "pow")
        MO_F64_Sin      -> ptext (sLit "sin")
        MO_F64_Cos      -> ptext (sLit "cos")
        MO_F64_Tan      -> ptext (sLit "tan")
        MO_F64_Sinh     -> ptext (sLit "sinh")
        MO_F64_Cosh     -> ptext (sLit "cosh")
        MO_F64_Tanh     -> ptext (sLit "tanh")
        MO_F64_Asin     -> ptext (sLit "asin")
        MO_F64_Acos     -> ptext (sLit "acos")
        MO_F64_Atan     -> ptext (sLit "atan")
        MO_F64_Log      -> ptext (sLit "log")
        MO_F64_Exp      -> ptext (sLit "exp")
        MO_F64_Sqrt     -> ptext (sLit "sqrt")
        MO_F32_Pwr      -> ptext (sLit "powf")
        MO_F32_Sin      -> ptext (sLit "sinf")
        MO_F32_Cos      -> ptext (sLit "cosf")
        MO_F32_Tan      -> ptext (sLit "tanf")
        MO_F32_Sinh     -> ptext (sLit "sinhf")
        MO_F32_Cosh     -> ptext (sLit "coshf")
        MO_F32_Tanh     -> ptext (sLit "tanhf")
        MO_F32_Asin     -> ptext (sLit "asinf")
        MO_F32_Acos     -> ptext (sLit "acosf")
        MO_F32_Atan     -> ptext (sLit "atanf")
        MO_F32_Log      -> ptext (sLit "logf")
        MO_F32_Exp      -> ptext (sLit "expf")
        MO_F32_Sqrt     -> ptext (sLit "sqrtf")
        MO_WriteBarrier -> ptext (sLit "write_barrier")
        MO_Memcpy       -> ptext (sLit "memcpy")
        MO_Memset       -> ptext (sLit "memset")
        MO_Memmove      -> ptext (sLit "memmove")
        (MO_BSwap w)    -> ptext (sLit $ bSwapLabel w)
        (MO_PopCnt w)   -> ptext (sLit $ popCntLabel w)
        (MO_UF_Conv w)  -> ptext (sLit $ word2FloatLabel w)

        MO_S_QuotRem  {} -> unsupported
        MO_U_QuotRem  {} -> unsupported
        MO_U_QuotRem2 {} -> unsupported
        MO_Add2       {} -> unsupported
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

mkJMP_ i = ptext (sLit "JMP_") <> parens i
mkFN_  i = ptext (sLit "FN_")  <> parens i -- externally visible function
mkIF_  i = ptext (sLit "IF_")  <> parens i -- locally visible


mkFB_, mkFE_ :: SDoc
mkFB_ = ptext (sLit "FB_") -- function code begin
mkFE_ = ptext (sLit "FE_") -- function code end

-- from includes/Stg.h
--
mkC_,mkW_,mkP_ :: SDoc

mkC_  = ptext (sLit "(C_)")        -- StgChar
mkW_  = ptext (sLit "(W_)")        -- StgWord
mkP_  = ptext (sLit "(P_)")        -- StgWord*

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
   | isPtrReg r1 && isPtrReg r2 && (off `rem` wORD_SIZE dflags == 0)
   = hcat [ pprAsPtrReg r1, equals, pprAsPtrReg r2, op, int off', semi ]
  where
        off1 = off `shiftR` wordShift dflags

        (op,off') | off >= 0  = (char '+', off1)
                  | otherwise = (char '-', -off1)

-- dest is a reg, rhs is anything.
-- We can't cast the lvalue, so we have to cast the rhs if necessary.  Casting
-- the lvalue elicits a warning from new GCC versions (3.4+).
pprAssign _ r1 r2
  | isFixedPtrReg r1             = mkAssign (mkP_ <> pprExpr1 r2)
  | Just ty <- strangeRegType r1 = mkAssign (parens ty <> pprExpr1 r2)
  | otherwise                    = mkAssign (pprExpr r2)
    where mkAssign x = if r1 == CmmGlobal BaseReg
                       then ptext (sLit "ASSIGN_BaseReg") <> parens x <> semi
                       else pprReg r1 <> ptext (sLit " = ") <> x <> semi

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
strangeRegType (CmmGlobal CCCS) = Just (ptext (sLit "struct CostCentreStack_ *"))
strangeRegType (CmmGlobal CurrentTSO) = Just (ptext (sLit "struct StgTSO_ *"))
strangeRegType (CmmGlobal CurrentNursery) = Just (ptext (sLit "struct bdescr_ *"))
strangeRegType (CmmGlobal BaseReg) = Just (ptext (sLit "struct StgRegTable_ *"))
strangeRegType _ = Nothing

-- pprReg just prints the register name.
--
pprReg :: CmmReg -> SDoc
pprReg r = case r of
        CmmLocal  local  -> pprLocalReg local
        CmmGlobal global -> pprGlobalReg global

pprAsPtrReg :: CmmReg -> SDoc
pprAsPtrReg (CmmGlobal (VanillaReg n gcp))
  = WARN( gcp /= VGcPtr, ppr n ) char 'R' <> int n <> ptext (sLit ".p")
pprAsPtrReg other_reg = pprReg other_reg

pprGlobalReg :: GlobalReg -> SDoc
pprGlobalReg gr = case gr of
    VanillaReg n _ -> char 'R' <> int n  <> ptext (sLit ".w")
        -- pprGlobalReg prints a VanillaReg as a .w regardless
        -- Example:     R1.w = R1.w & (-0x8UL);
        --              JMP_(*R1.p);
    FloatReg   n   -> char 'F' <> int n
    DoubleReg  n   -> char 'D' <> int n
    LongReg    n   -> char 'L' <> int n
    Sp             -> ptext (sLit "Sp")
    SpLim          -> ptext (sLit "SpLim")
    Hp             -> ptext (sLit "Hp")
    HpLim          -> ptext (sLit "HpLim")
    CCCS           -> ptext (sLit "CCCS")
    CurrentTSO     -> ptext (sLit "CurrentTSO")
    CurrentNursery -> ptext (sLit "CurrentNursery")
    HpAlloc        -> ptext (sLit "HpAlloc")
    BaseReg        -> ptext (sLit "BaseReg")
    EagerBlackholeInfo -> ptext (sLit "stg_EAGER_BLACKHOLE_info")
    GCEnter1       -> ptext (sLit "stg_gc_enter_1")
    GCFun          -> ptext (sLit "stg_gc_fun")
    other          -> panic $ "pprGlobalReg: Unsupported register: " ++ show other

pprLocalReg :: LocalReg -> SDoc
pprLocalReg (LocalReg uniq _) = char '_' <> ppr uniq

-- -----------------------------------------------------------------------------
-- Foreign Calls

pprCall :: SDoc -> CCallConv -> [Hinted CmmFormal] -> [Hinted CmmActual] -> SDoc
pprCall ppr_fn cconv results args
  | not (is_cishCC cconv)
  = panic $ "pprCall: unknown calling convention"

  | otherwise
  =
    ppr_assign results (ppr_fn <> parens (commafy (map pprArg args))) <> semi
  where
     ppr_assign []           rhs = rhs
     ppr_assign [(one,hint)] rhs
         = pprLocalReg one <> ptext (sLit " = ")
                 <> pprUnHint hint (localRegType one) <> rhs
     ppr_assign _other _rhs = panic "pprCall: multiple results"

     pprArg (expr, AddrHint)
        = cCast (ptext (sLit "void *")) expr
        -- see comment by machRepHintCType below
     pprArg (expr, SignedHint)
        = sdocWithDynFlags $ \dflags ->
          cCast (machRep_S_CType $ typeWidth $ cmmExprType dflags expr) expr
     pprArg (expr, _other)
        = pprExpr expr

     pprUnHint AddrHint   rep = parens (machRepCType rep)
     pprUnHint SignedHint rep = parens (machRepCType rep)
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
pprTempAndExternDecls :: [CmmBlock] -> (SDoc{-temps-}, SDoc{-externs-})
pprTempAndExternDecls stmts
  = (vcat (map pprTempDecl (uniqSetToList temps)),
     vcat (map (pprExternDecl False{-ToDo-}) (Map.keys lbls)))
  where (temps, lbls) = runTE (mapM_ te_BB stmts)

pprDataExterns :: [CmmStatic] -> SDoc
pprDataExterns statics
  = vcat (map (pprExternDecl False{-ToDo-}) (Map.keys lbls))
  where (_, lbls) = runTE (mapM_ te_Static statics)

pprTempDecl :: LocalReg -> SDoc
pprTempDecl l@(LocalReg _ rep)
  = hcat [ machRepCType rep, space, pprLocalReg l, semi ]

pprExternDecl :: Bool -> CLabel -> SDoc
pprExternDecl _in_srt lbl
  -- do not print anything for "known external" things
  | not (needsCDecl lbl) = empty
  | Just sz <- foreignLabelStdcallInfo lbl = stdcall_decl sz
  | otherwise =
        hcat [ visibility, label_type lbl,
               lparen, ppr lbl, text ");" ]
 where
  label_type lbl | isCFunctionLabel lbl = ptext (sLit "F_")
                 | otherwise            = ptext (sLit "I_")

  visibility
     | externallyVisibleCLabel lbl = char 'E'
     | otherwise                   = char 'I'

  -- If the label we want to refer to is a stdcall function (on Windows) then
  -- we must generate an appropriate prototype for it, so that the C compiler will
  -- add the @n suffix to the label (#2276)
  stdcall_decl sz = sdocWithDynFlags $ \dflags ->
        ptext (sLit "extern __attribute__((stdcall)) void ") <> ppr lbl
        <> parens (commafy (replicate (sz `quot` wORD_SIZE dflags) (machRep_U_CType (wordWidth dflags))))
        <> semi

type TEState = (UniqSet LocalReg, Map CLabel ())
newtype TE a = TE { unTE :: TEState -> (a, TEState) }

instance Functor TE where
      fmap = liftM

instance Applicative TE where
      pure = return
      (<*>) = ap

instance Monad TE where
   TE m >>= k  = TE $ \s -> case m s of (a, s') -> unTE (k a) s'
   return a    = TE $ \s -> (a, s)

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
te_Lit (CmmLabelDiffOff l1 _ _) = te_lbl l1
te_Lit _ = return ()

te_Stmt :: CmmNode e x -> TE ()
te_Stmt (CmmAssign r e)         = te_Reg r >> te_Expr e
te_Stmt (CmmStore l r)          = te_Expr l >> te_Expr r
te_Stmt (CmmUnsafeForeignCall target rs es)
  = do  te_Target target
        mapM_ te_temp rs
        mapM_ te_Expr es
te_Stmt (CmmCondBranch e _ _)   = te_Expr e
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

cCast :: SDoc -> CmmExpr -> SDoc
cCast ty expr = parens ty <> pprExpr1 expr

cLoad :: CmmExpr -> CmmType -> SDoc
cLoad expr rep
    = sdocWithPlatform $ \platform ->
      if bewareLoadStoreAlignment (platformArch platform)
      then let decl = machRepCType rep <+> ptext (sLit "x") <> semi
               struct = ptext (sLit "struct") <+> braces (decl)
               packed_attr = ptext (sLit "__attribute__((packed))")
               cast = parens (struct <+> packed_attr <> char '*')
           in parens (cast <+> pprExpr1 expr) <> ptext (sLit "->x")
      else char '*' <> parens (cCast (machRepPtrCType rep) expr)
    where -- On these platforms, unaligned loads are known to cause problems
          bewareLoadStoreAlignment ArchAlpha    = True
          bewareLoadStoreAlignment ArchMipseb   = True
          bewareLoadStoreAlignment ArchMipsel   = True
          bewareLoadStoreAlignment (ArchARM {}) = True
          -- Pessimistically assume that they will also cause problems
          -- on unknown arches
          bewareLoadStoreAlignment ArchUnknown  = True
          bewareLoadStoreAlignment _            = False

isCmmWordType :: DynFlags -> CmmType -> Bool
-- True of GcPtrReg/NonGcReg of native word size
isCmmWordType dflags ty = not (isFloatType ty)
                       && typeWidth ty == wordWidth dflags

-- This is for finding the types of foreign call arguments.  For a pointer
-- argument, we always cast the argument to (void *), to avoid warnings from
-- the C compiler.
machRepHintCType :: CmmType -> ForeignHint -> SDoc
machRepHintCType _   AddrHint   = ptext (sLit "void *")
machRepHintCType rep SignedHint = machRep_S_CType (typeWidth rep)
machRepHintCType rep _other     = machRepCType rep

machRepPtrCType :: CmmType -> SDoc
machRepPtrCType r
 = sdocWithDynFlags $ \dflags ->
   if isCmmWordType dflags r then ptext (sLit "P_")
                             else machRepCType r <> char '*'

machRepCType :: CmmType -> SDoc
machRepCType ty | isFloatType ty = machRep_F_CType w
                | otherwise      = machRep_U_CType w
                where
                  w = typeWidth ty

machRep_F_CType :: Width -> SDoc
machRep_F_CType W32 = ptext (sLit "StgFloat") -- ToDo: correct?
machRep_F_CType W64 = ptext (sLit "StgDouble")
machRep_F_CType _   = panic "machRep_F_CType"

machRep_U_CType :: Width -> SDoc
machRep_U_CType w
 = sdocWithDynFlags $ \dflags ->
   case w of
   _ | w == wordWidth dflags -> ptext (sLit "W_")
   W8  -> ptext (sLit "StgWord8")
   W16 -> ptext (sLit "StgWord16")
   W32 -> ptext (sLit "StgWord32")
   W64 -> ptext (sLit "StgWord64")
   _   -> panic "machRep_U_CType"

machRep_S_CType :: Width -> SDoc
machRep_S_CType w
 = sdocWithDynFlags $ \dflags ->
   case w of
   _ | w == wordWidth dflags -> ptext (sLit "I_")
   W8  -> ptext (sLit "StgInt8")
   W16 -> ptext (sLit "StgInt16")
   W32 -> ptext (sLit "StgInt32")
   W64 -> ptext (sLit "StgInt64")
   _   -> panic "machRep_S_CType"


-- ---------------------------------------------------------------------
-- print strings as valid C strings

pprStringInCStyle :: [Word8] -> SDoc
pprStringInCStyle s = doubleQuotes (text (concatMap charToC s))

-- ---------------------------------------------------------------------------
-- Initialising static objects with floating-point numbers.  We can't
-- just emit the floating point number, because C will cast it to an int
-- by rounding it.  We want the actual bit-representation of the float.

-- This is a hack to turn the floating point numbers into ints that we
-- can safely initialise to static locations.

big_doubles :: DynFlags -> Bool
big_doubles dflags
  | widthInBytes W64 == 2 * wORD_SIZE dflags = True
  | widthInBytes W64 == wORD_SIZE dflags     = False
  | otherwise = panic "big_doubles"

castFloatToIntArray :: STUArray s Int Float -> ST s (STUArray s Int Int)
castFloatToIntArray = U.castSTUArray

castDoubleToIntArray :: STUArray s Int Double -> ST s (STUArray s Int Int)
castDoubleToIntArray = U.castSTUArray

-- floats are always 1 word
floatToWord :: DynFlags -> Rational -> CmmLit
floatToWord dflags r
  = runST (do
        arr <- newArray_ ((0::Int),0)
        writeArray arr 0 (fromRational r)
        arr' <- castFloatToIntArray arr
        i <- readArray arr' 0
        return (CmmInt (toInteger i) (wordWidth dflags))
    )

doubleToWords :: DynFlags -> Rational -> [CmmLit]
doubleToWords dflags r
  | big_doubles dflags                  -- doubles are 2 words
  = runST (do
        arr <- newArray_ ((0::Int),1)
        writeArray arr 0 (fromRational r)
        arr' <- castDoubleToIntArray arr
        i1 <- readArray arr' 0
        i2 <- readArray arr' 1
        return [ CmmInt (toInteger i1) (wordWidth dflags)
               , CmmInt (toInteger i2) (wordWidth dflags)
               ]
    )
  | otherwise                           -- doubles are 1 word
  = runST (do
        arr <- newArray_ ((0::Int),0)
        writeArray arr 0 (fromRational r)
        arr' <- castDoubleToIntArray arr
        i <- readArray arr' 0
        return [ CmmInt (toInteger i) (wordWidth dflags) ]
    )

-- ---------------------------------------------------------------------------
-- Utils

wordShift :: DynFlags -> Int
wordShift dflags = widthInLog (wordWidth dflags)

commafy :: [SDoc] -> SDoc
commafy xs = hsep $ punctuate comma xs

-- Print in C hex format: 0x13fa
pprHexVal :: Integer -> Width -> SDoc
pprHexVal 0 _ = ptext (sLit "0x0")
pprHexVal w rep
  | w < 0     = parens (char '-' <> ptext (sLit "0x") <> go (-w) <> repsuffix rep)
  | otherwise = ptext (sLit "0x") <> go w <> repsuffix rep
  where
        -- type suffix for literals:
        -- Integer literals are unsigned in Cmm/C.  We explicitly cast to
        -- signed values for doing signed operations, but at all other
        -- times values are unsigned.  This also helps eliminate occasional
        -- warnings about integer overflow from gcc.

      repsuffix W64 = sdocWithDynFlags $ \dflags ->
               if cINT_SIZE       dflags == 8 then char 'U'
          else if cLONG_SIZE      dflags == 8 then ptext (sLit "UL")
          else if cLONG_LONG_SIZE dflags == 8 then ptext (sLit "ULL")
          else panic "pprHexVal: Can't find a 64-bit type"
      repsuffix _ = char 'U'

      go 0 = empty
      go w' = go q <> dig
           where
             (q,r) = w' `quotRem` 16
             dig | r < 10    = char (chr (fromInteger r + ord '0'))
                 | otherwise = char (chr (fromInteger r - 10 + ord 'a'))

