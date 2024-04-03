{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module GHC.CmmToAsm.Wasm.FromCmm
  ( alignmentFromWordType,
    globalInfoFromCmmGlobalReg,
    supportedCmmGlobalRegs,
    onCmmGroup,
  )
where

import Control.Monad
import qualified Data.ByteString as BS
import Data.Foldable
import Data.Functor
import Data.Semigroup
import Data.String
import Data.Traversable
import Data.Type.Equality
import GHC.Cmm
import GHC.Cmm.BlockId
import GHC.Cmm.CLabel
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Label
import GHC.Cmm.InitFini
import GHC.CmmToAsm.Wasm.Types
import GHC.CmmToAsm.Wasm.Utils
import GHC.Float
import GHC.Platform
import GHC.Prelude
import GHC.StgToCmm.CgUtils
import GHC.Types.Basic
import GHC.Types.ForeignCall
import GHC.Types.Unique
import GHC.Types.Unique.FM
import GHC.Types.Unique.Map
import GHC.Types.Unique.Set
import GHC.Types.Unique.Supply
import GHC.Utils.Outputable hiding ((<>))
import GHC.Utils.Panic
import GHC.Wasm.ControlFlow.FromCmm

-- | Calculate the wasm representation type from a 'CmmType'. This is
-- a lossy conversion, and sometimes we need to pass the original
-- 'CmmType' or at least its 'Width' around, so to properly add
-- subword truncation or extension logic.
someWasmTypeFromCmmType :: CmmType -> SomeWasmType
someWasmTypeFromCmmType t
  | isWord32 t = SomeWasmType TagI32
  | isWord64 t = SomeWasmType TagI64
  | t `cmmEqType` b16 = SomeWasmType TagI32
  | t `cmmEqType` b8 = SomeWasmType TagI32
  | isFloat64 t = SomeWasmType TagF64
  | isFloat32 t = SomeWasmType TagF32
  | otherwise =
      panic $
        "someWasmTypeFromCmmType: unsupported CmmType "
          <> showSDocOneLine defaultSDocContext (ppr t)

-- | Calculate the optional memory narrowing of a 'CmmLoad' or
-- 'CmmStore'.
wasmMemoryNarrowing :: WasmTypeTag t -> CmmType -> Maybe Int
wasmMemoryNarrowing ty ty_cmm = case (# ty, typeWidth ty_cmm #) of
  (# TagI32, W8 #) -> Just 8
  (# TagI32, W16 #) -> Just 16
  (# TagI32, W32 #) -> Nothing
  (# TagI64, W8 #) -> Just 8
  (# TagI64, W16 #) -> Just 16
  (# TagI64, W32 #) -> Just 32
  (# TagI64, W64 #) -> Nothing
  (# TagF32, W32 #) -> Nothing
  (# TagF64, W64 #) -> Nothing
  _ -> panic "wasmMemoryNarrowing: unreachable"

-- | Despite this is used by the WebAssembly native codegen, we use
-- 'pprCLabel' instead of 'pprAsmLabel' when emitting the textual
-- symbol name. Either one would work, but 'pprCLabel' makes the
-- output assembly code looks closer to the unregisterised codegen
-- output, which can be handy when using the unregisterised codegen as
-- a source of truth when debugging the native codegen.
symNameFromCLabel :: CLabel -> SymName
symNameFromCLabel lbl =
  fromString $
    showSDocOneLine defaultSDocContext {sdocStyle = PprCode} $
      pprCLabel genericPlatform lbl

-- | Calculate a symbol's visibility.
symVisibilityFromCLabel :: CLabel -> SymVisibility
symVisibilityFromCLabel lbl
  | externallyVisibleCLabel lbl = SymDefault
  | otherwise = SymStatic

-- | Calculate a symbol's kind, see haddock docs of 'SymKind' for more
-- explanation.
symKindFromCLabel :: CLabel -> SymKind
symKindFromCLabel lbl
  | isCFunctionLabel lbl = SymFunc
  | otherwise = SymData

-- | Calculate a data section's kind, see haddock docs of
-- 'DataSectionKind' for more explanation.
dataSectionKindFromCmmSection :: Section -> DataSectionKind
dataSectionKindFromCmmSection s = case sectionProtection s of
  ReadWriteSection -> SectionData
  _ -> SectionROData

-- | Calculate the natural alignment size given the platform word
-- type.
alignmentFromWordType :: WasmTypeTag w -> Alignment
alignmentFromWordType TagI32 = mkAlignment 4
alignmentFromWordType TagI64 = mkAlignment 8
alignmentFromWordType _ = panic "alignmentFromWordType: unreachable"

-- | Calculate a data section's alignment. As a conservative
-- optimization, a data section with a single CmmString/CmmFileEmbed
-- has no alignment requirement, otherwise we always align to the word
-- size to satisfy pointer tagging requirements and avoid unaligned
-- loads/stores.
alignmentFromCmmSection :: WasmTypeTag w -> [DataSectionContent] -> Alignment
alignmentFromCmmSection _ [DataASCII {}] = mkAlignment 1
alignmentFromCmmSection _ [DataIncBin {}] = mkAlignment 1
alignmentFromCmmSection t _ = alignmentFromWordType t

-- | Lower a 'CmmStatic'.
lower_CmmStatic :: CmmStatic -> WasmCodeGenM w DataSectionContent
lower_CmmStatic s = case s of
  CmmStaticLit (CmmInt i W8) -> pure $ DataI8 $ fromInteger $ narrowU W8 i
  CmmStaticLit (CmmInt i W16) -> pure $ DataI16 $ fromInteger $ narrowU W16 i
  CmmStaticLit (CmmInt i W32) -> pure $ DataI32 $ fromInteger $ narrowU W32 i
  CmmStaticLit (CmmInt i W64) -> pure $ DataI64 $ fromInteger $ narrowU W64 i
  CmmStaticLit (CmmFloat f W32) -> pure $ DataF32 $ fromRational f
  CmmStaticLit (CmmFloat d W64) -> pure $ DataF64 $ fromRational d
  CmmStaticLit (CmmLabel lbl) ->
    onAnySym lbl
      $> DataSym
        (symNameFromCLabel lbl)
        0
  CmmStaticLit (CmmLabelOff lbl o) ->
    onAnySym lbl
      $> DataSym
        (symNameFromCLabel lbl)
        o
  CmmUninitialised i -> pure $ DataSkip i
  CmmString b -> pure $ DataASCII b
  CmmFileEmbed f l -> pure $ DataIncBin f l
  _ -> panic "lower_CmmStatic: unreachable"

{-
Note [Register mapping on WebAssembly]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Unlike typical ISAs, WebAssembly doesn't expose a fixed set of
registers. For now, we map each Cmm LocalReg to a wasm local, and each
Cmm GlobalReg to a wasm global. The wasm globals are defined in
rts/wasm/Wasm.S, and must be kept in sync with
'globalInfoFromCmmGlobalReg' and 'supportedCmmGlobalRegs' here.

There are some other Cmm GlobalRegs which are still represented by
StgRegTable fields instead of wasm globals (e.g. HpAlloc). It's cheap
to add wasm globals, but other parts of rts logic only work with the
StgRegTable fields, so we also need to instrument StgRun/StgReturn to
sync the wasm globals with the StgRegTable. It's not really worth the
trouble.

-}
globalInfoFromCmmGlobalReg :: WasmTypeTag w -> GlobalReg -> Maybe GlobalInfo
globalInfoFromCmmGlobalReg t reg = case reg of
  VanillaReg i
    | i >= 1 && i <= 10 -> Just (fromString $ "__R" <> show i, ty_word)
  FloatReg i
    | i >= 1 && i <= 6 ->
        Just (fromString $ "__F" <> show i, SomeWasmType TagF32)
  DoubleReg i
    | i >= 1 && i <= 6 ->
        Just (fromString $ "__D" <> show i, SomeWasmType TagF64)
  LongReg i
    | i == 1 -> Just (fromString $ "__L" <> show i, SomeWasmType TagI64)
  Sp -> Just ("__Sp", ty_word)
  SpLim -> Just ("__SpLim", ty_word)
  Hp -> Just ("__Hp", ty_word)
  HpLim -> Just ("__HpLim", ty_word)
  _ -> Nothing
  where
    ty_word = SomeWasmType t

supportedCmmGlobalRegs :: [GlobalReg]
supportedCmmGlobalRegs =
  [VanillaReg i | i <- [1 .. 10]]
    <> [FloatReg i | i <- [1 .. 6]]
    <> [DoubleReg i | i <- [1 .. 6]]
    <> [LongReg i | i <- [1 .. 1]]
    <> [Sp, SpLim, Hp, HpLim]

-- | Truncate a subword.
truncSubword :: Width -> WasmTypeTag t -> WasmExpr w t -> WasmExpr w t
truncSubword W8 ty (WasmExpr instr) =
  WasmExpr $ instr `WasmConcat` WasmConst ty 0xFF `WasmConcat` WasmAnd ty
truncSubword W16 ty (WasmExpr instr) =
  WasmExpr $ instr `WasmConcat` WasmConst ty 0xFFFF `WasmConcat` WasmAnd ty
truncSubword _ _ expr = expr

-- | Sign-extend a subword.
extendSubword :: Width -> WasmTypeTag t -> WasmExpr w t -> WasmExpr w t
extendSubword W8 TagI32 (WasmExpr instr) =
  WasmExpr $ instr `WasmConcat` WasmI32Extend8S
extendSubword W16 TagI32 (WasmExpr instr) =
  WasmExpr $ instr `WasmConcat` WasmI32Extend16S
extendSubword W8 TagI64 (WasmExpr instr) =
  WasmExpr $ instr `WasmConcat` WasmI64Extend8S
extendSubword W16 TagI64 (WasmExpr instr) =
  WasmExpr $ instr `WasmConcat` WasmI64Extend16S
extendSubword W32 TagI64 (WasmExpr instr) =
  WasmExpr $ instr `WasmConcat` WasmI64Extend32S
extendSubword _ _ expr = expr

-- | Lower an unary homogeneous operation.
lower_MO_Un_Homo ::
  ( forall pre t.
    WasmTypeTag t ->
    WasmInstr
      w
      (t : pre)
      (t : pre)
  ) ->
  CLabel ->
  CmmType ->
  [CmmExpr] ->
  WasmCodeGenM w (SomeWasmExpr w)
lower_MO_Un_Homo op lbl t0 [x] = case someWasmTypeFromCmmType t0 of
  SomeWasmType ty -> do
    WasmExpr x_instr <- lower_CmmExpr_Typed lbl ty x
    pure $
      SomeWasmExpr ty $
        WasmExpr $
          x_instr `WasmConcat` op ty
lower_MO_Un_Homo _ _ _ _ = panic "lower_MO_Un_Homo: unreachable"

-- | Lower a binary homogeneous operation. Homogeneous: result type is
-- the same with operand types.
lower_MO_Bin_Homo ::
  ( forall pre t.
    WasmTypeTag t ->
    WasmInstr
      w
      (t : t : pre)
      (t : pre)
  ) ->
  CLabel ->
  CmmType ->
  [CmmExpr] ->
  WasmCodeGenM w (SomeWasmExpr w)
lower_MO_Bin_Homo op lbl t0 [x, y] = case someWasmTypeFromCmmType t0 of
  SomeWasmType ty -> do
    WasmExpr x_instr <- lower_CmmExpr_Typed lbl ty x
    WasmExpr y_instr <- lower_CmmExpr_Typed lbl ty y
    pure $
      SomeWasmExpr ty $
        WasmExpr $
          x_instr `WasmConcat` y_instr `WasmConcat` op ty
lower_MO_Bin_Homo _ _ _ _ = panic "lower_MO_Bin_Homo: unreachable"

-- | Lower a binary homogeneous operation, and truncate the result if
-- it's a subword.
lower_MO_Bin_Homo_Trunc ::
  (forall pre t. WasmTypeTag t -> WasmInstr w (t : t : pre) (t : pre)) ->
  CLabel ->
  Width ->
  [CmmExpr] ->
  WasmCodeGenM w (SomeWasmExpr w)
lower_MO_Bin_Homo_Trunc op lbl w0 [x, y] =
  case someWasmTypeFromCmmType (cmmBits w0) of
    SomeWasmType ty -> do
      WasmExpr x_instr <- lower_CmmExpr_Typed lbl ty x
      WasmExpr y_instr <- lower_CmmExpr_Typed lbl ty y
      pure $
        SomeWasmExpr ty $
          truncSubword w0 ty $
            WasmExpr $
              x_instr `WasmConcat` y_instr `WasmConcat` op ty
lower_MO_Bin_Homo_Trunc _ _ _ _ = panic "lower_MO_Bin_Homo_Trunc: unreachable"

-- | Lower a binary homogeneous operation, first sign extending the
-- operands, then truncating the result.
lower_MO_Bin_Homo_Ext_Trunc ::
  (forall pre t. WasmTypeTag t -> WasmInstr w (t : t : pre) (t : pre)) ->
  CLabel ->
  Width ->
  [CmmExpr] ->
  WasmCodeGenM w (SomeWasmExpr w)
lower_MO_Bin_Homo_Ext_Trunc op lbl w0 [x, y] =
  case someWasmTypeFromCmmType (cmmBits w0) of
    SomeWasmType ty -> do
      WasmExpr x_instr <-
        extendSubword w0 ty <$> lower_CmmExpr_Typed lbl ty x
      WasmExpr y_instr <-
        extendSubword w0 ty <$> lower_CmmExpr_Typed lbl ty y
      pure $
        SomeWasmExpr ty $
          truncSubword w0 ty $
            WasmExpr $
              x_instr `WasmConcat` y_instr `WasmConcat` op ty
lower_MO_Bin_Homo_Ext_Trunc _ _ _ _ =
  panic "lower_MO_Bin_Homo_Ext_Trunc: unreachable"

-- | Lower a relational binary operation, first sign extending the
-- operands. Relational: result type is a boolean (word type).
lower_MO_Bin_Rel_Ext ::
  (forall pre t. WasmTypeTag t -> WasmInstr w (t : t : pre) (w : pre)) ->
  CLabel ->
  Width ->
  [CmmExpr] ->
  WasmCodeGenM w (SomeWasmExpr w)
lower_MO_Bin_Rel_Ext op lbl w0 [x, y] =
  case someWasmTypeFromCmmType (cmmBits w0) of
    SomeWasmType ty -> do
      WasmExpr x_instr <-
        extendSubword w0 ty <$> lower_CmmExpr_Typed lbl ty x
      WasmExpr y_instr <-
        extendSubword w0 ty <$> lower_CmmExpr_Typed lbl ty y
      ty_word <- wasmWordTypeM
      pure $
        SomeWasmExpr ty_word $
          WasmExpr $
            x_instr `WasmConcat` y_instr `WasmConcat` op ty
lower_MO_Bin_Rel_Ext _ _ _ _ = panic "lower_MO_Bin_Rel_Ext: unreachable"

-- | Lower a relational binary operation.
lower_MO_Bin_Rel ::
  ( forall pre t.
    WasmTypeTag t ->
    WasmInstr
      w
      (t : t : pre)
      (w : pre)
  ) ->
  CLabel ->
  CmmType ->
  [CmmExpr] ->
  WasmCodeGenM w (SomeWasmExpr w)
lower_MO_Bin_Rel op lbl t0 [x, y] = case someWasmTypeFromCmmType t0 of
  SomeWasmType ty -> do
    WasmExpr x_instr <- lower_CmmExpr_Typed lbl ty x
    WasmExpr y_instr <- lower_CmmExpr_Typed lbl ty y
    ty_word <- wasmWordTypeM
    pure $
      SomeWasmExpr ty_word $
        WasmExpr $
          x_instr `WasmConcat` y_instr `WasmConcat` op ty
lower_MO_Bin_Rel _ _ _ _ = panic "lower_MO_Bin_Rel: unreachable"

-- | Cast a shiftL/shiftR RHS to the same type as LHS. Because we may
-- have a 64-bit LHS and 32-bit RHS, but wasm shift operators are
-- homogeneous.
shiftRHSCast ::
  CLabel ->
  WasmTypeTag t ->
  CmmExpr ->
  WasmCodeGenM
    w
    (WasmExpr w t)
shiftRHSCast lbl t1 x = do
  SomeWasmExpr t0 (WasmExpr x_instr) <- lower_CmmExpr lbl x
  if
      | Just Refl <- t0 `testEquality` t1 -> pure $ WasmExpr x_instr
      | TagI32 <- t0,
        TagI64 <- t1 ->
          pure $ WasmExpr $ x_instr `WasmConcat` WasmI64ExtendI32 Unsigned
      | otherwise -> panic "shiftRHSCast: unreachable"

-- | Lower a 'MO_Shl' operation, truncating the result.
lower_MO_Shl ::
  CLabel ->
  Width ->
  [CmmExpr] ->
  WasmCodeGenM
    w
    (SomeWasmExpr w)
lower_MO_Shl lbl w0 [x, y] = case someWasmTypeFromCmmType (cmmBits w0) of
  SomeWasmType ty -> do
    WasmExpr x_instr <- lower_CmmExpr_Typed lbl ty x
    WasmExpr y_instr <- shiftRHSCast lbl ty y
    pure $
      SomeWasmExpr ty $
        truncSubword w0 ty $
          WasmExpr $
            x_instr `WasmConcat` y_instr `WasmConcat` WasmShl ty
lower_MO_Shl _ _ _ = panic "lower_MO_Shl: unreachable"

-- | Lower a 'MO_U_Shr' operation.
lower_MO_U_Shr ::
  CLabel ->
  Width ->
  [CmmExpr] ->
  WasmCodeGenM
    w
    (SomeWasmExpr w)
lower_MO_U_Shr lbl w0 [x, y] = case someWasmTypeFromCmmType (cmmBits w0) of
  SomeWasmType ty -> do
    WasmExpr x_instr <- lower_CmmExpr_Typed lbl ty x
    WasmExpr y_instr <- shiftRHSCast lbl ty y
    pure $
      SomeWasmExpr ty $
        WasmExpr $
          x_instr `WasmConcat` y_instr `WasmConcat` WasmShr Unsigned ty
lower_MO_U_Shr _ _ _ = panic "lower_MO_U_Shr: unreachable"

-- | Lower a 'MO_S_Shr' operation, first sign-extending the LHS, then
-- truncating the result.
lower_MO_S_Shr ::
  CLabel ->
  Width ->
  [CmmExpr] ->
  WasmCodeGenM
    w
    (SomeWasmExpr w)
lower_MO_S_Shr lbl w0 [x, y] = case someWasmTypeFromCmmType (cmmBits w0) of
  SomeWasmType ty -> do
    WasmExpr x_instr <- extendSubword w0 ty <$> lower_CmmExpr_Typed lbl ty x
    WasmExpr y_instr <- shiftRHSCast lbl ty y
    pure $
      SomeWasmExpr ty $
        truncSubword w0 ty $
          WasmExpr $
            x_instr `WasmConcat` y_instr `WasmConcat` WasmShr Signed ty
lower_MO_S_Shr _ _ _ = panic "lower_MO_S_Shr: unreachable"

-- | Lower a 'MO_MulMayOflo' operation. It's translated to a ccall to
-- @hs_mulIntMayOflo@ function in @ghc-prim/cbits/mulIntMayOflo@,
-- otherwise it's quite non-trivial to implement as inline assembly.
lower_MO_MulMayOflo ::
  CLabel -> Width -> [CmmExpr] -> WasmCodeGenM w (SomeWasmExpr w)
lower_MO_MulMayOflo lbl w0 [x, y] = case someWasmTypeFromCmmType ty_cmm of
  SomeWasmType ty -> do
    WasmExpr x_instr <- lower_CmmExpr_Typed lbl ty x
    WasmExpr y_instr <- lower_CmmExpr_Typed lbl ty y
    onFuncSym "hs_mulIntMayOflo" [ty_cmm, ty_cmm] [ty_cmm]
    pure $
      SomeWasmExpr ty $
        WasmExpr $
          x_instr
            `WasmConcat` y_instr
            `WasmConcat` WasmCCall "hs_mulIntMayOflo"
  where
    ty_cmm = cmmBits w0
lower_MO_MulMayOflo _ _ _ = panic "lower_MO_MulMayOflo: unreachable"

-- | Lower an unary conversion operation.
lower_MO_Un_Conv ::
  ( forall pre t0 t1.
    WasmTypeTag t0 ->
    WasmTypeTag t1 ->
    WasmInstr w (t0 : pre) (t1 : pre)
  ) ->
  CLabel ->
  CmmType ->
  CmmType ->
  [CmmExpr] ->
  WasmCodeGenM w (SomeWasmExpr w)
lower_MO_Un_Conv op lbl t0 t1 [x] =
  case (# someWasmTypeFromCmmType t0, someWasmTypeFromCmmType t1 #) of
    (# SomeWasmType ty0, SomeWasmType ty1 #) -> do
      WasmExpr x_instr <- lower_CmmExpr_Typed lbl ty0 x
      pure $ SomeWasmExpr ty1 $ WasmExpr $ x_instr `WasmConcat` op ty0 ty1
lower_MO_Un_Conv _ _ _ _ _ = panic "lower_MO_Un_Conv: unreachable"

-- | Lower a 'MO_SS_Conv' operation.
lower_MO_SS_Conv ::
  CLabel ->
  Width ->
  Width ->
  [CmmExpr] ->
  WasmCodeGenM
    w
    (SomeWasmExpr w)
lower_MO_SS_Conv lbl w0 w1 [x]
  | w0 == w1 = lower_CmmExpr lbl x
lower_MO_SS_Conv lbl w0 w1 [CmmLoad ptr _ align]
  | w0 < w1,
    w1 <= W32 = do
      (WasmExpr ptr_instr, o) <- lower_CmmExpr_Ptr lbl ptr
      pure $
        SomeWasmExpr TagI32 $
          truncSubword w1 TagI32 $
            WasmExpr $
              ptr_instr
                `WasmConcat` WasmLoad
                  TagI32
                  (wasmMemoryNarrowing TagI32 (cmmBits w0))
                  Signed
                  o
                  align
  | w0 > w1 =
      SomeWasmExpr TagI32
        <$> lower_CmmLoad_Typed
          lbl
          ptr
          TagI32
          (cmmBits w1)
          align
lower_MO_SS_Conv lbl w0 W64 [CmmLoad ptr _ align] = do
  (WasmExpr ptr_instr, o) <- lower_CmmExpr_Ptr lbl ptr
  pure $
    SomeWasmExpr TagI64 $
      WasmExpr $
        ptr_instr
          `WasmConcat` WasmLoad
            TagI64
            (wasmMemoryNarrowing TagI64 (cmmBits w0))
            Signed
            o
            align
lower_MO_SS_Conv lbl w0 w1 [x]
  | w0 < w1,
    w1 <= W32 = do
      x_expr <- lower_CmmExpr_Typed lbl TagI32 x
      pure $
        SomeWasmExpr TagI32 $
          truncSubword w1 TagI32 $
            extendSubword w0 TagI32 x_expr
  | W32 >= w0,
    w0 > w1 = do
      x_expr <- lower_CmmExpr_Typed lbl TagI32 x
      pure $ SomeWasmExpr TagI32 $ truncSubword w1 TagI32 x_expr
lower_MO_SS_Conv lbl W32 W64 [x] = do
  WasmExpr x_instr <- lower_CmmExpr_Typed lbl TagI32 x
  pure $
    SomeWasmExpr TagI64 $
      WasmExpr $
        x_instr `WasmConcat` WasmI64ExtendI32 Signed
lower_MO_SS_Conv lbl w0 W64 [x] = do
  WasmExpr x_instr <- lower_CmmExpr_Typed lbl TagI32 x
  pure $
    SomeWasmExpr TagI64 $
      extendSubword w0 TagI64 $
        WasmExpr $
          x_instr `WasmConcat` WasmI64ExtendI32 Unsigned
lower_MO_SS_Conv lbl W64 w1 [x] = do
  WasmExpr x_instr <- lower_CmmExpr_Typed lbl TagI64 x
  pure $
    SomeWasmExpr TagI32 $
      truncSubword w1 TagI32 $
        WasmExpr $
          x_instr `WasmConcat` WasmI32WrapI64
lower_MO_SS_Conv _ _ _ _ = panic "lower_MO_SS_Conv: unreachable"

-- | Lower a 'MO_UU_Conv' operation.
lower_MO_UU_Conv ::
  CLabel ->
  Width ->
  Width ->
  [CmmExpr] ->
  WasmCodeGenM
    w
    (SomeWasmExpr w)
lower_MO_UU_Conv lbl w0 w1 [CmmLoad ptr _ align] =
  case someWasmTypeFromCmmType (cmmBits w1) of
    SomeWasmType ty ->
      SomeWasmExpr ty
        <$> lower_CmmLoad_Typed
          lbl
          ptr
          ty
          (cmmBits (min w0 w1))
          align
lower_MO_UU_Conv lbl w0 w1 [x]
  | w0 == w1 = lower_CmmExpr lbl x
  | w0 < w1, w1 <= W32 = lower_CmmExpr lbl x
  | W32 >= w0,
    w0 > w1 = do
      x_expr <- lower_CmmExpr_Typed lbl TagI32 x
      pure $ SomeWasmExpr TagI32 $ truncSubword w1 TagI32 x_expr
lower_MO_UU_Conv lbl _ W64 [x] = do
  WasmExpr x_instr <- lower_CmmExpr_Typed lbl TagI32 x
  pure $
    SomeWasmExpr TagI64 $
      WasmExpr $
        x_instr `WasmConcat` WasmI64ExtendI32 Unsigned
lower_MO_UU_Conv lbl W64 w1 [x] = do
  WasmExpr x_instr <- lower_CmmExpr_Typed lbl TagI64 x
  pure $
    SomeWasmExpr TagI32 $
      truncSubword w1 TagI32 $
        WasmExpr $
          x_instr `WasmConcat` WasmI32WrapI64
lower_MO_UU_Conv _ _ _ _ = panic "lower_MO_UU_Conv: unreachable"

-- | Lower a 'MO_FF_Conv' operation.
lower_MO_FF_Conv ::
  CLabel ->
  Width ->
  Width ->
  [CmmExpr] ->
  WasmCodeGenM
    w
    (SomeWasmExpr w)
lower_MO_FF_Conv lbl W32 W64 [x] = do
  WasmExpr x_instr <- lower_CmmExpr_Typed lbl TagF32 x
  pure $
    SomeWasmExpr TagF64 $
      WasmExpr $
        x_instr `WasmConcat` WasmF64PromoteF32
lower_MO_FF_Conv lbl W64 W32 [x] = do
  WasmExpr x_instr <- lower_CmmExpr_Typed lbl TagF64 x
  pure $
    SomeWasmExpr TagF32 $
      WasmExpr $
        x_instr `WasmConcat` WasmF32DemoteF64
lower_MO_FF_Conv _ _ _ _ = panic "lower_MO_FF_Conv: unreachable"


-- | Lower a 'MO_WF_Bitcast' operation. Note that this is not a conversion,
-- rather it reinterprets the data.
lower_MO_WF_Bitcast ::
  CLabel ->
  Width ->
  [CmmActual] ->
  WasmCodeGenM w (SomeWasmExpr w)
lower_MO_WF_Bitcast lbl W32 [x] = do
  WasmExpr x_instr <- lower_CmmExpr_Typed lbl TagI32 x
  pure $
    SomeWasmExpr TagF32 $
      WasmExpr  $
        x_instr `WasmConcat` WasmReinterpret TagI32 TagF32
lower_MO_WF_Bitcast lbl W64 [x] = do
  WasmExpr x_instr <- lower_CmmExpr_Typed lbl TagI64 x
  pure $
    SomeWasmExpr TagF64 $
      WasmExpr  $
        x_instr `WasmConcat` WasmReinterpret TagI64 TagF64
lower_MO_WF_Bitcast _ _ _ = panic "lower_MO_WF_Bitcast: unreachable"

-- | Lower a 'MO_FW_Bitcast' operation. Note that this is not a conversion,
-- rather it reinterprets the data.
lower_MO_FW_Bitcast ::
  CLabel ->
  Width ->
  [CmmActual] ->
  WasmCodeGenM w (SomeWasmExpr w)
lower_MO_FW_Bitcast lbl W32 [x] = do
  WasmExpr x_instr <- lower_CmmExpr_Typed lbl TagF32 x
  pure $
    SomeWasmExpr TagI32 $
      WasmExpr  $
        x_instr `WasmConcat` WasmReinterpret TagF32 TagI32
lower_MO_FW_Bitcast lbl W64 [x] = do
  WasmExpr x_instr <- lower_CmmExpr_Typed lbl TagF64 x
  pure $
    SomeWasmExpr TagI64 $
      WasmExpr  $
        x_instr `WasmConcat` WasmReinterpret TagF64 TagI64
lower_MO_FW_Bitcast _ _ _ = panic "lower_MO_FW_Bitcast: unreachable"

-- | Lower a 'CmmMachOp'.
lower_CmmMachOp ::
  CLabel ->
  MachOp ->
  [CmmExpr] ->
  WasmCodeGenM
    w
    (SomeWasmExpr w)
lower_CmmMachOp lbl (MO_RelaxedRead w0) [x] = lower_CmmExpr lbl (CmmLoad x (cmmBits w0) NaturallyAligned)
lower_CmmMachOp lbl (MO_Add w0) xs = lower_MO_Bin_Homo_Trunc WasmAdd lbl w0 xs
lower_CmmMachOp lbl (MO_Sub w0) xs = lower_MO_Bin_Homo_Trunc WasmSub lbl w0 xs
lower_CmmMachOp lbl (MO_Eq w0) xs = lower_MO_Bin_Rel WasmEq lbl (cmmBits w0) xs
lower_CmmMachOp lbl (MO_Ne w0) xs = lower_MO_Bin_Rel WasmNe lbl (cmmBits w0) xs
lower_CmmMachOp lbl (MO_Mul w0) xs = lower_MO_Bin_Homo_Trunc WasmMul lbl w0 xs
lower_CmmMachOp lbl (MO_S_MulMayOflo w0) xs = lower_MO_MulMayOflo lbl w0 xs
lower_CmmMachOp lbl (MO_S_Quot w0) xs =
  lower_MO_Bin_Homo_Ext_Trunc
    (WasmDiv Signed)
    lbl
    w0
    xs
lower_CmmMachOp lbl (MO_S_Rem w0) xs =
  lower_MO_Bin_Homo_Ext_Trunc
    (WasmRem Signed)
    lbl
    w0
    xs
lower_CmmMachOp lbl (MO_S_Neg w0) [x] =
  lower_CmmMachOp
    lbl
    (MO_Sub w0)
    [CmmLit $ CmmInt 0 w0, x]
lower_CmmMachOp lbl (MO_U_Quot w0) xs =
  lower_MO_Bin_Homo
    (WasmDiv Unsigned)
    lbl
    (cmmBits w0)
    xs
lower_CmmMachOp lbl (MO_U_Rem w0) xs =
  lower_MO_Bin_Homo
    (WasmRem Unsigned)
    lbl
    (cmmBits w0)
    xs
lower_CmmMachOp lbl (MO_S_Ge w0) xs =
  lower_MO_Bin_Rel_Ext
    (WasmGe Signed)
    lbl
    w0
    xs
lower_CmmMachOp lbl (MO_S_Le w0) xs =
  lower_MO_Bin_Rel_Ext
    (WasmLe Signed)
    lbl
    w0
    xs
lower_CmmMachOp lbl (MO_S_Gt w0) xs =
  lower_MO_Bin_Rel_Ext
    (WasmGt Signed)
    lbl
    w0
    xs
lower_CmmMachOp lbl (MO_S_Lt w0) xs =
  lower_MO_Bin_Rel_Ext
    (WasmLt Signed)
    lbl
    w0
    xs
lower_CmmMachOp lbl (MO_U_Ge w0) xs =
  lower_MO_Bin_Rel
    (WasmGe Unsigned)
    lbl
    (cmmBits w0)
    xs
lower_CmmMachOp lbl (MO_U_Le w0) xs =
  lower_MO_Bin_Rel
    (WasmLe Unsigned)
    lbl
    (cmmBits w0)
    xs
lower_CmmMachOp lbl (MO_U_Gt w0) xs =
  lower_MO_Bin_Rel
    (WasmGt Unsigned)
    lbl
    (cmmBits w0)
    xs
lower_CmmMachOp lbl (MO_U_Lt w0) xs =
  lower_MO_Bin_Rel
    (WasmLt Unsigned)
    lbl
    (cmmBits w0)
    xs
lower_CmmMachOp lbl (MO_F_Add w0) xs =
  lower_MO_Bin_Homo
    WasmAdd
    lbl
    (cmmFloat w0)
    xs
lower_CmmMachOp lbl (MO_F_Sub w0) xs =
  lower_MO_Bin_Homo
    WasmSub
    lbl
    (cmmFloat w0)
    xs
lower_CmmMachOp lbl (MO_F_Neg w0) xs =
  lower_MO_Un_Homo
    WasmNeg
    lbl
    (cmmFloat w0)
    xs
lower_CmmMachOp lbl (MO_F_Mul w0) xs =
  lower_MO_Bin_Homo
    WasmMul
    lbl
    (cmmFloat w0)
    xs
lower_CmmMachOp lbl (MO_F_Quot w0) xs =
  lower_MO_Bin_Homo
    (WasmDiv Signed)
    lbl
    (cmmFloat w0)
    xs
lower_CmmMachOp lbl (MO_F_Eq w0) xs =
  lower_MO_Bin_Rel
    WasmEq
    lbl
    (cmmFloat w0)
    xs
lower_CmmMachOp lbl (MO_F_Ne w0) xs =
  lower_MO_Bin_Rel
    WasmNe
    lbl
    (cmmFloat w0)
    xs
lower_CmmMachOp lbl (MO_F_Ge w0) xs =
  lower_MO_Bin_Rel
    (WasmGe Signed)
    lbl
    (cmmFloat w0)
    xs
lower_CmmMachOp lbl (MO_F_Le w0) xs =
  lower_MO_Bin_Rel
    (WasmLe Signed)
    lbl
    (cmmFloat w0)
    xs
lower_CmmMachOp lbl (MO_F_Gt w0) xs =
  lower_MO_Bin_Rel
    (WasmGt Signed)
    lbl
    (cmmFloat w0)
    xs
lower_CmmMachOp lbl (MO_F_Lt w0) xs =
  lower_MO_Bin_Rel
    (WasmLt Signed)
    lbl
    (cmmFloat w0)
    xs
lower_CmmMachOp lbl (MO_And w0) xs =
  lower_MO_Bin_Homo
    WasmAnd
    lbl
    (cmmBits w0)
    xs
lower_CmmMachOp lbl (MO_Or w0) xs = lower_MO_Bin_Homo WasmOr lbl (cmmBits w0) xs
lower_CmmMachOp lbl (MO_Xor w0) xs =
  lower_MO_Bin_Homo
    WasmXor
    lbl
    (cmmBits w0)
    xs
lower_CmmMachOp lbl (MO_Not w0) [x] =
  lower_CmmMachOp
    lbl
    (MO_Xor w0)
    [x, CmmLit $ CmmInt (widthMax w0) w0]
lower_CmmMachOp lbl (MO_Shl w0) xs = lower_MO_Shl lbl w0 xs
lower_CmmMachOp lbl (MO_U_Shr w0) xs = lower_MO_U_Shr lbl w0 xs
lower_CmmMachOp lbl (MO_S_Shr w0) xs = lower_MO_S_Shr lbl w0 xs
lower_CmmMachOp lbl (MO_SF_Round w0 w1) xs =
  lower_MO_Un_Conv
    (WasmConvert Signed)
    lbl
    (cmmBits w0)
    (cmmFloat w1)
    xs
lower_CmmMachOp lbl (MO_FS_Truncate w0 w1) xs =
  lower_MO_Un_Conv
    (WasmTruncSat Signed)
    lbl
    (cmmFloat w0)
    (cmmBits w1)
    xs
lower_CmmMachOp lbl (MO_SS_Conv w0 w1) xs = lower_MO_SS_Conv lbl w0 w1 xs
lower_CmmMachOp lbl (MO_UU_Conv w0 w1) xs = lower_MO_UU_Conv lbl w0 w1 xs
lower_CmmMachOp lbl (MO_XX_Conv w0 w1) xs = lower_MO_UU_Conv lbl w0 w1 xs
lower_CmmMachOp lbl (MO_FF_Conv w0 w1) xs = lower_MO_FF_Conv lbl w0 w1 xs
lower_CmmMachOp lbl (MO_FW_Bitcast w) xs  = lower_MO_FW_Bitcast lbl w xs
lower_CmmMachOp lbl (MO_WF_Bitcast w) xs  = lower_MO_WF_Bitcast lbl w xs
lower_CmmMachOp _ mop _ =
  pprPanic "lower_CmmMachOp: unreachable" $
    vcat [ text "offending MachOp:" <+> pprMachOp mop ]

-- | Lower a 'CmmLit'. Note that we don't emit 'f32.const' or
-- 'f64.const' for the time being, and instead emit their relative bit
-- pattern as int literals, then use an reinterpret cast. This is
-- simpler than dealing with textual representation of floating point
-- values.
lower_CmmLit :: CmmLit -> WasmCodeGenM w (SomeWasmExpr w)
lower_CmmLit lit = do
  ty_word <- wasmWordTypeM
  case lit of
    CmmInt i w -> case someWasmTypeFromCmmType (cmmBits w) of
      SomeWasmType ty ->
        pure $
          SomeWasmExpr ty $
            WasmExpr $
              WasmConst ty $
                narrowU w i
    CmmFloat f W32 ->
      pure $
        SomeWasmExpr TagF32 $
          WasmExpr $
            WasmConst
              TagI32
              (toInteger $ castFloatToWord32 $ fromRational f)
              `WasmConcat` WasmReinterpret TagI32 TagF32
    CmmFloat f W64 ->
      pure $
        SomeWasmExpr TagF64 $
          WasmExpr $
            WasmConst
              TagI64
              (toInteger $ castDoubleToWord64 $ fromRational f)
              `WasmConcat` WasmReinterpret TagI64 TagF64
    CmmLabel lbl' -> do
      onAnySym lbl'
      let sym = symNameFromCLabel lbl'
      pure $ SomeWasmExpr ty_word $ WasmExpr $ WasmSymConst sym
    CmmLabelOff lbl' o -> do
      onAnySym lbl'
      let sym = symNameFromCLabel lbl'
      pure $
        SomeWasmExpr ty_word $
          WasmExpr $
            WasmSymConst sym
              `WasmConcat` WasmConst ty_word (toInteger o)
              `WasmConcat` WasmAdd ty_word
    CmmBlock bid -> lower_CmmLit $ CmmLabel $ infoTblLbl bid
    _ -> panic "lower_CmmLit: unreachable"

--  | Lower a 'CmmReg'. Some of the logic here wouldn't be needed if
--  we have run 'fixStgRegisters' on the wasm NCG's input Cmm, but we
--  haven't run it yet for certain reasons.
lower_CmmReg :: CLabel -> CmmReg -> WasmCodeGenM w (SomeWasmExpr w)
lower_CmmReg _ (CmmLocal reg) = do
  (reg_i, SomeWasmType ty) <- onCmmLocalReg reg
  pure $ SomeWasmExpr ty $ WasmExpr $ WasmLocalGet ty reg_i
lower_CmmReg lbl (CmmGlobal (GlobalRegUse greg reg_use_ty)) = do
  ty_word <- wasmWordTypeM
  ty_word_cmm <- wasmWordCmmTypeM
  case greg of
    EagerBlackholeInfo ->
      pure $
        SomeWasmExpr ty_word $
          WasmExpr $
            WasmSymConst "__stg_EAGER_BLACKHOLE_info"
    GCEnter1 -> do
      onFuncSym "__stg_gc_enter_1" [] [ty_word_cmm]
      pure $ SomeWasmExpr ty_word $ WasmExpr $ WasmSymConst "__stg_gc_enter_1"
    GCFun -> do
      onFuncSym "__stg_gc_fun" [] [ty_word_cmm]
      pure $ SomeWasmExpr ty_word $ WasmExpr $ WasmSymConst "__stg_gc_fun"
    BaseReg -> do
      platform <- wasmPlatformM
      lower_CmmExpr lbl $ regTableOffset platform 0
    _other
      | Just (sym_global, SomeWasmType ty) <-
          globalInfoFromCmmGlobalReg ty_word greg ->
          pure $ SomeWasmExpr ty $ WasmExpr $ WasmGlobalGet ty sym_global
      | otherwise -> do
          platform <- wasmPlatformM
          case someWasmTypeFromCmmType reg_use_ty of
            SomeWasmType ty -> do
              (WasmExpr ptr_instr, o) <-
                lower_CmmExpr_Ptr lbl $
                  get_GlobalReg_addr platform greg
              pure $
                SomeWasmExpr ty $
                  WasmExpr $
                    ptr_instr
                      `WasmConcat` WasmLoad
                        ty
                        Nothing
                        Unsigned
                        o
                        NaturallyAligned

-- | Lower a 'CmmRegOff'.
lower_CmmRegOff :: CLabel -> CmmReg -> Int -> WasmCodeGenM w (SomeWasmExpr w)
lower_CmmRegOff lbl reg 0 = lower_CmmReg lbl reg
lower_CmmRegOff lbl reg o = do
  SomeWasmExpr ty (WasmExpr reg_instr) <- lower_CmmReg lbl reg
  pure $
    SomeWasmExpr ty $
      WasmExpr $
        reg_instr
          `WasmConcat` WasmConst
            ty
            (toInteger o)
          `WasmConcat` WasmAdd ty

-- | Lower a 'CmmLoad', passing in the expected wasm representation
-- type, and also the Cmm type (which contains width info needed for
-- memory narrowing).
--
-- The Cmm type system doesn't track signedness, so all 'CmmLoad's are
-- unsigned loads. However, as an optimization, we do emit signed
-- loads when a 'CmmLoad' result is immediately used as a 'MO_SS_Conv'
-- operand.
lower_CmmLoad_Typed ::
  CLabel ->
  CmmExpr ->
  WasmTypeTag t ->
  CmmType ->
  AlignmentSpec ->
  WasmCodeGenM w (WasmExpr w t)
lower_CmmLoad_Typed lbl ptr_expr ty ty_cmm align = do
  (WasmExpr ptr_instr, o) <- lower_CmmExpr_Ptr lbl ptr_expr
  pure $
    WasmExpr $
      ptr_instr
        `WasmConcat` WasmLoad
          ty
          (wasmMemoryNarrowing ty ty_cmm)
          Unsigned
          o
          align

-- | Lower a 'CmmLoad'.
lower_CmmLoad ::
  CLabel ->
  CmmExpr ->
  CmmType ->
  AlignmentSpec ->
  WasmCodeGenM
    w
    (SomeWasmExpr w)
lower_CmmLoad lbl ptr_expr ty_cmm align = case someWasmTypeFromCmmType ty_cmm of
  SomeWasmType ty ->
    SomeWasmExpr ty <$> lower_CmmLoad_Typed lbl ptr_expr ty ty_cmm align

-- | Lower a 'CmmExpr'.
lower_CmmExpr :: CLabel -> CmmExpr -> WasmCodeGenM w (SomeWasmExpr w)
lower_CmmExpr lbl expr = case expr of
  CmmLit lit -> lower_CmmLit lit
  CmmLoad ptr_expr ty_cmm align -> lower_CmmLoad lbl ptr_expr ty_cmm align
  CmmReg reg -> lower_CmmReg lbl reg
  CmmRegOff reg o -> lower_CmmRegOff lbl reg o
  CmmMachOp op xs -> lower_CmmMachOp lbl op xs
  _ -> panic "lower_CmmExpr: unreachable"

-- | Lower a 'CmmExpr', passing in the expected wasm representation
-- type.
lower_CmmExpr_Typed ::
  CLabel ->
  WasmTypeTag t ->
  CmmExpr ->
  WasmCodeGenM
    w
    (WasmExpr w t)
lower_CmmExpr_Typed lbl ty expr = do
  SomeWasmExpr ty' r <- lower_CmmExpr lbl expr
  if
      | Just Refl <- ty' `testEquality` ty -> pure r
      | otherwise -> panic "lower_CmmExpr_Typed: unreachable"

-- | Lower a 'CmmExpr' as a pointer, returning the pair of base
-- pointer and non-negative offset.
lower_CmmExpr_Ptr :: CLabel -> CmmExpr -> WasmCodeGenM w (WasmExpr w w, Int)
lower_CmmExpr_Ptr lbl ptr = do
  ty_word <- wasmWordTypeM
  case ptr of
    CmmLit (CmmLabelOff lbl o)
      | o >= 0 -> do
          instrs <-
            lower_CmmExpr_Typed
              lbl
              ty_word
              (CmmLit $ CmmLabel lbl)
          pure (instrs, o)
    CmmMachOp (MO_Add _) [base, CmmLit (CmmInt o _)]
      | o >= 0 -> do
          instrs <- lower_CmmExpr_Typed lbl ty_word base
          pure (instrs, fromInteger o)
    _ -> do
      instrs <- lower_CmmExpr_Typed lbl ty_word ptr
      pure (instrs, 0)

-- | Push a series of values onto the wasm value stack, returning the
-- result stack type.
type family
  WasmPushes (ts :: [WasmType]) (pre :: [WasmType]) ::
    [WasmType]
  where
  WasmPushes '[] pre = pre
  WasmPushes (t : ts) pre = WasmPushes ts (t : pre)

-- | Push the arguments onto the wasm value stack before a ccall.
data SomeWasmPreCCall w where
  SomeWasmPreCCall ::
    TypeList ts ->
    (forall pre. WasmInstr w pre (WasmPushes ts pre)) ->
    SomeWasmPreCCall w

-- | Pop the results into locals after a ccall.
data SomeWasmPostCCall w where
  SomeWasmPostCCall ::
    TypeList ts ->
    (forall post. WasmInstr w (WasmPushes ts post) post) ->
    SomeWasmPostCCall w

-- | Lower an unary homogeneous 'CallishMachOp' to a ccall.
lower_CMO_Un_Homo ::
  CLabel ->
  SymName ->
  [CmmFormal] ->
  [CmmActual] ->
  WasmCodeGenM w (WasmStatements w)
lower_CMO_Un_Homo lbl op [reg] [x] = do
  (ri, SomeWasmType ty) <- onCmmLocalReg reg
  WasmExpr x_instr <- lower_CmmExpr_Typed lbl ty x
  let ty_cmm = localRegType reg
  onFuncSym op [ty_cmm] [ty_cmm]
  pure $
    WasmStatements $
      x_instr `WasmConcat` WasmCCall op `WasmConcat` WasmLocalSet ty ri
lower_CMO_Un_Homo _ _ _ _ = panic "lower_CMO_Un_Homo: unreachable"

-- | Lower a binary homogeneous 'CallishMachOp' to a ccall.
lower_CMO_Bin_Homo ::
  CLabel ->
  SymName ->
  [CmmFormal] ->
  [CmmActual] ->
  WasmCodeGenM w (WasmStatements w)
lower_CMO_Bin_Homo lbl op [reg] [x, y] = do
  (ri, SomeWasmType ty) <- onCmmLocalReg reg
  WasmExpr x_instr <- lower_CmmExpr_Typed lbl ty x
  WasmExpr y_instr <- lower_CmmExpr_Typed lbl ty y
  let ty_cmm = localRegType reg
  onFuncSym op [ty_cmm, ty_cmm] [ty_cmm]
  pure $
    WasmStatements $
      x_instr
        `WasmConcat` y_instr
        `WasmConcat` WasmCCall op
        `WasmConcat` WasmLocalSet ty ri
lower_CMO_Bin_Homo _ _ _ _ = panic "lower_CMO_Bin_Homo: unreachable"

-- | Lower a 'MO_UF_Conv' operation.
lower_MO_UF_Conv ::
  CLabel ->
  Width ->
  [CmmFormal] ->
  [CmmActual] ->
  WasmCodeGenM w (WasmStatements w)
lower_MO_UF_Conv lbl W32 [reg] [x] = do
  ri <- onCmmLocalReg_Typed TagF32 reg
  SomeWasmExpr ty0 (WasmExpr x_instr) <- lower_CmmExpr lbl x
  pure $
    WasmStatements $
      x_instr
        `WasmConcat` WasmConvert Unsigned ty0 TagF32
        `WasmConcat` WasmLocalSet TagF32 ri
lower_MO_UF_Conv lbl W64 [reg] [x] = do
  ri <- onCmmLocalReg_Typed TagF64 reg
  SomeWasmExpr ty0 (WasmExpr x_instr) <- lower_CmmExpr lbl x
  pure $
    WasmStatements $
      x_instr
        `WasmConcat` WasmConvert Unsigned ty0 TagF64
        `WasmConcat` WasmLocalSet TagF64 ri
lower_MO_UF_Conv _ _ _ _ = panic "lower_MO_UF_Conv: unreachable"

-- | Lower a 'MO_Cmpxchg' operation to inline assembly. Currently we
-- target wasm without atomics and threads, so it's just lowered to
-- regular memory loads and stores.
lower_MO_Cmpxchg ::
  CLabel ->
  Width ->
  [CmmFormal] ->
  [CmmActual] ->
  WasmCodeGenM w (WasmStatements w)
lower_MO_Cmpxchg lbl w0 [reg] [ptr, expected, new] =
  case someWasmTypeFromCmmType ty_cmm of
    SomeWasmType ty -> do
      reg_i <- onCmmLocalReg_Typed ty reg
      let narrowing = wasmMemoryNarrowing ty ty_cmm
      (WasmExpr ptr_instr, o) <- lower_CmmExpr_Ptr lbl ptr
      WasmExpr expected_instr <- lower_CmmExpr_Typed lbl ty expected
      WasmExpr new_instr <- lower_CmmExpr_Typed lbl ty new
      pure $
        WasmStatements $
          ptr_instr
            `WasmConcat` WasmLoad ty narrowing Unsigned o NaturallyAligned
            `WasmConcat` WasmLocalTee ty reg_i
            `WasmConcat` expected_instr
            `WasmConcat` WasmEq ty
            `WasmConcat` WasmCond
              ( ptr_instr
                  `WasmConcat` new_instr
                  `WasmConcat` WasmStore ty narrowing o NaturallyAligned
              )
  where
    ty_cmm = cmmBits w0
lower_MO_Cmpxchg _ _ _ _ = panic "lower_MO_Cmpxchg: unreachable"

-- | Lower a 'CallishMachOp'.
lower_CallishMachOp ::
  CLabel ->
  CallishMachOp ->
  [CmmFormal] ->
  [CmmActual] ->
  WasmCodeGenM w (WasmStatements w)
lower_CallishMachOp lbl MO_F64_Pwr rs xs = lower_CMO_Bin_Homo lbl "pow" rs xs
lower_CallishMachOp lbl MO_F64_Sin rs xs = lower_CMO_Un_Homo lbl "sin" rs xs
lower_CallishMachOp lbl MO_F64_Cos rs xs = lower_CMO_Un_Homo lbl "cos" rs xs
lower_CallishMachOp lbl MO_F64_Tan rs xs = lower_CMO_Un_Homo lbl "tan" rs xs
lower_CallishMachOp lbl MO_F64_Sinh rs xs = lower_CMO_Un_Homo lbl "sinh" rs xs
lower_CallishMachOp lbl MO_F64_Cosh rs xs = lower_CMO_Un_Homo lbl "cosh" rs xs
lower_CallishMachOp lbl MO_F64_Tanh rs xs = lower_CMO_Un_Homo lbl "tanh" rs xs
lower_CallishMachOp lbl MO_F64_Asin rs xs = lower_CMO_Un_Homo lbl "asin" rs xs
lower_CallishMachOp lbl MO_F64_Acos rs xs = lower_CMO_Un_Homo lbl "acos" rs xs
lower_CallishMachOp lbl MO_F64_Atan rs xs = lower_CMO_Un_Homo lbl "atan" rs xs
lower_CallishMachOp lbl MO_F64_Asinh rs xs = lower_CMO_Un_Homo lbl "asinh" rs xs
lower_CallishMachOp lbl MO_F64_Acosh rs xs = lower_CMO_Un_Homo lbl "acosh" rs xs
lower_CallishMachOp lbl MO_F64_Atanh rs xs = lower_CMO_Un_Homo lbl "atanh" rs xs
lower_CallishMachOp lbl MO_F64_Log rs xs = lower_CMO_Un_Homo lbl "log" rs xs
lower_CallishMachOp lbl MO_F64_Log1P rs xs = lower_CMO_Un_Homo lbl "log1p" rs xs
lower_CallishMachOp lbl MO_F64_Exp rs xs = lower_CMO_Un_Homo lbl "exp" rs xs
lower_CallishMachOp lbl MO_F64_ExpM1 rs xs = lower_CMO_Un_Homo lbl "expm1" rs xs
lower_CallishMachOp lbl MO_F64_Fabs rs xs = lower_CMO_Un_Homo lbl "fabs" rs xs
lower_CallishMachOp lbl MO_F64_Sqrt rs xs = lower_CMO_Un_Homo lbl "sqrt" rs xs
lower_CallishMachOp lbl MO_F32_Pwr rs xs = lower_CMO_Bin_Homo lbl "powf" rs xs
lower_CallishMachOp lbl MO_F32_Sin rs xs = lower_CMO_Un_Homo lbl "sinf" rs xs
lower_CallishMachOp lbl MO_F32_Cos rs xs = lower_CMO_Un_Homo lbl "cosf" rs xs
lower_CallishMachOp lbl MO_F32_Tan rs xs = lower_CMO_Un_Homo lbl "tanf" rs xs
lower_CallishMachOp lbl MO_F32_Sinh rs xs = lower_CMO_Un_Homo lbl "sinhf" rs xs
lower_CallishMachOp lbl MO_F32_Cosh rs xs = lower_CMO_Un_Homo lbl "coshf" rs xs
lower_CallishMachOp lbl MO_F32_Tanh rs xs = lower_CMO_Un_Homo lbl "tanhf" rs xs
lower_CallishMachOp lbl MO_F32_Asin rs xs = lower_CMO_Un_Homo lbl "asinf" rs xs
lower_CallishMachOp lbl MO_F32_Acos rs xs = lower_CMO_Un_Homo lbl "acosf" rs xs
lower_CallishMachOp lbl MO_F32_Atan rs xs = lower_CMO_Un_Homo lbl "atanf" rs xs
lower_CallishMachOp lbl MO_F32_Asinh rs xs =
  lower_CMO_Un_Homo lbl "asinhf" rs xs
lower_CallishMachOp lbl MO_F32_Acosh rs xs =
  lower_CMO_Un_Homo lbl "acoshf" rs xs
lower_CallishMachOp lbl MO_F32_Atanh rs xs =
  lower_CMO_Un_Homo lbl "atanhf" rs xs
lower_CallishMachOp lbl MO_F32_Log rs xs = lower_CMO_Un_Homo lbl "logf" rs xs
lower_CallishMachOp lbl MO_F32_Log1P rs xs =
  lower_CMO_Un_Homo lbl "log1pf" rs xs
lower_CallishMachOp lbl MO_F32_Exp rs xs = lower_CMO_Un_Homo lbl "expf" rs xs
lower_CallishMachOp lbl MO_F32_ExpM1 rs xs =
  lower_CMO_Un_Homo lbl "expm1f" rs xs
lower_CallishMachOp lbl MO_F32_Fabs rs xs = lower_CMO_Un_Homo lbl "fabsf" rs xs
lower_CallishMachOp lbl MO_F32_Sqrt rs xs = lower_CMO_Un_Homo lbl "sqrtf" rs xs
lower_CallishMachOp lbl (MO_UF_Conv w0) rs xs = lower_MO_UF_Conv lbl w0 rs xs
lower_CallishMachOp _ MO_AcquireFence _ _ = pure $ WasmStatements WasmNop
lower_CallishMachOp _ MO_ReleaseFence _ _ = pure $ WasmStatements WasmNop
lower_CallishMachOp _ MO_SeqCstFence _ _ = pure $ WasmStatements WasmNop
lower_CallishMachOp _ MO_Touch _ _ = pure $ WasmStatements WasmNop
lower_CallishMachOp _ (MO_Prefetch_Data {}) _ _ = pure $ WasmStatements WasmNop
lower_CallishMachOp lbl (MO_Memcpy {}) [] xs = do
  ty_word_cmm <- wasmWordCmmTypeM
  lower_CmmUnsafeForeignCall_Drop lbl "memcpy" ty_word_cmm xs
lower_CallishMachOp lbl (MO_Memset {}) [] xs = do
  ty_word_cmm <- wasmWordCmmTypeM
  lower_CmmUnsafeForeignCall_Drop lbl "memset" ty_word_cmm xs
lower_CallishMachOp lbl (MO_Memmove {}) [] xs = do
  ty_word_cmm <- wasmWordCmmTypeM
  lower_CmmUnsafeForeignCall_Drop lbl "memmove" ty_word_cmm xs
lower_CallishMachOp lbl (MO_Memcmp {}) rs xs =
  lower_CmmUnsafeForeignCall
    lbl
    (Left "memcmp")
    Nothing
    CmmMayReturn
    rs
    xs
lower_CallishMachOp lbl (MO_PopCnt w0) rs xs =
  lower_CmmUnsafeForeignCall
    lbl
    (Left $ fromString $ "hs_popcnt" <> show (widthInBits w0))
    Nothing
    CmmMayReturn
    rs
    xs
lower_CallishMachOp lbl (MO_Pdep w0) rs xs =
  lower_CmmUnsafeForeignCall
    lbl
    (Left $ fromString $ "hs_pdep" <> show (widthInBits w0))
    Nothing
    CmmMayReturn
    rs
    xs
lower_CallishMachOp lbl (MO_Pext w0) rs xs =
  lower_CmmUnsafeForeignCall
    lbl
    (Left $ fromString $ "hs_pext" <> show (widthInBits w0))
    Nothing
    CmmMayReturn
    rs
    xs
lower_CallishMachOp lbl (MO_Clz w0) rs xs =
  lower_CmmUnsafeForeignCall
    lbl
    (Left $ fromString $ "hs_clz" <> show (widthInBits w0))
    Nothing
    CmmMayReturn
    rs
    xs
lower_CallishMachOp lbl (MO_Ctz w0) rs xs =
  lower_CmmUnsafeForeignCall
    lbl
    (Left $ fromString $ "hs_ctz" <> show (widthInBits w0))
    Nothing
    CmmMayReturn
    rs
    xs
lower_CallishMachOp lbl (MO_BSwap w0) rs xs =
  lower_CmmUnsafeForeignCall
    lbl
    (Left $ fromString $ "hs_bswap" <> show (widthInBits w0))
    Nothing
    CmmMayReturn
    rs
    xs
lower_CallishMachOp lbl (MO_BRev w0) rs xs =
  lower_CmmUnsafeForeignCall
    lbl
    (Left $ fromString $ "hs_bitrev" <> show (widthInBits w0))
    Nothing
    CmmMayReturn
    rs
    xs
lower_CallishMachOp lbl (MO_AtomicRMW w0 op) rs xs =
  lower_CmmUnsafeForeignCall
    lbl
    ( Left $
        fromString $
          ( case op of
              AMO_Add -> "hs_atomic_add"
              AMO_Sub -> "hs_atomic_sub"
              AMO_And -> "hs_atomic_and"
              AMO_Nand -> "hs_atomic_nand"
              AMO_Or -> "hs_atomic_or"
              AMO_Xor -> "hs_atomic_xor"
          )
            <> show (widthInBits w0)
    )
    Nothing
    CmmMayReturn
    rs
    xs
lower_CallishMachOp lbl (MO_AtomicRead w0 _) [reg] [ptr] = do
  SomeWasmExpr ty (WasmExpr ret_instr) <-
    lower_CmmLoad
      lbl
      ptr
      (cmmBits w0)
      NaturallyAligned
  ri <- onCmmLocalReg_Typed ty reg
  pure $ WasmStatements $ ret_instr `WasmConcat` WasmLocalSet ty ri
lower_CallishMachOp lbl (MO_AtomicWrite _ _) [] [ptr, val] =
  lower_CmmStore lbl ptr val NaturallyAligned
lower_CallishMachOp lbl (MO_Cmpxchg w0) rs xs = lower_MO_Cmpxchg lbl w0 rs xs
lower_CallishMachOp lbl (MO_Xchg w0) rs xs =
  lower_CmmUnsafeForeignCall
    lbl
    (Left $ fromString $ "hs_xchg" <> show (widthInBits w0))
    Nothing
    CmmMayReturn
    rs
    xs
lower_CallishMachOp lbl MO_SuspendThread rs xs =
  lower_CmmUnsafeForeignCall
    lbl
    (Left "suspendThread")
    Nothing
    CmmMayReturn
    rs
    xs
lower_CallishMachOp lbl MO_ResumeThread rs xs =
  lower_CmmUnsafeForeignCall
    lbl
    (Left "resumeThread")
    Nothing
    CmmMayReturn
    rs
    xs
lower_CallishMachOp _ _ _ _ = panic "lower_CallishMachOp: unreachable"

-- | Lower a ccall, but drop the result by assigning it to an unused
-- local. This is only used for lowering 'MO_Memcpy' and such, where
-- the libc functions do have a return value, but the corresponding
-- 'CallishMachOp' does not expect one.
lower_CmmUnsafeForeignCall_Drop ::
  CLabel ->
  SymName ->
  CmmType ->
  [CmmActual] ->
  WasmCodeGenM w (WasmStatements w)
lower_CmmUnsafeForeignCall_Drop lbl sym_callee ret_cmm_ty arg_exprs = do
  ret_uniq <- getUniqueM
  let ret_local = LocalReg ret_uniq ret_cmm_ty
  lower_CmmUnsafeForeignCall
    lbl
    (Left sym_callee)
    Nothing
    CmmMayReturn
    [ret_local]
    arg_exprs

-- | Lower a 'CmmUnsafeForeignCall'. The target is 'Either' a symbol,
-- which translates to a direct @call@, or an expression, which
-- translates to a @call_indirect@. The callee function signature is
-- inferred from the passed in arguments here.
lower_CmmUnsafeForeignCall ::
  CLabel ->
  (Either SymName CmmExpr) ->
  Maybe
    ([ForeignHint], [ForeignHint]) ->
  CmmReturnInfo ->
  [CmmFormal] ->
  [CmmActual] ->
  WasmCodeGenM w (WasmStatements w)
lower_CmmUnsafeForeignCall lbl target mb_hints ret_info ret_locals arg_exprs = do
  platform <- wasmPlatformM
  SomeWasmPreCCall arg_tys args_instr <-
    foldrM
      ( \(arg_expr, arg_hint) (SomeWasmPreCCall acc_tys acc_instr) -> do
          SomeWasmExpr arg_ty arg_wasm_expr <- lower_CmmExpr lbl arg_expr
          let WasmExpr arg_instr = case arg_hint of
                SignedHint ->
                  extendSubword
                    (cmmExprWidth platform arg_expr)
                    arg_ty
                    arg_wasm_expr
                _ -> arg_wasm_expr
          pure $
            SomeWasmPreCCall (arg_ty `TypeListCons` acc_tys) $
              arg_instr `WasmConcat` acc_instr
      )
      (SomeWasmPreCCall TypeListNil WasmNop)
      arg_exprs_hints
  SomeWasmPostCCall ret_tys ret_instr <-
    foldrM
      ( \(reg, ret_hint) (SomeWasmPostCCall acc_tys acc_instr) -> do
          (reg_i, SomeWasmType reg_ty) <- onCmmLocalReg reg
          pure $
            SomeWasmPostCCall (reg_ty `TypeListCons` acc_tys) $
              case (# ret_hint, cmmRegWidth $ CmmLocal reg #) of
                (# SignedHint, W8 #) ->
                  acc_instr
                    `WasmConcat` WasmConst reg_ty 0xFF
                    `WasmConcat` WasmAnd reg_ty
                    `WasmConcat` WasmLocalSet reg_ty reg_i
                (# SignedHint, W16 #) ->
                  acc_instr
                    `WasmConcat` WasmConst reg_ty 0xFFFF
                    `WasmConcat` WasmAnd reg_ty
                    `WasmConcat` WasmLocalSet reg_ty reg_i
                _ -> acc_instr `WasmConcat` WasmLocalSet reg_ty reg_i
      )
      (SomeWasmPostCCall TypeListNil WasmNop)
      ret_locals_hints
  case target of
    Left sym_callee -> do
      platform <- wasmPlatformM
      let arg_cmm_tys = map (cmmExprType platform) arg_exprs
          ret_cmm_tys = map localRegType ret_locals
      onFuncSym sym_callee arg_cmm_tys ret_cmm_tys
      pure $
        WasmStatements $
          args_instr
            `WasmConcat` WasmCCall sym_callee
            `WasmConcat` ( case ret_info of
                             CmmMayReturn -> ret_instr
                             CmmNeverReturns -> WasmUnreachable
                         )
    Right fptr_callee -> do
      (WasmExpr instr_callee, _) <- lower_CmmExpr_Ptr lbl fptr_callee
      pure $
        WasmStatements $
          args_instr
            `WasmConcat` instr_callee
            `WasmConcat` WasmCCallIndirect arg_tys ret_tys
            `WasmConcat` ( case ret_info of
                             CmmMayReturn -> ret_instr
                             CmmNeverReturns -> WasmUnreachable
                         )
  where
    (# arg_exprs_hints, ret_locals_hints #) = case mb_hints of
      Just (arg_hints, ret_hints) ->
        (# zip arg_exprs arg_hints, zip ret_locals ret_hints #)
      _ -> (# map (,NoHint) arg_exprs, map (,NoHint) ret_locals #)

-- | Lower a 'CmmStore'.
lower_CmmStore ::
  CLabel ->
  CmmExpr ->
  CmmExpr ->
  AlignmentSpec ->
  WasmCodeGenM
    w
    (WasmStatements w)
lower_CmmStore lbl ptr val align = do
  platform <- wasmPlatformM
  (WasmExpr ptr_instr, o) <- lower_CmmExpr_Ptr lbl ptr
  let ty_cmm = cmmExprType platform val
  SomeWasmExpr ty (WasmExpr val_instr) <- lower_CmmExpr lbl val
  pure $
    WasmStatements $
      ptr_instr
        `WasmConcat` val_instr
        `WasmConcat` WasmStore ty (wasmMemoryNarrowing ty ty_cmm) o align

-- | Lower a single Cmm action.
lower_CmmAction :: CLabel -> CmmNode O O -> WasmCodeGenM w (WasmStatements w)
lower_CmmAction lbl act = do
  ty_word <- wasmWordTypeM
  platform <- wasmPlatformM
  case act of
    CmmComment {} -> pure $ WasmStatements WasmNop
    CmmTick {} -> pure $ WasmStatements WasmNop
    CmmUnwind {} -> pure $ WasmStatements WasmNop
    CmmAssign (CmmLocal reg) e -> do
      (i, SomeWasmType ty_reg) <- onCmmLocalReg reg
      WasmExpr instrs <- lower_CmmExpr_Typed lbl ty_reg e
      pure $ WasmStatements $ instrs `WasmConcat` WasmLocalSet ty_reg i
    CmmAssign (CmmGlobal (GlobalRegUse reg _)) e
      | BaseReg <- reg -> pure $ WasmStatements WasmNop
      | Just (sym_global, SomeWasmType ty_reg) <-
          globalInfoFromCmmGlobalReg ty_word reg -> do
          WasmExpr instrs <- lower_CmmExpr_Typed lbl ty_reg e
          pure $
            WasmStatements $
              instrs `WasmConcat` WasmGlobalSet ty_reg sym_global
      | otherwise -> do
          (WasmExpr ptr_instr, o) <-
            lower_CmmExpr_Ptr lbl $ get_GlobalReg_addr platform reg
          SomeWasmExpr ty_e (WasmExpr instrs) <- lower_CmmExpr lbl e
          pure $
            WasmStatements $
              ptr_instr
                `WasmConcat` instrs
                `WasmConcat` WasmStore ty_e Nothing o NaturallyAligned
    CmmStore ptr val align -> lower_CmmStore lbl ptr val align
    CmmUnsafeForeignCall
      ( ForeignTarget
          (CmmLit (CmmLabel lbl_callee))
          (ForeignConvention conv arg_hints ret_hints ret_info)
        )
      ret_locals
      arg_exprs
        | conv `elem` [CCallConv, CApiConv] ->
            lower_CmmUnsafeForeignCall
              lbl
              (Left $ symNameFromCLabel lbl_callee)
              (Just (arg_hints, ret_hints))
              ret_info
              ret_locals
              arg_exprs
    CmmUnsafeForeignCall
      (ForeignTarget target_expr (ForeignConvention conv arg_hints ret_hints ret_info))
      ret_locals
      arg_exprs
        | conv `elem` [CCallConv, CApiConv] ->
            lower_CmmUnsafeForeignCall
              lbl
              (Right target_expr)
              (Just (arg_hints, ret_hints))
              ret_info
              ret_locals
              arg_exprs
    CmmUnsafeForeignCall (PrimTarget op) ret_locals arg_exprs ->
      lower_CallishMachOp lbl op ret_locals arg_exprs
    _ -> panic "lower_CmmAction: unreachable"

-- | Lower a block of Cmm actions.
lower_CmmActions ::
  CLabel ->
  Label ->
  Block CmmNode O O ->
  WasmCodeGenM
    w
    (WasmStatements w)
lower_CmmActions lbl _ blk =
  foldlM
    ( \(WasmStatements acc) act ->
        (\(WasmStatements stmts) -> WasmStatements $ acc `WasmConcat` stmts)
          <$> lower_CmmAction lbl act
    )
    (WasmStatements WasmNop)
    acts
  where
    acts = blockToList blk

-- | Lower a 'CmmGraph'.
lower_CmmGraph :: CLabel -> CmmGraph -> WasmCodeGenM w (FuncBody w)
lower_CmmGraph lbl g = do
  ty_word <- wasmWordTypeM
  platform <- wasmPlatformM
  us <- getUniqueSupplyM
  body <-
    structuredControl
      platform
      us
      (\_ -> lower_CmmExpr_Typed lbl ty_word)
      (lower_CmmActions lbl)
      g
  locals <- wasmStateM $ \s ->
    (#
      map snd $ detEltsUFM $ localRegs s,
      s {localRegs = emptyUFM, localRegsCount = 0}
    #)
  pure FuncBody {funcLocals = locals, funcBody = wasmControlCast $ body}

-- | Invoked once for each 'CLabel' which indexes a 'CmmData' or
-- 'CmmProc'.
onTopSym :: CLabel -> WasmCodeGenM w ()
onTopSym lbl = case sym_vis of
  SymDefault -> wasmModifyM $ \s ->
    s
      { defaultSyms =
          insertUniqueSet
            (getUnique sym)
            $ defaultSyms s
      }
  _ -> pure ()
  where
    sym = symNameFromCLabel lbl

    sym_vis = symVisibilityFromCLabel lbl

-- | Invoked for each function 'CLabel' with known type (e.g. a
-- 'CmmProc', or callee of 'CmmUnsafeForeignCall').
onFuncSym :: SymName -> [CmmType] -> [CmmType] -> WasmCodeGenM w ()
onFuncSym sym arg_tys ret_tys = wasmModifyM $
  \s@WasmCodeGenState {..} ->
    s
      { funcTypes =
          addToUniqMap
            funcTypes
            sym
            ( map someWasmTypeFromCmmType arg_tys,
              map someWasmTypeFromCmmType ret_tys
            )
      }

-- | Invoked for all other 'CLabel's along the way, e.g. in
-- 'CmmStatic's or 'CmmExpr's.
onAnySym :: CLabel -> WasmCodeGenM w ()
onAnySym lbl = case sym_kind of
  SymFunc -> do
    ty_word <- wasmWordTypeM
    wasmModifyM $ \s@WasmCodeGenState {..} ->
      s {funcTypes = addToUniqMap_C const funcTypes sym ([], [SomeWasmType ty_word])}
  _ -> pure ()
  where
    sym = symNameFromCLabel lbl

    sym_kind = symKindFromCLabel lbl

-- | Invoked for each 'LocalReg', returning its wasm local id and
-- representation type.
onCmmLocalReg :: LocalReg -> WasmCodeGenM w LocalInfo
onCmmLocalReg reg = wasmStateM $ \s@WasmCodeGenState {..} ->
  let reg_info =
        (localRegsCount, someWasmTypeFromCmmType $ localRegType reg)
   in case addToUFM_L (\_ i _ -> i) reg reg_info localRegs of
        (Just i, _) -> (# i, s #)
        (_, localRegs') ->
          (#
            reg_info,
            s
              { localRegs = localRegs',
                localRegsCount =
                  localRegsCount + 1
              }
          #)

-- | Invoked for each 'LocalReg' with expected representation type,
-- only returning its wasm local id.
onCmmLocalReg_Typed :: WasmTypeTag t -> LocalReg -> WasmCodeGenM w Int
onCmmLocalReg_Typed ty reg = do
  (i, SomeWasmType ty') <- onCmmLocalReg reg
  if
      | Just Refl <- ty' `testEquality` ty -> pure i
      | otherwise -> panic "onCmmLocalReg_Typed: unreachable"

-- | Invoked for dtors. We don't bother to implement dtors yet;
-- there's no native @.fini_array@ support for wasm, and the way
-- @clang@ handles dtors is generating a ctor that calls @atexit()@
-- for dtors. Which makes some sense, but we don't need to do the same
-- thing yet.
onFini :: [SymName] -> WasmCodeGenM w ()
onFini syms = do
  let n_finis = length syms
  when (n_finis /= 0) $ panic "dtors unsupported by wasm32 NCG"

-- | Invoked for ctors and dtors.
onCmmInitFini :: InitOrFini -> [CLabel] -> WasmCodeGenM w ()
onCmmInitFini iof lbls = do
  for_ lbls $ \lbl -> onFuncSym (symNameFromCLabel lbl) [] []
  case iof of
    IsInitArray -> wasmModifyM $ \s -> s {ctors = syms <> ctors s}
    IsFiniArray -> onFini syms
  where
    syms = map symNameFromCLabel lbls

-- | Invoked for each data section.
onCmmData :: CLabel -> Section -> [CmmStatic] -> WasmCodeGenM w ()
onCmmData lbl s statics = do
  ty_word <- wasmWordTypeM
  onTopSym lbl
  cs <- for statics lower_CmmStatic
  let sym = symNameFromCLabel lbl
      sec =
        DataSection
          { dataSectionKind =
              dataSectionKindFromCmmSection s,
            dataSectionAlignment =
              alignmentFromCmmSection ty_word cs,
            dataSectionContents =
              case cs of
                [DataASCII buf] -> [DataASCII $ buf `BS.snoc` 0]
                [DataIncBin p l] -> [DataIncBin p l, DataI8 0]
                _ -> cs
          }
  wasmModifyM $ \s ->
    s
      { dataSections =
          addToUniqMap (dataSections s) sym sec
      }

-- | Invoked for each 'CmmProc'.
onCmmProc :: CLabel -> CmmGraph -> WasmCodeGenM w ()
onCmmProc lbl g = do
  ty_word <- wasmWordCmmTypeM
  onTopSym lbl
  onFuncSym sym [] [ty_word]
  body <- lower_CmmGraph lbl g
  wasmModifyM $ \s -> s {funcBodies = addToUniqMap (funcBodies s) sym body}
  where
    sym = symNameFromCLabel lbl

-- | Invoked for each 'RawCmmDecl'.
onCmmDecl :: RawCmmDecl -> WasmCodeGenM w ()
onCmmDecl decl
  | Just (iof, lbls) <- isInitOrFiniArray decl = onCmmInitFini iof lbls
onCmmDecl (CmmData s (CmmStaticsRaw lbl statics)) = onCmmData lbl s statics
onCmmDecl (CmmProc _ lbl _ g) = onCmmProc lbl g

-- | Invoked for each 'RawCmmGroup'.
onCmmGroup :: RawCmmGroup -> WasmCodeGenM w ()
onCmmGroup cmms = wasmStateM $ \s0 ->
  (# (), foldl' (\s cmm -> wasmExecM (onCmmDecl cmm) s) s0 cmms #)
