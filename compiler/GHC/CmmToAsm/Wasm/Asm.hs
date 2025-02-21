{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}

module GHC.CmmToAsm.Wasm.Asm (asmTellEverything, execWasmAsmM) where

import Control.Monad
import Control.Monad.Trans.Reader
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder
import qualified Data.ByteString.Char8 as BS8
import Data.Coerce
import Data.Foldable
import Data.Maybe
import Data.Semigroup
import GHC.Cmm
import GHC.CmmToAsm.Ppr
import GHC.CmmToAsm.Wasm.FromCmm
import GHC.CmmToAsm.Wasm.Types
import GHC.CmmToAsm.Wasm.Utils
import GHC.Data.FastString
import GHC.Float
import GHC.Prelude
import GHC.Settings.Config (cProjectVersion)
import GHC.Types.Basic
import GHC.Types.Unique
import GHC.Types.Unique.Map
import GHC.Types.Unique.Set
import GHC.Utils.Monad.State.Strict
import GHC.Utils.Outputable hiding ((<>))
import GHC.Utils.Panic (panic)

-- | Reads current indentation, appends result to state
newtype WasmAsmM a = WasmAsmM (WasmAsmConfig -> Builder -> State Builder a)
  deriving
    ( Functor,
      Applicative,
      Monad
    )
    via (ReaderT WasmAsmConfig (ReaderT Builder (State Builder)))

instance Semigroup a => Semigroup (WasmAsmM a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (WasmAsmM a) where
  mempty = pure mempty

getConf :: WasmAsmM WasmAsmConfig
getConf = WasmAsmM $ \conf _ -> pure conf

-- | Default indent level is none
execWasmAsmM :: WasmAsmConfig -> WasmAsmM a -> Builder
execWasmAsmM conf (WasmAsmM m) =
  execState (m conf mempty) mempty

-- | Increase indent level by a tab
asmWithTab :: WasmAsmM a -> WasmAsmM a
asmWithTab (WasmAsmM m) =
  WasmAsmM $ \conf t -> m conf $! char7 '\t' <> t

-- | Writes a single line starting with the current indent
asmTellLine :: Builder -> WasmAsmM ()
asmTellLine b = WasmAsmM $ \_ t -> modify $ \acc -> acc <> t <> b <> char7 '\n'

-- | Writes a single line break
asmTellLF :: WasmAsmM ()
asmTellLF = WasmAsmM $ \_ _ -> modify $ \acc -> acc <> char7 '\n'

-- | Writes a line starting with a single tab, ignoring current indent
-- level
asmTellTabLine :: Builder -> WasmAsmM ()
asmTellTabLine b =
  WasmAsmM $ \_ _ -> modify $ \acc -> acc <> char7 '\t' <> b <> char7 '\n'

asmFromWasmType :: WasmTypeTag t -> Builder
asmFromWasmType ty = case ty of
  TagI32 -> "i32"
  TagI64 -> "i64"
  TagF32 -> "f32"
  TagF64 -> "f64"

asmFromSomeWasmType :: SomeWasmType -> Builder
asmFromSomeWasmType (SomeWasmType t) = asmFromWasmType t

asmFromSomeWasmTypes :: [SomeWasmType] -> Builder
asmFromSomeWasmTypes ts = "(" <> builderCommas asmFromSomeWasmType ts <> ")"

asmFromFuncType :: [SomeWasmType] -> [SomeWasmType] -> Builder
asmFromFuncType arg_tys ret_tys =
  asmFromSomeWasmTypes arg_tys <> " -> " <> asmFromSomeWasmTypes ret_tys

asmTellFuncType ::
  SymName -> ([SomeWasmType], [SomeWasmType]) -> WasmAsmM ()
asmTellFuncType sym (arg_tys, ret_tys) =
  asmTellTabLine $
    ".functype "
      <> asmFromSymName sym
      <> " "
      <> asmFromFuncType arg_tys ret_tys

asmTellLocals :: [SomeWasmType] -> WasmAsmM ()
asmTellLocals [] = mempty
asmTellLocals local_tys =
  asmTellTabLine $ ".local " <> builderCommas asmFromSomeWasmType local_tys

asmFromSymName :: SymName -> Builder
asmFromSymName = shortByteString . coerce fastStringToShortByteString

asmTellDefSym :: SymName -> WasmAsmM ()
asmTellDefSym sym = do
  WasmAsmConfig {..} <- getConf
  unless pic $ asmTellTabLine $ ".hidden " <> asm_sym
  asmTellTabLine $ ".globl " <> asm_sym
  where
    asm_sym = asmFromSymName sym

asmTellDataSectionContent :: WasmTypeTag w -> DataSectionContent -> WasmAsmM ()
asmTellDataSectionContent ty_word c = asmTellTabLine $ case c of
  DataI8 i -> ".int8 0x" <> word8Hex i
  DataI16 i -> ".int16 0x" <> word16Hex i
  DataI32 i -> ".int32 0x" <> word32Hex i
  DataI64 i -> ".int64 0x" <> word64Hex i
  DataF32 f -> ".int32 0x" <> word32Hex (castFloatToWord32 f)
  DataF64 d -> ".int64 0x" <> word64Hex (castDoubleToWord64 d)
  DataSym sym o ->
    ( case ty_word of
        TagI32 -> ".int32 "
        TagI64 -> ".int64 "
        _ -> panic "asmTellDataSectionContent: unreachable"
    )
      <> asmFromSymName sym
      <> ( case compare o 0 of
             EQ -> mempty
             GT -> "+" <> intDec o
             LT -> panic "asmTellDataSectionContent: negative offset"
         )
  DataSkip i -> ".skip " <> intDec i
  DataASCII s
    | not (BS.null s) && BS.last s == 0 ->
        ".asciz \""
          <> string7
            (showSDocOneLine defaultSDocContext $ pprASCII $ BS.init s)
          <> "\""
    | otherwise ->
        ".ascii \""
          <> string7
            (showSDocOneLine defaultSDocContext $ pprASCII s)
          <> "\""
  DataIncBin f _ ->
    ".incbin "
      <> string7
        (showSDocOneLine defaultSDocContext $ pprFilePathString f)

dataSectionContentSize :: WasmTypeTag w -> DataSectionContent -> Int
dataSectionContentSize ty_word c = case c of
  DataI8 {} -> 1
  DataI16 {} -> 2
  DataI32 {} -> 4
  DataI64 {} -> 8
  DataF32 {} -> 4
  DataF64 {} -> 8
  DataSym {} -> alignmentBytes $ alignmentFromWordType ty_word
  DataSkip i -> i
  DataASCII s -> BS.length s
  DataIncBin _ l -> l

dataSectionSize :: WasmTypeTag w -> [DataSectionContent] -> Int
dataSectionSize ty_word =
  coerce
    . foldMap'
      (Sum . dataSectionContentSize ty_word)

asmTellAlign :: Alignment -> WasmAsmM ()
asmTellAlign a = case alignmentBytes a of
  1 -> mempty
  i -> asmTellTabLine $ ".p2align " <> intDec (countTrailingZeros i)

asmTellSectionHeader :: Builder -> WasmAsmM ()
asmTellSectionHeader k = asmTellTabLine $ ".section " <> k <> ",\"\",@"

asmTellDataSection ::
  WasmTypeTag w -> UniqueSet -> SymName -> DataSection -> WasmAsmM ()
asmTellDataSection ty_word def_syms sym DataSection {..} = do
  when (getUnique sym `memberUniqueSet` def_syms) $ asmTellDefSym sym
  asmTellSectionHeader sec_name
  asmTellAlign dataSectionAlignment
  asmTellTabLine asm_size
  asmTellLine $ asm_sym <> ":"
  for_ dataSectionContents $ asmTellDataSectionContent ty_word
  asmTellLF
  where
    asm_sym = asmFromSymName sym

    sec_name =
      ( case dataSectionKind of
          SectionData -> ".data."
          SectionROData -> ".rodata."
      )
        <> asm_sym

    asm_size =
      ".size "
        <> asm_sym
        <> ", "
        <> intDec
          (dataSectionSize ty_word dataSectionContents)

asmFromWasmBlockType :: WasmTypeTag w -> WasmFunctionType pre post -> Builder
asmFromWasmBlockType
  _
  (WasmFunctionType {ft_pops = TypeListNil, ft_pushes = TypeListNil}) =
    mempty
asmFromWasmBlockType
  TagI32
  ( WasmFunctionType
      { ft_pops = TypeListNil,
        ft_pushes = TypeListCons TagI32 TypeListNil
      }
    ) =
    " i32"
asmFromWasmBlockType
  TagI64
  ( WasmFunctionType
      { ft_pops = TypeListNil,
        ft_pushes = TypeListCons TagI64 TypeListNil
      }
    ) =
    " i64"
asmFromWasmBlockType _ _ = panic "asmFromWasmBlockType: invalid block type"

asmFromAlignmentSpec :: AlignmentSpec -> Builder
asmFromAlignmentSpec NaturallyAligned = mempty
asmFromAlignmentSpec Unaligned = ":p2align=0"

asmTellWasmInstr :: WasmTypeTag w -> WasmInstr w pre post -> WasmAsmM ()
asmTellWasmInstr ty_word instr = case instr of
  WasmComment c -> asmTellLine $ stringUtf8 $ "# " <> c
  WasmNop -> mempty
  WasmDrop -> asmTellLine "drop"
  WasmUnreachable -> asmTellLine "unreachable"
  WasmConst TagI32 i -> asmTellLine $ "i32.const " <> integerDec i
  WasmConst TagI64 i -> asmTellLine $ "i64.const " <> integerDec i
  WasmConst {} -> panic "asmTellWasmInstr: unreachable"
  WasmSymConst sym -> do
    WasmAsmConfig {..} <- getConf
    let
      asm_sym = asmFromSymName sym
      (ty_const, ty_add) = case ty_word of
        TagI32 -> ("i32.const ", "i32.add")
        TagI64 -> ("i64.const ", "i64.add")
        _ -> panic "asmTellWasmInstr: invalid word type"
    traverse_ asmTellLine $ if
      | pic, getUnique sym `memberUniqueSet` mbrelSyms -> [
          "global.get __memory_base",
          ty_const <> asm_sym <> "@MBREL",
          ty_add
        ]
      | pic, getUnique sym `memberUniqueSet` tbrelSyms -> [
          "global.get __table_base",
          ty_const <> asm_sym <> "@TBREL",
          ty_add
        ]
      | pic -> [ "global.get " <> asm_sym <> "@GOT" ]
      | otherwise -> [ ty_const <> asm_sym ]
  WasmLoad ty (Just w) s o align ->
    asmTellLine $
      asmFromWasmType ty
        <> ".load"
        <> intDec w
        <> ( case s of
               Signed -> "_s"
               Unsigned -> "_u"
           )
        <> " "
        <> intDec o
        <> asmFromAlignmentSpec align
  WasmLoad ty Nothing _ o align ->
    asmTellLine $
      asmFromWasmType ty
        <> ".load"
        <> " "
        <> intDec o
        <> asmFromAlignmentSpec align
  WasmStore ty (Just w) o align ->
    asmTellLine $
      asmFromWasmType ty
        <> ".store"
        <> intDec w
        <> " "
        <> intDec o
        <> asmFromAlignmentSpec align
  WasmStore ty Nothing o align ->
    asmTellLine $
      asmFromWasmType ty
        <> ".store"
        <> " "
        <> intDec o
        <> asmFromAlignmentSpec align
  WasmGlobalGet _ sym -> asmTellLine $ "global.get " <> asmFromSymName sym
  WasmGlobalSet _ sym -> asmTellLine $ "global.set " <> asmFromSymName sym
  WasmLocalGet _ i -> asmTellLine $ "local.get " <> intDec i
  WasmLocalSet _ i -> asmTellLine $ "local.set " <> intDec i
  WasmLocalTee _ i -> asmTellLine $ "local.tee " <> intDec i
  WasmCCall sym -> asmTellLine $ "call " <> asmFromSymName sym
  WasmCCallIndirect arg_tys ret_tys ->
    asmTellLine $
      "call_indirect "
        <> asmFromFuncType
          (someWasmTypesFromTypeList arg_tys)
          (someWasmTypesFromTypeList ret_tys)
  WasmConcat instr0 instr1 -> do
    asmTellWasmInstr ty_word instr0
    asmTellWasmInstr ty_word instr1
  WasmReinterpret t0 t1 ->
    asmTellLine $
      asmFromWasmType t1 <> ".reinterpret_" <> asmFromWasmType t0
  WasmTruncSat Signed t0 t1 ->
    asmTellLine $
      asmFromWasmType t1 <> ".trunc_sat_" <> asmFromWasmType t0 <> "_s"
  WasmTruncSat Unsigned t0 t1 ->
    asmTellLine $
      asmFromWasmType t1 <> ".trunc_sat_" <> asmFromWasmType t0 <> "_u"
  WasmConvert Signed t0 t1 ->
    asmTellLine $
      asmFromWasmType t1 <> ".convert_" <> asmFromWasmType t0 <> "_s"
  WasmConvert Unsigned t0 t1 ->
    asmTellLine $
      asmFromWasmType t1 <> ".convert_" <> asmFromWasmType t0 <> "_u"
  WasmAdd ty -> asmTellLine $ asmFromWasmType ty <> ".add"
  WasmSub ty -> asmTellLine $ asmFromWasmType ty <> ".sub"
  WasmMul ty -> asmTellLine $ asmFromWasmType ty <> ".mul"
  WasmDiv _ TagF32 -> asmTellLine "f32.div"
  WasmDiv _ TagF64 -> asmTellLine "f64.div"
  WasmDiv Signed ty -> asmTellLine $ asmFromWasmType ty <> ".div_s"
  WasmDiv Unsigned ty -> asmTellLine $ asmFromWasmType ty <> ".div_u"
  WasmRem Signed ty -> asmTellLine $ asmFromWasmType ty <> ".rem_s"
  WasmRem Unsigned ty -> asmTellLine $ asmFromWasmType ty <> ".rem_u"
  WasmAnd ty -> asmTellLine $ asmFromWasmType ty <> ".and"
  WasmOr ty -> asmTellLine $ asmFromWasmType ty <> ".or"
  WasmXor ty -> asmTellLine $ asmFromWasmType ty <> ".xor"
  WasmEq ty -> asmTellLine $ asmFromWasmType ty <> ".eq"
  WasmNe ty -> asmTellLine $ asmFromWasmType ty <> ".ne"
  WasmLt _ TagF32 -> asmTellLine "f32.lt"
  WasmLt _ TagF64 -> asmTellLine "f64.lt"
  WasmLt Signed ty -> asmTellLine $ asmFromWasmType ty <> ".lt_s"
  WasmLt Unsigned ty -> asmTellLine $ asmFromWasmType ty <> ".lt_u"
  WasmGt _ TagF32 -> asmTellLine "f32.gt"
  WasmGt _ TagF64 -> asmTellLine "f64.gt"
  WasmGt Signed ty -> asmTellLine $ asmFromWasmType ty <> ".gt_s"
  WasmGt Unsigned ty -> asmTellLine $ asmFromWasmType ty <> ".gt_u"
  WasmLe _ TagF32 -> asmTellLine "f32.le"
  WasmLe _ TagF64 -> asmTellLine "f64.le"
  WasmLe Signed ty -> asmTellLine $ asmFromWasmType ty <> ".le_s"
  WasmLe Unsigned ty -> asmTellLine $ asmFromWasmType ty <> ".le_u"
  WasmGe _ TagF32 -> asmTellLine "f32.ge"
  WasmGe _ TagF64 -> asmTellLine "f64.ge"
  WasmGe Signed ty -> asmTellLine $ asmFromWasmType ty <> ".ge_s"
  WasmGe Unsigned ty -> asmTellLine $ asmFromWasmType ty <> ".ge_u"
  WasmShl ty -> asmTellLine $ asmFromWasmType ty <> ".shl"
  WasmShr Signed ty -> asmTellLine $ asmFromWasmType ty <> ".shr_s"
  WasmShr Unsigned ty -> asmTellLine $ asmFromWasmType ty <> ".shr_u"
  WasmI32Extend8S -> asmTellLine "i32.extend8_s"
  WasmI32Extend16S -> asmTellLine "i32.extend16_s"
  WasmI64Extend8S -> asmTellLine "i64.extend8_s"
  WasmI64Extend16S -> asmTellLine "i64.extend16_s"
  WasmI64Extend32S -> asmTellLine "i64.extend32_s"
  WasmI64ExtendI32 Signed -> asmTellLine "i64.extend_i32_s"
  WasmI64ExtendI32 Unsigned -> asmTellLine "i64.extend_i32_u"
  WasmI32WrapI64 -> asmTellLine "i32.wrap_i64"
  WasmF32DemoteF64 -> asmTellLine "f32.demote_f64"
  WasmF64PromoteF32 -> asmTellLine "f64.promote_f32"
  WasmAbs ty -> asmTellLine $ asmFromWasmType ty <> ".abs"
  WasmSqrt ty -> asmTellLine $ asmFromWasmType ty <> ".sqrt"
  WasmNeg ty -> asmTellLine $ asmFromWasmType ty <> ".neg"
  WasmMin ty -> asmTellLine $ asmFromWasmType ty <> ".min"
  WasmMax ty -> asmTellLine $ asmFromWasmType ty <> ".max"
  WasmCond t -> do
    asmTellLine "if"
    asmWithTab $ asmTellWasmInstr ty_word t
    asmTellLine "end_if"

asmTellWasmControl ::
  WasmTypeTag w ->
  WasmControl
    (WasmStatements w)
    (WasmExpr w a)
    pre
    post ->
  WasmAsmM ()
asmTellWasmControl ty_word c = case c of
  WasmPush _ (WasmExpr e) -> asmTellWasmInstr ty_word e
  WasmBlock bt c -> do
    asmTellLine $ "block" <> asmFromWasmBlockType ty_word bt
    asmWithTab $ asmTellWasmControl ty_word c
    asmTellLine "end_block"
  WasmLoop bt c -> do
    asmTellLine $ "loop" <> asmFromWasmBlockType ty_word bt
    asmWithTab $ asmTellWasmControl ty_word c
    asmTellLine "end_loop"
  WasmIfTop bt t f -> do
    asmTellLine $ "if" <> asmFromWasmBlockType ty_word bt
    asmWithTab $ asmTellWasmControl ty_word t
    asmTellLine "else"
    asmWithTab $ asmTellWasmControl ty_word f
    asmTellLine "end_if"
  WasmBr i -> asmTellLine $ "br " <> intDec i
  WasmFallthrough -> mempty
  WasmBrTable (WasmExpr e) _ ts t -> do
    asmTellWasmInstr ty_word e
    asmTellLine $ "br_table {" <> builderCommas intDec (ts <> [t]) <> "}"
  -- See Note [WasmTailCall]
  WasmTailCall (WasmExpr e) -> do
    WasmAsmConfig {..} <- getConf
    if
        | tailcall,
          WasmSymConst sym <- e ->
            asmTellLine $ "return_call " <> asmFromSymName sym
        | tailcall ->
            do
              asmTellWasmInstr ty_word e
              asmTellLine $
                "return_call_indirect "
                  <> asmFromFuncType
                    []
                    [SomeWasmType ty_word]
        | otherwise ->
            do
              asmTellWasmInstr ty_word e
              asmTellLine "return"
  WasmActions (WasmStatements a) -> asmTellWasmInstr ty_word a
  WasmSeq c0 c1 -> do
    asmTellWasmControl ty_word c0
    asmTellWasmControl ty_word c1

asmTellFunc ::
  WasmTypeTag w ->
  UniqueSet ->
  SymName ->
  (([SomeWasmType], [SomeWasmType]), FuncBody w) ->
  WasmAsmM ()
asmTellFunc ty_word def_syms sym (func_ty, FuncBody {..}) = do
  when (getUnique sym `memberUniqueSet` def_syms) $ asmTellDefSym sym
  asmTellSectionHeader $ ".text." <> asm_sym
  asmTellLine $ asm_sym <> ":"
  asmTellFuncType sym func_ty
  asmTellLocals funcLocals
  asmWithTab $ asmTellWasmControl ty_word funcBody
  asmTellTabLine "end_function"
  asmTellLF
  where
    asm_sym = asmFromSymName sym

asmTellGlobals :: WasmTypeTag w -> WasmAsmM ()
asmTellGlobals ty_word = do
  WasmAsmConfig {..} <- getConf
  when pic $ traverse_ asmTellTabLine [
      ".globaltype __memory_base, i32, immutable",
      ".globaltype __table_base, i32, immutable"
    ]
  for_ supportedCmmGlobalRegs $ \reg ->
    let
      (sym, ty) = fromJust $ globalInfoFromCmmGlobalReg ty_word reg
      asm_sym = asmFromSymName sym
     in do
      asmTellTabLine $
          ".globaltype "
            <> asm_sym
            <> ", "
            <> asmFromSomeWasmType ty
      when pic $ traverse_ asmTellTabLine [
          ".import_module " <> asm_sym <> ", regs",
          ".import_name " <> asm_sym <> ", " <> asm_sym
        ]
  asmTellLF

asmTellCtors :: WasmTypeTag w -> [SymName] -> WasmAsmM ()
asmTellCtors _ [] = mempty
asmTellCtors ty_word syms = do
  -- See Note [JSFFI initialization] for details
  asmTellSectionHeader ".init_array.101"
  asmTellAlign $ alignmentFromWordType ty_word
  for_ syms $ \sym ->
    asmTellTabLine $
      ( case ty_word of
          TagI32 -> ".int32 "
          TagI64 -> ".int64 "
          _ -> panic "asmTellCtors: unreachable"
      )
        <> asmFromSymName sym
  asmTellLF

asmTellBS :: ByteString -> WasmAsmM ()
asmTellBS s = do
  asmTellTabLine $ ".int8 " <> intDec (BS.length s)
  asmTellTabLine $
    ".ascii \""
      <> string7
        (showSDocOneLine defaultSDocContext $ pprASCII s)
      <> "\""

asmTellVec :: [WasmAsmM ()] -> WasmAsmM ()
asmTellVec xs = do
  asmTellTabLine $ ".int8 " <> intDec (length xs)
  sequence_ xs

asmTellProducers :: WasmAsmM ()
asmTellProducers = do
  asmTellSectionHeader ".custom_section.producers"
  asmTellVec
    [ do
        asmTellBS "processed-by"
        asmTellVec
          [ do
              asmTellBS "ghc"
              asmTellBS $ BS8.pack cProjectVersion
          ]
    ]

asmTellTargetFeatures :: WasmAsmM ()
asmTellTargetFeatures = do
  WasmAsmConfig {..} <- getConf
  asmTellSectionHeader ".custom_section.target_features"
  asmTellVec
    [ do
        asmTellTabLine ".int8 0x2b"
        asmTellBS feature
      | feature <-
          ["tail-call" | tailcall]
            <> [ "bulk-memory",
                 "mutable-globals",
                 "nontrapping-fptoint",
                 "reference-types",
                 "sign-ext"
               ]
    ]

asmTellEverything :: WasmTypeTag w -> WasmCodeGenState w -> WasmAsmM ()
asmTellEverything ty_word WasmCodeGenState {..} = do
  asmTellGlobals ty_word
  asm_functypes
  asm_funcs
  asm_data_secs
  asm_ctors
  asmTellProducers
  asmTellTargetFeatures
  where
    asm_functypes = do
      for_
        (detEltsUniqMap $ funcTypes `minusUniqMap` funcBodies)
        (uncurry asmTellFuncType)
      asmTellLF

    asm_funcs = do
      for_
        (detEltsUniqMap $ intersectUniqMap_C (,) funcTypes funcBodies)
        (uncurry $ asmTellFunc ty_word defaultSyms)
      asmTellLF

    asm_data_secs = do
      for_
        (detEltsUniqMap dataSections)
        (uncurry (asmTellDataSection ty_word defaultSyms))
      asmTellLF

    asm_ctors = asmTellCtors ty_word ctors
