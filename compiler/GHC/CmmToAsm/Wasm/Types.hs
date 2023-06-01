{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module GHC.CmmToAsm.Wasm.Types
  ( WasmType (..),
    WasmTypeTag (..),
    SomeWasmType (..),
    TypeList (..),
    someWasmTypesFromTypeList,
    WasmFunctionType (..),
    SymName (..),
    SymVisibility (..),
    SymKind (..),
    DataSectionKind (..),
    DataSectionContent (..),
    DataSection (..),
    GlobalInfo,
    LocalInfo,
    FuncBody (..),
    Signage (..),
    WasmInstr (..),
    WasmExpr (..),
    SomeWasmExpr (..),
    WasmStatements (..),
    WasmControl (..),
    BrTableInterval (..),
    wasmControlCast,
    WasmCodeGenState (..),
    initialWasmCodeGenState,
    WasmCodeGenM (..),
    wasmGetsM,
    wasmPlatformM,
    wasmWordTypeM,
    wasmWordCmmTypeM,
    wasmStateM,
    wasmModifyM,
    wasmExecM,
    wasmUniq,
  )
where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Coerce
import Data.Functor
import qualified GHC.Data.Word64Set as WS
import Data.Kind
import Data.String
import Data.Type.Equality
import Data.Word
import GHC.Cmm
import GHC.Data.FastString
import GHC.Float
import GHC.Platform
import GHC.Prelude
import GHC.Types.Basic
import GHC.Types.Unique
import GHC.Types.Unique.FM
import GHC.Types.Unique.Map
import GHC.Types.Unique.Supply
import GHC.Utils.Monad.State.Strict
import GHC.Utils.Outputable hiding ((<>))
import Unsafe.Coerce

-- | WebAssembly type of a WebAssembly value that WebAssembly code
-- could either expect on the evaluation stack or leave on the
-- evaluation stack.
data WasmType = I32 | I64 | F32 | F64

-- | Singleton type useful for programming with `WasmType` at the type
-- level.
data WasmTypeTag :: WasmType -> Type where
  TagI32 :: WasmTypeTag 'I32
  TagI64 :: WasmTypeTag 'I64
  TagF32 :: WasmTypeTag 'F32
  TagF64 :: WasmTypeTag 'F64

deriving instance Show (WasmTypeTag t)

instance TestEquality WasmTypeTag where
  TagI32 `testEquality` TagI32 = Just Refl
  TagI64 `testEquality` TagI64 = Just Refl
  TagF32 `testEquality` TagF32 = Just Refl
  TagF64 `testEquality` TagF64 = Just Refl
  _ `testEquality` _ = Nothing

data SomeWasmType where
  SomeWasmType :: WasmTypeTag t -> SomeWasmType

instance Eq SomeWasmType where
  SomeWasmType ty0 == SomeWasmType ty1
    | Just Refl <- ty0 `testEquality` ty1 = True
    | otherwise = False

-- | List of WebAssembly types used to describe the sequence of
-- WebAssembly values that a block of code may expect on the stack or
-- leave on the stack.
data TypeList :: [WasmType] -> Type where
  TypeListNil :: TypeList '[]
  TypeListCons :: WasmTypeTag t -> TypeList ts -> TypeList (t : ts)

someWasmTypesFromTypeList :: TypeList ts -> [SomeWasmType]
someWasmTypesFromTypeList TypeListNil = []
someWasmTypesFromTypeList (ty `TypeListCons` tys) =
  SomeWasmType ty : someWasmTypesFromTypeList tys

-- | The type of a WebAssembly function, loop, block, or conditional.
-- This type says what values the code expects to pop off the stack
-- and what values it promises to push.  The WebAssembly standard
-- requires that this type appear explicitly in the code.
data WasmFunctionType pre post = WasmFunctionType {ft_pops :: TypeList pre, ft_pushes :: TypeList post}

-- | For simplicity, we record other metadata in 'WasmCodeGenState' by
-- need, instead of carrying them along with 'SymName'.
newtype SymName = SymName FastString
  deriving (Eq, IsString, Show, Uniquable) via FastString
  deriving (Ord) via LexicalFastString

data SymVisibility
  = -- | Not defined in the current compilation unit.
    --
    -- @[ undefined binding=global vis=default ]@
    SymUndefined
  | -- | Defined, not visible to other compilation units.
    --
    -- @[ binding=local vis=default ]@
    SymStatic
  | -- | Defined, visible to other compilation units.
    --
    -- Adds @.hidden@ & @.globl@ directives in the output assembly.
    --
    -- @[ binding=global vis=hidden ]@
    SymDefault

-- | Represents whether a symbol is a data symbol or a function
-- symbol. Unlike linkers for other targets, @wasm-ld@ does panic at
-- link-time if it finds symbol kind inconsistency between the
-- definition site and other use sites.
--
-- Currently we solely rely on 'isCFunctionLabel' to determine a
-- symbol's kind, but it does take extra effort to make it work. The
-- main source of inconsistency arises from hand-written Cmm sources,
-- where it's possible to refer to external entities like @xxx_info@
-- and @xxx_closure@ without explicit @import CLOSURE@ declarations.
-- The Cmm parser will implicitly assume those are foreign function
-- labels, and then this will break the WebAssembly backend. #22368
-- provides more context on this issue.
--
-- tl;dr for any GHC contributor that accidentally triggers @wasm-ld@
-- errors when hacking Cmm: whatever data symbols are used in new
-- code, just add the corresponding @import CLOSURE@ declarations at
-- the top of that Cmm file.
data SymKind = SymData | SymFunc
  deriving (Eq)

-- | WebAssembly doesn't really have proper read-only memory regions
-- yet. Neverthless we add the .rodata logic here, wasm-ld will
-- aggregate all .rodata sections into a single one, which adds
-- possibility for runtime checks later, either via a customized
-- runtime, or via code instrumentation. See
-- <https://github.com/llvm/llvm-project/blob/b296aed8ae239c20ebdd7969e978f8d2a3b9c178/lld/wasm/Writer.cpp#L856>
data DataSectionKind = SectionData | SectionROData

-- | Neither Cmm or Wasm type system takes integer signedness into
-- account, therefore we always round up a 'CmmLit' to the right width
-- and handle it as an untyped integer.
data DataSectionContent
  = DataI8 Word8
  | DataI16 Word16
  | DataI32 Word32
  | DataI64 Word64
  | DataF32 Float
  | DataF64 Double
  | DataSym SymName Int
  | DataSkip Int
  | DataASCII ByteString
  | DataIncBin FilePath Int

data DataSection = DataSection
  { dataSectionKind :: DataSectionKind,
    dataSectionAlignment ::
      Alignment,
    dataSectionContents :: [DataSectionContent]
  }

-- | We need to remember the symbols. Determinism is achieved by
-- sorting symbols before writing the assembly.
type SymMap = UniqMap SymName

-- | No need to remember the symbols.
type SymSet = WS.Word64Set

type GlobalInfo = (SymName, SomeWasmType)

type LocalInfo = (Int, SomeWasmType)

data FuncBody w = FuncBody
  { funcLocals :: [SomeWasmType],
    -- | Most are Cmm functions, but may also contain synthesized
    -- function of other types, sigh.
    funcBody :: WasmControl (WasmStatements w) (WasmExpr w w) '[] '[w]
  }

data Signage = Signed | Unsigned

-- | The @w@ type variable in the Wasm IR stands for "platform word
-- type", so 'TagI32' on wasm32, and 'TagI64' on wasm64. This way, we
-- can make the codegen logic work on both wasm32/wasm64 in a
-- type-safe manner.
data WasmInstr :: WasmType -> [WasmType] -> [WasmType] -> Type where
  WasmComment :: String -> WasmInstr w pre pre
  WasmNop :: WasmInstr w pre pre
  WasmDrop :: WasmInstr w (t : pre) pre
  WasmUnreachable :: WasmInstr w pre post
  WasmConst :: WasmTypeTag t -> Integer -> WasmInstr w pre (t : pre)
  WasmSymConst :: SymName -> WasmInstr w pre (w : pre)
  WasmLoad ::
    WasmTypeTag t ->
    Maybe Int ->
    Signage ->
    Int ->
    AlignmentSpec ->
    WasmInstr w (w : pre) (t : pre)
  WasmStore ::
    WasmTypeTag t ->
    Maybe Int ->
    Int ->
    AlignmentSpec ->
    WasmInstr
      w
      (t : w : pre)
      pre
  WasmGlobalGet :: WasmTypeTag t -> SymName -> WasmInstr w pre (t : pre)
  WasmGlobalSet :: WasmTypeTag t -> SymName -> WasmInstr w (t : pre) pre
  WasmLocalGet :: WasmTypeTag t -> Int -> WasmInstr w pre (t : pre)
  WasmLocalSet :: WasmTypeTag t -> Int -> WasmInstr w (t : pre) pre
  WasmLocalTee :: WasmTypeTag t -> Int -> WasmInstr w (t : pre) (t : pre)
  WasmCCall :: SymName -> WasmInstr w pre post
  WasmCCallIndirect ::
    TypeList arg_tys ->
    TypeList ret_tys ->
    WasmInstr
      w
      (w : pre)
      post
  WasmConcat ::
    WasmInstr w pre mid ->
    WasmInstr w mid post ->
    WasmInstr w pre post
  WasmReinterpret ::
    WasmTypeTag t0 ->
    WasmTypeTag t1 ->
    WasmInstr
      w
      (t0 : pre)
      (t1 : pre)
  WasmTruncSat ::
    Signage ->
    WasmTypeTag t0 ->
    WasmTypeTag t1 ->
    WasmInstr
      w
      (t0 : pre)
      (t1 : pre)
  WasmConvert ::
    Signage ->
    WasmTypeTag t0 ->
    WasmTypeTag t1 ->
    WasmInstr
      w
      (t0 : pre)
      (t1 : pre)
  WasmAdd :: WasmTypeTag t -> WasmInstr w (t : t : pre) (t : pre)
  WasmSub :: WasmTypeTag t -> WasmInstr w (t : t : pre) (t : pre)
  WasmMul :: WasmTypeTag t -> WasmInstr w (t : t : pre) (t : pre)
  WasmDiv :: Signage -> WasmTypeTag t -> WasmInstr w (t : t : pre) (t : pre)
  WasmRem :: Signage -> WasmTypeTag t -> WasmInstr w (t : t : pre) (t : pre)
  WasmAnd :: WasmTypeTag t -> WasmInstr w (t : t : pre) (t : pre)
  WasmOr :: WasmTypeTag t -> WasmInstr w (t : t : pre) (t : pre)
  WasmXor :: WasmTypeTag t -> WasmInstr w (t : t : pre) (t : pre)
  WasmEq :: WasmTypeTag t -> WasmInstr w (t : t : pre) (w : pre)
  WasmNe :: WasmTypeTag t -> WasmInstr w (t : t : pre) (w : pre)
  WasmLt :: Signage -> WasmTypeTag t -> WasmInstr w (t : t : pre) (w : pre)
  WasmGt :: Signage -> WasmTypeTag t -> WasmInstr w (t : t : pre) (w : pre)
  WasmLe :: Signage -> WasmTypeTag t -> WasmInstr w (t : t : pre) (w : pre)
  WasmGe :: Signage -> WasmTypeTag t -> WasmInstr w (t : t : pre) (w : pre)
  WasmShl :: WasmTypeTag t -> WasmInstr w (t : t : pre) (t : pre)
  WasmShr :: Signage -> WasmTypeTag t -> WasmInstr w (t : t : pre) (t : pre)
  WasmI32Extend8S :: WasmInstr w ('I32 : pre) ('I32 : pre)
  WasmI32Extend16S :: WasmInstr w ('I32 : pre) ('I32 : pre)
  WasmI64Extend8S :: WasmInstr w ('I64 : pre) ('I64 : pre)
  WasmI64Extend16S :: WasmInstr w ('I64 : pre) ('I64 : pre)
  WasmI64Extend32S :: WasmInstr w ('I64 : pre) ('I64 : pre)
  WasmI64ExtendI32 :: Signage -> WasmInstr w ('I32 : pre) ('I64 : pre)
  WasmI32WrapI64 :: WasmInstr w ('I64 : pre) ('I32 : pre)
  WasmF32DemoteF64 :: WasmInstr w ('F64 : pre) ('F32 : pre)
  WasmF64PromoteF32 :: WasmInstr w ('F32 : pre) ('F64 : pre)
  WasmAbs :: WasmTypeTag t -> WasmInstr w (t : pre) (t : pre)
  WasmNeg :: WasmTypeTag t -> WasmInstr w (t : pre) (t : pre)
  WasmCond :: WasmInstr w pre pre -> WasmInstr w (w : pre) pre

newtype WasmExpr w t = WasmExpr (forall pre. WasmInstr w pre (t : pre))

data SomeWasmExpr w where
  SomeWasmExpr :: WasmTypeTag t -> WasmExpr w t -> SomeWasmExpr w

newtype WasmStatements w = WasmStatements (forall pre. WasmInstr w pre pre)

-- | Representation of WebAssembly control flow.
-- Normally written as
-- @
--   WasmControl s e pre post
-- @
-- Type parameter `s` is the type of (unspecified) statements.
-- It might be instantiated with an open Cmm block or with a sequence
-- of Wasm instructions.
-- Parameter `e` is the type of expressions.
-- Parameter `pre` represents the values that are expected on the
-- WebAssembly stack when the code runs, and `post` represents
-- the state of the stack on completion.
data WasmControl :: Type -> Type -> [WasmType] -> [WasmType] -> Type where
  WasmPush :: WasmTypeTag t -> e -> WasmControl s e stack (t : stack)
  WasmBlock ::
    WasmFunctionType pre post ->
    WasmControl s e pre post ->
    WasmControl s e pre post
  WasmLoop ::
    WasmFunctionType pre post ->
    WasmControl s e pre post ->
    WasmControl s e pre post
  WasmIfTop ::
    WasmFunctionType pre post ->
    WasmControl s e pre post ->
    WasmControl s e pre post ->
    WasmControl s e ('I32 : pre) post
  WasmBr :: Int -> WasmControl s e dropped destination -- not typechecked
  WasmFallthrough :: WasmControl s e dropped destination
  -- generates no code, but has the same type as a branch
  WasmBrTable ::
    e ->
    BrTableInterval -> -- for testing
    [Int] -> -- targets
    Int -> -- default target
    WasmControl s e dropped destination
  -- invariant: the table interval is contained
  -- within [0 .. pred (length targets)]

  -- Note [WasmTailCall]
  -- ~~~~~~~~~~~~~~~~~~~
  -- This represents the exit point of each CmmGraph: tail calling the
  -- destination in CmmCall. The STG stack may grow before the call,
  -- but it's always a tail call in the sense that the C call stack is
  -- guaranteed not to grow.
  --
  -- In the wasm backend, WasmTailCall is lowered to different
  -- assembly code given whether the wasm tail-call extension is
  -- enabled:
  --
  -- When tail-call is not enabled (which is the default as of today),
  -- a WasmTailCall is lowered to code that pushes the callee function
  -- pointer onto the value stack and returns immediately. The actual
  -- call is done by the trampoline in StgRun.
  --
  -- When tail-call is indeed enabled via passing -mtail-call in
  -- CONF_CC_OPTS_STAGE2 at configure time, a WasmTailCall is lowered
  -- to return_call/return_call_indirect, thus tail calling into its
  -- callee without returning to StgRun.
  WasmTailCall ::
    e ->
    WasmControl s e t1star t2star -- as per type system
  WasmActions ::
    s ->
    WasmControl s e stack stack -- basic block: one entry, one exit
  WasmSeq ::
    WasmControl s e pre mid ->
    WasmControl s e mid post ->
    WasmControl s e pre post

data BrTableInterval = BrTableInterval {bti_lo :: Integer, bti_count :: Integer}
  deriving (Show)

instance Outputable BrTableInterval where
  ppr range =
    brackets $
      hcat
        [integer (bti_lo range), text "..", integer hi]
    where
      hi = bti_lo range + bti_count range - 1

wasmControlCast :: WasmControl s e pre post -> WasmControl s e pre' post'
wasmControlCast = unsafeCoerce

data WasmCodeGenState w = WasmCodeGenState
  { -- | Target platform
    wasmPlatform :: Platform,
    -- | Defined symbols with 'SymDefault' visibility.
    defaultSyms :: SymSet,
    -- | Function types, defined or not. There may exist a function
    -- whose type is unknown (e.g. as a function pointer), in that
    -- case we fall back to () -> (), it's imperfect but works with
    -- wasm-ld.
    funcTypes :: SymMap ([SomeWasmType], [SomeWasmType]),
    -- | Defined function bodies.
    funcBodies :: SymMap (FuncBody w),
    -- | Defined data sections.
    dataSections :: SymMap DataSection,
    -- | ctors in the current compilation unit.
    ctors :: [SymName],
    localRegs ::
      UniqFM LocalReg LocalInfo,
    localRegsCount ::
      Int,
    wasmUniqSupply :: UniqSupply
  }

initialWasmCodeGenState :: Platform -> UniqSupply -> WasmCodeGenState w
initialWasmCodeGenState platform us =
  WasmCodeGenState
    { wasmPlatform =
        platform,
      defaultSyms = WS.empty,
      funcTypes = emptyUniqMap,
      funcBodies =
        emptyUniqMap,
      dataSections = emptyUniqMap,
      ctors =
        [],
      localRegs = emptyUFM,
      localRegsCount = 0,
      wasmUniqSupply = us
    }

newtype WasmCodeGenM w a = WasmCodeGenM (State (WasmCodeGenState w) a)
  deriving newtype (Functor, Applicative, Monad)

wasmGetsM :: (WasmCodeGenState w -> a) -> WasmCodeGenM w a
wasmGetsM = coerce . gets

wasmPlatformM :: WasmCodeGenM w Platform
wasmPlatformM = wasmGetsM wasmPlatform

wasmWordTypeM :: WasmCodeGenM w (WasmTypeTag w)
wasmWordTypeM = wasmGetsM $ \s ->
  if target32Bit $ wasmPlatform s
    then unsafeCoerce TagI32
    else unsafeCoerce TagI64

wasmWordCmmTypeM :: WasmCodeGenM w CmmType
wasmWordCmmTypeM = wasmGetsM (bWord . wasmPlatform)

wasmStateM ::
  (WasmCodeGenState w -> (# a, WasmCodeGenState w #)) ->
  WasmCodeGenM w a
wasmStateM = coerce . State

wasmModifyM :: (WasmCodeGenState w -> WasmCodeGenState w) -> WasmCodeGenM w ()
wasmModifyM = coerce . modify

wasmExecM :: WasmCodeGenM w a -> WasmCodeGenState w -> WasmCodeGenState w
wasmExecM (WasmCodeGenM s) = execState s

wasmUniq :: WasmCodeGenM w Unique
wasmUniq = wasmStateM $
  \s@WasmCodeGenState {..} -> case takeUniqFromSupply wasmUniqSupply of
    (u, us) -> (# u, s {wasmUniqSupply = us} #)
