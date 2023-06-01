{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.StgToJS.Types
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Jeffrey Young  <jeffrey.young@iohk.io>
--                Luite Stegeman <luite.stegeman@iohk.io>
--                Sylvain Henry  <sylvain.henry@iohk.io>
--                Josh Meredith  <josh.meredith@iohk.io>
-- Stability   :  experimental
--
--
-- Module that holds the Types required for the StgToJS pass
-----------------------------------------------------------------------------

module GHC.StgToJS.Types where

import GHC.Prelude

import GHC.JS.Syntax
import GHC.JS.Make
import GHC.JS.Ppr ()

import GHC.Stg.Syntax
import GHC.Core.TyCon

import GHC.Types.Unique
import GHC.Types.Unique.FM
import GHC.Types.Var
import GHC.Types.ForeignCall

import Control.Monad.Trans.State.Strict
import GHC.Utils.Outputable (Outputable (..), text, SDocContext, (<+>), ($$))

import GHC.Data.FastString
import GHC.Data.FastMutInt

import GHC.Unit.Module

import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.ByteString as BS
import           Data.Monoid
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import           Control.DeepSeq
import           Data.Word

-- | A State monad over IO holding the generator state.
type G = StateT GenState IO

-- | The JS code generator state
data GenState = GenState
  { gsSettings  :: !StgToJSConfig         -- ^ codegen settings, read-only
  , gsModule    :: !Module                -- ^ current module
  , gsId        :: {-# UNPACK #-} !FastMutInt -- ^ unique number for the id generator
  , gsIdents    :: !IdCache               -- ^ hash consing for identifiers from a Unique
  , gsUnfloated :: !(UniqFM Id CgStgExpr) -- ^ unfloated arguments
  , gsGroup     :: GenGroupState          -- ^ state for the current binding group
  , gsGlobal    :: [JStat]                -- ^ global (per module) statements (gets included when anything else from the module is used)
  }

-- | The JS code generator state relevant for the current binding group
data GenGroupState = GenGroupState
  { ggsToplevelStats :: [JStat]        -- ^ extra toplevel statements for the binding group
  , ggsClosureInfo   :: [ClosureInfo]  -- ^ closure metadata (info tables) for the binding group
  , ggsStatic        :: [StaticInfo]   -- ^ static (CAF) data in our binding group
  , ggsStack         :: [StackSlot]    -- ^ stack info for the current expression
  , ggsStackDepth    :: Int            -- ^ current stack depth
  , ggsExtraDeps     :: Set OtherSymb  -- ^ extra dependencies for the linkable unit that contains this group
  , ggsGlobalIdCache :: GlobalIdCache
  , ggsForeignRefs   :: [ForeignJSRef]
  }

-- | The Configuration record for the StgToJS pass
data StgToJSConfig = StgToJSConfig
  -- flags
  { csInlinePush      :: !Bool
  , csInlineBlackhole :: !Bool
  , csInlineLoadRegs  :: !Bool
  , csInlineEnter     :: !Bool
  , csInlineAlloc     :: !Bool
  , csTraceRts        :: !Bool
  , csAssertRts       :: !Bool
  , csBoundsCheck     :: !Bool
  , csDebugAlloc      :: !Bool
  , csTraceForeign    :: !Bool
  , csProf            :: !Bool -- ^ Profiling enabled
  , csRuntimeAssert   :: !Bool -- ^ Enable runtime assertions
  -- settings
  , csContext         :: !SDocContext
  }

-- | Information relevenat to code generation for closures.
data ClosureInfo = ClosureInfo
  { ciVar     :: Ident      -- ^ object being infod
  , ciRegs    :: CIRegs     -- ^ size of the payload (in number of JS values)
  , ciName    :: FastString -- ^ friendly name for printing
  , ciLayout  :: CILayout   -- ^ heap/stack layout of the object
  , ciType    :: CIType     -- ^ type of the object, with extra info where required
  , ciStatic  :: CIStatic   -- ^ static references of this object
  }
  deriving stock (Eq, Show, Generic)

-- | Closure information, 'ClosureInfo', registers
data CIRegs
  = CIRegsUnknown                     -- ^ A value witnessing a state of unknown registers
  | CIRegs { ciRegsSkip  :: Int       -- ^ unused registers before actual args start
           , ciRegsTypes :: [VarType] -- ^ args
           }
  deriving stock (Eq, Ord, Show, Generic)

instance NFData CIRegs

-- | Closure Information, 'ClosureInfo', layout
data CILayout
  = CILayoutVariable            -- ^ layout stored in object itself, first position from the start
  | CILayoutUnknown             -- ^ fixed size, but content unknown (for example stack apply frame)
      { layoutSize :: !Int
      }
  | CILayoutFixed               -- ^ whole layout known
      { layoutSize :: !Int      -- ^ closure size in array positions, including entry
      , layout     :: [VarType] -- ^ The set of sized Types to layout
      }
  deriving stock (Eq, Ord, Show, Generic)

instance NFData CILayout

-- | The type of 'ClosureInfo'
data CIType
  = CIFun { citArity :: !Int         -- ^ function arity
          , citRegs  :: !Int         -- ^ number of registers for the args
          }
  | CIThunk                          -- ^ The closure is a THUNK
  | CICon { citConstructor :: !Int } -- ^ The closure is a Constructor
  | CIPap                            -- ^ The closure is a Partial Application
  | CIBlackhole                      -- ^ The closure is a black hole
  | CIStackFrame                     -- ^ The closure is a stack frame
  deriving stock (Eq, Ord, Show, Generic)

instance NFData CIType

-- | Static references that must be kept alive
newtype CIStatic = CIStaticRefs { staticRefs :: [FastString] }
  deriving stock   (Eq, Generic)
  deriving newtype (Semigroup, Monoid, Show)

-- | static refs: array = references, null = nothing to report
--   note: only works after all top-level objects have been created
instance ToJExpr CIStatic where
  toJExpr (CIStaticRefs [])  = null_ -- [je| null |]
  toJExpr (CIStaticRefs rs)  = toJExpr (map TxtI rs)

-- | Free variable types
data VarType
  = PtrV     -- ^ pointer = reference to heap object (closure object)
  | VoidV    -- ^ no fields
  | DoubleV  -- ^ A Double: one field
  | IntV     -- ^ An Int (32bit because JS): one field
  | LongV    -- ^ A Long: two fields one for the upper 32bits, one for the lower (NB: JS is little endian)
  | AddrV    -- ^ a pointer not to the heap: two fields, array + index
  | RtsObjV  -- ^ some RTS object from GHCJS (for example TVar#, MVar#, MutVar#, Weak#)
  | ObjV     -- ^ some JS object, user supplied, be careful around these, can be anything
  | ArrV     -- ^ boxed array
  deriving stock (Eq, Ord, Enum, Bounded, Show, Generic)

instance NFData VarType

instance ToJExpr VarType where
  toJExpr = toJExpr . fromEnum

-- | The type of identifiers. These determine the suffix of generated functions
-- in JS Land. For example, the entry function for the 'Just' constructor is a
-- 'IdConEntry' which compiles to:
-- @
-- function h$baseZCGHCziMaybeziJust_con_e() { return h$rs() };
-- @
-- which just returns whatever the stack point is pointing to. Whereas the entry
-- function to 'Just' is an 'IdEntry' and does the work. It compiles to:
-- @
-- function h$baseZCGHCziMaybeziJust_e() {
--    var h$$baseZCGHCziMaybezieta_8KXnScrCjF5 = h$r2;
--    h$r1 = h$c1(h$baseZCGHCziMaybeziJust_con_e, h$$baseZCGHCziMaybezieta_8KXnScrCjF5);
--    return h$rs();
--    };
-- @
-- Which loads some payload from register 2, and applies the Constructor Entry
-- function for the Just to the payload, returns the result in register 1 and
-- returns whatever is on top of the stack
data IdType
  = IdPlain     -- ^ A plain identifier for values, no suffix added
  | IdEntry     -- ^ An entry function, suffix = "_e" in 'GHC.StgToJS.Ids.makeIdentForId'
  | IdConEntry  -- ^ A Constructor entry function, suffix = "_con_e" in 'GHC.StgToJS.Ids.makeIdentForId'
  deriving (Enum, Eq, Ord)

-- | Keys to differentiate Ident's in the ID Cache
data IdKey
  = IdKey !Word64 !Int !IdType
  deriving (Eq, Ord)

-- | Some other symbol
data OtherSymb
  = OtherSymb !Module !FastString
  deriving Eq

instance Ord OtherSymb where
  compare (OtherSymb m1 t1) (OtherSymb m2 t2)
    = stableModuleCmp m1 m2 <> lexicalCompareFS t1 t2

-- | The identifier cache indexed on 'IdKey' local to a module
newtype IdCache = IdCache (M.Map IdKey Ident)

-- | The global Identifier Cache
newtype GlobalIdCache = GlobalIdCache (UniqFM Ident (IdKey, Id))

-- | A Stack Slot is either known or unknown. We avoid maybe here for more
-- strictness.
data StackSlot
  = SlotId !Id !Int
  | SlotUnknown
  deriving (Eq, Ord)

data StaticInfo = StaticInfo
  { siVar    :: !FastString    -- ^ global object
  , siVal    :: !StaticVal     -- ^ static initialization
  , siCC     :: !(Maybe Ident) -- ^ optional CCS name
  } deriving stock (Eq, Show, Typeable, Generic)

data StaticVal
  = StaticFun     !FastString [StaticArg]
    -- ^ heap object for function
  | StaticThunk   !(Maybe (FastString,[StaticArg]))
    -- ^ heap object for CAF (field is Nothing when thunk is initialized in an
    -- alternative way, like string thunks through h$str)
  | StaticUnboxed !StaticUnboxed
    -- ^ unboxed constructor (Bool, Int, Double etc)
  | StaticData    !FastString [StaticArg]
    -- ^ regular datacon app
  | StaticList    [StaticArg] (Maybe FastString)
    -- ^ list initializer (with optional tail)
  deriving stock (Eq, Show, Generic)

data StaticUnboxed
  = StaticUnboxedBool         !Bool
  | StaticUnboxedInt          !Integer
  | StaticUnboxedDouble       !SaneDouble
  | StaticUnboxedString       !BS.ByteString
  | StaticUnboxedStringOffset !BS.ByteString
  deriving stock (Eq, Ord, Show, Generic)

instance NFData StaticUnboxed

-- | Static Arguments. Static Arguments are things that are statically
-- allocated, i.e., they exist at program startup. These are static heap objects
-- or literals or things that have been floated to the top level binding by ghc.
data StaticArg
  = StaticObjArg !FastString             -- ^ reference to a heap object
  | StaticLitArg !StaticLit              -- ^ literal
  | StaticConArg !FastString [StaticArg] -- ^ unfloated constructor
  deriving stock (Eq, Show, Generic)

instance Outputable StaticArg where
  ppr x = text (show x)

-- | A Static literal value
data StaticLit
  = BoolLit   !Bool
  | IntLit    !Integer
  | NullLit
  | DoubleLit !SaneDouble -- should we actually use double here?
  | StringLit !FastString
  | BinLit    !BS.ByteString
  | LabelLit  !Bool !FastString -- ^ is function pointer, label (also used for string / binary init)
  deriving (Eq, Show, Generic)

instance Outputable StaticLit where
  ppr x = text (show x)


instance ToJExpr StaticLit where
  toJExpr (BoolLit b)           = toJExpr b
  toJExpr (IntLit i)            = toJExpr i
  toJExpr NullLit               = null_
  toJExpr (DoubleLit d)         = toJExpr (unSaneDouble d)
  toJExpr (StringLit t)         = app (mkFastString "h$str") [toJExpr t]
  toJExpr (BinLit b)            = app (mkFastString "h$rstr") [toJExpr (map toInteger (BS.unpack b))]
  toJExpr (LabelLit _isFun lbl) = var lbl

-- | A foreign reference to some JS code
data ForeignJSRef = ForeignJSRef
  { foreignRefSrcSpan  :: !FastString
  , foreignRefPattern  :: !FastString
  , foreignRefSafety   :: !Safety
  , foreignRefCConv    :: !CCallConv
  , foreignRefArgs     :: ![FastString]
  , foreignRefResult   :: !FastString
  } deriving stock (Generic)

-- | data used to generate one ObjUnit in our object file
data LinkableUnit = LinkableUnit
  { luObjUnit      :: ObjUnit       -- ^ serializable unit info
  , luIdExports    :: [Id]          -- ^ exported names from haskell identifiers
  , luOtherExports :: [FastString]  -- ^ other exports
  , luIdDeps       :: [Id]          -- ^ identifiers this unit depends on
  , luPseudoIdDeps :: [Unique]      -- ^ pseudo-id identifiers this unit depends on (fixme)
  , luOtherDeps    :: [OtherSymb]   -- ^ symbols not from a haskell id that this unit depends on
  , luRequired     :: Bool          -- ^ always link this unit
  , luForeignRefs  :: [ForeignJSRef]
  }

-- | one toplevel block in the object file
data ObjUnit = ObjUnit
  { oiSymbols  :: ![FastString]   -- ^ toplevel symbols (stored in index)
  , oiClInfo   :: ![ClosureInfo]  -- ^ closure information of all closures in block
  , oiStatic   :: ![StaticInfo]   -- ^ static closure data
  , oiStat     :: JStat           -- ^ the code
  , oiRaw      :: !BS.ByteString  -- ^ raw JS code
  , oiFExports :: ![ExpFun]
  , oiFImports :: ![ForeignJSRef]
  }

data ExpFun = ExpFun
  { isIO   :: !Bool
  , args   :: [JSFFIType]
  , result :: !JSFFIType
  } deriving (Eq, Ord, Show)

-- | Types of FFI values
data JSFFIType
  = Int8Type
  | Int16Type
  | Int32Type
  | Int64Type
  | Word8Type
  | Word16Type
  | Word32Type
  | Word64Type
  | DoubleType
  | ByteArrayType
  | PtrType
  | RefType
  deriving (Show, Ord, Eq, Enum)


-- | Typed expression
data TypedExpr = TypedExpr
  { typex_typ  :: !PrimRep
  , typex_expr :: [JExpr]
  }

instance Outputable TypedExpr where
  ppr x = text "TypedExpr: " <+> ppr (typex_expr x)
          $$  text "PrimReps: " <+> ppr (typex_typ x)

-- | A Primop result is either an inlining of some JS payload, or a primitive
-- call to a JS function defined in Shim files in base.
data PrimRes
  = PrimInline JStat  -- ^ primop is inline, result is assigned directly
  | PRPrimCall JStat  -- ^ primop is async call, primop returns the next
                      --     function to run. result returned to stack top in registers

data ExprResult
  = ExprCont
  | ExprInline (Maybe [JExpr])
  deriving (Eq)

newtype ExprValData = ExprValData [JExpr]
  deriving newtype (Eq)

-- | A Closure is one of six types
data ClosureType
  = Thunk       -- ^ The closure is a THUNK
  | Fun         -- ^ The closure is a Function
  | Pap         -- ^ The closure is a Partial Application
  | Con         -- ^ The closure is a Constructor
  | Blackhole   -- ^ The closure is a Blackhole
  | StackFrame  -- ^ The closure is a stack frame
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Convert 'ClosureType' to an Int
ctNum :: ClosureType -> Int
ctNum Fun        = 1
ctNum Con        = 2
ctNum Thunk      = 0
ctNum Pap        = 3
ctNum Blackhole  = 5
ctNum StackFrame = -1

-- | Convert 'ClosureType' to a String
ctJsName :: ClosureType -> String
ctJsName = \case
  Thunk      -> "CLOSURE_TYPE_THUNK"
  Fun        -> "CLOSURE_TYPE_FUN"
  Pap        -> "CLOSURE_TYPE_PAP"
  Con        -> "CLOSURE_TYPE_CON"
  Blackhole  -> "CLOSURE_TYPE_BLACKHOLE"
  StackFrame -> "CLOSURE_TYPE_STACKFRAME"

instance ToJExpr ClosureType where
  toJExpr e = toJExpr (ctNum e)


-- | A thread is in one of 4 states
data ThreadStatus
  = Running   -- ^ The thread is running
  | Blocked   -- ^ The thread is blocked
  | Finished  -- ^ The thread is done
  | Died      -- ^ The thread has died
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Convert the status of a thread in JS land to an Int
threadStatusNum :: ThreadStatus -> Int
threadStatusNum = \case
  Running  -> 0
  Blocked  -> 1
  Finished -> 16
  Died     -> 17

-- | convert the status of a thread in JS land to a string
threadStatusJsName :: ThreadStatus -> String
threadStatusJsName = \case
  Running  -> "THREAD_RUNNING"
  Blocked  -> "THREAD_BLOCKED"
  Finished -> "THREAD_FINISHED"
  Died     -> "THREAD_DIED"
