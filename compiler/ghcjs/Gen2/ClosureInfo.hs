{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Gen2.ClosureInfo where

import           Control.DeepSeq
import           GHC.Generics

import           Data.Aeson (ToJSON(..), object, (.=))
import           Data.Array
import qualified Data.Bits as Bits
import qualified Data.Binary     as DB
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
-- import           Data.Default
import qualified Data.Map as M
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable (Typeable)

import           Compiler.JMacro hiding ((.=))
import           Compiler.JMacro.Combinators
import           Compiler.JMacro.Symbols
import           Compiler.JMacro.Lens(Default(..))

import           Gen2.StgAst ()
import           Gen2.Utils

import           ForeignCall (CCallConv(..), Safety(..))
import           DynFlags
import           StgSyn
import           DataCon
import           TyCon
import           RepType
import           Type
import           Id
import           Util
import           Outputable hiding ((<>))
import           TyCoRep
import Prelude

-- closure types
data CType = Thunk | Fun | Pap | Con | Blackhole | StackFrame
  deriving (Show, Eq, Ord, Enum, Bounded)

--
ctNum :: CType -> Int
ctNum Fun        = 1
ctNum Con        = 2
ctNum Thunk      = 0 -- 4
ctNum Pap        = 3 -- 8
-- ctNum Ind        = 4 -- 16
ctNum Blackhole  = 5 -- 32
ctNum StackFrame = -1

instance ToJExpr CType where
  toJExpr e = toJExpr (ctNum e)

-- function argument and free variable types
data VarType = PtrV     -- pointer = reference to heap object (closure object)
             | VoidV    -- no fields
--             | FloatV   -- one field -- no single precision supported
             | DoubleV  -- one field
             | IntV     -- one field
             | LongV    -- two fields
             | AddrV    -- a pointer not to the heap: two fields, array + index
             | RtsObjV  -- some RTS object from GHCJS (for example TVar#, MVar#, MutVar#, Weak#)
             | ObjV     -- some JS object, user supplied, be careful around these, can be anything
             | ArrV     -- boxed array
                deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance NFData VarType

data ForeignRef = ForeignRef { foreignRefSrcSpan  :: Text
                             , foreignRefPattern  :: Text
                             , foreignRefSafety   :: Safety
                             , foreignRefCConv    :: CCallConv
                             , foreignRefArgs     :: [Text]
                             , foreignRefResult   :: Text
                             } deriving (Eq, Ord, Show, Generic)

instance NFData ForeignRef
instance DB.Binary ForeignRef
instance ToJSON ForeignRef where
  toJSON (ForeignRef srcSpan pat sfty cc args res) = object
    [ "span"      .= srcSpan
    , "pattern"   .= pat
    , "safety"    .= safetyText sfty
    , "cconv"     .= cconvText cc
    , "arguments" .= args
    , "result"    .= res
    ]

-- instance ToJSON

safetyText :: Safety -> Text
safetyText PlaySafe          = "safe"
safetyText PlayInterruptible = "interruptible"
safetyText PlayRisky         = "unsafe"

cconvText :: CCallConv -> Text
cconvText StdCallConv        = "stdcall"
cconvText CCallConv          = "ccall"
cconvText CApiConv           = "capi"
cconvText PrimCallConv       = "prim"
cconvText JavaScriptCallConv = "javascript"

-- fixme, remove these orphans
deriving instance Generic Safety
deriving instance Enum Safety
deriving instance Ord Safety
instance DB.Binary Safety
instance NFData Safety

deriving instance Generic CCallConv
deriving instance Enum CCallConv
deriving instance Ord CCallConv
instance DB.Binary CCallConv
instance NFData CCallConv

-- can we unbox C x to x, only if x is represented as a Number
isUnboxableCon :: DataCon -> Bool
isUnboxableCon dc
  | [t] <- dataConRepArgTys dc, [t1] <- typeVt t =
       isUnboxable t1 &&
       dataConTag dc == 1 &&
       length (tyConDataCons $ dataConTyCon dc) == 1
  | otherwise = False

-- one-constructor types with one primitive field represented as a JS Number
-- can be unboxed
isUnboxable :: VarType -> Bool
isUnboxable DoubleV = True
isUnboxable IntV    = True -- includes Char#
isUnboxable _       = False

varSize :: VarType -> Int
varSize VoidV = 0
varSize LongV = 2 -- hi, low
varSize AddrV = 2 -- obj/array, offset
varSize _     = 1

typeSize :: Type -> Int
typeSize t = sum . map varSize . typeVt $ t

isVoid :: VarType -> Bool
isVoid VoidV = True
isVoid _     = False

isPtr :: VarType -> Bool
isPtr PtrV = True
isPtr _    = False

isSingleVar :: VarType -> Bool
isSingleVar v = varSize v == 1

isMultiVar :: VarType -> Bool
isMultiVar v = varSize v > 1

-- can we pattern match on these values in a case?
isMatchable :: [VarType] -> Bool
isMatchable [DoubleV] = True
isMatchable [IntV]    = True
isMatchable _         = False

tyConVt :: HasDebugCallStack => TyCon -> [VarType]
tyConVt = typeVt . mkTyConTy

idVt :: HasDebugCallStack => Id -> [VarType]
idVt = typeVt . idType

typeVt :: HasDebugCallStack => Type -> [VarType]
typeVt t | isRuntimeRepKindedTy t {- || isRuntimeRepTy t -} = []
typeVt t = map primRepVt (typePrimRep t)-- map uTypeVt (repTypeArgs t)

uTypeVt :: HasDebugCallStack => UnaryType -> VarType
uTypeVt ut = trace' ("uTypeVt: " ++ show ut ++ "\n" ++ (show $ isRuntimeRepKindedTy ut)
                                            ++ "\n" ++ (show $ isRuntimeRepTy ut)
                                            ++ "\n" ++ (show $ showDbgTy ut))
                               (uTypeVt0 ut)
  where
                               showDbgTy ty = "isPrimitiveType: " ++
                                 case splitTyConApp_maybe ty of
                                   Just (tc, ty_args) -> show (tc, tyConArity tc, ty_args)
                                   Nothing            -> "<not tyconapp>"

  {-
case repType t of
             MultiRep uts   -> concatMap typeVt (dropRuntimeRepArgs uts)
             UnaryRep ut       -> [uTypeVt ut] -}

-- only use if you know it's not an unboxed tuple
uTypeVt0 :: HasDebugCallStack => UnaryType -> VarType
uTypeVt0 ut
  | isRuntimeRepKindedTy ut = VoidV
--  | isRuntimeRepTy ut = VoidV
  -- GHC panics on this otherwise
  | Just (tc, ty_args) <- splitTyConApp_maybe ut
  , length ty_args /= tyConArity tc = PtrV
  | isPrimitiveType ut = (primTypeVt ut)
  | otherwise          =
    case typePrimRep' ut of
      []   -> VoidV
      [pt] -> primRepVt pt
      _    -> panic ("uTypeVt: not unary" ++ show ut)
    -- primRepVt . typePrimRep' $ ut
--   where

primRepVt :: HasDebugCallStack => PrimRep -> VarType
primRepVt VoidRep     = VoidV
primRepVt LiftedRep   = PtrV -- fixme does ByteArray# ever map to this?
primRepVt UnliftedRep = RtsObjV
primRepVt IntRep      = IntV
primRepVt Int8Rep     = IntV
primRepVt Int16Rep    = IntV
primRepVt Int32Rep    = IntV
primRepVt WordRep     = IntV
primRepVt Word8Rep    = IntV
primRepVt Word16Rep   = IntV
primRepVt Word32Rep   = IntV
primRepVt Int64Rep    = LongV
primRepVt Word64Rep   = LongV
primRepVt AddrRep     = AddrV
primRepVt FloatRep    = DoubleV
primRepVt DoubleRep   = DoubleV
primRepVt (VecRep{})  = error "uTypeVt: vector types are unsupported"

typePrimRep' :: HasDebugCallStack => UnaryType -> [PrimRep]
typePrimRep' ty = kindPrimRep' empty (typeKind ty)

-- | Find the primitive representation of a 'TyCon'. Defined here to
-- avoid module loops. Call this only on unlifted tycons.
tyConPrimRep' :: HasDebugCallStack => TyCon -> [PrimRep]
tyConPrimRep' tc = kindPrimRep' empty res_kind
  where
    res_kind = tyConResKind tc

-- | Take a kind (of shape @TYPE rr@) and produce the 'PrimRep' of values
-- of types of this kind.
-- | Take a kind (of shape @TYPE rr@) and produce the 'PrimRep's
-- of values of types of this kind.
kindPrimRep' :: HasDebugCallStack => SDoc -> Kind -> [PrimRep]
kindPrimRep' doc ki
  | Just ki' <- coreView ki
  = kindPrimRep' doc ki'
kindPrimRep' doc (TyConApp _typ [runtime_rep])
  = -- ASSERT( typ `hasKey` tYPETyConKey )
    runtimeRepPrimRep doc runtime_rep
kindPrimRep' doc ki
  = pprPanic "kindPrimRep'" (ppr ki $$ doc)

primTypeVt :: HasDebugCallStack => Type -> VarType
primTypeVt t = case tyConAppTyCon_maybe (unwrapType t) of
                                   Nothing -> error "primTypeVt: not a TyCon"
                                   Just tc -> go (show tc)
  where
   pr xs = "ghc-prim:GHC.Prim." ++ xs
   go st
    | st == pr "Addr#"               = AddrV
    | st == pr "Int#"                = IntV
    | st == pr "Int64#"              = LongV
    | st == pr "Char#"               = IntV
    | st == pr "Word#"               = IntV
    | st == pr "Word8#"              = IntV
    | st == pr "Word16#"             = IntV
    | st == pr "Int8#"               = IntV
    | st == pr "Int16#"              = IntV
    | st == pr "Word64#"             = LongV
    | st == pr "Double#"             = DoubleV
    | st == pr "Float#"              = DoubleV
    | st == pr "Array#"              = ArrV
    | st == pr "MutableArray#"       = ArrV
    | st == pr "ByteArray#"          = ObjV -- can contain any JS reference, used for JSVal
    | st == pr "MutableByteArray#"   = ObjV -- can contain any JS reference, used for JSVal
    | st == pr "ArrayArray#"         = ArrV
    | st == pr "MutableArrayArray#"  = ArrV
    | st == pr "MutVar#"             = RtsObjV
    | st == pr "TVar#"               = RtsObjV
    | st == pr "MVar#"               = RtsObjV
    | st == pr "State#"              = VoidV
    | st == pr "RealWorld"           = VoidV
    | st == pr "ThreadId#"           = RtsObjV
    | st == pr "Weak#"               = RtsObjV
    | st == pr "StablePtr#"          = AddrV
    | st == pr "StableName#"         = RtsObjV
    | st == pr "Void#"               = VoidV
    | st == pr "Proxy#"              = VoidV
    | st == pr "MutVar#"             = RtsObjV
    | st == pr "BCO#"                = RtsObjV -- fixme what do we need here?
    | st == pr "~#"                  = VoidV -- coercion token?
    | st == pr "~R#"                 = VoidV -- role
    | st == pr "Any"                 = PtrV
    | st == pr "SmallMutableArray#"  = ArrV
    | st == pr "SmallArray#"         = ArrV
    | st == pr "Compact#"            = ObjV -- unsupported?
    | st == pr "TYPE"                = PtrV -- ?
    | st == "Data.Dynamic.Obj"       = PtrV -- ?
    | otherwise = error ("primTypeVt: unrecognized primitive type: " ++ st)

argVt :: HasDebugCallStack => StgArg -> VarType
argVt a = trace' ("argVt: " ++ show a)
                            (uTypeVt . stgArgType $ a)

instance ToJExpr VarType where
  toJExpr = toJExpr . fromEnum

data ClosureInfo = ClosureInfo
     { ciVar     :: Text      -- ^ object being infod
     , ciRegs    :: CIRegs    -- ^ things in registers when this is the next closure to enter
     , ciName    :: Text      -- ^ friendly name for printing
     , ciLayout  :: CILayout  -- ^ heap/stack layout of the object
     , ciType    :: CIType    -- ^ type of the object, with extra info where required
     , ciStatic  :: CIStatic  -- ^ static references of this object
     }
  deriving (Eq, Ord, Show, Generic)

instance NFData ClosureInfo

-- fixme where to move this?
closure :: ClosureInfo -> JStat -> JStat
closure ci body = TxtI (ciVar ci) ||= jLam body # toStat ci

conClosure :: Text -> Text -> CILayout -> Int -> JStat
conClosure symbol name layout constr =
  closure (ClosureInfo symbol (CIRegs 0 [PtrV]) name layout (CICon constr) noStatic)
          (returnS (stack .! sp))

data CIType = CIFun { citArity :: Int  -- ^ function arity
                    , citRegs  :: Int  -- ^ number of registers for the args
                    }
            | CIThunk
            | CICon { citConstructor :: Int }
            | CIPap
            | CIBlackhole
            | CIStackFrame
  deriving (Eq, Ord, Show, Generic)

instance NFData CIType

data CIRegs = CIRegsUnknown
            | CIRegs { ciRegsSkip  :: Int       -- ^ unused registers before actual args start
                     , ciRegsTypes :: [VarType] -- ^ args
                     }
  deriving (Eq, Ord, Show, Generic)

instance NFData CIRegs

data CIStatic = -- CIStaticParent { staticParent :: Ident } -- ^ static refs are stored in parent in fungroup
                CIStaticRefs   { staticRefs :: [Text] } -- ^ list of refs that need to be kept alive
  deriving (Eq, Ord, Show, Generic)

instance NFData CIStatic

noStatic :: CIStatic
noStatic = CIStaticRefs []

-- | static refs: array = references, null = nothing to report
--   note: only works after all top-level objects have been created
instance ToJExpr CIStatic where
  toJExpr (CIStaticRefs [])  = null_ -- [je| null |]
  toJExpr (CIStaticRefs rs)  = toJExpr (map TxtI rs)

data CILayout = CILayoutVariable            -- layout stored in object itself, first position from the start
              | CILayoutUnknown             -- fixed size, but content unknown (for example stack apply frame)
                  { layoutSize :: !Int
                  }
              | CILayoutFixed               -- whole layout known
                  { layoutSize :: !Int      -- closure size in array positions, including entry
                  , layout     :: [VarType]
                  }
  deriving (Eq, Ord, Show, Generic)

instance NFData CILayout

-- standard fixed layout: payload types
-- payload starts at .d1 for heap objects, entry closest to Sp for stack frames
fixedLayout :: [VarType] -> CILayout
fixedLayout vts = CILayoutFixed (sum (map varSize vts)) vts

layoutSizeMaybe :: CILayout -> Maybe Int
layoutSizeMaybe (CILayoutUnknown n) = Just n
layoutSizeMaybe (CILayoutFixed n _) = Just n
layoutSizeMaybe _                   = Nothing

{-
  Some stack frames don't need explicit information, since the
  frame size can be determined from inspecting the types on the stack

  requirements:
    - stack frame
    - fixed size, known layout
    - one register value
    - no ObjV (the next function on the stack should be the start of the next frame, not something in this frame)
    - no static references
 -}
implicitLayout :: ClosureInfo -> Bool
implicitLayout ci
  | CILayoutFixed _ layout <- ciLayout ci
  , CIStaticRefs []        <- ciStatic ci
  , CIStackFrame           <- ciType ci
  , CIRegs 0 rs            <- ciRegs ci =
      sum (map varSize rs) == 1 &&
      null (filter (==ObjV) layout)
  | otherwise = False

-- | note: the statements only work after all top-level objects have been created
instance ToStat ClosureInfo where
  toStat = closureInfoStat False

closureInfoStat :: Bool -> ClosureInfo -> JStat
closureInfoStat debug (ClosureInfo obj rs name layout CIThunk srefs) =
    setObjInfoL debug obj rs layout Thunk name 0 srefs
closureInfoStat debug (ClosureInfo obj rs name layout (CIFun arity nregs) srefs) =
    setObjInfoL debug obj rs layout Fun name (mkArityTag arity nregs) srefs
closureInfoStat debug (ClosureInfo obj rs name layout (CICon con) srefs) =
    setObjInfoL debug obj rs layout Con name con srefs
closureInfoStat debug (ClosureInfo obj rs name layout CIBlackhole srefs)   =
    setObjInfoL debug obj rs layout Blackhole name 0 srefs
closureInfoStat debug (ClosureInfo obj rs name layout CIPap srefs)  =
    setObjInfoL debug obj rs layout Pap name 0 srefs
closureInfoStat debug (ClosureInfo obj rs name layout CIStackFrame srefs) =
    setObjInfoL debug obj rs layout StackFrame name 0 srefs

mkArityTag :: Int -> Int -> Int
mkArityTag arity registers = arity Bits..|. (registers `Bits.shiftL` 8)

setObjInfoL :: Bool      -- ^ debug: output symbol names
            -> Text      -- ^ the object name
            -> CIRegs    -- ^ things in registers
            -> CILayout  -- ^ layout of the object
            -> CType     -- ^ closure type
            -> Text      -- ^ object name, for printing
            -> Int       -- ^ `a' argument, depends on type (arity, conid)
            -> CIStatic  -- ^ static refs
            -> JStat
setObjInfoL debug obj rs CILayoutVariable t n a =
  setObjInfo debug obj t n [] a (-1) rs
setObjInfoL debug obj rs (CILayoutUnknown size) t n a =
  setObjInfo debug obj t n xs a size rs
    where
      xs  = toTypeList (replicate size ObjV)
setObjInfoL debug obj rs (CILayoutFixed size layout) t n a =
  setObjInfo debug obj t n xs a size rs
    where
      xs   = toTypeList layout

toTypeList :: [VarType] -> [Int]
toTypeList = concatMap (\x -> replicate (varSize x) (fromEnum x))

setObjInfo :: Bool       -- ^ debug: output all symbol names
           -> Text       -- ^ the thing to modify
           -> CType      -- ^ closure type
           -> Text       -- ^ object name, for printing
           -> [Int]      -- ^ list of item types in the object, if known (free variables, datacon fields)
           -> Int        -- ^ extra 'a' parameter, for constructor tag or arity
           -> Int        -- ^ object size, -1 (number of vars) for unknown
           -> CIRegs     -- ^ things in registers [VarType]  -- ^ things in registers
           -> CIStatic   -- ^ static refs
           -> JStat
setObjInfo debug obj t name fields a size regs static
   | debug     = appS "h$setObjInfo" [var obj, e t, e name, e fields, e a, e size, e (regTag regs), e static] -- error "setObjInfo1" -- [j| h$setObjInfo(`TxtI obj`, `t`, `name`, `fields`, `a`, `size`, `regTag regs`, `static`); |]
   | otherwise = appS "h$o" [var obj, e t, e a, e size, e (regTag regs), e static] -- error "setObjInfo2" -- [j| h$o(`TxtI obj`,`t`,`a`,`size`,`regTag regs`,`static`); |]
  where
    regTag CIRegsUnknown       = -1
    regTag (CIRegs skip types) =
      let nregs = sum $ map varSize types
      in  skip + (nregs `Bits.shiftL` 8)


data StaticInfo = StaticInfo { siVar    :: !Text          -- ^ global object
                             , siVal    :: !StaticVal     -- ^ static initialization
                             , siCC     :: !(Maybe Ident) -- ^ optional CCS name
                             }
  deriving (Eq, Ord, Show, Typeable, Generic)

instance NFData StaticInfo

data StaticVal = StaticFun     !Text   [StaticArg]       -- ^ heap object for function
               | StaticThunk   !(Maybe (Text,[StaticArg]))-- ^ heap object for CAF (field is Nothing when thunk is initialized in an alternative way, like string thunks through h$str)
               | StaticUnboxed !StaticUnboxed            -- ^ unboxed constructor (Bool, Int, Double etc)
               | StaticData    !Text [StaticArg]         -- ^ regular datacon app
               | StaticList    [StaticArg] (Maybe Text)  -- ^ list initializer (with optional tail)
  deriving (Eq, Ord, Show, Typeable, Generic)

instance NFData StaticVal

data StaticUnboxed = StaticUnboxedBool         !Bool
                   | StaticUnboxedInt          !Integer
                   | StaticUnboxedDouble       !SaneDouble
                   | StaticUnboxedString       !ByteString
                   | StaticUnboxedStringOffset !ByteString
  deriving (Eq, Ord, Show, Typeable, Generic)

instance NFData StaticUnboxed

data StaticArg = StaticObjArg !Text             -- ^ reference to a heap object
               | StaticLitArg !StaticLit        -- ^ literal
               | StaticConArg !Text [StaticArg] -- ^ unfloated constructor
  deriving (Eq, Ord, Show, Typeable, Generic)

instance NFData StaticArg

data StaticLit = BoolLit   !Bool
               | IntLit    !Integer
               | NullLit
               | DoubleLit !SaneDouble -- should we actually use double here?
               | StringLit !Text
               | BinLit    !ByteString
               | LabelLit  !Bool !Text -- ^ is function pointer, label (also used for string / binary init)
  deriving (Eq, Ord, Show, Typeable, Generic)

instance NFData StaticLit

instance ToJExpr StaticArg where
  toJExpr (StaticLitArg l) = toJExpr l
  toJExpr (StaticObjArg t) = ValExpr (JVar (TxtI t))
  toJExpr (StaticConArg c args) =
    -- FIXME: cost-centre stack
    allocDynamicE def (ValExpr . JVar . TxtI $ c) (map toJExpr args) Nothing

instance ToJExpr StaticLit where
  toJExpr (BoolLit b)           = toJExpr b
  toJExpr (IntLit i)            = toJExpr i
  toJExpr NullLit               = null_
  toJExpr (DoubleLit d)         = toJExpr (unSaneDouble d)
  toJExpr (StringLit t)         = app "h$str" [e t] -- error "StringLit" -- [je| h$str(`t`)() |]                           -- fixme this duplicates the string!
  toJExpr (BinLit b)            = app "h$rstr" [e (map toInteger (B.unpack b))] -- error "BinLit" -- [je| h$rstr(`map toInteger (B.unpack b)`)() |] -- fixme this duplicates the string
  toJExpr (LabelLit _isFun lbl) = var lbl -- error "LabelLit" -- [je| `JVar (TxtI lbl)` |]

-- | declare and do first-pass init of a global object (create JS object for heap objects)
staticDeclStat :: StaticInfo
               -> JStat
staticDeclStat (StaticInfo si sv _) =
  let si' = TxtI si
      ssv (StaticUnboxed u)       = Just (ssu u)
      ssv (StaticThunk Nothing)   = Nothing
      ssv _                       = Just (app "h$d" []) -- error "StaticUnboxed" -- Just [je| h$d() |]
      ssu (StaticUnboxedBool b)   = app "h$p" [e b] -- error "StaticUnboxedBool" -- [je| h$p(`b`) |]
      ssu (StaticUnboxedInt i)    = app "h$p" [e i] -- error "StaticUnboxedInt" -- [je| h$p(`i`) |]
      ssu (StaticUnboxedDouble d) = app "h$p" [e (unSaneDouble d)] -- error "StaticUnboxedDouble" -- [je| h$p(`unSaneDouble d`) |]
      ssu (StaticUnboxedString str) = ApplExpr (initStr str) []
      ssu (StaticUnboxedStringOffset {}) = 0

  -- fixme, we shouldn't do h$di, we need to record the statement to init the thunks
  in  maybe (appS "h$di" [e si']) (\v -> DeclStat si' # e si' |= v) (ssv sv)
      -- error "staticDeclStat" -- maybe [j| h$di(`si'`); |] (\v -> DeclStat si' <> error "staticDeclStat" {- [j| `si'` = `v`; |]-}) (ssv sv)

-- | initialize a global object. all global objects have to be declared (staticInfoDecl) first
--   (this is only used with -debug, normal init would go through the static data table)
staticInitStat :: Bool         -- ^ profiling enabled
               -> StaticInfo
               -> JStat
staticInitStat _prof (StaticInfo i sv cc) =
  case sv of
    StaticData con args  -> appS "h$sti" ([var i, var con, toJExpr args] ++ ccArg)
    StaticFun f args     -> appS "h$sti" ([var i, var f, toJExpr args] ++ ccArg)
    StaticList args mt   ->
      appS "h$stl" ([var i, toJExpr args, toJExpr $ maybe null_ (toJExpr . TxtI) mt] ++ ccArg)
    StaticThunk (Just (f,args)) -> 
      appS "h$stc" ([var i, var f, toJExpr args] ++ ccArg)
    _                    -> mempty
  where
    ccArg = maybeToList (fmap toJExpr cc)


initStr :: ByteString -> JExpr
initStr str =
  case decodeModifiedUTF8 str of
    Just t  -> app "h$str" [ValExpr (JStr t)]
    Nothing -> app "h$rstr" [e $ map toInteger (B.unpack str)]
      -- error "initStr"
      -- [je| h$rstr(`map toInteger (B.unpack str)`) |]

allocDynamicE :: CgSettings -> JExpr -> [JExpr] -> Maybe JExpr -> JExpr
allocDynamicE s entry free cc
  | csInlineAlloc s || length free > 24 =
      ValExpr . jhFromList $ [ ("f", entry)
                             , ("d1", fillObj1)
                             , ("d2", fillObj2)
                             , ("m", 0)
                             ] ++
                             maybe [] (\cid -> [("cc", cid)]) cc
  | otherwise = ApplExpr allocFun (toJExpr entry : free ++ maybeToList cc)
  where
    allocFun = allocClsA ! length free
    (fillObj1,fillObj2)
       = case free of
                []  -> (null_, null_)
                [x] -> (x,null_)
                [x,y] -> (x,y)
                (x:xs) -> (x,toJExpr (JHash $ M.fromList (zip dataFields xs)))
    dataFields = map (T.pack . ('d':) . show) [(1::Int)..]

allocClsA :: Array Int JExpr
allocClsA = listArray (0, 1024) (toJExpr (TxtI "h$c") : map f [(1::Int)..1024])
  where
    f n = toJExpr . TxtI . T.pack $ "h$c" ++ show n

allocData :: Array Int JExpr
allocData = listArray (1, 1024) (map f [(1::Int)..1024])
  where
    f n = toJExpr . TxtI . T.pack $ "h$d" ++ show n

data CgSettings = CgSettings
  { csInlinePush      :: Bool
  , csInlineBlackhole :: Bool
  , csInlineLoadRegs  :: Bool
  , csInlineEnter     :: Bool
  , csInlineAlloc     :: Bool
  , csTraceRts        :: Bool
  , csAssertRts       :: Bool
  , csDebugAlloc      :: Bool
  , csTraceForeign    :: Bool
  , csProf            :: Bool
  }

instance Default CgSettings where
  def = CgSettings False False False False False False False False False False

dfCgSettings :: DynFlags -> CgSettings
dfCgSettings df = def { csTraceRts  = "-DGHCJS_TRACE_RTS"  `elem` opt_P df
                      , csAssertRts = "-DGHCJS_ASSERT_RTS" `elem` opt_P df
                      , csDebugAlloc = "-DGHCJS_DEBUG_ALLOC" `elem` opt_P df
                      , csProf      = WayProf `elem` ways df
                                      -- FIXME: this part is inlined from Settings.hs to avoid circular imports
                      }


returnStack :: JStat
-- returnStack = [j| return `Stack`[`Sp`]; |]
returnStack = returnS $ app "h$rs" [] -- error "returnStack" -- [j| return h$rs(); |]
