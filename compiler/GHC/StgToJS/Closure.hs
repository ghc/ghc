{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module GHC.StgToJS.Closure
  ( closureInfoStat
  , closure
  , conClosure
  , Closure (..)
  , newClosure
  , assignClosure
  , CopyCC (..)
  , copyClosure
  , mkClosure
  -- $names
  , allocData
  , allocClsA
  , dataName
  , clsName
  , dataFieldName
  , varName
  , jsClosureCount
  )
where

import GHC.Prelude
import GHC.Data.FastString

import GHC.StgToJS.Heap
import GHC.StgToJS.Types
import GHC.StgToJS.Utils

import GHC.JS.Make
import GHC.JS.JStg.Syntax
import GHC.JS.JStg.Monad
import GHC.JS.Ident

import GHC.Types.Unique.Map

import Data.Array
import Data.Monoid
import qualified Data.Bits as Bits

-- | Generate statements to set infotable field values for the given ClosureInfo
--
-- Depending on debug flag, it generates h$setObjInfo(...) or h$o(...). The
-- latter form doesn't store the pretty-printed name in the closure to save
-- space.
closureInfoStat :: Bool -> ClosureInfo -> JStgStat
closureInfoStat debug ci
  = setObjInfoL debug (ciVar ci) (ciRegs ci) (ciLayout ci) ty (ciName ci) tag (ciStatic ci)
      where
        !ty = case ciType ci of
          CIThunk      -> Thunk
          CIFun {}     -> Fun
          CICon {}     -> Con
          CIBlackhole  -> Blackhole
          CIPap        -> Pap
          CIStackFrame -> StackFrame
        !tag = case ciType ci of
          CIThunk           -> 0
          CIFun arity nregs -> mkArityTag arity nregs
          CICon con         -> con
          CIBlackhole       -> 0
          CIPap             -> 0
          CIStackFrame      -> 0


setObjInfoL :: Bool        -- ^ debug: output symbol names
            -> Ident       -- ^ the object name
            -> CIRegs      -- ^ things in registers
            -> CILayout    -- ^ layout of the object
            -> ClosureType -- ^ closure type
            -> FastString  -- ^ object name, for printing
            -> Int         -- ^ `a' argument, depends on type (arity, conid)
            -> CIStatic    -- ^ static refs
            -> JStgStat
setObjInfoL debug obj rs layout t n a
  = setObjInfo debug obj t n field_types a size rs
      where
        size = case layout of
          CILayoutVariable   -> (-1)
          CILayoutUnknown sz -> sz
          CILayoutFixed sz _ -> sz
        field_types = case layout of
          CILayoutVariable     -> []
          CILayoutUnknown size -> to_type_list (replicate size ObjV)
          CILayoutFixed _ fs   -> to_type_list fs
        to_type_list = concatMap (\x -> replicate (varSize x) (fromEnum x))

setObjInfo :: Bool        -- ^ debug: output all symbol names
           -> Ident       -- ^ the thing to modify
           -> ClosureType -- ^ closure type
           -> FastString  -- ^ object name, for printing
           -> [Int]       -- ^ list of item types in the object, if known (free variables, datacon fields)
           -> Int         -- ^ extra 'a' parameter, for constructor tag or arity
           -> Int         -- ^ object size, -1 (number of vars) for unknown
           -> CIRegs      -- ^ things in registers
           -> CIStatic    -- ^ static refs
           -> JStgStat
setObjInfo debug obj t name fields a size regs static
   | debug     = appS "h$setObjInfo" [ toJExpr obj
                                     , toJExpr t
                                     , toJExpr name
                                     , toJExpr fields
                                     , toJExpr a
                                     , toJExpr size
                                     , toJExpr (regTag regs)
                                     , toJExpr static
                                     ]
   | otherwise = appS "h$o" [ toJExpr obj
                            , toJExpr t
                            , toJExpr a
                            , toJExpr size
                            , toJExpr (regTag regs)
                            , toJExpr static
                            ]
  where
    regTag CIRegsUnknown       = -1
    regTag (CIRegs skip types) =
      let nregs = sum $ fmap varSize types
      in  skip + (nregs `Bits.shiftL` 8)

-- | Special case of closures that do not need to generate any @fresh@ names
closure :: ClosureInfo    -- ^ object being info'd see @ciVar@
         -> JSM JStgStat  -- ^ rhs
         -> JSM JStgStat
closure ci body = do
  f <- jFunction' (ciVar ci) body
  return $ f `mappend` closureInfoStat False ci

conClosure :: Ident -> FastString -> CILayout -> Int -> JSM JStgStat
conClosure symbol name layout constr = closure ci body
  where
    ci = ClosureInfo
          { ciVar = symbol
          , ciRegs = CIRegs 0 [PtrV]
          , ciName = name
          , ciLayout = layout
          , ciType = CICon constr
          , ciStatic = mempty
          }
    body   = pure returnStack

-- | Used to pass arguments to newClosure with some safety
data Closure = Closure
  { clInfo   :: JStgExpr        -- ^ InfoTable object
  , clField1 :: JStgExpr        -- ^ Payload field 1
  , clField2 :: JStgExpr        -- ^ Payload field 2
  , clMeta   :: JStgExpr
  , clCC     :: Maybe JStgExpr
  }

newClosure :: Closure -> JStgExpr
newClosure Closure{..} =
  let xs = [ (closureInfo_  , clInfo)
           , (closureField1_, clField1)
           , (closureField2_, clField2)
           , (closureMeta_  , clMeta)
           ]
  in case clCC of
    -- CC field is optional (probably to minimize code size as we could assign
    -- null_, but we get the same effect implicitly)
    Nothing -> ValExpr (jhFromList xs)
    Just cc -> ValExpr (jhFromList $ (closureCC_,cc) : xs)

assignClosure :: JStgExpr -> Closure -> JStgStat
assignClosure t Closure{..} = BlockStat
  [ closureInfo   t |= clInfo
  , closureField1 t |= clField1
  , closureField2 t |= clField2
  , closureMeta   t |= clMeta
  ] <> case clCC of
      Nothing -> mempty
      Just cc -> closureCC t |= cc

data CopyCC = CopyCC | DontCopyCC

copyClosure :: CopyCC -> JStgExpr -> JStgExpr -> JStgStat
copyClosure copy_cc t s = BlockStat
  [ closureInfo   t |= closureInfo   s
  , closureField1 t |= closureField1 s
  , closureField2 t |= closureField2 s
  , closureMeta   t |= closureMeta   s
  ] <> case copy_cc of
      DontCopyCC -> mempty
      CopyCC     -> closureCC t |= closureCC s

mkClosure :: JStgExpr -> [JStgExpr] -> JStgExpr -> Maybe JStgExpr -> Closure
mkClosure info fields meta cc = Closure
  { clInfo   = info
  , clField1 = x1
  , clField2 = x2
  , clMeta   = meta
  , clCC     = cc
  }
  where
    x1 = case fields of
           []  -> null_
           x:_ -> x
    x2 = case fields of
           []     -> null_
           [_]    -> null_
           [_,x]  -> x
           _:x:xs -> ValExpr . JHash . listToUniqMap $ zip (fmap dataFieldName [1..]) (x:xs)


-------------------------------------------------------------------------------
--                             Name Caches
-------------------------------------------------------------------------------
-- $names

-- | Cache "dXXX" field names
dataFieldCache :: Array Int FastString
dataFieldCache = listArray (0,nFieldCache) (fmap (mkFastString . ('d':) . show) [(0::Int)..nFieldCache])

-- | Data names are used in the AST, and logging has determined that 255 is the maximum number we see.
nFieldCache :: Int
nFieldCache  = 255

-- | We use this in the RTS to determine the number of generated closures. These closures use the names
-- cached here, so we bind them to the same number.
jsClosureCount :: Int
jsClosureCount  = 24

dataFieldName :: Int -> FastString
dataFieldName i
  | i < 0 || i > nFieldCache = mkFastString ('d' : show i)
  | otherwise                = dataFieldCache ! i

-- | Cache "h$dXXX" names
dataCache :: Array Int FastString
dataCache = listArray (0,jsClosureCount) (fmap (mkFastString . ("h$d"++) . show) [(0::Int)..jsClosureCount])

dataName :: Int -> FastString
dataName i
  | i < 0 || i > nFieldCache = mkFastString ("h$d" ++ show i)
  | otherwise                = dataCache ! i

allocData :: Int -> JStgExpr
allocData i = toJExpr (global (dataName i))

-- | Cache "h$cXXX" names
clsCache :: Array Int FastString
clsCache = listArray (0,jsClosureCount) (fmap (mkFastString . ("h$c"++) . show) [(0::Int)..jsClosureCount])

clsName :: Int -> FastString
clsName i
  | i < 0 || i > jsClosureCount = mkFastString ("h$c" ++ show i)
  | otherwise                   = clsCache ! i

allocClsA :: Int -> JStgExpr
allocClsA i = toJExpr (global (clsName i))

-- | Cache "xXXX" names
varCache :: Array Int Ident
varCache = listArray (0,jsClosureCount) (fmap (global . mkFastString . ('x':) . show) [(0::Int)..jsClosureCount])

varName :: Int -> Ident
varName i
  | i < 0 || i > jsClosureCount = global $ mkFastString ('x' : show i)
  | otherwise                   = varCache ! i

