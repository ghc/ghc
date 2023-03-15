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
import GHC.StgToJS.CoreUtils
import GHC.StgToJS.Regs (stack,sp)

import GHC.JS.Make
import GHC.JS.Unsat.Syntax

import GHC.Types.Unique.Map

import Data.Array
import Data.Monoid
import qualified Data.Bits as Bits

closureInfoStat :: Bool -> ClosureInfo -> JStat
closureInfoStat debug (ClosureInfo obj rs name layout ctype srefs)
  = setObjInfoL debug obj rs layout ty name tag srefs
      where
        !ty = case ctype of
          CIThunk      -> Thunk
          CIFun {}     -> Fun
          CICon {}     -> Con
          CIBlackhole  -> Blackhole
          CIPap        -> Pap
          CIStackFrame -> StackFrame
        !tag = case ctype of
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
            -> JStat
setObjInfoL debug obj rs layout t n a
  = setObjInfo debug obj t n field_types a size rs
      where
        size = case layout of
          CILayoutVariable   -> (-1)
          CILayoutUnknown sz -> sz
          CILayoutFixed sz _ -> sz
        field_types = case layout of
          CILayoutVariable     -> []
          CILayoutUnknown size -> toTypeList (replicate size ObjV)
          CILayoutFixed _ fs   -> toTypeList fs

setObjInfo :: Bool        -- ^ debug: output all symbol names
           -> Ident       -- ^ the thing to modify
           -> ClosureType -- ^ closure type
           -> FastString  -- ^ object name, for printing
           -> [Int]       -- ^ list of item types in the object, if known (free variables, datacon fields)
           -> Int         -- ^ extra 'a' parameter, for constructor tag or arity
           -> Int         -- ^ object size, -1 (number of vars) for unknown
           -> CIRegs      -- ^ things in registers
           -> CIStatic    -- ^ static refs
           -> JStat
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
      let nregs = sum $ map varSize types
      in  skip + (nregs `Bits.shiftL` 8)

closure :: ClosureInfo -- ^ object being info'd see @ciVar@ in @ClosureInfo@
        -> JStat       -- ^ rhs
        -> JStat
closure ci body = (ciVar ci ||= jLam body) `mappend` closureInfoStat False ci

conClosure :: Ident -> FastString -> CILayout -> Int -> JStat
conClosure symbol name layout constr =
  closure (ClosureInfo symbol (CIRegs 0 [PtrV]) name layout (CICon constr) mempty)
          (returnS (stack .! sp))

-- | Used to pass arguments to newClosure with some safety
data Closure = Closure
  { clEntry  :: JExpr
  , clField1 :: JExpr
  , clField2 :: JExpr
  , clMeta   :: JExpr
  , clCC     :: Maybe JExpr
  }

newClosure :: Closure -> JExpr
newClosure Closure{..} =
  let xs = [ (closureEntry_ , clEntry)
           , (closureField1_, clField1)
           , (closureField2_, clField2)
           , (closureMeta_  , clMeta)
           ]
  in case clCC of
    -- CC field is optional (probably to minimize code size as we could assign
    -- null_, but we get the same effect implicitly)
    Nothing -> ValExpr (jhFromList xs)
    Just cc -> ValExpr (jhFromList $ (closureCC_,cc) : xs)

assignClosure :: JExpr -> Closure -> JStat
assignClosure t Closure{..} = BlockStat
  [ closureEntry  t |= clEntry
  , closureField1 t |= clField1
  , closureField2 t |= clField2
  , closureMeta   t |= clMeta
  ] <> case clCC of
      Nothing -> mempty
      Just cc -> closureCC t |= cc

data CopyCC = CopyCC | DontCopyCC

copyClosure :: CopyCC -> JExpr -> JExpr -> JStat
copyClosure copy_cc t s = BlockStat
  [ closureEntry  t |= closureEntry  s
  , closureField1 t |= closureField1 s
  , closureField2 t |= closureField2 s
  , closureMeta   t |= closureMeta   s
  ] <> case copy_cc of
      DontCopyCC -> mempty
      CopyCC     -> closureCC t |= closureCC s

mkClosure :: JExpr -> [JExpr] -> JExpr -> Maybe JExpr -> Closure
mkClosure entry fields meta cc = Closure
  { clEntry  = entry
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
           _:x:xs -> ValExpr . JHash . listToUniqMap $ zip (map dataFieldName [1..]) (x:xs)


-------------------------------------------------------------------------------
--                             Name Caches
-------------------------------------------------------------------------------
-- $names

-- | Cache "dXXX" field names
dataFieldCache :: Array Int FastString
dataFieldCache = listArray (0,nFieldCache) (map (mkFastString . ('d':) . show) [(0::Int)..nFieldCache])

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
dataCache = listArray (0,jsClosureCount) (map (mkFastString . ("h$d"++) . show) [(0::Int)..jsClosureCount])

dataName :: Int -> FastString
dataName i
  | i < 0 || i > nFieldCache = mkFastString ("h$d" ++ show i)
  | otherwise                = dataCache ! i

allocData :: Int -> JExpr
allocData i = toJExpr (TxtI (dataName i))

-- | Cache "h$cXXX" names
clsCache :: Array Int FastString
clsCache = listArray (0,jsClosureCount) (map (mkFastString . ("h$c"++) . show) [(0::Int)..jsClosureCount])

clsName :: Int -> FastString
clsName i
  | i < 0 || i > jsClosureCount = mkFastString ("h$c" ++ show i)
  | otherwise                   = clsCache ! i

allocClsA :: Int -> JExpr
allocClsA i = toJExpr (TxtI (clsName i))

-- | Cache "xXXX" names
varCache :: Array Int Ident
varCache = listArray (0,jsClosureCount) (map (TxtI . mkFastString . ('x':) . show) [(0::Int)..jsClosureCount])

varName :: Int -> Ident
varName i
  | i < 0 || i > jsClosureCount = TxtI $ mkFastString ('x' : show i)
  | otherwise                   = varCache ! i
