{-# LANGUAGE LambdaCase #-}
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
  )
where

import GHC.Prelude
import GHC.Data.ShortText

import GHC.StgToJS.Heap
import GHC.StgToJS.Types
import GHC.StgToJS.CoreUtils
import GHC.StgToJS.Regs (stack,sp)

import GHC.JS.Make
import GHC.JS.Syntax

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
            -> ShortText   -- ^ the object name
            -> CIRegs      -- ^ things in registers
            -> CILayout    -- ^ layout of the object
            -> ClosureType -- ^ closure type
            -> ShortText   -- ^ object name, for printing
            -> Int         -- ^ `a' argument, depends on type (arity, conid)
            -> CIStatic    -- ^ static refs
            -> JStat
setObjInfoL debug obj rs layout t n a
  = setObjInfo debug obj t n field_types a size rs
      where
        size = case layout of
          CILayoutVariable     -> (-1)
          CILayoutUnknown size -> size
          CILayoutFixed size _ -> size
        field_types = case layout of
          CILayoutVariable     -> []
          CILayoutUnknown size -> toTypeList (replicate size ObjV)
          CILayoutFixed _ fs   -> toTypeList fs

setObjInfo :: Bool        -- ^ debug: output all symbol names
           -> ShortText   -- ^ the thing to modify
           -> ClosureType -- ^ closure type
           -> ShortText   -- ^ object name, for printing
           -> [Int]       -- ^ list of item types in the object, if known (free variables, datacon fields)
           -> Int         -- ^ extra 'a' parameter, for constructor tag or arity
           -> Int         -- ^ object size, -1 (number of vars) for unknown
           -> CIRegs      -- ^ things in registers
           -> CIStatic    -- ^ static refs
           -> JStat
setObjInfo debug obj t name fields a size regs static
   | debug     = appS "h$setObjInfo" [ var obj
                                     , toJExpr t
                                     , toJExpr name
                                     , toJExpr fields
                                     , toJExpr a
                                     , toJExpr size
                                     , toJExpr (regTag regs)
                                     , toJExpr static
                                     ]
   | otherwise = appS "h$o" [ var obj
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
closure ci body = (TxtI (ciVar ci)||= jLam body) `mappend` closureInfoStat False ci

conClosure :: ShortText -> ShortText -> CILayout -> Int -> JStat
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
