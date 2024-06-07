{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module GHC.StgToJS.Literal
  ( genLit
  , genStaticLit
  )
where

import GHC.Prelude

import GHC.JS.JStg.Syntax
import GHC.JS.Make
import GHC.JS.Ident

import GHC.StgToJS.Ids
import GHC.StgToJS.Monad
import GHC.StgToJS.Symbols
import GHC.StgToJS.Types

import GHC.Data.FastString
import GHC.Types.Literal
import GHC.Types.Basic
import GHC.Types.RepType
import GHC.Utils.Misc
import GHC.Utils.Panic
import GHC.Utils.Outputable
import GHC.Float

import Data.Bits as Bits
import Data.Char (ord)

-- | Generate JS expressions for a Literal
--
-- Literals represented with 2 values:
--  * Addr# (Null and Strings): array and offset
--  * 64-bit values: high 32-bit, low 32-bit
--  * labels: call to h$mkFunctionPtr and 0, or function name and 0
genLit :: HasDebugCallStack => Literal -> G [JStgExpr]
genLit = \case
  LitChar c     -> return [ toJExpr (ord c) ]
  LitString str ->
    freshIdent >>= \strLit@(identFS -> strLitT) ->
      freshIdent >>= \strOff@(identFS -> strOffT) -> do
        emitStatic strLitT (StaticUnboxed (StaticUnboxedString str)) Nothing
        emitStatic strOffT (StaticUnboxed (StaticUnboxedStringOffset str)) Nothing
        return [ Var strLit, Var strOff ]
  LitNullAddr              -> return [ null_, ValExpr (JInt 0) ]
  LitNumber nt v           -> case nt of
    LitNumInt     -> return [ toJExpr v ]
    LitNumInt8    -> return [ toJExpr v ]
    LitNumInt16   -> return [ toJExpr v ]
    LitNumInt32   -> return [ toJExpr v ]
    LitNumInt64   -> return [ toJExpr (Bits.shiftR v 32), toU32Expr v ]
    LitNumWord    -> return [ toU32Expr v ]
    LitNumWord8   -> return [ toU32Expr v ]
    LitNumWord16  -> return [ toU32Expr v ]
    LitNumWord32  -> return [ toU32Expr v ]
    LitNumWord64  -> return [ toU32Expr (Bits.shiftR v 32), toU32Expr v ]
    LitNumBigNat  -> panic "genLit: unexpected BigNat that should have been removed in CorePrep"
  LitFloat r               -> return [ toJExpr (r2f r) ]
  LitDouble r              -> return [ toJExpr (r2d r) ]
  LitLabel name fod
    | fod == IsFunction      -> return [ ApplExpr hdMkFunctionPtr
                                                  [global (mkRawSymbol True name)]
                                       , ValExpr (JInt 0)
                                       ]
    | otherwise              -> return [ toJExpr (global (mkRawSymbol True name))
                                       , ValExpr (JInt 0)
                                       ]
  LitRubbish _ rr_ty ->
    -- Generate appropriate rubbish literals, otherwise it might trip up the
    -- code generator when a primop is applied to a rubbish literal (see #24664)
    let reps = runtimeRepPrimRep (text "GHC.StgToJS.Literal.genLit") rr_ty
        rub  = \case
                  BoxedRep _ -> [ null_ ]
                  AddrRep    -> [ null_, ValExpr (JInt 0) ]
                  WordRep    -> [ ValExpr (JInt 0) ]
                  Word8Rep   -> [ ValExpr (JInt 0) ]
                  Word16Rep  -> [ ValExpr (JInt 0) ]
                  Word32Rep  -> [ ValExpr (JInt 0) ]
                  Word64Rep  -> [ ValExpr (JInt 0), ValExpr (JInt 0) ]
                  IntRep     -> [ ValExpr (JInt 0) ]
                  Int8Rep    -> [ ValExpr (JInt 0) ]
                  Int16Rep   -> [ ValExpr (JInt 0) ]
                  Int32Rep   -> [ ValExpr (JInt 0) ]
                  Int64Rep   -> [ ValExpr (JInt 0), ValExpr (JInt 0) ]
                  DoubleRep  -> [ ValExpr (JInt 0) ]
                  FloatRep   -> [ ValExpr (JInt 0) ]
                  VecRep _ _ -> panic "GHC.StgToJS.Literal.genLit: VecRep unsupported"
    in return (concatMap rub reps)

-- | generate a literal for the static init tables
genStaticLit :: Literal -> G [StaticLit]
genStaticLit = \case
  LitChar c                -> return [ IntLit (fromIntegral $ ord c) ]
  LitString str
    | True                 -> return [ StringLit (mkFastStringByteString str), IntLit 0]
    -- \|  invalid UTF8         -> return [ BinLit str, IntLit 0]
  LitNullAddr              -> return [ NullLit, IntLit 0 ]
  LitNumber nt v           -> case nt of
    LitNumInt     -> return [ IntLit v ]
    LitNumInt8    -> return [ IntLit v ]
    LitNumInt16   -> return [ IntLit v ]
    LitNumInt32   -> return [ IntLit v ]
    LitNumInt64   -> return [ IntLit (v `Bits.shiftR` 32), toU32Lit v ]
    LitNumWord    -> return [ toU32Lit v ]
    LitNumWord8   -> return [ toU32Lit v ]
    LitNumWord16  -> return [ toU32Lit v ]
    LitNumWord32  -> return [ toU32Lit v ]
    LitNumWord64  -> return [ toU32Lit (v `Bits.shiftR` 32), toU32Lit v ]
    LitNumBigNat  -> panic "genStaticLit: unexpected BigNat that should have been removed in CorePrep"
  LitFloat r               -> return [ DoubleLit . SaneDouble . r2f $ r ]
  LitDouble r              -> return [ DoubleLit . SaneDouble . r2d $ r ]
  LitLabel name fod        -> return [ LabelLit (fod == IsFunction) (mkRawSymbol True name)
                                     , IntLit 0 ]
  l -> pprPanic "genStaticLit" (ppr l)

-- make an unsigned 32 bit number from this unsigned one, lower 32 bits
toU32Expr :: Integer -> JStgExpr
toU32Expr i = Int (i Bits..&. 0xFFFFFFFF) .>>>. 0

-- make an unsigned 32 bit number from this unsigned one, lower 32 bits
toU32Lit :: Integer -> StaticLit
toU32Lit i = IntLit (i Bits..&. 0xFFFFFFFF)

r2d :: Rational -> Double
r2d = realToFrac

r2f :: Rational -> Double
r2f = float2Double . realToFrac
