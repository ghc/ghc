{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiWayIf #-}

-- disable this warning because of all the lambdas matching on primops'
-- arguments.
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module GHC.StgToJS.Prim
  ( genPrim
  )
where

import GHC.Prelude

import GHC.JS.JStg.Syntax hiding (YieldOp)
import GHC.JS.JStg.Monad
import GHC.JS.Make
import GHC.JS.Ident

import GHC.StgToJS.Heap
import GHC.StgToJS.Types
import GHC.StgToJS.Profiling
import GHC.StgToJS.Regs
import GHC.StgToJS.Symbols

import GHC.Core.Type

import GHC.Builtin.PrimOps
import GHC.Tc.Utils.TcType (isBoolTy)
import GHC.Utils.Encoding (zEncodeString)

import GHC.Data.FastString
import GHC.Utils.Outputable (renderWithContext, defaultSDocContext, ppr)

genPrim :: Bool     -- ^ Profiling (cost-centres) enabled
        -> Bool     -- ^ Array bounds-checking enabled
        -> Type
        -> PrimOp   -- ^ the primitive operation
        -> [JStgExpr]  -- ^ where to store the result
        -> [JStgExpr]  -- ^ arguments
        -> JSM PrimRes
genPrim prof bound ty op = case op of
  CharGtOp        -> \[r] [x,y] -> pure $ PrimInline $ r |= if10 (x .>. y)
  CharGeOp        -> \[r] [x,y] -> pure $ PrimInline $ r |= if10 (x .>=. y)
  CharEqOp        -> \[r] [x,y] -> pure $ PrimInline $ r |= if10 (x .===. y)
  CharNeOp        -> \[r] [x,y] -> pure $ PrimInline $ r |= if10 (x .!==. y)
  CharLtOp        -> \[r] [x,y] -> pure $ PrimInline $ r |= if10 (x .<. y)
  CharLeOp        -> \[r] [x,y] -> pure $ PrimInline $ r |= if10 (x .<=. y)
  OrdOp           -> \[r] [x]   -> pure $ PrimInline $ r |= x

  Int8ToWord8Op   -> \[r] [x]   -> pure $ PrimInline $ r |= mask8 x
  Word8ToInt8Op   -> \[r] [x]   -> pure $ PrimInline $ r |= signExtend8 x
  Int16ToWord16Op -> \[r] [x]   -> pure $ PrimInline $ r |= mask16 x
  Word16ToInt16Op -> \[r] [x]   -> pure $ PrimInline $ r |= signExtend16 x
  Int32ToWord32Op -> \[r] [x]   -> pure $ PrimInline $ r |= x .>>>. zero_
  Word32ToInt32Op -> \[r] [x]   -> pure $ PrimInline $ r |= toI32 x

------------------------------ Int ----------------------------------------------

  IntAddOp        -> \[r] [x,y] -> pure $ PrimInline $ r |= toI32 (Add x y)
  IntSubOp        -> \[r] [x,y] -> pure $ PrimInline $ r |= toI32 (Sub x y)
  IntMulOp        -> \[r] [x,y] -> pure $ PrimInline $ r |= app hdMulImulStr [x, y]
  IntMul2Op       -> \[c,hr,lr] [x,y] -> pure $ PrimInline $ appT [c,hr,lr] hdHsTimesInt2Str [x, y]
  IntMulMayOfloOp -> \[r] [x,y] -> do
    PrimInline <$>
      jVar \tmp ->
             pure $ mconcat
             [ tmp |= Mul x y
             , r   |= if01 (tmp .===. toI32 tmp)
             ]
  IntQuotOp       -> \[r]   [x,y] -> pure $ PrimInline $ r |= toI32 (Div x y)
  IntRemOp        -> \[r]   [x,y] -> pure $ PrimInline $ r |= Mod x y
  IntQuotRemOp    -> \[q,r] [x,y] -> pure $ PrimInline $ mconcat
                                     [ q |= toI32 (Div x y)
                                     , r |= x `Sub` (Mul y q)
                                     ]
  IntAndOp        -> \[r] [x,y]   -> pure $ PrimInline $ r |= BAnd x y
  IntOrOp         -> \[r] [x,y]   -> pure $ PrimInline $ r |= BOr  x y
  IntXorOp        -> \[r] [x,y]   -> pure $ PrimInline $ r |= BXor x y
  IntNotOp        -> \[r] [x]     -> pure $ PrimInline $ r |= BNot x

  IntNegOp        -> \[r] [x]   -> pure $ PrimInline $ r |= toI32 (Negate x)
-- add with carry: overflow == 0 iff no overflow
  IntAddCOp       -> \[r,overf] [x,y] ->
    PrimInline <$>
    jVar \tmp ->
           pure $ mconcat
           [ tmp   |= Add x y
           , r     |= toI32 tmp
           , overf |= if10 (r .!=. tmp)
           ]
  IntSubCOp       -> \[r,overf] [x,y] ->
    PrimInline <$>
    jVar \tmp ->
           pure $ mconcat
           [ tmp   |= Sub x y
           , r     |= toI32 tmp
           , overf |= if10 (r .!=. tmp)
           ]
  IntGtOp           -> \[r] [x,y] -> pure $ PrimInline $ r |= if10 (x .>. y)
  IntGeOp           -> \[r] [x,y] -> pure $ PrimInline $ r |= if10 (x .>=. y)
  IntEqOp           -> \[r] [x,y] -> pure $ PrimInline $ r |= if10 (x .===. y)
  IntNeOp           -> \[r] [x,y] -> pure $ PrimInline $ r |= if10 (x .!==. y)
  IntLtOp           -> \[r] [x,y] -> pure $ PrimInline $ r |= if10 (x .<. y)
  IntLeOp           -> \[r] [x,y] -> pure $ PrimInline $ r |= if10 (x .<=. y)
  ChrOp             -> \[r] [x]   -> pure $ PrimInline $ r |= x
  IntToWordOp       -> \[r] [x]   -> pure $ PrimInline $ r |= x .>>>. zero_
  IntToFloatOp      -> \[r] [x]   -> pure $ PrimInline $ r |= x
  IntToDoubleOp     -> \[r] [x]   -> pure $ PrimInline $ r |= x
  IntSllOp          -> \[r] [x,y] -> pure $ PrimInline $ r |= x .<<. y
  IntSraOp          -> \[r] [x,y] -> pure $ PrimInline $ r |= x .>>. y
  IntSrlOp          -> \[r] [x,y] -> pure $ PrimInline $ r |= toI32 (x .>>>. y)

------------------------------ Int8 ---------------------------------------------

  Int8ToIntOp       -> \[r] [x]       -> pure $ PrimInline $ r |= x
  IntToInt8Op       -> \[r] [x]       -> pure $ PrimInline $ r |= signExtend8 x
  Int8NegOp         -> \[r] [x]       -> pure $ PrimInline $ r |= signExtend8 (Negate x)
  Int8AddOp         -> \[r] [x,y]     -> pure $ PrimInline $ r |= signExtend8 (Add x y)
  Int8SubOp         -> \[r] [x,y]     -> pure $ PrimInline $ r |= signExtend8 (Sub x y)
  Int8MulOp         -> \[r] [x,y]     -> pure $ PrimInline $ r |= signExtend8 (Mul x y)
  Int8QuotOp        -> \[r] [x,y]     -> pure $ PrimInline $ r |= signExtend8 (quotShortInt 8 x y)
  Int8RemOp         -> \[r] [x,y]     -> pure $ PrimInline $ r |= signExtend8 (remShortInt 8 x y)
  Int8QuotRemOp     -> \[r1,r2] [x,y] -> pure $ PrimInline $ mconcat
                                         [ r1 |= signExtend8 (quotShortInt 8 x y)
                                         , r2 |= signExtend8 (remShortInt 8 x y)
                                         ]
  Int8EqOp          -> \[r] [x,y] -> pure $ PrimInline $ r |= if10 (x .===. y)
  Int8GeOp          -> \[r] [x,y] -> pure $ PrimInline $ r |= if10 ((x .<<. (Int 24)) .>=. (y .<<. (Int 24)))
  Int8GtOp          -> \[r] [x,y] -> pure $ PrimInline $ r |= if10 ((x .<<. (Int 24)) .>.  (y .<<. (Int 24)))
  Int8LeOp          -> \[r] [x,y] -> pure $ PrimInline $ r |= if10 ((x .<<. (Int 24)) .<=. (y .<<. (Int 24)))
  Int8LtOp          -> \[r] [x,y] -> pure $ PrimInline $ r |= if10 ((x .<<. (Int 24)) .<.  (y .<<. (Int 24)))
  Int8NeOp          -> \[r] [x,y] -> pure $ PrimInline $ r |= if10 (x .!==. y)

  Int8SraOp         -> \[r] [x,i]   -> pure $ PrimInline $ r |= x .>>. i
  Int8SrlOp         -> \[r] [x,i]   -> pure $ PrimInline $ r |= signExtend8 (mask8 x .>>>. i)
  Int8SllOp         -> \[r] [x,i]   -> pure $ PrimInline $ r |= signExtend8 (mask8 (x .<<. i))

------------------------------ Word8 --------------------------------------------

  Word8ToWordOp      -> \[r] [x]       -> pure $ PrimInline $ r |= mask8 x
  WordToWord8Op      -> \[r] [x]       -> pure $ PrimInline $ r |= mask8 x

  Word8AddOp         -> \[r] [x,y]     -> pure $ PrimInline $ r |= mask8 (Add x y)
  Word8SubOp         -> \[r] [x,y]     -> pure $ PrimInline $ r |= mask8 (Sub x y)
  Word8MulOp         -> \[r] [x,y]     -> pure $ PrimInline $ r |= mask8 (Mul x y)
  Word8QuotOp        -> \[r] [x,y]     -> pure $ PrimInline $ r |= mask8 (Div x y)
  Word8RemOp         -> \[r] [x,y]     -> pure $ PrimInline $ r |= Mod x y
  Word8QuotRemOp     -> \[r1,r2] [x,y] -> pure $ PrimInline $ mconcat
                                          [ r1 |= toI32 (Div x y)
                                          , r2 |= Mod x y
                                          ]
  Word8EqOp          -> \[r] [x,y] -> pure $ PrimInline $ r |= if10 (x .===. y)
  Word8GeOp          -> \[r] [x,y] -> pure $ PrimInline $ r |= if10 (x .>=. y)
  Word8GtOp          -> \[r] [x,y] -> pure $ PrimInline $ r |= if10 (x .>. y)
  Word8LeOp          -> \[r] [x,y] -> pure $ PrimInline $ r |= if10 (x .<=. y)
  Word8LtOp          -> \[r] [x,y] -> pure $ PrimInline $ r |= if10 (x .<. y)
  Word8NeOp          -> \[r] [x,y] -> pure $ PrimInline $ r |= if10 (x .!==. y)

  Word8AndOp         -> \[r] [x,y]   -> pure $ PrimInline $ r |= BAnd x y
  Word8OrOp          -> \[r] [x,y]   -> pure $ PrimInline $ r |= BOr  x y
  Word8XorOp         -> \[r] [x,y]   -> pure $ PrimInline $ r |= BXor x y
  Word8NotOp         -> \[r] [x]     -> pure $ PrimInline $ r |= BXor x (Int 0xff)

  Word8SllOp         -> \[r] [x,i]   -> pure $ PrimInline $ r |= mask8 (x .<<. i)
  Word8SrlOp         -> \[r] [x,i]   -> pure $ PrimInline $ r |= x .>>>. i

------------------------------ Int16 -------------------------------------------

  Int16ToIntOp       -> \[r] [x]       -> pure $ PrimInline $ r |= x
  IntToInt16Op       -> \[r] [x]       -> pure $ PrimInline $ r |= signExtend16 x

  Int16NegOp         -> \[r] [x]       -> pure $ PrimInline $ r |= signExtend16 (Negate x)
  Int16AddOp         -> \[r] [x,y]     -> pure $ PrimInline $ r |= signExtend16 (Add x y)
  Int16SubOp         -> \[r] [x,y]     -> pure $ PrimInline $ r |= signExtend16 (Sub x y)
  Int16MulOp         -> \[r] [x,y]     -> pure $ PrimInline $ r |= signExtend16 (Mul x y)
  Int16QuotOp        -> \[r] [x,y]     -> pure $ PrimInline $ r |= signExtend16 (quotShortInt 16 x y)
  Int16RemOp         -> \[r] [x,y]     -> pure $ PrimInline $ r |= signExtend16 (remShortInt 16 x y)
  Int16QuotRemOp     -> \[r1,r2] [x,y] -> pure $ PrimInline $ mconcat
                                          [ r1 |= signExtend16 (quotShortInt 16 x y)
                                          , r2 |= signExtend16 (remShortInt 16 x y)
                                          ]
  Int16EqOp          -> \[r] [x,y] -> pure $ PrimInline $ r |= if10 (x .===. y)
  Int16GeOp          -> \[r] [x,y] -> pure $ PrimInline $ r |= if10 ((x .<<. (Int 16)) .>=. (y .<<. (Int 16)))
  Int16GtOp          -> \[r] [x,y] -> pure $ PrimInline $ r |= if10 ((x .<<. (Int 16)) .>.  (y .<<. (Int 16)))
  Int16LeOp          -> \[r] [x,y] -> pure $ PrimInline $ r |= if10 ((x .<<. (Int 16)) .<=. (y .<<. (Int 16)))
  Int16LtOp          -> \[r] [x,y] -> pure $ PrimInline $ r |= if10 ((x .<<. (Int 16)) .<.  (y .<<. (Int 16)))
  Int16NeOp          -> \[r] [x,y] -> pure $ PrimInline $ r |= if10 (x .!==. y)

  Int16SraOp         -> \[r] [x,i]   -> pure $ PrimInline $ r |= x .>>. i
  Int16SrlOp         -> \[r] [x,i]   -> pure $ PrimInline $ r |= signExtend16 (mask16 x .>>>. i)
  Int16SllOp         -> \[r] [x,i]   -> pure $ PrimInline $ r |= signExtend16 (x .<<. i)

------------------------------ Word16 ------------------------------------------

  Word16ToWordOp     -> \[r] [x]   -> pure $ PrimInline $ r |= x
  WordToWord16Op     -> \[r] [x]   -> pure $ PrimInline $ r |= mask16 x

  Word16AddOp        -> \[r] [x,y] -> pure $ PrimInline $ r |= mask16 (Add x y)
  Word16SubOp        -> \[r] [x,y] -> pure $ PrimInline $ r |= mask16 (Sub x y)
  Word16MulOp        -> \[r] [x,y] -> pure $ PrimInline $ r |= mask16 (Mul x y)
  Word16QuotOp       -> \[r] [x,y] -> pure $ PrimInline $ r |= mask16 (Div x y)
  Word16RemOp        -> \[r] [x,y] -> pure $ PrimInline $ r |= Mod x y
  Word16QuotRemOp    -> \[r1,r2] [x,y] -> pure $ PrimInline $ mconcat
                                          [ r1 |= toI32 (Div x y)
                                          , r2 |= Mod x y
                                          ]
  Word16EqOp         -> \[r] [x,y] -> pure $ PrimInline $ r |= if10 (x .===. y)
  Word16GeOp         -> \[r] [x,y] -> pure $ PrimInline $ r |= if10 (x .>=. y)
  Word16GtOp         -> \[r] [x,y] -> pure $ PrimInline $ r |= if10 (x .>. y)
  Word16LeOp         -> \[r] [x,y] -> pure $ PrimInline $ r |= if10 (x .<=. y)
  Word16LtOp         -> \[r] [x,y] -> pure $ PrimInline $ r |= if10 (x .<. y)
  Word16NeOp         -> \[r] [x,y] -> pure $ PrimInline $ r |= if10 (x .!==. y)

  Word16AndOp        -> \[r] [x,y]   -> pure $ PrimInline $ r |= BAnd x y
  Word16OrOp         -> \[r] [x,y]   -> pure $ PrimInline $ r |= BOr  x y
  Word16XorOp        -> \[r] [x,y]   -> pure $ PrimInline $ r |= BXor x y
  Word16NotOp        -> \[r] [x]     -> pure $ PrimInline $ r |= BXor x (Int 0xffff)

  Word16SllOp        -> \[r] [x,i]   -> pure $ PrimInline $ r |= mask16 (x .<<. i)
  Word16SrlOp        -> \[r] [x,i]   -> pure $ PrimInline $ r |= x .>>>. i

------------------------------ Int32 --------------------------------------------

  Int32ToIntOp       -> \[r] [x]   -> pure $ PrimInline $ r |= x
  IntToInt32Op       -> \[r] [x]   -> pure $ PrimInline $ r |= x

  Int32NegOp         -> \rs  xs    -> genPrim prof bound ty IntNegOp rs xs
  Int32AddOp         -> \rs  xs    -> genPrim prof bound ty IntAddOp rs xs
  Int32SubOp         -> \rs  xs    -> genPrim prof bound ty IntSubOp rs xs
  Int32MulOp         -> \rs  xs    -> genPrim prof bound ty IntMulOp rs xs
  Int32QuotOp        -> \rs  xs    -> genPrim prof bound ty IntQuotOp rs xs
  Int32RemOp         -> \rs  xs    -> genPrim prof bound ty IntRemOp rs xs
  Int32QuotRemOp     -> \rs  xs    -> genPrim prof bound ty IntQuotRemOp rs xs

  Int32EqOp          -> \rs  xs    -> genPrim prof bound ty IntEqOp rs xs
  Int32GeOp          -> \rs  xs    -> genPrim prof bound ty IntGeOp rs xs
  Int32GtOp          -> \rs  xs    -> genPrim prof bound ty IntGtOp rs xs
  Int32LeOp          -> \rs  xs    -> genPrim prof bound ty IntLeOp rs xs
  Int32LtOp          -> \rs  xs    -> genPrim prof bound ty IntLtOp rs xs
  Int32NeOp          -> \rs  xs    -> genPrim prof bound ty IntNeOp rs xs

  Int32SraOp         -> \rs  xs    -> genPrim prof bound ty IntSraOp rs xs
  Int32SrlOp         -> \rs  xs    -> genPrim prof bound ty IntSrlOp rs xs
  Int32SllOp         -> \rs  xs    -> genPrim prof bound ty IntSllOp rs xs

------------------------------ Word32 -------------------------------------------

  Word32ToWordOp     -> \[r] [x]   -> pure $ PrimInline $ r |= x
  WordToWord32Op     -> \[r] [x]   -> pure $ PrimInline $ r |= x

  Word32AddOp        -> \rs  xs    -> genPrim prof bound ty WordAddOp rs xs
  Word32SubOp        -> \rs  xs    -> genPrim prof bound ty WordSubOp rs xs
  Word32MulOp        -> \rs  xs    -> genPrim prof bound ty WordMulOp rs xs
  Word32QuotOp       -> \rs  xs    -> genPrim prof bound ty WordQuotOp rs xs
  Word32RemOp        -> \rs  xs    -> genPrim prof bound ty WordRemOp rs xs
  Word32QuotRemOp    -> \rs  xs    -> genPrim prof bound ty WordQuotRemOp rs xs

  Word32EqOp         -> \rs  xs    -> genPrim prof bound ty WordEqOp rs xs
  Word32GeOp         -> \rs  xs    -> genPrim prof bound ty WordGeOp rs xs
  Word32GtOp         -> \rs  xs    -> genPrim prof bound ty WordGtOp rs xs
  Word32LeOp         -> \rs  xs    -> genPrim prof bound ty WordLeOp rs xs
  Word32LtOp         -> \rs  xs    -> genPrim prof bound ty WordLtOp rs xs
  Word32NeOp         -> \rs  xs    -> genPrim prof bound ty WordNeOp rs xs

  Word32AndOp        -> \rs xs     -> genPrim prof bound ty WordAndOp rs xs
  Word32OrOp         -> \rs xs     -> genPrim prof bound ty WordOrOp rs xs
  Word32XorOp        -> \rs xs     -> genPrim prof bound ty WordXorOp rs xs
  Word32NotOp        -> \rs xs     -> genPrim prof bound ty WordNotOp rs xs

  Word32SllOp        -> \rs xs     -> genPrim prof bound ty WordSllOp rs xs
  Word32SrlOp        -> \rs xs     -> genPrim prof bound ty WordSrlOp rs xs

------------------------------ Int64 --------------------------------------------

  Int64ToIntOp      -> \[r] [_h,l] -> pure $ PrimInline $ r |= toI32 l

  Int64NegOp        -> \[r_h,r_l] [h,l] ->
      pure $ PrimInline $ mconcat
        [ r_l |= toU32 (Add (BNot l) one_)
        , r_h |= toI32 (Add (BNot h) (Not r_l))
        ]

  Int64AddOp  -> \[hr,lr] [h0,l0,h1,l1] -> pure $ PrimInline $ appT [hr,lr] hdHsPlusInt64Str  [h0,l0,h1,l1]
  Int64SubOp  -> \[hr,lr] [h0,l0,h1,l1] -> pure $ PrimInline $ appT [hr,lr] hdHsMinusInt64Str [h0,l0,h1,l1]
  Int64MulOp  -> \[hr,lr] [h0,l0,h1,l1] -> pure $ PrimInline $ appT [hr,lr] hdHsTimesInt64Str [h0,l0,h1,l1]
  Int64QuotOp -> \[hr,lr] [h0,l0,h1,l1] -> pure $ PrimInline $ appT [hr,lr] hdHsQuotInt64Str  [h0,l0,h1,l1]
  Int64RemOp  -> \[hr,lr] [h0,l0,h1,l1] -> pure $ PrimInline $ appT [hr,lr] hdHsRemInt64Str   [h0,l0,h1,l1]

  Int64SllOp  -> \[hr,lr] [h,l,n] -> pure $ PrimInline $ appT [hr,lr] hdHsUncheckedShiftLLInt64Str [h,l,n]
  Int64SraOp  -> \[hr,lr] [h,l,n] -> pure $ PrimInline $ appT [hr,lr] hdHsUncheckedShiftRAInt64Str [h,l,n]
  Int64SrlOp  -> \[hr,lr] [h,l,n] -> pure $ PrimInline $ appT [hr,lr] hdHsUncheckedShiftRLInt64Str [h,l,n]

  Int64ToWord64Op   -> \[r1,r2] [x1,x2] ->
      pure $ PrimInline $ mconcat
       [ r1 |= toU32 x1
       , r2 |= x2
       ]
  IntToInt64Op      -> \[r1,r2] [x] ->
      pure $ PrimInline $ mconcat
       [ r1 |= if_ (x .<. zero_) (Negate one_) zero_ -- sign-extension
       , r2 |= toU32 x
       ]

  Int64EqOp -> \[r] [h0,l0,h1,l1] -> pure $ PrimInline $ r |= if10 (LAnd (l0 .===. l1) (h0 .===. h1))
  Int64NeOp -> \[r] [h0,l0,h1,l1] -> pure $ PrimInline $ r |= if10 (LOr (l0 .!==. l1) (h0 .!==. h1))
  Int64GeOp -> \[r] [h0,l0,h1,l1] -> pure $ PrimInline $ r |= if10 (LOr (h0 .>. h1) (LAnd (h0 .===. h1) (l0 .>=. l1)))
  Int64GtOp -> \[r] [h0,l0,h1,l1] -> pure $ PrimInline $ r |= if10 (LOr (h0 .>. h1) (LAnd (h0 .===. h1) (l0 .>. l1)))
  Int64LeOp -> \[r] [h0,l0,h1,l1] -> pure $ PrimInline $ r |= if10 (LOr (h0 .<. h1) (LAnd (h0 .===. h1) (l0 .<=. l1)))
  Int64LtOp -> \[r] [h0,l0,h1,l1] -> pure $ PrimInline $ r |= if10 (LOr (h0 .<. h1) (LAnd (h0 .===. h1) (l0 .<. l1)))

------------------------------ Word64 -------------------------------------------

  Word64ToWordOp    -> \[r] [_x1,x2] -> pure $ PrimInline $ r |= x2

  WordToWord64Op    -> \[rh,rl] [x] ->
    pure $ PrimInline $ mconcat
     [ rh |= zero_
     , rl |= x
     ]

  Word64ToInt64Op   -> \[r1,r2] [x1,x2] ->
    pure $ PrimInline $ mconcat
     [ r1 |= toI32 x1
     , r2 |= x2
     ]

  Word64EqOp -> \[r] [h0,l0,h1,l1] -> pure $ PrimInline $ r |= if10 (LAnd (l0 .===. l1) (h0 .===. h1))
  Word64NeOp -> \[r] [h0,l0,h1,l1] -> pure $ PrimInline $ r |= if10 (LOr (l0 .!==. l1) (h0 .!==. h1))
  Word64GeOp -> \[r] [h0,l0,h1,l1] -> pure $ PrimInline $ r |= if10 (LOr (h0 .>. h1) (LAnd (h0 .===. h1) (l0 .>=. l1)))
  Word64GtOp -> \[r] [h0,l0,h1,l1] -> pure $ PrimInline $ r |= if10 (LOr (h0 .>. h1) (LAnd (h0 .===. h1) (l0 .>. l1)))
  Word64LeOp -> \[r] [h0,l0,h1,l1] -> pure $ PrimInline $ r |= if10 (LOr (h0 .<. h1) (LAnd (h0 .===. h1) (l0 .<=. l1)))
  Word64LtOp -> \[r] [h0,l0,h1,l1] -> pure $ PrimInline $ r |= if10 (LOr (h0 .<. h1) (LAnd (h0 .===. h1) (l0 .<. l1)))

  Word64SllOp -> \[hr,lr] [h,l,n] -> pure $ PrimInline $ appT [hr,lr] hdHsUncheckedShiftLWord64Str [h,l,n]
  Word64SrlOp -> \[hr,lr] [h,l,n] -> pure $ PrimInline $ appT [hr,lr] hdHsUncheckedShiftRWord64Str [h,l,n]

  Word64OrOp  -> \[hr,hl] [h0, l0, h1, l1] ->
      pure $ PrimInline $ mconcat
        [ hr |= toU32 (BOr h0 h1)
        , hl |= toU32 (BOr l0 l1)
        ]

  Word64AndOp -> \[hr,hl] [h0, l0, h1, l1] ->
      pure $ PrimInline $ mconcat
        [ hr |= toU32 (BAnd h0 h1)
        , hl |= toU32 (BAnd l0 l1)
        ]

  Word64XorOp -> \[hr,hl] [h0, l0, h1, l1] ->
      pure $ PrimInline $ mconcat
        [ hr |= toU32 (BXor h0 h1)
        , hl |= toU32 (BXor l0 l1)
        ]

  Word64NotOp -> \[hr,hl] [h, l] ->
      pure $ PrimInline $ mconcat
        [ hr |= toU32 (BNot h)
        , hl |= toU32 (BNot l)
        ]

  Word64AddOp  -> \[hr,lr] [h0,l0,h1,l1] -> pure $ PrimInline $ appT [hr,lr] hdHsPlusWord64Str  [h0,l0,h1,l1]
  Word64SubOp  -> \[hr,lr] [h0,l0,h1,l1] -> pure $ PrimInline $ appT [hr,lr] hdHsMinusWord64Str [h0,l0,h1,l1]
  Word64MulOp  -> \[hr,lr] [h0,l0,h1,l1] -> pure $ PrimInline $ appT [hr,lr] hdHsTimesWord64Str [h0,l0,h1,l1]
  Word64QuotOp -> \[hr,lr] [h0,l0,h1,l1] -> pure $ PrimInline $ appT [hr,lr] hdHsQuotWord64Str  [h0,l0,h1,l1]
  Word64RemOp  -> \[hr,lr] [h0,l0,h1,l1] -> pure $ PrimInline $ appT [hr,lr] hdHsRemWord64Str   [h0,l0,h1,l1]

------------------------------ Word ---------------------------------------------

  WordAddOp  -> \[r]   [x,y] -> pure $ PrimInline $ r |= (x `Add` y) .>>>. zero_
  WordAddCOp -> \[r,c] [x,y] ->
      PrimInline <$>
      jVar \tmp ->
             pure $ mconcat
             [ tmp |= x `Add` y
             , r   |= toU32 tmp
             , c   |= if10 (tmp .!==. r)
             ]
  WordSubCOp  -> \[r,c] [x,y] ->
      pure $ PrimInline $ mconcat
        [ r |= toU32 (Sub x y)
        , c |= if10 (y .>. x)
        ]
  WordAdd2Op    -> \[h,l] [x,y] -> pure $ PrimInline $ appT [h,l] hdWordAdd2 [x,y]
  WordSubOp     -> \  [r] [x,y] -> pure $ PrimInline $ r |= toU32 (Sub x y)
  WordMulOp     -> \  [r] [x,y] -> pure $ PrimInline $ r |= toU32 (app hdMulImulStr [x, y])
  WordMul2Op    -> \[h,l] [x,y] -> pure $ PrimInline $ appT [h,l] hdMul2Word32Str [x,y]
  WordQuotOp    -> \  [q] [x,y] -> pure $ PrimInline $ q |= app hdQuotWord32Str [x,y]
  WordRemOp     -> \  [r] [x,y] -> pure $ PrimInline $ r |= app hdRemWord32Str [x,y]
  WordQuotRemOp -> \[q,r] [x,y] -> pure $ PrimInline $ appT [q,r] hdQuotRemWord32Str [x,y]
  WordQuotRem2Op   -> \[q,r] [xh,xl,y] -> pure $ PrimInline $ appT [q,r] hdQuotRem2Word32Str [xh,xl,y]
  WordAndOp        -> \[r] [x,y] -> pure $ PrimInline $ r |= toU32 (BAnd x y)
  WordOrOp         -> \[r] [x,y] -> pure $ PrimInline $ r |= toU32 (BOr  x y)
  WordXorOp        -> \[r] [x,y] -> pure $ PrimInline $ r |= toU32 (BXor x y)
  WordNotOp        -> \[r] [x]   -> pure $ PrimInline $ r |= toU32 (BNot x)
  WordSllOp        -> \[r] [x,y] -> pure $ PrimInline $ r |= toU32 (x .<<. y)
  WordSrlOp        -> \[r] [x,y] -> pure $ PrimInline $ r |= x .>>>. y
  WordToIntOp      -> \[r] [x]   -> pure $ PrimInline $ r |= toI32 x
  WordGtOp         -> \[r] [x,y] -> pure $ PrimInline $ r |= if10 (x .>.  y)
  WordGeOp         -> \[r] [x,y] -> pure $ PrimInline $ r |= if10 (x .>=. y)
  WordEqOp         -> \[r] [x,y] -> pure $ PrimInline $ r |= if10 (x .===. y)
  WordNeOp         -> \[r] [x,y] -> pure $ PrimInline $ r |= if10 (x .!==. y)
  WordLtOp         -> \[r] [x,y] -> pure $ PrimInline $ r |= if10 (x .<.  y)
  WordLeOp         -> \[r] [x,y] -> pure $ PrimInline $ r |= if10 (x .<=. y)
  WordToDoubleOp   -> \[r] [x]   -> pure $ PrimInline $ r |= x
  WordToFloatOp    -> \[r] [x]   -> pure $ PrimInline $ r |= math_fround [x]
  PopCnt8Op        -> \[r] [x]   -> pure $ PrimInline $ r |= hdPopCntTab .! (mask8 x)
  PopCnt16Op       -> \[r] [x]   -> pure $ PrimInline $ r |= Add (hdPopCntTab .! (mask8 x))
                                                      (hdPopCntTab .! (mask8 (x .>>>. Int 8)))

  PopCnt32Op  -> \[r] [x]     -> pure $ PrimInline $ r |= app hdPopCnt32Str [x]
  PopCnt64Op  -> \[r] [x1,x2] -> pure $ PrimInline $ r |= app hdPopCnt64Str [x1,x2]
  PopCntOp    -> \[r] [x]     -> genPrim prof bound ty PopCnt32Op [r] [x]
  Pdep8Op     -> \[r] [s,m]   -> pure $ PrimInline $ r |= app hdPDep8Str  [s,m]
  Pdep16Op    -> \[r] [s,m]   -> pure $ PrimInline $ r |= app hdPDep16Str [s,m]
  Pdep32Op    -> \[r] [s,m]   -> pure $ PrimInline $ r |= app hdPDep32Str [s,m]
  Pdep64Op    -> \[ra,rb] [sa,sb,ma,mb] -> pure $ PrimInline $ appT [ra,rb] hdPDep64Str [sa,sb,ma,mb]
  PdepOp      -> \rs xs                 -> genPrim prof bound ty Pdep32Op rs xs
  Pext8Op     -> \[r] [s,m] -> pure $ PrimInline $ r |= app hdPExit8Str  [s,m]
  Pext16Op    -> \[r] [s,m] -> pure $ PrimInline $ r |= app hdPExit16Str [s,m]
  Pext32Op    -> \[r] [s,m] -> pure $ PrimInline $ r |= app hdPExit32Str [s,m]
  Pext64Op    -> \[ra,rb] [sa,sb,ma,mb] -> pure $ PrimInline $ appT [ra,rb] hdPExit64Str [sa,sb,ma,mb]
  PextOp      -> \rs xs     -> genPrim prof bound ty Pext32Op rs xs

  ClzOp       -> \[r]   [x]     -> pure $ PrimInline $ r |= app  hdClz32Str [x]
  Clz8Op      -> \[r]   [x]     -> pure $ PrimInline $ r |= app  hdClz8Str  [x]
  Clz16Op     -> \[r]   [x]     -> pure $ PrimInline $ r |= app  hdClz16Str [x]
  Clz32Op     -> \[r]   [x]     -> pure $ PrimInline $ r |= app  hdClz32Str [x]
  Clz64Op     -> \[r]   [x1,x2] -> pure $ PrimInline $ r |= app  hdClz64Str [x1,x2]
  CtzOp       -> \[r]   [x]     -> pure $ PrimInline $ r |= app  hdCtz32Str [x]
  Ctz8Op      -> \[r]   [x]     -> pure $ PrimInline $ r |= app  hdCtz8Str  [x]
  Ctz16Op     -> \[r]   [x]     -> pure $ PrimInline $ r |= app  hdCtz16Str [x]
  Ctz32Op     -> \[r]   [x]     -> pure $ PrimInline $ r |= app  hdCtz32Str [x]
  Ctz64Op     -> \[r]   [x1,x2] -> pure $ PrimInline $ r |= app  hdCtz64Str [x1,x2]

  BSwap16Op   -> \[r] [x]   -> pure $ PrimInline $
      r |= BOr ((mask8 x) .<<. (Int 8))
               (mask8 (x .>>>. (Int 8)))
  BSwap32Op   -> \[r] [x]   -> pure $ PrimInline $
      r |= toU32 ((x .<<. (Int 24))
            `BOr` ((BAnd x (Int 0xFF00)) .<<. (Int 8))
            `BOr` ((BAnd x (Int 0xFF0000)) .>>. (Int 8))
            `BOr` (x .>>>. (Int 24)))
  BSwap64Op   -> \[r1,r2] [x,y] -> pure $ PrimInline $ appT [r1,r2] hdBSwap64Str [x,y]
  BSwapOp     -> \[r] [x]       -> genPrim prof bound ty BSwap32Op [r] [x]

  BRevOp      -> \[r] [w] -> genPrim prof bound ty BRev32Op [r] [w]
  BRev8Op     -> \[r] [w] -> pure $ PrimInline $ r |= (app hdReverseWordStr [w] .>>>. Int 24)
  BRev16Op    -> \[r] [w] -> pure $ PrimInline $ r |= (app hdReverseWordStr [w] .>>>. Int 16)
  BRev32Op    -> \[r] [w] -> pure $ PrimInline $ r |= app  hdReverseWordStr [w]
  BRev64Op    -> \[rh,rl] [h,l] -> pure $ PrimInline $ mconcat
                           [ rl |= app hdReverseWordStr [h]
                           , rh |= app hdReverseWordStr [l]
                           ]

------------------------------ Narrow -------------------------------------------

  Narrow8IntOp    -> \[r] [x] -> pure $ PrimInline $ r |= signExtend8  x
  Narrow16IntOp   -> \[r] [x] -> pure $ PrimInline $ r |= signExtend16 x
  Narrow32IntOp   -> \[r] [x] -> pure $ PrimInline $ r |= toI32  x
  Narrow8WordOp   -> \[r] [x] -> pure $ PrimInline $ r |= mask8  x
  Narrow16WordOp  -> \[r] [x] -> pure $ PrimInline $ r |= mask16 x
  Narrow32WordOp  -> \[r] [x] -> pure $ PrimInline $ r |= toU32  x

------------------------------ Double -------------------------------------------

  DoubleGtOp        -> \[r] [x,y] -> pure $ PrimInline $ r |= if10 (x .>.   y)
  DoubleGeOp        -> \[r] [x,y] -> pure $ PrimInline $ r |= if10 (x .>=.  y)
  DoubleEqOp        -> \[r] [x,y] -> pure $ PrimInline $ r |= if10 (x .===. y)
  DoubleNeOp        -> \[r] [x,y] -> pure $ PrimInline $ r |= if10 (x .!==. y)
  DoubleLtOp        -> \[r] [x,y] -> pure $ PrimInline $ r |= if10 (x .<.   y)
  DoubleLeOp        -> \[r] [x,y] -> pure $ PrimInline $ r |= if10 (x .<=.  y)
  DoubleAddOp       -> \[r] [x,y] -> pure $ PrimInline $ r |= Add x y
  DoubleSubOp       -> \[r] [x,y] -> pure $ PrimInline $ r |= Sub x y
  DoubleMulOp       -> \[r] [x,y] -> pure $ PrimInline $ r |= Mul x y
  DoubleDivOp       -> \[r] [x,y] -> pure $ PrimInline $ r |= Div x y
  DoubleNegOp       -> \[r] [x]   -> pure $ PrimInline $ r |= Negate x
  DoubleFabsOp      -> \[r] [x]   -> pure $ PrimInline $ r |= math_abs [x]
  DoubleToIntOp     -> \[r] [x]   -> pure $ PrimInline $ r |= toI32 x
  DoubleToFloatOp   -> \[r] [x]   -> pure $ PrimInline $ r |= math_fround [x]
  DoubleExpOp       -> \[r] [x]   -> pure $ PrimInline $ r |= math_exp  [x]
  DoubleExpM1Op     -> \[r] [x]   -> pure $ PrimInline $ r |= math_expm1 [x]
  DoubleLogOp       -> \[r] [x]   -> pure $ PrimInline $ r |= math_log  [x]
  DoubleLog1POp     -> \[r] [x]   -> pure $ PrimInline $ r |= math_log1p [x]
  DoubleSqrtOp      -> \[r] [x]   -> pure $ PrimInline $ r |= math_sqrt [x]
  DoubleSinOp       -> \[r] [x]   -> pure $ PrimInline $ r |= math_sin  [x]
  DoubleCosOp       -> \[r] [x]   -> pure $ PrimInline $ r |= math_cos  [x]
  DoubleTanOp       -> \[r] [x]   -> pure $ PrimInline $ r |= math_tan  [x]
  DoubleAsinOp      -> \[r] [x]   -> pure $ PrimInline $ r |= math_asin [x]
  DoubleAcosOp      -> \[r] [x]   -> pure $ PrimInline $ r |= math_acos [x]
  DoubleAtanOp      -> \[r] [x]   -> pure $ PrimInline $ r |= math_atan [x]
  DoubleSinhOp      -> \[r] [x]   -> pure $ PrimInline $ r |= math_sinh [x]
  DoubleCoshOp      -> \[r] [x]   -> pure $ PrimInline $ r |= math_cosh [x]
  DoubleTanhOp      -> \[r] [x]   -> pure $ PrimInline $ r |= math_tanh [x]
  DoubleAsinhOp     -> \[r] [x]   -> pure $ PrimInline $ r |= math_asinh [x]
  DoubleAcoshOp     -> \[r] [x]   -> pure $ PrimInline $ r |= math_acosh [x]
  DoubleAtanhOp     -> \[r] [x]   -> pure $ PrimInline $ r |= math_atanh [x]
  DoublePowerOp     -> \[r] [x,y] -> pure $ PrimInline $ r |= math_pow [x,y]
  DoubleDecode_2IntOp  -> \[s,h,l,e] [x] -> pure $ PrimInline $ appT [s,h,l,e] hdDecodeDouble2IntStr   [x]
  DoubleDecode_Int64Op -> \[s1,s2,e] [d] -> pure $ PrimInline $ appT [e,s1,s2] hdDecodeDoubleInt64Str  [d]
  CastDoubleToWord64Op -> \[rh,rl] [x]   -> pure $ PrimInline $ appT [rh,rl]   hdCastDoubleToWord64Str [x]
  CastWord64ToDoubleOp -> \[r]     [h,l] -> pure $ PrimInline $ appT [r]       hdCastWord64ToDoubleStr [h,l]

  DoubleFMAdd  -> unhandledPrimop op
  DoubleFMSub  -> unhandledPrimop op
  DoubleFNMAdd -> unhandledPrimop op
  DoubleFNMSub -> unhandledPrimop op

------------------------------ Float --------------------------------------------

  FloatGtOp         -> \[r] [x,y] -> pure $ PrimInline $ r |= if10 (x .>.   y)
  FloatGeOp         -> \[r] [x,y] -> pure $ PrimInline $ r |= if10 (x .>=.  y)
  FloatEqOp         -> \[r] [x,y] -> pure $ PrimInline $ r |= if10 (x .===. y)
  FloatNeOp         -> \[r] [x,y] -> pure $ PrimInline $ r |= if10 (x .!==. y)
  FloatLtOp         -> \[r] [x,y] -> pure $ PrimInline $ r |= if10 (x .<.   y)
  FloatLeOp         -> \[r] [x,y] -> pure $ PrimInline $ r |= if10 (x .<=.  y)
  FloatAddOp        -> \[r] [x,y] -> pure $ PrimInline $ r |= math_fround [Add x y]
  FloatSubOp        -> \[r] [x,y] -> pure $ PrimInline $ r |= math_fround [Sub x y]
  FloatMulOp        -> \[r] [x,y] -> pure $ PrimInline $ r |= math_fround [Mul x y]
  FloatDivOp        -> \[r] [x,y] -> pure $ PrimInline $ r |= math_fround [Div x y]
  FloatNegOp        -> \[r] [x]   -> pure $ PrimInline $ r |= Negate x
  FloatFabsOp       -> \[r] [x]   -> pure $ PrimInline $ r |= math_abs [x]
  FloatToIntOp      -> \[r] [x]   -> pure $ PrimInline $ r |= toI32 x
  FloatExpOp        -> \[r] [x]   -> pure $ PrimInline $ r |= math_fround [math_exp [x]]
  FloatExpM1Op      -> \[r] [x]   -> pure $ PrimInline $ r |= math_fround [math_expm1 [x]]
  FloatLogOp        -> \[r] [x]   -> pure $ PrimInline $ r |= math_fround [math_log [x]]
  FloatLog1POp      -> \[r] [x]   -> pure $ PrimInline $ r |= math_fround [math_log1p [x]]
  FloatSqrtOp       -> \[r] [x]   -> pure $ PrimInline $ r |= math_fround [math_sqrt [x]]
  FloatSinOp        -> \[r] [x]   -> pure $ PrimInline $ r |= math_fround [math_sin [x]]
  FloatCosOp        -> \[r] [x]   -> pure $ PrimInline $ r |= math_fround [math_cos [x]]
  FloatTanOp        -> \[r] [x]   -> pure $ PrimInline $ r |= math_fround [math_tan [x]]
  FloatAsinOp       -> \[r] [x]   -> pure $ PrimInline $ r |= math_fround [math_asin [x]]
  FloatAcosOp       -> \[r] [x]   -> pure $ PrimInline $ r |= math_fround [math_acos [x]]
  FloatAtanOp       -> \[r] [x]   -> pure $ PrimInline $ r |= math_fround [math_atan [x]]
  FloatSinhOp       -> \[r] [x]   -> pure $ PrimInline $ r |= math_fround [math_sinh [x]]
  FloatCoshOp       -> \[r] [x]   -> pure $ PrimInline $ r |= math_fround [math_cosh [x]]
  FloatTanhOp       -> \[r] [x]   -> pure $ PrimInline $ r |= math_fround [math_tanh [x]]
  FloatAsinhOp      -> \[r] [x]   -> pure $ PrimInline $ r |= math_fround [math_asinh [x]]
  FloatAcoshOp      -> \[r] [x]   -> pure $ PrimInline $ r |= math_fround [math_acosh [x]]
  FloatAtanhOp      -> \[r] [x]   -> pure $ PrimInline $ r |= math_fround [math_atanh [x]]
  FloatPowerOp      -> \[r] [x,y] -> pure $ PrimInline $ r |= math_fround [math_pow [x,y]]
  FloatToDoubleOp   -> \[r] [x]   -> pure $ PrimInline $ r |= x
  FloatDecode_IntOp -> \[s,e] [x] -> pure $ PrimInline $ appT [s,e] hdDecodeFloatIntStr  [x]
  CastFloatToWord32Op -> \[r] [x] -> pure $ PrimInline $ appT [r] hdCastFloatToWord32Str [x]
  CastWord32ToFloatOp -> \[r] [x] -> pure $ PrimInline $ appT [r] hdCastWord32ToFloatStr [x]


  FloatFMAdd  -> unhandledPrimop op
  FloatFMSub  -> unhandledPrimop op
  FloatFNMAdd -> unhandledPrimop op
  FloatFNMSub -> unhandledPrimop op

------------------------------ Arrays -------------------------------------------

  NewArrayOp           -> \[r] [l,e]   -> pure $ PrimInline $ r |= app hdNewArrayStr [l,e]
  ReadArrayOp          -> \[r] [a,i]   -> pure $ PrimInline $ bnd_arr bound a i (r |= a .! i)
  WriteArrayOp         -> \[]  [a,i,v] -> pure $ PrimInline $ bnd_arr bound a i (a .! i |= v)
  SizeofArrayOp        -> \[r] [a]     -> pure $ PrimInline $ r |= a .^ lngth
  SizeofMutableArrayOp -> \[r] [a]     -> pure $ PrimInline $ r |= a .^ lngth
  IndexArrayOp         -> \[r] [a,i]   -> pure $ PrimInline $ bnd_arr bound a i (r |= a .! i)
  UnsafeFreezeArrayOp  -> \[r] [a]     -> pure $ PrimInline $ r |= a
  UnsafeThawArrayOp    -> \[r] [a]     -> pure $ PrimInline $ r |= a
  CopyArrayOp          -> \[] [a,o1,ma,o2,n] ->
    PrimInline <$>
    jVar \tmp ->
      pure $ bnd_arr_range bound a o1 n
      $ bnd_arr_range bound ma o2 n
      $ mconcat [ tmp |= Int 0
                , WhileStat False (tmp .<. n) $
                  mconcat [ ma .! (Add tmp o2) |= a .! (Add tmp o1)
                          , preIncrS tmp
                          ]

                ]
  CopyMutableArrayOp  -> \[]  [a1,o1,a2,o2,n] ->
    pure $ PrimInline
      $ bnd_arr_range bound a1 o1 n
      $ bnd_arr_range bound a2 o2 n
      $ appS hdCopyMutableArrayStr [a1,o1,a2,o2,n]

  CloneArrayOp        -> \[r] [a,start,n]     ->
    pure $ PrimInline
      $ bnd_arr_range bound a start n
      $ r |= app hdSliceArrayStr [a,start,n]

  CloneMutableArrayOp -> \[r] [a,start,n]     ->
    pure $ PrimInline
      $ bnd_arr_range bound a start n
      $ r |= app hdSliceArrayStr [a,start,n]

  FreezeArrayOp       -> \[r] [a,start,n]     ->
    pure $ PrimInline
      $ bnd_arr_range bound a start n
      $ r |= app hdSliceArrayStr [a,start,n]

  ThawArrayOp         -> \[r] [a,start,n]     ->
    pure $ PrimInline
      $ bnd_arr_range bound a start n
      $ r |= app hdSliceArrayStr [a,start,n]

  CasArrayOp          -> \[s,o] [a,i,old,new] ->
    PrimInline <$>
    jVar \tmp ->
      pure $ bnd_arr bound a i
      $ mconcat
          [ tmp |= a .! i
          , ifBlockS (tmp .===. old)
                     [ o |= new
                     , a .! i |= new
                     , s |= zero_
                     ]
                     [ s |= one_
                     , o |= tmp
                     ]
          ]

------------------------------ Small Arrays -------------------------------------

  NewSmallArrayOp            -> \[a]   [n,e]         -> pure $ PrimInline $ a |= app hdNewArrayStr [n,e]
  ReadSmallArrayOp           -> \[r]   [a,i]         -> pure $ PrimInline $ bnd_arr bound a i (r |= a .! i)
  WriteSmallArrayOp          -> \[]    [a,i,e]       -> pure $ PrimInline $ bnd_arr bound a i (a .! i |= e)
  SizeofSmallArrayOp         -> \[r]   [a]           -> pure $ PrimInline $ r |= a .^ lngth
  SizeofSmallMutableArrayOp  -> \[r]   [a]           -> pure $ PrimInline $ r |= a .^ lngth
  IndexSmallArrayOp          -> \[r]   [a,i]         -> pure $ PrimInline $ bnd_arr bound a i (r |= a .! i)
  UnsafeFreezeSmallArrayOp   -> \[r]   [a]           -> pure $ PrimInline $ r |= a
  UnsafeThawSmallArrayOp     -> \[r]   [a]           -> pure $ PrimInline $ r |= a
  CopySmallArrayOp           -> \[]    [s,si,d,di,n] ->
    PrimInline <$>
    jVar \tmp ->
      pure $ bnd_arr_range bound s si n
      $ bnd_arr_range bound d di n
      $ mconcat [ tmp |= Sub n one_
                , WhileStat False (tmp .>=. zero_)
                  $ mconcat [ d .! (Add di tmp) |= s .! (Add si tmp)
                          , postDecrS tmp
                          ]
                ]
  CopySmallMutableArrayOp    -> \[]    [s,si,d,di,n] ->
    pure $ PrimInline
      $ bnd_arr_range bound s si n
      $ bnd_arr_range bound d di n
      $ appS hdCopyMutableArrayStr [s,si,d,di,n]

  CloneSmallArrayOp          -> \[r]   [a,o,n]       -> pure $ PrimInline $ cloneArray bound r a o n
  CloneSmallMutableArrayOp   -> \[r]   [a,o,n]       -> pure $ PrimInline $ cloneArray bound r a o n
  FreezeSmallArrayOp         -> \[r]   [a,o,n]       -> pure $ PrimInline $ cloneArray bound r a o n
  ThawSmallArrayOp           -> \[r]   [a,o,n]       -> pure $ PrimInline $ cloneArray bound r a o n

  CasSmallArrayOp            -> \[s,o] [a,i,old,new] ->
    PrimInline <$>
    jVar \tmp ->
      pure $ bnd_arr bound a i
      $ mconcat
        [ tmp |= a .! i
        , ifBlockS (tmp .===. old)
            [ o |= new
            , a .! i |= new
            , s |= zero_
            ]
            [ s |= one_
            , o |= tmp
            ]
        ]

------------------------------- Byte Arrays -------------------------------------

  NewByteArrayOp_Char               -> \[r]   [l]        -> pure $ PrimInline (newByteArray r l)
  NewPinnedByteArrayOp_Char         -> \[r]   [l]        -> pure $ PrimInline (newByteArray r l)
  NewAlignedPinnedByteArrayOp_Char  -> \[r]   [l,_align] -> pure $ PrimInline (newByteArray r l)
  MutableByteArrayIsPinnedOp        -> \[r]   [_]        -> pure $ PrimInline $ r |= one_
  ByteArrayIsPinnedOp               -> \[r]   [_]        -> pure $ PrimInline $ r |= one_
  ByteArrayIsWeaklyPinnedOp         -> \[r]   [_]        -> pure $ PrimInline $ r |= one_
  MutableByteArrayIsWeaklyPinnedOp  -> \[r]   [_]        -> pure $ PrimInline $ r |= one_
  ByteArrayContents_Char            -> \[a,o] [b]        -> pure $ PrimInline $ mconcat [a |= b, o |= zero_]
  MutableByteArrayContents_Char     -> \[a,o] [b]        -> pure $ PrimInline $ mconcat [a |= b, o |= zero_]
  ShrinkMutableByteArrayOp_Char     -> \[]    [a,n]      -> pure $ PrimInline $ appS hdShrinkMutableByteArrayStr [a,n]
  ResizeMutableByteArrayOp_Char     -> \[r]   [a,n]      -> pure $ PrimInline $ r |= app hdResizeMutableByteArrayStr [a,n]
  UnsafeFreezeByteArrayOp           -> \[a]   [b]        -> pure $ PrimInline $ a |= b
  UnsafeThawByteArrayOp             -> \[a]   [b]        -> pure $ PrimInline $ a |= b
  SizeofByteArrayOp                 -> \[r]   [a]        -> pure $ PrimInline $ r |= a .^ len
  SizeofMutableByteArrayOp          -> \[r]   [a]        -> pure $ PrimInline $ r |= a .^ len
  GetSizeofMutableByteArrayOp       -> \[r]   [a]        -> pure $ PrimInline $ r |= a .^ len

  IndexByteArrayOp_Char      -> \[r]   [a,i] -> pure $ PrimInline $ bnd_ix8  bound a i $ r |= read_u8  a i
  IndexByteArrayOp_WideChar  -> \[r]   [a,i] -> pure $ PrimInline $ bnd_ix32 bound a i $ r |= read_i32 a i
  IndexByteArrayOp_Int       -> \[r]   [a,i] -> pure $ PrimInline $ bnd_ix32 bound a i $ r |= read_i32 a i
  IndexByteArrayOp_Word      -> \[r]   [a,i] -> pure $ PrimInline $ bnd_ix32 bound a i $ r |= read_u32 a i
  IndexByteArrayOp_Addr      -> \[r,o] [a,i] -> pure $ PrimInline $ bnd_ix32 bound a i $ read_addr a i r o
  IndexByteArrayOp_Float     -> \[r]   [a,i] -> pure $ PrimInline $ bnd_ix32 bound a i $ r |= read_f32 a i
  IndexByteArrayOp_Double    -> \[r]   [a,i] -> pure $ PrimInline $ bnd_ix64 bound a i $ r |= read_f64 a i
  IndexByteArrayOp_StablePtr -> \[r,o] [a,i] -> pure $ PrimInline $ bnd_ix32 bound a i $ read_stableptr a i r o
  IndexByteArrayOp_Int8      -> \[r]   [a,i] -> pure $ PrimInline $ bnd_ix8  bound a i $ r |= read_i8  a i
  IndexByteArrayOp_Int16     -> \[r]   [a,i] -> pure $ PrimInline $ bnd_ix16 bound a i $ r |= read_i16 a i
  IndexByteArrayOp_Int32     -> \[r]   [a,i] -> pure $ PrimInline $ bnd_ix32 bound a i $ r |= read_i32 a i
  IndexByteArrayOp_Int64     -> \[h,l] [a,i] -> pure $ PrimInline $ bnd_ix64 bound a i $ read_i64 a i h l
  IndexByteArrayOp_Word8     -> \[r]   [a,i] -> pure $ PrimInline $ bnd_ix8  bound a i $ r |= read_u8  a i
  IndexByteArrayOp_Word16    -> \[r]   [a,i] -> pure $ PrimInline $ bnd_ix16 bound a i $ r |= read_u16 a i
  IndexByteArrayOp_Word32    -> \[r]   [a,i] -> pure $ PrimInline $ bnd_ix32 bound a i $ r |= read_u32 a i
  IndexByteArrayOp_Word64    -> \[h,l] [a,i] -> pure $ PrimInline $ bnd_ix64 bound a i $ read_u64 a i h l

  ReadByteArrayOp_Char       -> \[r]   [a,i] -> pure $ PrimInline $ bnd_ix8 bound a i $ r |= read_u8  a i
  ReadByteArrayOp_WideChar   -> \[r]   [a,i] -> pure $ PrimInline $ bnd_ix32 bound a i $ r |= read_i32 a i
  ReadByteArrayOp_Int        -> \[r]   [a,i] -> pure $ PrimInline $ bnd_ix32 bound a i $ r |= read_i32 a i
  ReadByteArrayOp_Word       -> \[r]   [a,i] -> pure $ PrimInline $ bnd_ix32 bound a i $ r |= read_u32 a i
  ReadByteArrayOp_Addr       -> \[r,o] [a,i] -> pure $ PrimInline $ bnd_ix32 bound a i $ read_addr a i r o
  ReadByteArrayOp_Float      -> \[r]   [a,i] -> pure $ PrimInline $ bnd_ix32 bound a i $ r |= read_f32 a i
  ReadByteArrayOp_Double     -> \[r]   [a,i] -> pure $ PrimInline $ bnd_ix64 bound a i $ r |= read_f64 a i
  ReadByteArrayOp_StablePtr  -> \[r,o] [a,i] -> pure $ PrimInline $ bnd_ix32 bound a i $ read_stableptr a i r o
  ReadByteArrayOp_Int8       -> \[r]   [a,i] -> pure $ PrimInline $ bnd_ix8  bound a i $ r |= read_i8  a i
  ReadByteArrayOp_Int16      -> \[r]   [a,i] -> pure $ PrimInline $ bnd_ix16 bound a i $ r |= read_i16 a i
  ReadByteArrayOp_Int32      -> \[r]   [a,i] -> pure $ PrimInline $ bnd_ix32 bound a i $ r |= read_i32 a i
  ReadByteArrayOp_Int64      -> \[h,l] [a,i] -> pure $ PrimInline $ bnd_ix64 bound a i $ read_i64 a i h l
  ReadByteArrayOp_Word8      -> \[r]   [a,i] -> pure $ PrimInline $ bnd_ix8  bound a i $ r |= read_u8  a i
  ReadByteArrayOp_Word16     -> \[r]   [a,i] -> pure $ PrimInline $ bnd_ix16 bound a i $ r |= read_u16 a i
  ReadByteArrayOp_Word32     -> \[r]   [a,i] -> pure $ PrimInline $ bnd_ix32 bound a i $ r |= read_u32 a i
  ReadByteArrayOp_Word64     -> \[h,l] [a,i] -> pure $ PrimInline $ bnd_ix64 bound a i $ read_u64 a i h l

  WriteByteArrayOp_Char      -> \[] [a,i,e]   -> pure $ PrimInline $ bnd_ix8  bound a i $ write_u8  a i e
  WriteByteArrayOp_WideChar  -> \[] [a,i,e]   -> pure $ PrimInline $ bnd_ix32 bound a i $ write_i32 a i e
  WriteByteArrayOp_Int       -> \[] [a,i,e]   -> pure $ PrimInline $ bnd_ix32 bound a i $ write_i32 a i e
  WriteByteArrayOp_Word      -> \[] [a,i,e]   -> pure $ PrimInline $ bnd_ix32 bound a i $ write_u32 a i e
  WriteByteArrayOp_Addr      -> \[] [a,i,r,o] -> pure $ PrimInline $ bnd_ix32 bound a i $ write_addr a i r o
  WriteByteArrayOp_Float     -> \[] [a,i,e]   -> pure $ PrimInline $ bnd_ix32 bound a i $ write_f32 a i e
  WriteByteArrayOp_Double    -> \[] [a,i,e]   -> pure $ PrimInline $ bnd_ix64 bound a i $ write_f64 a i e
  WriteByteArrayOp_StablePtr -> \[] [a,i,r,o] -> pure $ PrimInline $ bnd_ix32 bound a i $ write_stableptr a i r o
  WriteByteArrayOp_Int8      -> \[] [a,i,e]   -> pure $ PrimInline $ bnd_ix8  bound a i $ write_i8  a i e
  WriteByteArrayOp_Int16     -> \[] [a,i,e]   -> pure $ PrimInline $ bnd_ix16 bound a i $ write_i16 a i e
  WriteByteArrayOp_Int32     -> \[] [a,i,e]   -> pure $ PrimInline $ bnd_ix32 bound a i $ write_i32 a i e
  WriteByteArrayOp_Int64     -> \[] [a,i,h,l] -> pure $ PrimInline $ bnd_ix64 bound a i $ write_i64 a i h l
  WriteByteArrayOp_Word8     -> \[] [a,i,e]   -> pure $ PrimInline $ bnd_ix8  bound a i $ write_u8  a i e
  WriteByteArrayOp_Word16    -> \[] [a,i,e]   -> pure $ PrimInline $ bnd_ix16 bound a i $ write_u16 a i e
  WriteByteArrayOp_Word32    -> \[] [a,i,e]   -> pure $ PrimInline $ bnd_ix32 bound a i $ write_u32 a i e
  WriteByteArrayOp_Word64    -> \[] [a,i,h,l] -> pure $ PrimInline $ bnd_ix64 bound a i $ write_u64 a i h l

  CompareByteArraysOp -> \[r] [a1,o1,a2,o2,n] ->
      pure . PrimInline
      . bnd_ba_range bound a1 o1 n
      . bnd_ba_range bound a2 o2 n
      $ r |= app hdCompareByteArraysStr [a1,o1,a2,o2,n]

  -- We assume the arrays aren't overlapping since they're of different types
  -- (ByteArray vs MutableByteArray, Addr# vs MutableByteArray#, [Mutable]ByteArray# vs Addr#)
  CopyByteArrayOp                      -> \[] [a1,o1,a2,o2,n] -> pure $ copyByteArray False bound a1 o1 a2 o2 n
  CopyAddrToByteArrayOp                -> \[] [a1,o1,a2,o2,n] -> pure $ copyByteArray False bound a1 o1 a2 o2 n
  CopyMutableByteArrayToAddrOp         -> \[] [a1,o1,a2,o2,n] -> pure $ copyByteArray False bound a1 o1 a2 o2 n
  CopyMutableByteArrayNonOverlappingOp -> \[] [a1,o1,a2,o2,n] -> pure $ copyByteArray False bound a1 o1 a2 o2 n
  CopyAddrToAddrNonOverlappingOp       -> \[] [a1,o1,a2,o2,n] -> pure $ copyByteArray False bound a1 o1 a2 o2 n
  CopyByteArrayToAddrOp                -> \[] [a1,o1,a2,o2,n] -> pure $ copyByteArray False bound a1 o1 a2 o2 n

  CopyMutableByteArrayOp               -> \[] [a1,o1,a2,o2,n] -> pure $ copyByteArray True  bound a1 o1 a2 o2 n
  CopyAddrToAddrOp                     -> \[] [a1,o1,a2,o2,n] -> pure $ copyByteArray True  bound a1 o1 a2 o2 n

  SetByteArrayOp -> \[] [a,o,n,v] ->
    PrimInline <$> jVar \tmpIdent ->
    do
      let tmp = global $ identFS tmpIdent
      pure . bnd_ba_range bound a o n $
        mconcat [ tmpIdent ||= zero_
                , WhileStat False (tmp .<. n) $
                  mconcat [ write_u8 a (Add o tmp) v
                          , postIncrS tmp
                          ]
                ]
  SetAddrRangeOp -> \[] xs@[_a,_o,_n,_v] -> genPrim prof bound ty SetByteArrayOp [] xs

  AtomicReadByteArrayOp_Int  -> \[r]   [a,i]   -> pure $ PrimInline $ bnd_ix32 bound a i $ r |= read_i32 a i
  AtomicWriteByteArrayOp_Int -> \[]    [a,i,v] -> pure $ PrimInline $ bnd_ix32 bound a i $ write_i32 a i v
  FetchAddByteArrayOp_Int    -> \[r]   [a,i,v] -> pure $ PrimInline $ bnd_ix32 bound a i $ fetchOpByteArray Add  r a i v
  FetchSubByteArrayOp_Int    -> \[r]   [a,i,v] -> pure $ PrimInline $ bnd_ix32 bound a i $ fetchOpByteArray Sub  r a i v
  FetchAndByteArrayOp_Int    -> \[r]   [a,i,v] -> pure $ PrimInline $ bnd_ix32 bound a i $ fetchOpByteArray BAnd r a i v
  FetchOrByteArrayOp_Int     -> \[r]   [a,i,v] -> pure $ PrimInline $ bnd_ix32 bound a i $ fetchOpByteArray BOr  r a i v
  FetchNandByteArrayOp_Int   -> \[r]   [a,i,v] -> pure $ PrimInline $ bnd_ix32 bound a i $ fetchOpByteArray (\x y -> BNot (BAnd x y)) r a i v
  FetchXorByteArrayOp_Int    -> \[r]   [a,i,v] -> pure $ PrimInline $ bnd_ix32 bound a i $ fetchOpByteArray BXor r a i v

------------------------------- Addr# ------------------------------------------

  AddrAddOp   -> \[a',o'] [a,o,i]         -> pure $ PrimInline $ mconcat [a' |= a, o' |= Add o i]
  AddrSubOp   -> \[i]     [_a1,o1,_a2,o2] -> pure $ PrimInline $ i |= Sub o1 o2
  AddrRemOp   -> \[r]     [_a,o,i]        -> pure $ PrimInline $ r |= Mod o i
  AddrToIntOp -> \[i]     [_a,o]          -> pure $ PrimInline $ i |= o -- only usable for comparisons within one range
  IntToAddrOp -> \[a,o]   [i]             -> pure $ PrimInline $ mconcat [a |= null_, o |= i]
  AddrGtOp -> \[r] [a1,o1,a2,o2] -> pure $ PrimInline $ r |= if10 (app hdComparePointerStr [a1,o1,a2,o2] .>. zero_)
  AddrGeOp -> \[r] [a1,o1,a2,o2] -> pure $ PrimInline $ r |= if10 (app hdComparePointerStr [a1,o1,a2,o2] .>=. zero_)
  AddrEqOp -> \[r] [a1,o1,a2,o2] -> pure $ PrimInline $ r |= if10 (app hdComparePointerStr [a1,o1,a2,o2] .===. zero_)
  AddrNeOp -> \[r] [a1,o1,a2,o2] -> pure $ PrimInline $ r |= if10 (app hdComparePointerStr [a1,o1,a2,o2] .!==. zero_)
  AddrLtOp -> \[r] [a1,o1,a2,o2] -> pure $ PrimInline $ r |= if10 (app hdComparePointerStr [a1,o1,a2,o2] .<. zero_)
  AddrLeOp -> \[r] [a1,o1,a2,o2] -> pure $ PrimInline $ r |= if10 (app hdComparePointerStr [a1,o1,a2,o2] .<=. zero_)

------------------------------- Addr Indexing: Unboxed Arrays -------------------

  IndexOffAddrOp_Char      -> \[r]     [a,o,i] -> pure $ PrimInline $ r |= read_boff_u8  a (off8  o i)
  IndexOffAddrOp_WideChar  -> \[r]     [a,o,i] -> pure $ PrimInline $ r |= read_boff_i32 a (off32 o i)
  IndexOffAddrOp_Int       -> \[r]     [a,o,i] -> pure $ PrimInline $ r |= read_boff_i32 a (off32 o i)
  IndexOffAddrOp_Word      -> \[r]     [a,o,i] -> pure $ PrimInline $ r |= read_boff_u32 a (off32 o i)
  IndexOffAddrOp_Addr      -> \[ra,ro] [a,o,i] -> pure $ PrimInline $ read_boff_addr a (off32 o i) ra ro
  IndexOffAddrOp_Float     -> \[r]     [a,o,i] -> pure $ PrimInline $ r |= read_boff_f32 a (off32 o i)
  IndexOffAddrOp_Double    -> \[r]     [a,o,i] -> pure $ PrimInline $ r |= read_boff_f64 a (off64 o i)
  IndexOffAddrOp_StablePtr -> \[ra,ro] [a,o,i] -> pure $ PrimInline $ read_boff_stableptr a (off32 o i) ra ro
  IndexOffAddrOp_Int8      -> \[r]     [a,o,i] -> pure $ PrimInline $ r |= read_boff_i8  a (off8  o i)
  IndexOffAddrOp_Int16     -> \[r]     [a,o,i] -> pure $ PrimInline $ r |= read_boff_i16 a (off16 o i)
  IndexOffAddrOp_Int32     -> \[r]     [a,o,i] -> pure $ PrimInline $ r |= read_boff_i32 a (off32 o i)
  IndexOffAddrOp_Int64     -> \[h,l]   [a,o,i] -> pure $ PrimInline $ read_boff_i64 a (off64 o i) h l
  IndexOffAddrOp_Word8     -> \[r]     [a,o,i] -> pure $ PrimInline $ r |= read_boff_u8  a (off8  o i)
  IndexOffAddrOp_Word16    -> \[r]     [a,o,i] -> pure $ PrimInline $ r |= read_boff_u16 a (off16 o i)
  IndexOffAddrOp_Word32    -> \[r]     [a,o,i] -> pure $ PrimInline $ r |= read_boff_u32 a (off32 o i)
  IndexOffAddrOp_Word64    -> \[h,l]   [a,o,i] -> pure $ PrimInline $ read_boff_u64 a (off64 o i) h l

  ReadOffAddrOp_Char       -> \[r]     [a,o,i] -> pure $ PrimInline $ r |= read_boff_u8  a (off8  o i)
  ReadOffAddrOp_WideChar   -> \[r]     [a,o,i] -> pure $ PrimInline $ r |= read_boff_i32 a (off32 o i)
  ReadOffAddrOp_Int        -> \[r]     [a,o,i] -> pure $ PrimInline $ r |= read_boff_i32 a (off32 o i)
  ReadOffAddrOp_Word       -> \[r]     [a,o,i] -> pure $ PrimInline $ r |= read_boff_u32 a (off32 o i)
  ReadOffAddrOp_Addr       -> \[ra,ro] [a,o,i] -> pure $ PrimInline $ read_boff_addr a (off32 o i) ra ro
  ReadOffAddrOp_Float      -> \[r]     [a,o,i] -> pure $ PrimInline $ r |= read_boff_f32 a (off32 o i)
  ReadOffAddrOp_Double     -> \[r]     [a,o,i] -> pure $ PrimInline $ r |= read_boff_f64 a (off64 o i)
  ReadOffAddrOp_StablePtr  -> \[ra,ro] [a,o,i] -> pure $ PrimInline $ read_boff_stableptr a (off32 o i) ra ro
  ReadOffAddrOp_Int8       -> \[r]     [a,o,i] -> pure $ PrimInline $ r |= read_boff_i8  a (off8  o i)
  ReadOffAddrOp_Int16      -> \[r]     [a,o,i] -> pure $ PrimInline $ r |= read_boff_i16 a (off16 o i)
  ReadOffAddrOp_Int32      -> \[r]     [a,o,i] -> pure $ PrimInline $ r |= read_boff_i32 a (off32 o i)
  ReadOffAddrOp_Int64      -> \[h,l]   [a,o,i] -> pure $ PrimInline $ read_boff_i64 a (off64 o i) h l
  ReadOffAddrOp_Word8      -> \[r]     [a,o,i] -> pure $ PrimInline $ r |= read_boff_u8  a (off8  o i)
  ReadOffAddrOp_Word16     -> \[r]     [a,o,i] -> pure $ PrimInline $ r |= read_boff_u16 a (off16 o i)
  ReadOffAddrOp_Word32     -> \[r]     [a,o,i] -> pure $ PrimInline $ r |= read_boff_u32 a (off32 o i)
  ReadOffAddrOp_Word64     -> \[h,l]   [a,o,i] -> pure $ PrimInline $ read_boff_u64 a (off64 o i) h l

  WriteOffAddrOp_Char      -> \[] [a,o,i,v]     -> pure $ PrimInline $ write_boff_u8  a (off8  o i) v
  WriteOffAddrOp_WideChar  -> \[] [a,o,i,v]     -> pure $ PrimInline $ write_boff_i32 a (off32 o i) v
  WriteOffAddrOp_Int       -> \[] [a,o,i,v]     -> pure $ PrimInline $ write_boff_i32 a (off32 o i) v
  WriteOffAddrOp_Word      -> \[] [a,o,i,v]     -> pure $ PrimInline $ write_boff_u32 a (off32 o i) v
  WriteOffAddrOp_Addr      -> \[] [a,o,i,va,vo] -> pure $ PrimInline $ write_boff_addr a (off32 o i) va vo
  WriteOffAddrOp_Float     -> \[] [a,o,i,v]     -> pure $ PrimInline $ write_boff_f32 a (off32 o i) v
  WriteOffAddrOp_Double    -> \[] [a,o,i,v]     -> pure $ PrimInline $ write_boff_f64 a (off64 o i) v
  WriteOffAddrOp_StablePtr -> \[] [a,o,i,va,vo] -> pure $ PrimInline $ write_boff_stableptr a (off32 o i) va vo
  WriteOffAddrOp_Int8      -> \[] [a,o,i,v]     -> pure $ PrimInline $ write_boff_i8  a (off8  o i) v
  WriteOffAddrOp_Int16     -> \[] [a,o,i,v]     -> pure $ PrimInline $ write_boff_i16 a (off16 o i) v
  WriteOffAddrOp_Int32     -> \[] [a,o,i,v]     -> pure $ PrimInline $ write_boff_i32 a (off32 o i) v
  WriteOffAddrOp_Int64     -> \[] [a,o,i,h,l]   -> pure $ PrimInline $ write_boff_i64 a (off64 o i) h l
  WriteOffAddrOp_Word8     -> \[] [a,o,i,v]     -> pure $ PrimInline $ write_boff_u8  a (off8  o i) v
  WriteOffAddrOp_Word16    -> \[] [a,o,i,v]     -> pure $ PrimInline $ write_boff_u16 a (off16 o i) v
  WriteOffAddrOp_Word32    -> \[] [a,o,i,v]     -> pure $ PrimInline $ write_boff_u32 a (off32 o i) v
  WriteOffAddrOp_Word64    -> \[] [a,o,i,h,l]   -> pure $ PrimInline $ write_boff_u64 a (off64 o i) h l

  IndexOffAddrOp_Word8AsChar      -> \[r]     [a,o,i] -> pure $ PrimInline $ r |= read_boff_u8  a (off8  o i)
  IndexOffAddrOp_Word8AsWideChar  -> \[r]     [a,o,i] -> pure $ PrimInline $ r |= read_boff_i32 a (off8 o i)
  IndexOffAddrOp_Word8AsInt       -> \[r]     [a,o,i] -> pure $ PrimInline $ r |= read_boff_i32 a (off8 o i)
  IndexOffAddrOp_Word8AsWord      -> \[r]     [a,o,i] -> pure $ PrimInline $ r |= read_boff_u32 a (off8 o i)
  IndexOffAddrOp_Word8AsAddr      -> \[ra,ro] [a,o,i] -> pure $ PrimInline $ read_boff_addr a (off8 o i) ra ro
  IndexOffAddrOp_Word8AsFloat     -> \[r]     [a,o,i] -> pure $ PrimInline $ r |= read_boff_f32 a (off8 o i)
  IndexOffAddrOp_Word8AsDouble    -> \[r]     [a,o,i] -> pure $ PrimInline $ r |= read_boff_f64 a (off8 o i)
  IndexOffAddrOp_Word8AsStablePtr -> \[ra,ro] [a,o,i] -> pure $ PrimInline $ read_boff_stableptr a (off8 o i) ra ro
  IndexOffAddrOp_Word8AsInt16     -> \[r]     [a,o,i] -> pure $ PrimInline $ r |= read_boff_i16 a (off8 o i)
  IndexOffAddrOp_Word8AsInt32     -> \[r]     [a,o,i] -> pure $ PrimInline $ r |= read_boff_i32 a (off8 o i)
  IndexOffAddrOp_Word8AsInt64     -> \[h,l]   [a,o,i] -> pure $ PrimInline $ read_boff_i64 a (off8 o i) h l
  IndexOffAddrOp_Word8AsWord16    -> \[r]     [a,o,i] -> pure $ PrimInline $ r |= read_boff_u16 a (off8 o i)
  IndexOffAddrOp_Word8AsWord32    -> \[r]     [a,o,i] -> pure $ PrimInline $ r |= read_boff_u32 a (off8 o i)
  IndexOffAddrOp_Word8AsWord64    -> \[h,l]   [a,o,i] -> pure $ PrimInline $ read_boff_u64 a (off8 o i) h l

  ReadOffAddrOp_Word8AsChar       -> \[r]     [a,o,i] -> pure $ PrimInline $ r |= read_boff_u8  a (off8  o i)
  ReadOffAddrOp_Word8AsWideChar   -> \[r]     [a,o,i] -> pure $ PrimInline $ r |= read_boff_i32 a (off8 o i)
  ReadOffAddrOp_Word8AsInt        -> \[r]     [a,o,i] -> pure $ PrimInline $ r |= read_boff_i32 a (off8 o i)
  ReadOffAddrOp_Word8AsWord       -> \[r]     [a,o,i] -> pure $ PrimInline $ r |= read_boff_u32 a (off8 o i)
  ReadOffAddrOp_Word8AsAddr       -> \[ra,ro] [a,o,i] -> pure $ PrimInline $ read_boff_addr a (off8 o i) ra ro
  ReadOffAddrOp_Word8AsFloat      -> \[r]     [a,o,i] -> pure $ PrimInline $ r |= read_boff_f32 a (off8 o i)
  ReadOffAddrOp_Word8AsDouble     -> \[r]     [a,o,i] -> pure $ PrimInline $ r |= read_boff_f64 a (off8 o i)
  ReadOffAddrOp_Word8AsStablePtr  -> \[ra,ro] [a,o,i] -> pure $ PrimInline $ read_boff_stableptr a (off8 o i) ra ro
  ReadOffAddrOp_Word8AsInt16      -> \[r]     [a,o,i] -> pure $ PrimInline $ r |= read_boff_i16 a (off8 o i)
  ReadOffAddrOp_Word8AsInt32      -> \[r]     [a,o,i] -> pure $ PrimInline $ r |= read_boff_i32 a (off8 o i)
  ReadOffAddrOp_Word8AsInt64      -> \[h,l]   [a,o,i] -> pure $ PrimInline $ read_boff_i64 a (off8 o i) h l
  ReadOffAddrOp_Word8AsWord16     -> \[r]     [a,o,i] -> pure $ PrimInline $ r |= read_boff_u16 a (off8 o i)
  ReadOffAddrOp_Word8AsWord32     -> \[r]     [a,o,i] -> pure $ PrimInline $ r |= read_boff_u32 a (off8 o i)
  ReadOffAddrOp_Word8AsWord64     -> \[h,l]   [a,o,i] -> pure $ PrimInline $ read_boff_u64 a (off8 o i) h l

  WriteOffAddrOp_Word8AsChar      -> \[] [a,o,i,v]     -> pure $ PrimInline $ write_boff_u8  a (off8  o i) v
  WriteOffAddrOp_Word8AsWideChar  -> \[] [a,o,i,v]     -> pure $ PrimInline $ write_boff_i32 a (off8 o i) v
  WriteOffAddrOp_Word8AsInt       -> \[] [a,o,i,v]     -> pure $ PrimInline $ write_boff_i32 a (off8 o i) v
  WriteOffAddrOp_Word8AsWord      -> \[] [a,o,i,v]     -> pure $ PrimInline $ write_boff_u32 a (off8 o i) v
  WriteOffAddrOp_Word8AsAddr      -> \[] [a,o,i,va,vo] -> pure $ PrimInline $ write_boff_addr a (off8 o i) va vo
  WriteOffAddrOp_Word8AsFloat     -> \[] [a,o,i,v]     -> pure $ PrimInline $ write_boff_f32 a (off8 o i) v
  WriteOffAddrOp_Word8AsDouble    -> \[] [a,o,i,v]     -> pure $ PrimInline $ write_boff_f64 a (off8 o i) v
  WriteOffAddrOp_Word8AsStablePtr -> \[] [a,o,i,va,vo] -> pure $ PrimInline $ write_boff_stableptr a (off8 o i) va vo
  WriteOffAddrOp_Word8AsInt16     -> \[] [a,o,i,v]     -> pure $ PrimInline $ write_boff_i16 a (off8 o i) v
  WriteOffAddrOp_Word8AsInt32     -> \[] [a,o,i,v]     -> pure $ PrimInline $ write_boff_i32 a (off8 o i) v
  WriteOffAddrOp_Word8AsInt64     -> \[] [a,o,i,h,l]   -> pure $ PrimInline $ write_boff_i64 a (off8 o i) h l
  WriteOffAddrOp_Word8AsWord16    -> \[] [a,o,i,v]     -> pure $ PrimInline $ write_boff_u16 a (off8 o i) v
  WriteOffAddrOp_Word8AsWord32    -> \[] [a,o,i,v]     -> pure $ PrimInline $ write_boff_u32 a (off8 o i) v
  WriteOffAddrOp_Word8AsWord64    -> \[] [a,o,i,h,l]   -> pure $ PrimInline $ write_boff_u64 a (off8 o i) h l

------------------------------- Mutable variables --------------------------------------
  NewMutVarOp           -> \[r] [x]       -> pure $ PrimInline $ r |= New (app hdMutVarStr [x])
  ReadMutVarOp          -> \[r] [m]       -> pure $ PrimInline $ r |= m .^ val
  WriteMutVarOp         -> \[] [m,x]      -> pure $ PrimInline $ m .^ val |= x
  AtomicModifyMutVar2Op -> \[r1,r2] [m,f] -> pure $ PrimInline $ appT [r1,r2] hdAtomicModifyMutVar2Str [m,f]
  AtomicModifyMutVar_Op -> \[r1,r2] [m,f] -> pure $ PrimInline $ appT [r1,r2] hdAtomicModifyMutVarStr [m,f]

  AtomicSwapMutVarOp    -> \[r] [mv,v] -> pure $ PrimInline $ mconcat
                                          [ r |= mv .^ val, mv .^ val |= v ]
  CasMutVarOp -> \[status,r] [mv,o,n] -> pure $ PrimInline $ ifS (mv .^ val .===. o)
                   (mconcat [status |= zero_, r |= n, mv .^ val |= n])
                   (mconcat [status |= one_ , r |= mv .^ val])

------------------------------- Exceptions --------------------------------------

  CatchOp -> \[_r] [a,handler] -> pure $ PRPrimCall $ returnS (app hdCatchStr [a, handler])

                             -- fully ignore the result arity as it can use 1 or 2
                             -- slots, depending on the return type.
  RaiseOp                 -> \_r [a] -> pure $ PRPrimCall $ returnS (app hdThrowStr [a, false_])
  RaiseIOOp               -> \_r [a] -> pure $ PRPrimCall $ returnS (app hdThrowStr [a, false_])
  RaiseUnderflowOp        -> \_r []  -> pure $ PRPrimCall $ returnS (app hdThrowStr [hdInternalExceptionTypeUnderflow, false_])
  RaiseOverflowOp         -> \_r []  -> pure $ PRPrimCall $ returnS (app hdThrowStr [hdInternalExceptionTypeOverflow, false_])
  RaiseDivZeroOp          -> \_r []  -> pure $ PRPrimCall $ returnS (app hdThrowStr [hdInternalExceptionTypeDivZero, false_])
  MaskAsyncExceptionsOp   -> \_r [a] -> pure $ PRPrimCall $ returnS (app hdMaskAsyncStr [a])
  MaskUninterruptibleOp   -> \_r [a] -> pure $ PRPrimCall $ returnS (app hdMaskUnintAsyncStr [a])
  UnmaskAsyncExceptionsOp -> \_r [a] -> pure $ PRPrimCall $ returnS (app hdUnmaskAsyncStr [a])

  MaskStatus -> \[r] [] -> pure $ PrimInline $ r |= app "h$maskStatus" []

------------------------------- STM-accessible Mutable Variables  --------------

  AtomicallyOp -> \[_r] [a]   -> pure $ PRPrimCall $ returnS (app hdAtomicallyStr [a])
  RetryOp      -> \_r   []    -> pure $ PRPrimCall $ returnS (app hdStmRetryStr [])
  CatchRetryOp -> \[_r] [a,b] -> pure $ PRPrimCall $ returnS (app hdStmCatchRetryStr [a,b])
  CatchSTMOp   -> \[_r] [a,h] -> pure $ PRPrimCall $ returnS (app hdCatchStmStr [a,h])
  NewTVarOp    -> \[tv] [v]   -> pure $ PrimInline $ tv |= app hdNewTVar    [v]
  ReadTVarOp   -> \[r] [tv]   -> pure $ PrimInline $ r  |= app hdReadTVar   [tv]
  ReadTVarIOOp -> \[r] [tv]   -> pure $ PrimInline $ r  |= app hdReadTVarIO [tv]
  WriteTVarOp  -> \[] [tv,v]  -> pure $ PrimInline $ appS hdWriteTVar [tv,v]

------------------------------- Synchronized Mutable Variables ------------------

  NewMVarOp     -> \[r]   []    -> pure $ PrimInline $ r |= New (app hdMVarStr [])
  TakeMVarOp    -> \[_r]  [m]   -> pure $ PRPrimCall $ returnS (app hdTakeMVarStr [m])
  TryTakeMVarOp -> \[r,v] [m]   -> pure $ PrimInline $ appT [r,v] hdTryTakeMVarStr [m]
  PutMVarOp     -> \[]    [m,v] -> pure $ PRPrimCall $ returnS (app hdPutMVarStr [m,v])
  TryPutMVarOp  -> \[r]   [m,v] -> pure $ PrimInline $ r |= app hdTryPutMVarStr [m,v]
  ReadMVarOp    -> \[_r]  [m]   -> pure $ PRPrimCall $ returnS (app hdReadMVarStr [m])
  TryReadMVarOp -> \[r,v] [m]   -> pure $ PrimInline $ mconcat
                                   [ v |= m .^ val
                                   , r |= if01 (v .===. null_)
                                   ]
  IsEmptyMVarOp -> \[r]   [m]   -> pure $ PrimInline $ r |= if10 (m .^ val .===. null_)

------------------------------- Delay/Wait Ops ---------------------------------

  DelayOp     -> \[] [t]  -> pure $ PRPrimCall $ returnS (app hdDelayThread [t])
  WaitReadOp  -> \[] [fd] -> pure $ PRPrimCall $ returnS (app hdWaitRead    [fd])
  WaitWriteOp -> \[] [fd] -> pure $ PRPrimCall $ returnS (app hdWaitWrite   [fd])

------------------------------- Concurrency Primitives -------------------------

  ForkOp                 -> \[_tid] [x]    -> pure $ PRPrimCall $ returnS (app hdFork [x, true_])
  ForkOnOp               -> \[_tid] [_p,x] -> pure $ PRPrimCall $ returnS (app hdFork [x, true_]) -- ignore processor argument
  KillThreadOp           -> \[] [tid,ex]   -> pure $ PRPrimCall $ returnS (app hdKillThread [tid,ex])
  YieldOp                -> \[] []         -> pure $ PRPrimCall $ returnS (app hdYield [])
  MyThreadIdOp           -> \[r] []        -> pure $ PrimInline $ r |= hdCurrentThread
  IsCurrentThreadBoundOp -> \[r] []        -> pure $ PrimInline $ r |= one_
  NoDuplicateOp          -> \[] []         -> pure $ PrimInline mempty -- don't need to do anything as long as we have eager blackholing
  ThreadStatusOp         -> \[stat,cap,locked] [tid] -> pure $ PrimInline $ appT [stat, cap, locked] hdThreadStatus [tid]
  ListThreadsOp          -> \[r] [] -> pure $ PrimInline $ appT [r] hdListThreads []
  GetThreadLabelOp       -> \[r1, r2] [t]  -> pure $ PrimInline $ appT [r1, r2] hdGetThreadLabel [t]
  LabelThreadOp          -> \[] [t,l]      -> pure $ PrimInline $ t .^ label |= l

------------------------------- Weak Pointers -----------------------------------

  MkWeakOp              -> \[r] [o,b,c] -> pure $ PrimInline $ r |= app hdMakeWeak [o,b,c]
  MkWeakNoFinalizerOp   -> \[r] [o,b]   -> pure $ PrimInline $ r |= app hdMakeWeakNoFinalizer [o,b]
  AddCFinalizerToWeakOp -> \[r] [_a1,_a1o,_a2,_a2o,_i,_a3,_a3o,_w] -> pure $ PrimInline $ r |= one_
  DeRefWeakOp           -> \[f,v] [w] -> pure $ PrimInline $ mconcat
                                         [ v |= w .^ val
                                         , f |= if01 (v .===. null_)
                                         ]
  FinalizeWeakOp     -> \[fl,fin] [w] -> pure $ PrimInline $ appT [fin, fl] hdFinalizeWeak [w]
  TouchOp            -> \[] [_e]      -> pure $ PrimInline mempty
  KeepAliveOp        -> \[_r] [x, f]  -> pure $ PRPrimCall $ ReturnStat (app hdKeepAlive [x, f])


------------------------------ Stable pointers and names ------------------------

  MakeStablePtrOp -> \[s1,s2] [a] -> pure $ PrimInline $ mconcat
      [ s1 |= hdStablePtrBuf
      , s2 |= app hdMakeStablePtrStr [a]
      ]
  DeRefStablePtrOp -> \[r] [_s1,s2]            -> pure $ PrimInline $ r |= app hdDeRefStablePtr [s2]
  EqStablePtrOp    -> \[r] [_sa1,sa2,_sb1,sb2] -> pure $ PrimInline $ r |= if10 (sa2 .===. sb2)

  MakeStableNameOp  -> \[r] [a] -> pure $ PrimInline $ r |= app hdMakeStableName [a]
  StableNameToIntOp -> \[r] [s] -> pure $ PrimInline $ r |= app hdStableNameInt  [s]

------------------------------ Compact normal form -----------------------------

  CompactNewOp           -> \[c] [s]   -> pure $ PrimInline $ c |= app hdCompactNew [s]
  CompactResizeOp        -> \[]  [c,s] -> pure $ PrimInline $ appS hdCompactResize [c,s]
  CompactContainsOp      -> \[r] [c,v] -> pure $ PrimInline $ r |= app hdCompactContains [c,v]
  CompactContainsAnyOp   -> \[r] [v]   -> pure $ PrimInline $ r |= app hdCompactContainsAny [v]
  CompactGetFirstBlockOp -> \[ra,ro,s] [c] ->
    pure $ PrimInline $ appT [ra,ro,s] hdCompactGetFirstBlock [c]
  CompactGetNextBlockOp -> \[ra,ro,s] [c,a,o] ->
    pure $ PrimInline $ appT [ra,ro,s] hdCompactGetNextBlock [c,a,o]
  CompactAllocateBlockOp -> \[ra,ro] [size,sa,so] ->
    pure $ PrimInline $ appT [ra,ro] hdCompactAllocateBlock [size,sa,so]
  CompactFixupPointersOp -> \[c,newroota, newrooto] [blocka,blocko,roota,rooto] ->
    pure $ PrimInline $ appT [c,newroota,newrooto] hdCompactFixupPointers  [blocka,blocko,roota,rooto]
  CompactAdd -> \[_r] [c,o] ->
    pure $ PRPrimCall $ returnS (app hdCompactAdd [c,o])
  CompactAddWithSharing -> \[_r] [c,o] ->
    pure $ PRPrimCall $ returnS (app hdCompactAddWithSharing [c,o])
  CompactSize -> \[s] [c] ->
    pure $ PrimInline $ s |= app hdCompactSize [c]

------------------------------ Unsafe pointer equality --------------------------

  ReallyUnsafePtrEqualityOp -> \[r] [p1,p2] -> pure $ PrimInline $ r |= if10 (p1 .===. p2)

------------------------------ Parallelism --------------------------------------

  ParOp     -> \[r] [_a] -> pure $ PrimInline $ r |= zero_
  SparkOp   -> \[r] [a]  -> pure $ PrimInline $ r |= a
  NumSparks -> \[r] []   -> pure $ PrimInline $ r |= zero_

------------------------------ Tag to enum stuff --------------------------------

  DataToTagSmallOp -> \[_r] [d] -> pure $ PRPrimCall $ mconcat
      [ stack .! PreInc sp |= global (identFS hdDataToTagEntryStr)
      , returnS (app hdEntryStr [d])
      ]
  DataToTagLargeOp -> \[_r] [d] -> pure $ PRPrimCall $ mconcat
      [ stack .! PreInc sp |= global (identFS hdDataToTagEntryStr)
      , returnS (app hdEntryStr [d])
      ]
  TagToEnumOp -> \[r] [tag] -> pure $ PrimInline $
    if isBoolTy ty
    then r |= IfExpr tag true_ false_
    else r |= app hdTagToEnum [tag]

------------------------------ Bytecode operations ------------------------------

  AddrToAnyOp -> \[r] [d,_o] -> pure $ PrimInline $ r |= d

------------------------------ Profiling (CCS)  ------------------------------

  GetCCSOfOp -> \[a, o] [obj] -> pure $ PrimInline $ mconcat
    if prof
    then [ a |= if_ (isObject obj)
           (app hdBuildCCSPtrStr [obj .^ ccStr])
           null_
         , o |= zero_
         ]
    else [ a |= null_
         , o |= zero_
         ]

  GetCurrentCCSOp -> \[a, o] [_dummy_arg] ->
    let ptr = if prof then app hdBuildCCSPtrStr [jCurrentCCS]
                      else null_
    in pure $ PrimInline $ mconcat
        [ a |= ptr
        , o |= zero_
        ]

  ClearCCSOp -> \[_r] [x] -> pure $ PRPrimCall $ ReturnStat (app hdClearCCSStr [x])

------------------------------ Eventlog -------------------

  TraceEventOp       -> \[] [ed,eo]     -> pure $ PrimInline $ appS hdTraceEventStr [ed,eo]
  TraceEventBinaryOp -> \[] [ed,eo,len] -> pure $ PrimInline $ appS hdTraceEventBinaryStr [ed,eo,len]
  TraceMarkerOp      -> \[] [ed,eo]     -> pure $ PrimInline $ appS hdTraceMarkerStr [ed,eo]

------------------------------ ByteArray -------------------

  IndexByteArrayOp_Word8AsChar      -> \[r]   [a,i] -> pure $ PrimInline $ bnd_ba8  bound a i $ r |= read_boff_u8  a i
  IndexByteArrayOp_Word8AsWideChar  -> \[r]   [a,i] -> pure $ PrimInline $ bnd_ba32 bound a i $ r |= read_boff_i32 a i
  IndexByteArrayOp_Word8AsAddr      -> \[r,o] [a,i] -> pure $ PrimInline $ bnd_ba32 bound a i $ read_boff_addr a i r o
  IndexByteArrayOp_Word8AsFloat     -> \[r]   [a,i] -> pure $ PrimInline $ bnd_ba32 bound a i $ r |= read_boff_f32 a i
  IndexByteArrayOp_Word8AsDouble    -> \[r]   [a,i] -> pure $ PrimInline $ bnd_ba64 bound a i $ r |= read_boff_f64 a i
  IndexByteArrayOp_Word8AsStablePtr -> \[r,o] [a,i] -> pure $ PrimInline $ bnd_ba32 bound a i $ read_boff_stableptr a i r o
  IndexByteArrayOp_Word8AsInt16     -> \[r]   [a,i] -> pure $ PrimInline $ bnd_ba16 bound a i $ r |= read_boff_i16 a i
  IndexByteArrayOp_Word8AsInt32     -> \[r]   [a,i] -> pure $ PrimInline $ bnd_ba32 bound a i $ r |= read_boff_i32 a i
  IndexByteArrayOp_Word8AsInt64     -> \[h,l] [a,i] -> pure $ PrimInline $ bnd_ba64 bound a i $ read_boff_i64 a i h l
  IndexByteArrayOp_Word8AsInt       -> \[r]   [a,i] -> pure $ PrimInline $ bnd_ba32 bound a i $ r |= read_boff_i32  a i
  IndexByteArrayOp_Word8AsWord16    -> \[r]   [a,i] -> pure $ PrimInline $ bnd_ba16 bound a i $ r |= read_boff_u16  a i
  IndexByteArrayOp_Word8AsWord32    -> \[r]   [a,i] -> pure $ PrimInline $ bnd_ba32 bound a i $ r |= read_boff_u32  a i
  IndexByteArrayOp_Word8AsWord64    -> \[h,l] [a,i] -> pure $ PrimInline $ bnd_ba64 bound a i $ read_boff_u64 a i h l
  IndexByteArrayOp_Word8AsWord      -> \[r]   [a,i] -> pure $ PrimInline $ bnd_ba32 bound a i $ r |= read_boff_u32  a i

  ReadByteArrayOp_Word8AsChar       -> \[r]   [a,i] -> pure $ PrimInline $ bnd_ba8  bound a i $ r |= read_boff_u8  a i
  ReadByteArrayOp_Word8AsWideChar   -> \[r]   [a,i] -> pure $ PrimInline $ bnd_ba32 bound a i $ r |= read_boff_i32 a i
  ReadByteArrayOp_Word8AsAddr       -> \[r,o] [a,i] -> pure $ PrimInline $ bnd_ba32 bound a i $ read_boff_addr a i r o
  ReadByteArrayOp_Word8AsFloat      -> \[r]   [a,i] -> pure $ PrimInline $ bnd_ba32 bound a i $ r |= read_boff_f32 a i
  ReadByteArrayOp_Word8AsDouble     -> \[r]   [a,i] -> pure $ PrimInline $ bnd_ba64 bound a i $ r |= read_boff_f64 a i
  ReadByteArrayOp_Word8AsStablePtr  -> \[r,o] [a,i] -> pure $ PrimInline $ bnd_ba32 bound a i $ read_boff_stableptr a i r o
  ReadByteArrayOp_Word8AsInt16      -> \[r]   [a,i] -> pure $ PrimInline $ bnd_ba16 bound a i $ r |= read_boff_i16 a i
  ReadByteArrayOp_Word8AsInt32      -> \[r]   [a,i] -> pure $ PrimInline $ bnd_ba32 bound a i $ r |= read_boff_i32 a i
  ReadByteArrayOp_Word8AsInt64      -> \[h,l] [a,i] -> pure $ PrimInline $ bnd_ba64 bound a i $ read_boff_i64 a i h l
  ReadByteArrayOp_Word8AsInt        -> \[r]   [a,i] -> pure $ PrimInline $ bnd_ba32 bound a i $ r |= read_boff_i32  a i
  ReadByteArrayOp_Word8AsWord16     -> \[r]   [a,i] -> pure $ PrimInline $ bnd_ba16 bound a i $ r |= read_boff_u16  a i
  ReadByteArrayOp_Word8AsWord32     -> \[r]   [a,i] -> pure $ PrimInline $ bnd_ba32 bound a i $ r |= read_boff_u32  a i
  ReadByteArrayOp_Word8AsWord64     -> \[h,l] [a,i] -> pure $ PrimInline $ bnd_ba64 bound a i $ read_boff_u64 a i h l
  ReadByteArrayOp_Word8AsWord       -> \[r]   [a,i] -> pure $ PrimInline $ bnd_ba32 bound a i $ r |= read_boff_u32  a i

  WriteByteArrayOp_Word8AsChar      -> \[] [a,i,e]   -> pure $ PrimInline $ bnd_ba8  bound a i $ write_boff_i8  a i e
  WriteByteArrayOp_Word8AsWideChar  -> \[] [a,i,e]   -> pure $ PrimInline $ bnd_ba32 bound a i $ write_boff_i32 a i e
  WriteByteArrayOp_Word8AsAddr      -> \[] [a,i,r,o] -> pure $ PrimInline $ bnd_ba32 bound a i $ write_boff_addr a i r o
  WriteByteArrayOp_Word8AsFloat     -> \[] [a,i,e]   -> pure $ PrimInline $ bnd_ba32 bound a i $ write_boff_f32 a i e
  WriteByteArrayOp_Word8AsDouble    -> \[] [a,i,e]   -> pure $ PrimInline $ bnd_ba64 bound a i $ write_boff_f64 a i e
  WriteByteArrayOp_Word8AsStablePtr -> \[] [a,i,_,o] -> pure $ PrimInline $ bnd_ba32 bound a i $ write_boff_i32 a i o
  WriteByteArrayOp_Word8AsInt16     -> \[] [a,i,e]   -> pure $ PrimInline $ bnd_ba16 bound a i $ write_boff_i16 a i e
  WriteByteArrayOp_Word8AsInt32     -> \[] [a,i,e]   -> pure $ PrimInline $ bnd_ba32 bound a i $ write_boff_i32 a i e
  WriteByteArrayOp_Word8AsInt64     -> \[] [a,i,h,l] -> pure $ PrimInline $ bnd_ba64 bound a i $ write_boff_i64 a i h l
  WriteByteArrayOp_Word8AsInt       -> \[] [a,i,e]   -> pure $ PrimInline $ bnd_ba32 bound a i $ write_boff_i32 a i e
  WriteByteArrayOp_Word8AsWord16    -> \[] [a,i,e]   -> pure $ PrimInline $ bnd_ba16 bound a i $ write_boff_u16 a i e
  WriteByteArrayOp_Word8AsWord32    -> \[] [a,i,e]   -> pure $ PrimInline $ bnd_ba32 bound a i $ write_boff_u32 a i e
  WriteByteArrayOp_Word8AsWord64    -> \[] [a,i,h,l] -> pure $ PrimInline $ bnd_ba64 bound a i $ write_boff_u64 a i h l
  WriteByteArrayOp_Word8AsWord      -> \[] [a,i,e]   -> pure $ PrimInline $ bnd_ba32 bound a i $ write_boff_u32 a i e

  CasByteArrayOp_Int                -> \[r] [a,i,o,n] -> pure $ PrimInline $ bnd_ix32 bound a i $ casOp read_i32 write_i32 r a i o n
  CasByteArrayOp_Int8               -> \[r] [a,i,o,n] -> pure $ PrimInline $ bnd_ix8  bound a i $ casOp read_i8  write_i8  r a i o n
  CasByteArrayOp_Int16              -> \[r] [a,i,o,n] -> pure $ PrimInline $ bnd_ix16 bound a i $ casOp read_i16 write_i16 r a i o n
  CasByteArrayOp_Int32              -> \[r] [a,i,o,n] -> pure $ PrimInline $ bnd_ix32 bound a i $ casOp read_i32 write_i32 r a i o n

  CasByteArrayOp_Int64              -> \[rh,rl] [a,i,oh,ol,nh,nl] -> pure $ PrimInline $ bnd_ix64 bound a i $ casOp2 read_i64 write_i64 (rh,rl) a i (oh,ol) (nh,nl)

  CasAddrOp_Addr                    -> \[ra,ro] [a,o,oa,oo,na,no] -> pure $ PrimInline $ casOp2 read_boff_addr write_boff_addr (ra,ro) a o (oa,oo) (na,no)
  CasAddrOp_Word                    -> \[r] [a,o,old,new] -> pure $ PrimInline $ casOp read_u32 write_u32 r a o old new
  CasAddrOp_Word8                   -> \[r] [a,o,old,new] -> pure $ PrimInline $ casOp read_u8  write_u8  r a o old new
  CasAddrOp_Word16                  -> \[r] [a,o,old,new] -> pure $ PrimInline $ casOp read_u16 write_u16 r a o old new
  CasAddrOp_Word32                  -> \[r] [a,o,old,new] -> pure $ PrimInline $ casOp read_u32 write_u32 r a o old new
  CasAddrOp_Word64                  -> \[rh,rl] [a,o,oh,ol,nh,nl] -> pure $ PrimInline $ casOp2 read_u64 write_u64 (rh,rl) a o (oh,ol) (nh,nl)

  FetchAddAddrOp_Word               -> \[r] [a,o,v] -> pure $ PrimInline $ fetchOpAddr Add   r a o v
  FetchSubAddrOp_Word               -> \[r] [a,o,v] -> pure $ PrimInline $ fetchOpAddr Sub   r a o v
  FetchAndAddrOp_Word               -> \[r] [a,o,v] -> pure $ PrimInline $ fetchOpAddr BAnd  r a o v
  FetchNandAddrOp_Word              -> \[r] [a,o,v] -> pure $ PrimInline $ fetchOpAddr ((BNot .) . BAnd) r a o v
  FetchOrAddrOp_Word                -> \[r] [a,o,v] -> pure $ PrimInline $ fetchOpAddr BOr   r a o v
  FetchXorAddrOp_Word               -> \[r] [a,o,v] -> pure $ PrimInline $ fetchOpAddr BXor  r a o v

  InterlockedExchange_Addr          -> \[ra,ro] [a1,o1,a2,o2] -> pure $ PrimInline $ mconcat
                                          [ read_boff_addr a1 o1 ra ro
                                          , write_boff_addr a1 o1 a2 o2
                                          ]
  InterlockedExchange_Word          -> \[r] [a,o,w] -> pure $ PrimInline $ mconcat
                                          [ r |= read_boff_u32 a o
                                          , write_boff_u32 a o w
                                          ]

  ShrinkSmallMutableArrayOp_Char    -> \[]  [a,n] -> pure $ PrimInline $ appS hdShrinkMutableCharArrayStr  [a,n]
  GetSizeofSmallMutableArrayOp      -> \[r] [a]   -> pure $ PrimInline $ r |= a .^ lngth

  AtomicReadAddrOp_Word             -> \[r] [a,o]   -> pure $ PrimInline $ r |= read_boff_u32 a o
  AtomicWriteAddrOp_Word            -> \[]  [a,o,w] -> pure $ PrimInline $ write_boff_u32 a o w


------------------------------ Unhandled primops -------------------

  NewPromptTagOp                    -> unhandledPrimop op
  PromptOp                          -> unhandledPrimop op
  Control0Op                        -> unhandledPrimop op

  NewIOPortOp                       -> unhandledPrimop op
  ReadIOPortOp                      -> unhandledPrimop op
  WriteIOPortOp                     -> unhandledPrimop op

  GetSparkOp                        -> unhandledPrimop op
  AnyToAddrOp                       -> unhandledPrimop op
  MkApUpd0_Op                       -> unhandledPrimop op
  NewBCOOp                          -> unhandledPrimop op
  UnpackClosureOp                   -> unhandledPrimop op
  ClosureSizeOp                     -> unhandledPrimop op
  GetApStackValOp                   -> unhandledPrimop op
  WhereFromOp                       -> unhandledPrimop op -- should be easily implementable with o.f.n

  SetThreadAllocationCounter        -> unhandledPrimop op

------------------------------- Vector -----------------------------------------
-- For now, vectors are unsupported on the JS backend. Simply put, they do not
-- make much sense to support given support for arrays and lack of SIMD support
-- in JS. We could try to roll something special but we would not be able to
-- give any performance guarentees to the user and so we leave these has
-- unhandled for now.
  VecBroadcastOp _ _ _              -> unhandledPrimop op
  VecPackOp _ _ _                   -> unhandledPrimop op
  VecUnpackOp _ _ _                 -> unhandledPrimop op
  VecInsertOp _ _ _                 -> unhandledPrimop op
  VecAddOp _ _ _                    -> unhandledPrimop op
  VecSubOp _ _ _                    -> unhandledPrimop op
  VecMulOp _ _ _                    -> unhandledPrimop op
  VecDivOp _ _ _                    -> unhandledPrimop op
  VecQuotOp _ _ _                   -> unhandledPrimop op
  VecRemOp _ _ _                    -> unhandledPrimop op
  VecNegOp _ _ _                    -> unhandledPrimop op
  VecIndexByteArrayOp _ _ _         -> unhandledPrimop op
  VecReadByteArrayOp _ _ _          -> unhandledPrimop op
  VecWriteByteArrayOp _ _ _         -> unhandledPrimop op
  VecIndexOffAddrOp _ _ _           -> unhandledPrimop op
  VecReadOffAddrOp _ _ _            -> unhandledPrimop op
  VecWriteOffAddrOp _ _ _           -> unhandledPrimop op

  VecFMAdd  {} -> unhandledPrimop op
  VecFMSub  {} -> unhandledPrimop op
  VecFNMAdd {} -> unhandledPrimop op
  VecFNMSub {} -> unhandledPrimop op

  VecIndexScalarByteArrayOp _ _ _   -> unhandledPrimop op
  VecReadScalarByteArrayOp _ _ _    -> unhandledPrimop op
  VecWriteScalarByteArrayOp _ _ _   -> unhandledPrimop op
  VecIndexScalarOffAddrOp _ _ _     -> unhandledPrimop op
  VecReadScalarOffAddrOp _ _ _      -> unhandledPrimop op
  VecWriteScalarOffAddrOp _ _ _     -> unhandledPrimop op
  VecShuffleOp _ _ _                -> unhandledPrimop op

  PrefetchByteArrayOp3              -> noOp
  PrefetchMutableByteArrayOp3       -> noOp
  PrefetchAddrOp3                   -> noOp
  PrefetchValueOp3                  -> noOp
  PrefetchByteArrayOp2              -> noOp
  PrefetchMutableByteArrayOp2       -> noOp
  PrefetchAddrOp2                   -> noOp
  PrefetchValueOp2                  -> noOp
  PrefetchByteArrayOp1              -> noOp
  PrefetchMutableByteArrayOp1       -> noOp
  PrefetchAddrOp1                   -> noOp
  PrefetchValueOp1                  -> noOp
  PrefetchByteArrayOp0              -> noOp
  PrefetchMutableByteArrayOp0       -> noOp
  PrefetchAddrOp0                   -> noOp
  PrefetchValueOp0                  -> noOp

unhandledPrimop :: PrimOp -> [JStgExpr] -> [JStgExpr] -> JSM PrimRes
unhandledPrimop op rs as = pure $ PrimInline $ mconcat
  [ appS "h$log" $ pure $ String $
    fsLit $ mconcat
           [ "warning, unhandled primop: "
           , renderWithContext defaultSDocContext (ppr op)
           , " "
           , show (length rs, length as)
           ]
  , appS (mkFastString $ unpackFS hdPrimOpStr ++ zEncodeString (renderWithContext defaultSDocContext (ppr op))) as
    -- copyRes
  , mconcat $ zipWith (\r reg -> r |= foreignRegister reg) rs (enumFrom Ret1)
  ]

-- | A No Op, used for primops the JS platform cannot or do not support. For
-- example, the prefetching primops do not make sense on the JS platform because
-- we do not have enough control over memory to provide any kind of prefetching
-- mechanism. Hence, these are NoOps.
noOp :: Foldable f => f a -> f a -> JSM PrimRes
noOp = const . const . pure $ PrimInline mempty

-- tuple returns
appT :: [JStgExpr] -> FastString -> [JStgExpr] -> JStgStat
appT []     f xs = appS f xs
appT (r:rs) f xs = mconcat
  [ r |= app f xs
  , mconcat (zipWith (\r ret -> r |= (foreignRegister ret)) rs (enumFrom Ret1))
  ]

--------------------------------------------
-- ByteArray indexing
--------------------------------------------

-- For every ByteArray, the RTS creates the following views:
--  i3: Int32 view
--  u8: Word8 view
--  u1: Word16 view
--  f3: Float32 view
--  f6: Float64 view
--  dv: generic DataView
-- It seems a bit weird to mix Int and Word views like this, but perhaps they
-- are the more common.
--
-- See 'h$newByteArray' in 'ghc/rts/js/mem.js' for details.
--
-- Note that *byte* indexing can only be done with the generic DataView. Use
-- read_boff_* and write_boff_* for this.
--
-- Other read_* and write_* helpers directly use the more specific views.
-- Prefer using them over idx_* to make your intent clearer.

idx_i32, idx_u8, idx_u16, idx_f64, idx_f32 :: JStgExpr -> JStgExpr -> JStgExpr
idx_i32 a i = IdxExpr (a .^ i3) i
idx_u8  a i = IdxExpr (a .^ u8) i
idx_u16 a i = IdxExpr (a .^ u1) i
idx_f64 a i = IdxExpr (a .^ f6) i
idx_f32 a i = IdxExpr (a .^ f3) i

read_u8 :: JStgExpr -> JStgExpr -> JStgExpr
read_u8 a i = idx_u8 a i

read_u16 :: JStgExpr -> JStgExpr -> JStgExpr
read_u16 a i = idx_u16 a i

read_u32 :: JStgExpr -> JStgExpr -> JStgExpr
read_u32 a i = toU32 (idx_i32 a i)

read_i8 :: JStgExpr -> JStgExpr -> JStgExpr
read_i8 a i = signExtend8 (idx_u8 a i)

read_i16 :: JStgExpr -> JStgExpr -> JStgExpr
read_i16 a i = signExtend16 (idx_u16 a i)

read_i32 :: JStgExpr -> JStgExpr -> JStgExpr
read_i32 a i = idx_i32 a i

read_f32 :: JStgExpr -> JStgExpr -> JStgExpr
read_f32 a i = idx_f32 a i

read_f64 :: JStgExpr -> JStgExpr -> JStgExpr
read_f64 a i = idx_f64 a i

read_u64 :: JStgExpr -> JStgExpr -> JStgExpr -> JStgExpr -> JStgStat
read_u64 a i rh rl = mconcat
  [ rl |= read_u32 a (i .<<. one_)
  , rh |= read_u32 a (Add one_ (i .<<. one_))
  ]

read_i64 :: JStgExpr -> JStgExpr -> JStgExpr -> JStgExpr -> JStgStat
read_i64 a i rh rl = mconcat
  [ rl |= read_u32 a (i .<<. one_)
  , rh |= read_i32 a (Add one_ (i .<<. one_))
  ]

--------------------------------------
-- Addr#
--------------------------------------

write_addr :: JStgExpr -> JStgExpr -> JStgExpr -> JStgExpr -> JStgStat
write_addr a i r o = mconcat
  [ write_i32 a i o
    -- create the hidden array for arrays if it doesn't exist
  , ifS (Not (a .^ arr)) (a .^ arr |= ValExpr (JList [])) mempty
  , a .^ arr .! (i .<<. two_) |= r
  ]

read_addr :: JStgExpr -> JStgExpr -> JStgExpr -> JStgExpr -> JStgStat
read_addr a i r o = mconcat
  [ o |= read_i32 a i
  , r |= if_ ((a .^ arr) .&&. (a .^ arr .! (i .<<. two_)))
            (a .^ arr .! (i .<<. two_))
            null_
  ]

read_boff_addr :: JStgExpr -> JStgExpr -> JStgExpr -> JStgExpr -> JStgStat
read_boff_addr a i r o = mconcat
  [ o |= read_boff_i32 a i
  , r |= if_ ((a .^ arr) .&&. (a .^ arr .! i))
            (a .^ arr .! i)
            null_
  ]

write_boff_addr :: JStgExpr -> JStgExpr -> JStgExpr -> JStgExpr -> JStgStat
write_boff_addr a i r o = mconcat
  [ write_boff_i32 a i o
    -- create the hidden array for arrays if it doesn't exist
  , ifS (Not (a .^ arr)) (a .^ arr |= ValExpr (JList [])) mempty
  , a .^ arr .! i |= r
  ]


--------------------------------------
-- StablePtr
--------------------------------------

read_stableptr :: JStgExpr -> JStgExpr -> JStgExpr -> JStgExpr -> JStgStat
read_stableptr a i r o = mconcat
  [ o |= read_i32 a i
  , ifS (o .===. zero_)
      (r |= null_)
      (r |= hdStablePtrBuf)
  ]

read_boff_stableptr :: JStgExpr -> JStgExpr -> JStgExpr -> JStgExpr -> JStgStat
read_boff_stableptr a i r o = mconcat
  [ o |= read_boff_i32 a i
  , ifS (o .===. zero_)
      (r |= null_)
      (r |= hdStablePtrBuf)
  ]

write_stableptr :: JStgExpr -> JStgExpr -> JStgExpr -> JStgExpr -> JStgStat
write_stableptr a i _r o = write_i32 a i o
  -- don't store "r" as it must be h$stablePtrBuf or null

write_boff_stableptr :: JStgExpr -> JStgExpr -> JStgExpr -> JStgExpr -> JStgStat
write_boff_stableptr a i _r o = write_boff_i32 a i o
  -- don't store "r" as it must be h$stablePtrBuf or null

write_u8 :: JStgExpr -> JStgExpr -> JStgExpr -> JStgStat
write_u8 a i v = idx_u8 a i |= v

write_u16 :: JStgExpr -> JStgExpr -> JStgExpr -> JStgStat
write_u16 a i v = idx_u16 a i |= v

write_u32 :: JStgExpr -> JStgExpr -> JStgExpr -> JStgStat
write_u32 a i v = idx_i32 a i |= v

write_i8 :: JStgExpr -> JStgExpr -> JStgExpr -> JStgStat
write_i8 a i v = idx_u8 a i |= v

write_i16 :: JStgExpr -> JStgExpr -> JStgExpr -> JStgStat
write_i16 a i v = idx_u16 a i |= v

write_i32 :: JStgExpr -> JStgExpr -> JStgExpr -> JStgStat
write_i32 a i v = idx_i32 a i |= v

write_f32 :: JStgExpr -> JStgExpr -> JStgExpr -> JStgStat
write_f32 a i v = idx_f32 a i |= v

write_f64 :: JStgExpr -> JStgExpr -> JStgExpr -> JStgStat
write_f64 a i v = idx_f64 a i |= v

write_u64 :: JStgExpr -> JStgExpr -> JStgExpr -> JStgExpr -> JStgStat
write_u64 a i h l = mconcat
  [ write_u32 a (i .<<. one_)         l
  , write_u32 a (Add one_ (i .<<. one_)) h
  ]

write_i64 :: JStgExpr -> JStgExpr -> JStgExpr -> JStgExpr -> JStgStat
write_i64 a i h l = mconcat
  [ write_u32 a (i .<<. one_)         l
  , write_i32 a (Add one_ (i .<<. one_)) h
  ]

-- Data View helper functions: byte indexed!
--
-- The argument list consists of the array @a@, the index @i@, and the new value
-- to set (in the case of a setter) @v@.

write_boff_i8, write_boff_u8, write_boff_i16, write_boff_u16, write_boff_i32, write_boff_u32, write_boff_f32, write_boff_f64 :: JStgExpr -> JStgExpr -> JStgExpr -> JStgStat
write_boff_i8  a i v = write_i8 a i v
write_boff_u8  a i v = write_u8 a i v
write_boff_i16 a i v = ApplStat (a .^ dv .^ setInt16  ) [i, v, true_]
write_boff_u16 a i v = ApplStat (a .^ dv .^ setUint16 ) [i, v, true_]
write_boff_i32 a i v = ApplStat (a .^ dv .^ setInt32  ) [i, v, true_]
write_boff_u32 a i v = ApplStat (a .^ dv .^ setUint32 ) [i, v, true_]
write_boff_f32 a i v = ApplStat (a .^ dv .^ setFloat32) [i, v, true_]
write_boff_f64 a i v = ApplStat (a .^ dv .^ setFloat64) [i, v, true_]

write_boff_i64, write_boff_u64 :: JStgExpr -> JStgExpr -> JStgExpr -> JStgExpr -> JStgStat
write_boff_i64 a i h l = mconcat
  [ write_boff_i32 a (Add i (Int 4)) h
  , write_boff_u32 a i l
  ]
write_boff_u64 a i h l = mconcat
  [ write_boff_u32 a (Add i (Int 4)) h
  , write_boff_u32 a i l
  ]

read_boff_i8, read_boff_u8, read_boff_i16, read_boff_u16, read_boff_i32, read_boff_u32, read_boff_f32, read_boff_f64 :: JStgExpr -> JStgExpr -> JStgExpr
read_boff_i8  a i = read_i8 a i
read_boff_u8  a i = read_u8 a i
read_boff_i16 a i = ApplExpr (a .^ dv .^ getInt16  ) [i, true_]
read_boff_u16 a i = ApplExpr (a .^ dv .^ getUint16 ) [i, true_]
read_boff_i32 a i = ApplExpr (a .^ dv .^ getInt32  ) [i, true_]
read_boff_u32 a i = ApplExpr (a .^ dv .^ getUint32 ) [i, true_]
read_boff_f32 a i = ApplExpr (a .^ dv .^ getFloat32) [i, true_]
read_boff_f64 a i = ApplExpr (a .^ dv .^ getFloat64) [i, true_]

read_boff_i64 :: JStgExpr -> JStgExpr -> JStgExpr -> JStgExpr -> JStgStat
read_boff_i64 a i rh rl = mconcat
  [ rh |= read_boff_i32 a (Add i (Int 4))
  , rl |= read_boff_u32 a i
  ]

read_boff_u64 :: JStgExpr -> JStgExpr -> JStgExpr -> JStgExpr -> JStgStat
read_boff_u64 a i rh rl = mconcat
  [ rh |= read_boff_u32 a (Add i (Int 4))
  , rl |= read_boff_u32 a i
  ]

fetchOpByteArray :: (JStgExpr -> JStgExpr -> JStgExpr) -> JStgExpr -> JStgExpr -> JStgExpr -> JStgExpr -> JStgStat
fetchOpByteArray op tgt src i v = mconcat
  [ tgt |= read_i32 src i
  , write_i32 src i (op tgt v)
  ]

fetchOpAddr :: (JStgExpr -> JStgExpr -> JStgExpr) -> JStgExpr -> JStgExpr -> JStgExpr -> JStgExpr -> JStgStat
fetchOpAddr op tgt src i v = mconcat
  [ tgt |= read_boff_u32 src i
  , write_boff_u32 src i (op tgt v)
  ]

casOp
  :: (JStgExpr -> JStgExpr -> JStgExpr)          -- read
  -> (JStgExpr -> JStgExpr -> JStgExpr -> JStgStat) -- write
  -> JStgExpr                     -- target register to store result
  -> JStgExpr                     -- source array
  -> JStgExpr                     -- index
  -> JStgExpr                     -- old value to compare
  -> JStgExpr                     -- new value to write
  -> JStgStat
casOp read write tgt src i old new = mconcat
  [ tgt |= read src i
  , ifS (tgt .===. old)
        (write src i new)
         mempty
  ]

casOp2
  :: (JStgExpr -> JStgExpr -> JStgExpr -> JStgExpr -> JStgStat) -- read
  -> (JStgExpr -> JStgExpr -> JStgExpr -> JStgExpr -> JStgStat) -- write
  -> (JStgExpr,JStgExpr)             -- target registers to store result
  -> JStgExpr                     -- source array
  -> JStgExpr                     -- index
  -> (JStgExpr,JStgExpr)             -- old value to compare
  -> (JStgExpr,JStgExpr)             -- new value to write
  -> JStgStat
casOp2 read write (tgt1,tgt2) src i (old1,old2) (new1,new2) = mconcat
  [ read src i tgt1 tgt2
  , ifS ((tgt2 .===. old2) .&&. (tgt1 .===. old1))
        (write src i new1 new2)
         mempty
  ]

--------------------------------------------------------------------------------
--                            Lifted Arrays
--------------------------------------------------------------------------------
-- | lifted arrays
cloneArray :: Bool -> JStgExpr -> JStgExpr -> JStgExpr -> JStgExpr -> JStgStat
cloneArray bound_check tgt src start len =
  bnd_arr_range bound_check src start len
  $ mconcat
      [ tgt |= ApplExpr (src .^ slice) [start, Add len start]
      , tgt .^ closureMeta_ |= zero_
      , tgt .^ ghcjsArray   |= true_
      ]

newByteArray :: JStgExpr -> JStgExpr -> JStgStat
newByteArray tgt len =
  tgt |= app hdNewByteArrayStr [len]

-- | Check that index is positive and below a max value. Halt the process with
-- error code 134 otherwise. This is used to implement -fcheck-prim-bounds
check_bound
  :: JStgExpr -- ^ Max index expression
  -> Bool  -- ^ Should we do bounds checking?
  -> JStgExpr -- ^ Index
  -> JStgStat -- ^ Result
  -> JStgStat
check_bound _         False _ r = r
check_bound max_index True  i r = mconcat
  [ jwhenS ((i .<. zero_) .||. (i .>=. max_index)) $
      returnS (app hdExitProcess [Int 134])
  , r
  ]

-- | Bounds checking using ".length" property (Arrays)
bnd_arr
  :: Bool  -- ^ Should we do bounds checking?
  -> JStgExpr -- ^ Array
  -> JStgExpr -- ^ Index
  -> JStgStat -- ^ Result
  -> JStgStat
bnd_arr do_check arr = check_bound (arr .^ lngth) do_check

-- | Range bounds checking using ".length" property (Arrays)
--
-- Empty ranges trivially pass the check
bnd_arr_range
  :: Bool  -- ^ Should we do bounds checking?
  -> JStgExpr -- ^ Array
  -> JStgExpr -- ^ Index
  -> JStgExpr -- ^ Range size
  -> JStgStat -- ^ Result
  -> JStgStat
bnd_arr_range False _arr _i _n r = r
bnd_arr_range True   arr  i  n r =
  ifS (n .<. zero_) (returnS $ app hdExitProcess [Int 134]) $
  -- Empty ranges trivially pass the check
  ifS (n .===. zero_)
      r
      (bnd_arr True arr i $ bnd_arr True arr (Add i (Sub n one_)) r)

-- | Bounds checking using ".len" property (ByteArrays)
bnd_ba
  :: Bool  -- ^ Should we do bounds checking?
  -> JStgExpr -- ^ Array
  -> JStgExpr -- ^ Index
  -> JStgStat -- ^ Result
  -> JStgStat
bnd_ba do_check arr = check_bound (arr .^ len) do_check

-- | ByteArray bounds checking (byte offset, 8-bit value)
bnd_ba8 :: Bool -> JStgExpr -> JStgExpr -> JStgStat -> JStgStat
bnd_ba8 = bnd_ba

-- | ByteArray bounds checking (byte offset, 16-bit value)
bnd_ba16 :: Bool -> JStgExpr -> JStgExpr -> JStgStat -> JStgStat
bnd_ba16 do_check arr idx r =
  -- check that idx non incremented is in range:
  -- (idx + 1) may be in range while idx isn't
  bnd_ba do_check arr idx
  $ bnd_ba do_check arr (Add idx one_) r

-- | ByteArray bounds checking (byte offset, 32-bit value)
bnd_ba32 :: Bool -> JStgExpr -> JStgExpr -> JStgStat -> JStgStat
bnd_ba32 do_check arr idx r =
  -- check that idx non incremented is in range:
  -- (idx + 3) may be in range while idx isn't
  bnd_ba do_check arr idx
  $ bnd_ba do_check arr (Add idx three_) r

-- | ByteArray bounds checking (byte offset, 64-bit value)
bnd_ba64 :: Bool -> JStgExpr -> JStgExpr -> JStgStat -> JStgStat
bnd_ba64 do_check arr idx r =
  -- check that idx non incremented is in range:
  -- (idx + 7) may be in range while idx isn't
  bnd_ba do_check arr idx
  $ bnd_ba do_check arr (Add idx (Int 7)) r

-- | ByteArray bounds checking (8-bit offset, 8-bit value)
bnd_ix8 :: Bool -> JStgExpr -> JStgExpr -> JStgStat -> JStgStat
bnd_ix8 = bnd_ba8

-- | ByteArray bounds checking (16-bit offset, 16-bit value)
bnd_ix16 :: Bool -> JStgExpr -> JStgExpr -> JStgStat -> JStgStat
bnd_ix16 do_check arr idx r = bnd_ba16 do_check arr (idx .<<. one_) r

-- | ByteArray bounds checking (32-bit offset, 32-bit value)
bnd_ix32 :: Bool -> JStgExpr -> JStgExpr -> JStgStat -> JStgStat
bnd_ix32 do_check arr idx r = bnd_ba32 do_check arr (idx .<<. two_) r

-- | ByteArray bounds checking (64-bit offset, 64-bit value)
bnd_ix64 :: Bool -> JStgExpr -> JStgExpr -> JStgStat -> JStgStat
bnd_ix64 do_check arr idx r = bnd_ba64 do_check arr (idx .<<. three_) r

-- | Bounds checking on a range and using ".len" property (ByteArrays)
--
-- Empty ranges trivially pass the check
bnd_ba_range
  :: Bool  -- ^ Should we do bounds checking?
  -> JStgExpr -- ^ Array
  -> JStgExpr -- ^ Index
  -> JStgExpr -- ^ Range size
  -> JStgStat -- ^ Result
  -> JStgStat
bnd_ba_range False _  _ _ r = r
bnd_ba_range True  xs i n r =
  ifS (n .<. zero_) (returnS $ app hdExitProcess [Int 134]) $
  -- Empty ranges trivially pass the check
  ifS (n .===. zero_)
      r
      (bnd_ba True xs (Add i (Sub n one_)) (bnd_ba True xs i r))

checkOverlapByteArray
  :: Bool  -- ^ Should we do bounds checking?
  -> JStgExpr -- ^ First array
  -> JStgExpr -- ^ First offset
  -> JStgExpr -- ^ Second array
  -> JStgExpr -- ^ Second offset
  -> JStgExpr -- ^ Range size
  -> JStgStat -- ^ Result
  -> JStgStat
checkOverlapByteArray False _ _ _ _ _ r    = r
checkOverlapByteArray True a1 o1 a2 o2 n r =
  ifS (app hdCheckOverlapByteArrayStr [a1, o1, a2, o2, n])
    r
    (returnS $ app hdExitProcess [Int 134])

copyByteArray :: Bool -> Bool -> JStgExpr -> JStgExpr -> JStgExpr -> JStgExpr -> JStgExpr -> PrimRes
copyByteArray allow_overlap bound a1 o1 a2 o2 n = PrimInline $
  check $ appS hdCopyMutableByteArrayStr [a1,o1,a2,o2,n]
  where
      check = bnd_ba_range bound a1 o1 n
              . bnd_ba_range bound a2 o2 n
              . (if not allow_overlap then checkOverlapByteArray bound a1 o1 a2 o2 n else id)

-- e|0 (32 bit signed integer truncation) required because of JS numbers. e|0
-- converts e to an Int32. Note that e|0 _is still a Double_ because JavaScript.
-- So (x|0) * (y|0) can still return values outside of the Int32 range. You have
-- been warned!
toI32 :: JStgExpr -> JStgExpr
toI32 e = BOr e zero_

-- e>>>0  (32 bit unsigned integer truncation)
-- required because of JS numbers. e>>>0 converts e to a Word32
-- so  (-2147483648)       >>> 0  = 2147483648
-- and ((-2147483648) >>>0) | 0   = -2147483648
toU32 :: JStgExpr -> JStgExpr
toU32 e = e .>>>. zero_

quotShortInt :: Int -> JStgExpr -> JStgExpr -> JStgExpr
quotShortInt bits x y = BAnd (signed x `Div` signed y) mask
  where
    signed z = (z .<<. shift) .>>. shift
    shift    = Int (32 - toInteger bits)
    mask     = Int (((2::Integer) ^ toInteger bits) - 1)

remShortInt :: Int -> JStgExpr -> JStgExpr -> JStgExpr
remShortInt bits x y = BAnd (signed x `Mod` signed y) mask
  where
    signed z = (z .<<. shift) .>>. shift
    shift    = Int (32 - toInteger bits)
    mask     = Int (((2::Integer) ^ toInteger bits) - 1)
