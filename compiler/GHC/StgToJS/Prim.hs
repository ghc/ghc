{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
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

import GHC.JS.Syntax hiding (JUOp (..))
import GHC.JS.Make

import GHC.StgToJS.Heap
import GHC.StgToJS.Types
import GHC.StgToJS.Profiling
import GHC.StgToJS.Regs

import GHC.Core.Type

import GHC.Builtin.PrimOps
import GHC.Tc.Utils.TcType (isBoolTy)
import GHC.Utils.Encoding (zEncodeString)

import GHC.Data.FastString
import GHC.Utils.Outputable (renderWithContext, defaultSDocContext, ppr)
import Data.Maybe


genPrim :: Bool     -- ^ Profiling (cost-centres) enabled
        -> Type
        -> PrimOp   -- ^ the primitive operation
        -> [JExpr]  -- ^ where to store the result
        -> [JExpr]  -- ^ arguments
        -> PrimRes
genPrim prof ty op = case op of
  CharGtOp        -> \[r] [x,y] -> PrimInline $ r |= if10 (x .>. y)
  CharGeOp        -> \[r] [x,y] -> PrimInline $ r |= if10 (x .>=. y)
  CharEqOp        -> \[r] [x,y] -> PrimInline $ r |= if10 (x .===. y)
  CharNeOp        -> \[r] [x,y] -> PrimInline $ r |= if10 (x .!==. y)
  CharLtOp        -> \[r] [x,y] -> PrimInline $ r |= if10 (x .<. y)
  CharLeOp        -> \[r] [x,y] -> PrimInline $ r |= if10 (x .<=. y)
  OrdOp           -> \[r] [x]   -> PrimInline $ r |= x

  Int8ToWord8Op   -> \[r] [x]   -> PrimInline $ r |= mask8 x
  Word8ToInt8Op   -> \[r] [x]   -> PrimInline $ r |= signExtend8 x
  Int16ToWord16Op -> \[r] [x]   -> PrimInline $ r |= mask16 x
  Word16ToInt16Op -> \[r] [x]   -> PrimInline $ r |= signExtend16 x
  Int32ToWord32Op -> \[r] [x]   -> PrimInline $ r |= x .>>>. zero_
  Word32ToInt32Op -> \[r] [x]   -> PrimInline $ r |= i32 x

------------------------------ Int ----------------------------------------------

  IntAddOp        -> \[r] [x,y] -> PrimInline $ r |= i32 (Add x y)
  IntSubOp        -> \[r] [x,y] -> PrimInline $ r |= i32 (Sub x y)
  IntMulOp        -> \[r] [x,y] -> PrimInline $ r |= app "h$mulInt32" [x, y]
  IntMul2Op       -> \[c,hr,lr] [x,y] -> PrimInline $ appT [c,hr,lr] "h$hs_timesInt2" [x, y]
  IntMulMayOfloOp -> \[r] [x,y] -> PrimInline $ jVar \tmp -> mconcat
                                            [ tmp |= Mul x y
                                            , r   |= if01 (tmp .===. i32 tmp)
                                            ]
  IntQuotOp       -> \[r]   [x,y] -> PrimInline $ r |= i32 (Div x y)
  IntRemOp        -> \[r]   [x,y] -> PrimInline $ r |= Mod x y
  IntQuotRemOp    -> \[q,r] [x,y] -> PrimInline $ mconcat
                                            [ q |= i32 (Div x y)
                                            , r |= x `Sub` (Mul y q)
                                            ]
  IntAndOp        -> \[r] [x,y]   -> PrimInline $ r |= BAnd x y
  IntOrOp         -> \[r] [x,y]   -> PrimInline $ r |= BOr  x y
  IntXorOp        -> \[r] [x,y]   -> PrimInline $ r |= BXor x y
  IntNotOp        -> \[r] [x]     -> PrimInline $ r |= BNot x

  IntNegOp        -> \[r] [x]   -> PrimInline $ r |= i32 (Negate x)
-- add with carry: overflow == 0 iff no overflow
  IntAddCOp       -> \[r,overf] [x,y] ->
      PrimInline $ jVar \rt -> mconcat
        [ rt    |= Add x y
        , r     |= i32 rt
        , overf |= if10 (r .!=. rt)
        ]
  IntSubCOp       -> \[r,overf] [x,y] ->
      PrimInline $ jVar \rt -> mconcat
        [ rt    |= Sub x y
        , r     |= i32 rt
        , overf |= if10 (r .!=. rt)
        ]
  IntGtOp           -> \[r] [x,y] -> PrimInline $ r |= if10 (x .>. y)
  IntGeOp           -> \[r] [x,y] -> PrimInline $ r |= if10 (x .>=. y)
  IntEqOp           -> \[r] [x,y] -> PrimInline $ r |= if10 (x .===. y)
  IntNeOp           -> \[r] [x,y] -> PrimInline $ r |= if10(x .!==. y)
  IntLtOp           -> \[r] [x,y] -> PrimInline $ r |= if10 (x .<. y)
  IntLeOp           -> \[r] [x,y] -> PrimInline $ r |= if10 (x .<=. y)
  ChrOp             -> \[r] [x]   -> PrimInline $ r |= x
  IntToWordOp       -> \[r] [x]   -> PrimInline $ r |= x .>>>. 0
  IntToFloatOp      -> \[r] [x]   -> PrimInline $ r |= x
  IntToDoubleOp     -> \[r] [x]   -> PrimInline $ r |= x
  IntSllOp          -> \[r] [x,y] -> PrimInline $ r |= x .<<. y
  IntSraOp          -> \[r] [x,y] -> PrimInline $ r |= x .>>. y
  IntSrlOp          -> \[r] [x,y] -> PrimInline $ r |= i32 (x .>>>. y)

------------------------------ Int8 ---------------------------------------------

  Int8ToIntOp       -> \[r] [x]       -> PrimInline $ r |= x
  IntToInt8Op       -> \[r] [x]       -> PrimInline $ r |= signExtend8 x
  Int8NegOp         -> \[r] [x]       -> PrimInline $ r |= signExtend8 (Negate x)
  Int8AddOp         -> \[r] [x,y]     -> PrimInline $ r |= signExtend8 (Add x y)
  Int8SubOp         -> \[r] [x,y]     -> PrimInline $ r |= signExtend8 (Sub x y)
  Int8MulOp         -> \[r] [x,y]     -> PrimInline $ r |= signExtend8 (Mul x y)
  Int8QuotOp        -> \[r] [x,y]     -> PrimInline $ r |= quotShortInt 8 x y
  Int8RemOp         -> \[r] [x,y]     -> PrimInline $ r |= remShortInt 8 x y
  Int8QuotRemOp     -> \[r1,r2] [x,y] -> PrimInline $ mconcat
                                                [ r1 |= quotShortInt 8 x y
                                                , r2 |= remShortInt 8 x y
                                                ]
  Int8EqOp          -> \[r] [x,y] -> PrimInline $ r |= if10 (x .===. y)
  Int8GeOp          -> \[r] [x,y] -> PrimInline $ r |= if10 ((x .<<. (Int 24)) .>=. (y .<<. (Int 24)))
  Int8GtOp          -> \[r] [x,y] -> PrimInline $ r |= if10 ((x .<<. (Int 24)) .>.  (y .<<. (Int 24)))
  Int8LeOp          -> \[r] [x,y] -> PrimInline $ r |= if10 ((x .<<. (Int 24)) .<=. (y .<<. (Int 24)))
  Int8LtOp          -> \[r] [x,y] -> PrimInline $ r |= if10 ((x .<<. (Int 24)) .<.  (y .<<. (Int 24)))
  Int8NeOp          -> \[r] [x,y] -> PrimInline $ r |= if10 (x .!==. y)

  Int8SraOp         -> \[r] [x,i]   -> PrimInline $ r |= x .>>. i
  Int8SrlOp         -> \[r] [x,i]   -> PrimInline $ r |= signExtend8 (mask8 x .>>>. i)
  Int8SllOp         -> \[r] [x,i]   -> PrimInline $ r |= signExtend8 (mask8 (x .<<. i))

------------------------------ Word8 --------------------------------------------

  Word8ToWordOp      -> \[r] [x]       -> PrimInline $ r |= mask8 x
  WordToWord8Op      -> \[r] [x]       -> PrimInline $ r |= mask8 x

  Word8AddOp         -> \[r] [x,y]     -> PrimInline $ r |= mask8 (Add x y)
  Word8SubOp         -> \[r] [x,y]     -> PrimInline $ r |= mask8 (Sub x y)
  Word8MulOp         -> \[r] [x,y]     -> PrimInline $ r |= mask8 (Mul x y)
  Word8QuotOp        -> \[r] [x,y]     -> PrimInline $ r |= mask8 (Div x y)
  Word8RemOp         -> \[r] [x,y]     -> PrimInline $ r |= Mod x y
  Word8QuotRemOp     -> \[r1,r2] [x,y] -> PrimInline $ mconcat
                                                  [ r1 |= i32 (Div x y)
                                                  , r2 |= Mod x y
                                                  ]
  Word8EqOp          -> \[r] [x,y] -> PrimInline $ r |= if10 (x .===. y)
  Word8GeOp          -> \[r] [x,y] -> PrimInline $ r |= if10 (x .>=. y)
  Word8GtOp          -> \[r] [x,y] -> PrimInline $ r |= if10 (x .>. y)
  Word8LeOp          -> \[r] [x,y] -> PrimInline $ r |= if10 (x .<=. y)
  Word8LtOp          -> \[r] [x,y] -> PrimInline $ r |= if10 (x .<. y)
  Word8NeOp          -> \[r] [x,y] -> PrimInline $ r |= if10 (x .!==. y)

  Word8AndOp         -> \[r] [x,y]   -> PrimInline $ r |= BAnd x y
  Word8OrOp          -> \[r] [x,y]   -> PrimInline $ r |= BOr  x y
  Word8XorOp         -> \[r] [x,y]   -> PrimInline $ r |= BXor x y
  Word8NotOp         -> \[r] [x]     -> PrimInline $ r |= BXor x (Int 0xff)

  Word8SllOp         -> \[r] [x,i]   -> PrimInline $ r |= mask8 (x .<<. i)
  Word8SrlOp         -> \[r] [x,i]   -> PrimInline $ r |= x .>>>. i

------------------------------ Int16 -------------------------------------------

  Int16ToIntOp       -> \[r] [x]       -> PrimInline $ r |= x
  IntToInt16Op       -> \[r] [x]       -> PrimInline $ r |= signExtend16 x

  Int16NegOp         -> \[r] [x]       -> PrimInline $ r |= signExtend16 (Negate x)
  Int16AddOp         -> \[r] [x,y]     -> PrimInline $ r |= signExtend16 (Add x y)
  Int16SubOp         -> \[r] [x,y]     -> PrimInline $ r |= signExtend16 (Sub x y)
  Int16MulOp         -> \[r] [x,y]     -> PrimInline $ r |= signExtend16 (Mul x y)
  Int16QuotOp        -> \[r] [x,y]     -> PrimInline $ r |= quotShortInt 16 x y
  Int16RemOp         -> \[r] [x,y]     -> PrimInline $ r |= remShortInt 16 x y
  Int16QuotRemOp     -> \[r1,r2] [x,y] -> PrimInline $ mconcat
                                                [ r1 |= quotShortInt 16 x y
                                                , r2 |= remShortInt 16 x y
                                                ]
  Int16EqOp          -> \[r] [x,y] -> PrimInline $ r |= if10 (x .===. y)
  Int16GeOp          -> \[r] [x,y] -> PrimInline $ r |= if10 ((x .<<. (Int 16)) .>=. (y .<<. (Int 16)))
  Int16GtOp          -> \[r] [x,y] -> PrimInline $ r |= if10 ((x .<<. (Int 16)) .>.  (y .<<. (Int 16)))
  Int16LeOp          -> \[r] [x,y] -> PrimInline $ r |= if10 ((x .<<. (Int 16)) .<=. (y .<<. (Int 16)))
  Int16LtOp          -> \[r] [x,y] -> PrimInline $ r |= if10 ((x .<<. (Int 16)) .<.  (y .<<. (Int 16)))
  Int16NeOp          -> \[r] [x,y] -> PrimInline $ r |= if10 (x .!==. y)

  Int16SraOp         -> \[r] [x,i]   -> PrimInline $ r |= x .>>. i
  Int16SrlOp         -> \[r] [x,i]   -> PrimInline $ r |= signExtend16 (mask16 x .>>>. i)
  Int16SllOp         -> \[r] [x,i]   -> PrimInline $ r |= signExtend16 (x .<<. i)

------------------------------ Word16 ------------------------------------------

  Word16ToWordOp     -> \[r] [x]   -> PrimInline $ r |= x
  WordToWord16Op     -> \[r] [x]   -> PrimInline $ r |= mask16 x

  Word16AddOp        -> \[r] [x,y] -> PrimInline $ r |= mask16 (Add x y)
  Word16SubOp        -> \[r] [x,y] -> PrimInline $ r |= mask16 (Sub x y)
  Word16MulOp        -> \[r] [x,y] -> PrimInline $ r |= mask16 (Mul x y)
  Word16QuotOp       -> \[r] [x,y] -> PrimInline $ r |= mask16 (Div x y)
  Word16RemOp        -> \[r] [x,y] -> PrimInline $ r |= Mod x y
  Word16QuotRemOp    -> \[r1,r2] [x,y] -> PrimInline $ mconcat
                                                [ r1 |= i32 (Div x y)
                                                , r2 |= Mod x y
                                                ]
  Word16EqOp         -> \[r] [x,y] -> PrimInline $ r |= if10 (x .===. y)
  Word16GeOp         -> \[r] [x,y] -> PrimInline $ r |= if10 (x .>=. y)
  Word16GtOp         -> \[r] [x,y] -> PrimInline $ r |= if10 (x .>. y)
  Word16LeOp         -> \[r] [x,y] -> PrimInline $ r |= if10 (x .<=. y)
  Word16LtOp         -> \[r] [x,y] -> PrimInline $ r |= if10 (x .<. y)
  Word16NeOp         -> \[r] [x,y] -> PrimInline $ r |= if10 (x .!==. y)

  Word16AndOp        -> \[r] [x,y]   -> PrimInline $ r |= BAnd x y
  Word16OrOp         -> \[r] [x,y]   -> PrimInline $ r |= BOr  x y
  Word16XorOp        -> \[r] [x,y]   -> PrimInline $ r |= BXor x y
  Word16NotOp        -> \[r] [x]     -> PrimInline $ r |= BXor x (Int 0xffff)

  Word16SllOp        -> \[r] [x,i]   -> PrimInline $ r |= mask16 (x .<<. i)
  Word16SrlOp        -> \[r] [x,i]   -> PrimInline $ r |= x .>>>. i

------------------------------ Int32 --------------------------------------------

  Int32ToIntOp       -> \[r] [x]   -> PrimInline $ r |= x
  IntToInt32Op       -> \[r] [x]   -> PrimInline $ r |= x

  Int32NegOp         -> \rs  xs    -> genPrim prof ty IntNegOp rs xs
  Int32AddOp         -> \rs  xs    -> genPrim prof ty IntAddOp rs xs
  Int32SubOp         -> \rs  xs    -> genPrim prof ty IntSubOp rs xs
  Int32MulOp         -> \rs  xs    -> genPrim prof ty IntMulOp rs xs
  Int32QuotOp        -> \rs  xs    -> genPrim prof ty IntQuotOp rs xs
  Int32RemOp         -> \rs  xs    -> genPrim prof ty IntRemOp rs xs
  Int32QuotRemOp     -> \rs  xs    -> genPrim prof ty IntQuotRemOp rs xs

  Int32EqOp          -> \rs  xs    -> genPrim prof ty IntEqOp rs xs
  Int32GeOp          -> \rs  xs    -> genPrim prof ty IntGeOp rs xs
  Int32GtOp          -> \rs  xs    -> genPrim prof ty IntGtOp rs xs
  Int32LeOp          -> \rs  xs    -> genPrim prof ty IntLeOp rs xs
  Int32LtOp          -> \rs  xs    -> genPrim prof ty IntLtOp rs xs
  Int32NeOp          -> \rs  xs    -> genPrim prof ty IntNeOp rs xs

  Int32SraOp         -> \rs  xs    -> genPrim prof ty IntSraOp rs xs
  Int32SrlOp         -> \rs  xs    -> genPrim prof ty IntSrlOp rs xs
  Int32SllOp         -> \rs  xs    -> genPrim prof ty IntSllOp rs xs

------------------------------ Word32 -------------------------------------------

  Word32ToWordOp     -> \[r] [x]   -> PrimInline $ r |= x
  WordToWord32Op     -> \[r] [x]   -> PrimInline $ r |= x

  Word32AddOp        -> \rs  xs    -> genPrim prof ty WordAddOp rs xs
  Word32SubOp        -> \rs  xs    -> genPrim prof ty WordSubOp rs xs
  Word32MulOp        -> \rs  xs    -> genPrim prof ty WordMulOp rs xs
  Word32QuotOp       -> \rs  xs    -> genPrim prof ty WordQuotOp rs xs
  Word32RemOp        -> \rs  xs    -> genPrim prof ty WordRemOp rs xs
  Word32QuotRemOp    -> \rs  xs    -> genPrim prof ty WordQuotRemOp rs xs

  Word32EqOp         -> \rs  xs    -> genPrim prof ty WordEqOp rs xs
  Word32GeOp         -> \rs  xs    -> genPrim prof ty WordGeOp rs xs
  Word32GtOp         -> \rs  xs    -> genPrim prof ty WordGtOp rs xs
  Word32LeOp         -> \rs  xs    -> genPrim prof ty WordLeOp rs xs
  Word32LtOp         -> \rs  xs    -> genPrim prof ty WordLtOp rs xs
  Word32NeOp         -> \rs  xs    -> genPrim prof ty WordNeOp rs xs

  Word32AndOp        -> \rs xs     -> genPrim prof ty WordAndOp rs xs
  Word32OrOp         -> \rs xs     -> genPrim prof ty WordOrOp rs xs
  Word32XorOp        -> \rs xs     -> genPrim prof ty WordXorOp rs xs
  Word32NotOp        -> \rs xs     -> genPrim prof ty WordNotOp rs xs

  Word32SllOp        -> \rs xs     -> genPrim prof ty WordSllOp rs xs
  Word32SrlOp        -> \rs xs     -> genPrim prof ty WordSrlOp rs xs

------------------------------ Int64 --------------------------------------------

  Int64ToIntOp      -> \[r] [_h,l] -> PrimInline $ r |= i32 l

  Int64NegOp        -> \[r_h,r_l] [h,l] ->
      PrimInline $ mconcat
        [ r_l |= u32 (BNot l + 1)
        , r_h |= i32 (BNot h + Not r_l)
        ]

  Int64AddOp  -> \[hr,lr] [h0,l0,h1,l1] -> PrimInline $ appT [hr,lr] "h$hs_plusInt64"  [h0,l0,h1,l1]
  Int64SubOp  -> \[hr,lr] [h0,l0,h1,l1] -> PrimInline $ appT [hr,lr] "h$hs_minusInt64" [h0,l0,h1,l1]
  Int64MulOp  -> \[hr,lr] [h0,l0,h1,l1] -> PrimInline $ appT [hr,lr] "h$hs_timesInt64" [h0,l0,h1,l1]
  Int64QuotOp -> \[hr,lr] [h0,l0,h1,l1] -> PrimInline $ appT [hr,lr] "h$hs_quotInt64"  [h0,l0,h1,l1]
  Int64RemOp  -> \[hr,lr] [h0,l0,h1,l1] -> PrimInline $ appT [hr,lr] "h$hs_remInt64"   [h0,l0,h1,l1]

  Int64SllOp  -> \[hr,lr] [h,l,n] -> PrimInline $ appT [hr,lr] "h$hs_uncheckedShiftLLInt64" [h,l,n]
  Int64SraOp  -> \[hr,lr] [h,l,n] -> PrimInline $ appT [hr,lr] "h$hs_uncheckedShiftRAInt64" [h,l,n]
  Int64SrlOp  -> \[hr,lr] [h,l,n] -> PrimInline $ appT [hr,lr] "h$hs_uncheckedShiftRLInt64" [h,l,n]

  Int64ToWord64Op   -> \[r1,r2] [x1,x2] ->
      PrimInline $ mconcat
       [ r1 |= u32 x1
       , r2 |= x2
       ]
  IntToInt64Op      -> \[r1,r2] [x] ->
      PrimInline $ mconcat
       [ r1 |= if_ (x .<. 0) (-1) 0 -- sign-extension
       , r2 |= u32 x
       ]

  Int64EqOp -> \[r] [h0,l0,h1,l1] -> PrimInline $ r |= if10 (LAnd (l0 .===. l1) (h0 .===. h1))
  Int64NeOp -> \[r] [h0,l0,h1,l1] -> PrimInline $ r |= if10 (LOr (l0 .!==. l1) (h0 .!==. h1))
  Int64GeOp -> \[r] [h0,l0,h1,l1] -> PrimInline $ r |= if10 (LOr (h0 .>. h1) (LAnd (h0 .===. h1) (l0 .>=. l1)))
  Int64GtOp -> \[r] [h0,l0,h1,l1] -> PrimInline $ r |= if10 (LOr (h0 .>. h1) (LAnd (h0 .===. h1) (l0 .>. l1)))
  Int64LeOp -> \[r] [h0,l0,h1,l1] -> PrimInline $ r |= if10 (LOr (h0 .<. h1) (LAnd (h0 .===. h1) (l0 .<=. l1)))
  Int64LtOp -> \[r] [h0,l0,h1,l1] -> PrimInline $ r |= if10 (LOr (h0 .<. h1) (LAnd (h0 .===. h1) (l0 .<. l1)))

------------------------------ Word64 -------------------------------------------

  Word64ToWordOp    -> \[r] [_x1,x2] -> PrimInline $ r |= x2

  WordToWord64Op    -> \[rh,rl] [x] ->
    PrimInline $ mconcat
     [ rh |= 0
     , rl |= x
     ]

  Word64ToInt64Op   -> \[r1,r2] [x1,x2] ->
    PrimInline $ mconcat
     [ r1 |= i32 x1
     , r2 |= x2
     ]

  Word64EqOp -> \[r] [h0,l0,h1,l1] -> PrimInline $ r |= if10 (LAnd (l0 .===. l1) (h0 .===. h1))
  Word64NeOp -> \[r] [h0,l0,h1,l1] -> PrimInline $ r |= if10 (LOr (l0 .!==. l1) (h0 .!==. h1))
  Word64GeOp -> \[r] [h0,l0,h1,l1] -> PrimInline $ r |= if10 (LOr (h0 .>. h1) (LAnd (h0 .===. h1) (l0 .>=. l1)))
  Word64GtOp -> \[r] [h0,l0,h1,l1] -> PrimInline $ r |= if10 (LOr (h0 .>. h1) (LAnd (h0 .===. h1) (l0 .>. l1)))
  Word64LeOp -> \[r] [h0,l0,h1,l1] -> PrimInline $ r |= if10 (LOr (h0 .<. h1) (LAnd (h0 .===. h1) (l0 .<=. l1)))
  Word64LtOp -> \[r] [h0,l0,h1,l1] -> PrimInline $ r |= if10 (LOr (h0 .<. h1) (LAnd (h0 .===. h1) (l0 .<. l1)))

  Word64SllOp -> \[hr,lr] [h,l,n] -> PrimInline $ appT [hr,lr] "h$hs_uncheckedShiftLWord64" [h,l,n]
  Word64SrlOp -> \[hr,lr] [h,l,n] -> PrimInline $ appT [hr,lr] "h$hs_uncheckedShiftRWord64" [h,l,n]

  Word64OrOp  -> \[hr,hl] [h0, l0, h1, l1] ->
      PrimInline $ mconcat
        [ hr |= u32 (BOr h0 h1)
        , hl |= u32 (BOr l0 l1)
        ]

  Word64AndOp -> \[hr,hl] [h0, l0, h1, l1] ->
      PrimInline $ mconcat
        [ hr |= u32 (BAnd h0 h1)
        , hl |= u32 (BAnd l0 l1)
        ]

  Word64XorOp -> \[hr,hl] [h0, l0, h1, l1] ->
      PrimInline $ mconcat
        [ hr |= u32 (BXor h0 h1)
        , hl |= u32 (BXor l0 l1)
        ]

  Word64NotOp -> \[hr,hl] [h, l] ->
      PrimInline $ mconcat
        [ hr |= u32 (BNot h)
        , hl |= u32 (BNot l)
        ]

  Word64AddOp  -> \[hr,lr] [h0,l0,h1,l1] -> PrimInline $ appT [hr,lr] "h$hs_plusWord64"  [h0,l0,h1,l1]
  Word64SubOp  -> \[hr,lr] [h0,l0,h1,l1] -> PrimInline $ appT [hr,lr] "h$hs_minusWord64" [h0,l0,h1,l1]
  Word64MulOp  -> \[hr,lr] [h0,l0,h1,l1] -> PrimInline $ appT [hr,lr] "h$hs_timesWord64" [h0,l0,h1,l1]
  Word64QuotOp -> \[hr,lr] [h0,l0,h1,l1] -> PrimInline $ appT [hr,lr] "h$hs_quotWord64"  [h0,l0,h1,l1]
  Word64RemOp  -> \[hr,lr] [h0,l0,h1,l1] -> PrimInline $ appT [hr,lr] "h$hs_remWord64"   [h0,l0,h1,l1]

------------------------------ Word ---------------------------------------------

  WordAddOp  -> \[r]   [x,y] -> PrimInline $ r |= (x `Add` y) .>>>. zero_
  WordAddCOp -> \[r,c] [x,y] -> PrimInline $
      jVar \t -> mconcat
        [ t |= x `Add` y
        , r |= u32 t
        , c |= if10 (t .!==. r)
        ]
  WordSubCOp  -> \[r,c] [x,y] ->
      PrimInline $ mconcat
        [ r |= u32 (Sub x y)
        , c |= if10 (y .>. x)
        ]
  WordAdd2Op    -> \[h,l] [x,y] -> PrimInline $ appT [h,l] "h$wordAdd2" [x,y]
  WordSubOp     -> \  [r] [x,y] -> PrimInline $ r |= u32 (Sub x y)
  WordMulOp     -> \  [r] [x,y] -> PrimInline $ r |= app "h$mulWord32" [x, y]
  WordMul2Op    -> \[h,l] [x,y] -> PrimInline $ appT [h,l] "h$mul2Word32" [x,y]
  WordQuotOp    -> \  [q] [x,y] -> PrimInline $ q |= app "h$quotWord32" [x,y]
  WordRemOp     -> \  [r] [x,y] -> PrimInline $ r |= app "h$remWord32" [x,y]
  WordQuotRemOp -> \[q,r] [x,y] -> PrimInline $ appT [q,r] "h$quotRemWord32" [x,y]
  WordQuotRem2Op   -> \[q,r] [xh,xl,y] -> PrimInline $ appT [q,r] "h$quotRem2Word32" [xh,xl,y]
  WordAndOp        -> \[r] [x,y] -> PrimInline $ r |= u32 (BAnd x y)
  WordOrOp         -> \[r] [x,y] -> PrimInline $ r |= u32 (BOr  x y)
  WordXorOp        -> \[r] [x,y] -> PrimInline $ r |= u32 (BXor x y)
  WordNotOp        -> \[r] [x]   -> PrimInline $ r |= u32 (BNot x)
  WordSllOp        -> \[r] [x,y] -> PrimInline $ r |= u32 (x .<<. y)
  WordSrlOp        -> \[r] [x,y] -> PrimInline $ r |= x .>>>. y
  WordToIntOp      -> \[r] [x]   -> PrimInline $ r |= i32 x
  WordGtOp         -> \[r] [x,y] -> PrimInline $ r |= if10 (x .>.  y)
  WordGeOp         -> \[r] [x,y] -> PrimInline $ r |= if10 (x .>=. y)
  WordEqOp         -> \[r] [x,y] -> PrimInline $ r |= if10 (x .===. y)
  WordNeOp         -> \[r] [x,y] -> PrimInline $ r |= if10 (x .!==. y)
  WordLtOp         -> \[r] [x,y] -> PrimInline $ r |= if10 (x .<.  y)
  WordLeOp         -> \[r] [x,y] -> PrimInline $ r |= if10 (x .<=. y)
  WordToDoubleOp   -> \[r] [x]   -> PrimInline $ r |= (Add (BAnd x (Int 0x7FFFFFFF)) (x .>>>. (Int 31))) `Mul` Int 2147483648
  WordToFloatOp    -> \[r] [x]   -> PrimInline $ r |= (Add (BAnd x (Int 0x7FFFFFFF)) (x .>>>. (Int 31))) `Mul` Int 2147483648
  PopCnt8Op        -> \[r] [x]   -> PrimInline $ r |= var "h$popCntTab" .! (mask8 x)
  PopCnt16Op       -> \[r] [x]   -> PrimInline $ r |= Add (var "h$popCntTab" .! (mask8 x))
                                                      (var "h$popCntTab" .! (mask8 (x .>>>. Int 8)))

  PopCnt32Op  -> \[r] [x]     -> PrimInline $ r |= app "h$popCnt32" [x]
  PopCnt64Op  -> \[r] [x1,x2] -> PrimInline $ r |= app "h$popCnt64" [x1,x2]
  PopCntOp    -> \[r] [x]     -> genPrim prof ty PopCnt32Op [r] [x]
  Pdep8Op     -> \[r] [s,m]   -> PrimInline $ r |= app "h$pdep8"  [s,m]
  Pdep16Op    -> \[r] [s,m]   -> PrimInline $ r |= app "h$pdep16" [s,m]
  Pdep32Op    -> \[r] [s,m]   -> PrimInline $ r |= app "h$pdep32" [s,m]
  Pdep64Op    -> \[ra,rb] [sa,sb,ma,mb] -> PrimInline $ appT [ra,rb] "h$pdep64" [sa,sb,ma,mb]
  PdepOp      -> \rs xs                 -> genPrim prof ty Pdep32Op rs xs
  Pext8Op     -> \[r] [s,m] -> PrimInline $ r |= app "h$pext8" [s,m]
  Pext16Op    -> \[r] [s,m] -> PrimInline $ r |= app "h$pext16" [s,m]
  Pext32Op    -> \[r] [s,m] -> PrimInline $ r |= app "h$pext32" [s,m]
  Pext64Op    -> \[ra,rb] [sa,sb,ma,mb] -> PrimInline $ appT [ra,rb] "h$pext64" [sa,sb,ma,mb]
  PextOp      -> \rs xs     -> genPrim prof ty Pext32Op rs xs

  ClzOp       -> \[r]   [x]     -> PrimInline $ r |= app "h$clz32" [x]
  Clz8Op      -> \[r]   [x]     -> PrimInline $ r |= app "h$clz8"  [x]
  Clz16Op     -> \[r]   [x]     -> PrimInline $ r |= app "h$clz16" [x]
  Clz32Op     -> \[r]   [x]     -> PrimInline $ r |= app "h$clz32" [x]
  Clz64Op     -> \[r]   [x1,x2] -> PrimInline $ r |= app "h$clz64" [x1,x2]
  CtzOp       -> \[r]   [x]     -> PrimInline $ r |= app "h$ctz32" [x]
  Ctz8Op      -> \[r]   [x]     -> PrimInline $ r |= app "h$ctz8"  [x]
  Ctz16Op     -> \[r]   [x]     -> PrimInline $ r |= app "h$ctz16" [x]
  Ctz32Op     -> \[r]   [x]     -> PrimInline $ r |= app "h$ctz32" [x]
  Ctz64Op     -> \[r]   [x1,x2] -> PrimInline $ r |= app "h$ctz64" [x1,x2]

  BSwap16Op   -> \[r] [x]   -> PrimInline $
      r |= BOr ((mask8 x) .<<. (Int 8))
               (mask8 (x .>>>. (Int 8)))
  BSwap32Op   -> \[r] [x]   -> PrimInline $
      r |= u32 ((x .<<. (Int 24))
            `BOr` ((BAnd x (Int 0xFF00)) .<<. (Int 8))
            `BOr` ((BAnd x (Int 0xFF0000)) .>>. (Int 8))
            `BOr` (x .>>>. (Int 24)))
  BSwap64Op   -> \[r1,r2] [x,y] -> PrimInline $ appT [r1,r2] "h$bswap64" [x,y]
  BSwapOp     -> \[r] [x]       -> genPrim prof ty BSwap32Op [r] [x]

------------------------------ Narrow -------------------------------------------

  Narrow8IntOp    -> \[r] [x] -> PrimInline $ r |= (BAnd x (Int 0x7F)) `Sub` (BAnd x (Int 0x80))
  Narrow16IntOp   -> \[r] [x] -> PrimInline $ r |= (BAnd x (Int 0x7FFF)) `Sub` (BAnd x (Int 0x8000))
  Narrow32IntOp   -> \[r] [x] -> PrimInline $ r |= i32 x
  Narrow8WordOp   -> \[r] [x] -> PrimInline $ r |= mask8 x
  Narrow16WordOp  -> \[r] [x] -> PrimInline $ r |= mask16 x
  Narrow32WordOp  -> \[r] [x] -> PrimInline $ r |= u32 x

------------------------------ Double -------------------------------------------

  DoubleGtOp        -> \[r] [x,y] -> PrimInline $ r |= if10 (x .>. y)
  DoubleGeOp        -> \[r] [x,y] -> PrimInline $ r |= if10 (x .>=. y)
  DoubleEqOp        -> \[r] [x,y] -> PrimInline $ r |= if10 (x .===. y)
  DoubleNeOp        -> \[r] [x,y] -> PrimInline $ r |= if10 (x .!==. y)
  DoubleLtOp        -> \[r] [x,y] -> PrimInline $ r |= if10 (x .<. y)
  DoubleLeOp        -> \[r] [x,y] -> PrimInline $ r |= if10 (x .<=. y)
  DoubleAddOp       -> \[r] [x,y] -> PrimInline $ r |= Add x y
  DoubleSubOp       -> \[r] [x,y] -> PrimInline $ r |= Sub x y
  DoubleMulOp       -> \[r] [x,y] -> PrimInline $ r |= Mul x y
  DoubleDivOp       -> \[r] [x,y] -> PrimInline $ r |= Div x y
  DoubleNegOp       -> \[r] [x]   -> PrimInline $ r |= Negate x
  DoubleFabsOp      -> \[r] [x]   -> PrimInline $ r |= math_abs [x]
  DoubleToIntOp     -> \[r] [x]   -> PrimInline $ r |= i32 x
  DoubleToFloatOp   -> \[r] [x]   -> PrimInline $ r |= app "h$fround" [x]
  DoubleExpOp       -> \[r] [x]   -> PrimInline $ r |= math_exp [x]
  DoubleLogOp       -> \[r] [x]   -> PrimInline $ r |= math_log [x]
  DoubleSqrtOp      -> \[r] [x]   -> PrimInline $ r |= math_sqrt [x]
  DoubleSinOp       -> \[r] [x]   -> PrimInline $ r |= math_sin [x]
  DoubleCosOp       -> \[r] [x]   -> PrimInline $ r |= math_cos [x]
  DoubleTanOp       -> \[r] [x]   -> PrimInline $ r |= math_tan [x]
  DoubleAsinOp      -> \[r] [x]   -> PrimInline $ r |= math_asin [x]
  DoubleAcosOp      -> \[r] [x]   -> PrimInline $ r |= math_acos [x]
  DoubleAtanOp      -> \[r] [x]   -> PrimInline $ r |= math_atan [x]
  DoubleSinhOp      -> \[r] [x]   -> PrimInline $ r |= (math_exp [x] `Sub` math_exp [Negate x]) `Div` two_
  DoubleCoshOp      -> \[r] [x]   -> PrimInline $ r |= (math_exp [x] `Add` math_exp [Negate x]) `Div` two_
  DoubleTanhOp      -> \[r] [x]   -> PrimInline $ r |= (math_exp [Mul two_ x] `Sub` one_) `Div` (math_exp [Mul two_ x] `Add` one_)
  DoubleAsinhOp     -> \[r] [x]   -> PrimInline $ r |= math_asinh [x]
  DoubleAcoshOp     -> \[r] [x]   -> PrimInline $ r |= math_acosh [x]
  DoubleAtanhOp     -> \[r] [x]   -> PrimInline $ r |= math_atanh [x]
  DoublePowerOp     -> \[r] [x,y] -> PrimInline $ r |= math_pow [x,y]
  DoubleDecode_2IntOp  -> \[s,h,l,e] [x] -> PrimInline $ appT [s,h,l,e] "h$decodeDouble2Int" [x]
  DoubleDecode_Int64Op -> \[s1,s2,e] [d] -> PrimInline $ appT [e,s1,s2] "h$decodeDoubleInt64" [d]

------------------------------ Float --------------------------------------------

  FloatGtOp         -> \[r] [x,y] -> PrimInline $ r |= if10 (x .>. y)
  FloatGeOp         -> \[r] [x,y] -> PrimInline $ r |= if10 (x .>=. y)
  FloatEqOp         -> \[r] [x,y] -> PrimInline $ r |= if10 (x .===. y)
  FloatNeOp         -> \[r] [x,y] -> PrimInline $ r |= if10 (x .!==. y)
  FloatLtOp         -> \[r] [x,y] -> PrimInline $ r |= if10 (x .<. y)
  FloatLeOp         -> \[r] [x,y] -> PrimInline $ r |= if10 (x .<=. y)
  FloatAddOp        -> \[r] [x,y] -> PrimInline $ r |= Add x y
  FloatSubOp        -> \[r] [x,y] -> PrimInline $ r |= Sub x y
  FloatMulOp        -> \[r] [x,y] -> PrimInline $ r |= Mul x y
  FloatDivOp        -> \[r] [x,y] -> PrimInline $ r |= Div x y
  FloatNegOp        -> \[r] [x]   -> PrimInline $ r |= Negate x
  FloatFabsOp       -> \[r] [x]   -> PrimInline $ r |= math_abs [x]
  FloatToIntOp      -> \[r] [x]   -> PrimInline $ r |= i32 x
  FloatExpOp        -> \[r] [x]   -> PrimInline $ r |= math_exp [x]
  FloatLogOp        -> \[r] [x]   -> PrimInline $ r |= math_log [x]
  FloatSqrtOp       -> \[r] [x]   -> PrimInline $ r |= math_sqrt [x]
  FloatSinOp        -> \[r] [x]   -> PrimInline $ r |= math_sin [x]
  FloatCosOp        -> \[r] [x]   -> PrimInline $ r |= math_cos [x]
  FloatTanOp        -> \[r] [x]   -> PrimInline $ r |= math_tan [x]
  FloatAsinOp       -> \[r] [x]   -> PrimInline $ r |= math_asin [x]
  FloatAcosOp       -> \[r] [x]   -> PrimInline $ r |= math_acos [x]
  FloatAtanOp       -> \[r] [x]   -> PrimInline $ r |= math_atan [x]
  FloatSinhOp       -> \[r] [x]   -> PrimInline $ r |= (math_exp [x] `Sub` math_exp [Negate x]) `Div` two_
  FloatCoshOp       -> \[r] [x]   -> PrimInline $ r |= (math_exp [x] `Add` math_exp [Negate x]) `Div` two_
  FloatTanhOp       -> \[r] [x]   -> PrimInline $ r |= (math_exp [Mul two_ x] `Sub` one_) `Div` (math_exp [Mul two_ x] `Add` one_)
  FloatAsinhOp      -> \[r] [x]   -> PrimInline $ r |= math_asinh [x]
  FloatAcoshOp      -> \[r] [x]   -> PrimInline $ r |= math_acosh [x]
  FloatAtanhOp      -> \[r] [x]   -> PrimInline $ r |= math_atanh [x]
  FloatPowerOp      -> \[r] [x,y] -> PrimInline $ r |= math_pow [x,y]
  FloatToDoubleOp   -> \[r] [x]   -> PrimInline $ r |= x
  FloatDecode_IntOp -> \[s,e] [x] -> PrimInline $ appT [s,e] "h$decodeFloatInt" [x]

------------------------------ Arrays -------------------------------------------

  NewArrayOp           -> \[r] [l,e]   -> PrimInline (newArray r l e)
  ReadArrayOp          -> \[r] [a,i]   -> PrimInline $ r |= a .! i
  WriteArrayOp         -> \[]  [a,i,v] -> PrimInline $ a .! i |= v
  SizeofArrayOp        -> \[r] [a]     -> PrimInline $ r |= a .^ "length"
  SizeofMutableArrayOp -> \[r] [a]     -> PrimInline $ r |= a .^ "length"
  IndexArrayOp         -> \[r] [a,i]   -> PrimInline $ r |= a .! i
  UnsafeFreezeArrayOp  -> \[r] [a]     -> PrimInline $ r |= a
  UnsafeThawArrayOp    -> \[r] [a]     -> PrimInline $ r |= a
  CopyArrayOp          -> \[] [a,o1,ma,o2,n] ->
      PrimInline $ loopBlockS (Int 0) (.<. n) \i ->
        [ ma .! (Add i o2) |= a .! (Add i o1)
        , preIncrS i
        ]
  CopyMutableArrayOp  -> \[]  [a1,o1,a2,o2,n] -> genPrim prof ty CopyArrayOp [] [a1,o1,a2,o2,n]
  CloneArrayOp        -> \[r] [a,start,n]     -> PrimInline $ r |= app "h$sliceArray" [a,start,n]
  CloneMutableArrayOp -> \[r] [a,start,n]     -> genPrim prof ty CloneArrayOp [r] [a,start,n]
  FreezeArrayOp       -> \[r] [a,start,n]     -> PrimInline $ r |= app "h$sliceArray" [a,start,n]
  ThawArrayOp         -> \[r] [a,start,n]     -> PrimInline $ r |= app "h$sliceArray" [a,start,n]
  CasArrayOp          -> \[s,o] [a,i,old,new] -> PrimInline $
      jVar \x -> mconcat
        [ x |= a .! i
        , ifBlockS (x .===. old)
                   [ o |= new
                   , a .! i |= new
                   , s |= zero_
                   ]
                   [ s |= one_
                   , o |= x
                   ]
        ]

------------------------------ Small Arrays -------------------------------------

  NewSmallArrayOp            -> \[a]   [n,e]         -> PrimInline $ a |= app "h$newArray" [n,e]
  ReadSmallArrayOp           -> \[r]   [a,i]         -> PrimInline $ r |= a .! i
  WriteSmallArrayOp          -> \[]    [a,i,e]       -> PrimInline $ a .! i |= e
  SizeofSmallArrayOp         -> \[r]   [a]           -> PrimInline $ r |= a .^ "length"
  SizeofSmallMutableArrayOp  -> \[r]   [a]           -> PrimInline $ r |= a .^ "length"
  IndexSmallArrayOp          -> \[r]   [a,i]         -> PrimInline $ r |= a .! i
  UnsafeFreezeSmallArrayOp   -> \[r]   [a]           -> PrimInline $ r |= a
  UnsafeThawSmallArrayOp     -> \[r]   [a]           -> PrimInline $ r |= a
  CopySmallArrayOp           -> \[]    [s,si,d,di,n] -> PrimInline $
      loopBlockS (Sub n one_) (.>=. zero_) \i ->
        [ d .! (Add di i) |= s .! (Add si i)
        , postDecrS i
        ]
  CopySmallMutableArrayOp    -> \[]    [s,si,d,di,n] -> PrimInline $
      loopBlockS (Sub n one_) (.>=. zero_) \i ->
        [ d .! (Add di i) |= s .! (Add si i)
        , postDecrS i
        ]
  CloneSmallArrayOp          -> \[r]   [a,o,n]       -> PrimInline $ cloneArray r a (Just o) n
  CloneSmallMutableArrayOp   -> \[r]   [a,o,n]       -> PrimInline $ cloneArray r a (Just o) n
  FreezeSmallArrayOp         -> \[r]   [a,o,n]       -> PrimInline $ cloneArray r a (Just o) n
  ThawSmallArrayOp           -> \[r]   [a,o,n]       -> PrimInline $ cloneArray r a (Just o) n
  CasSmallArrayOp            -> \[s,o] [a,i,old,new] -> PrimInline $ jVar \x -> mconcat
                                                                [ x |= a .! i
                                                                , ifBlockS (x .===. old)
                                                                    [ o |= new
                                                                    , a .! i |= new
                                                                    , s |= zero_
                                                                    ]
                                                                    [ s |= one_
                                                                    , o |= x
                                                                    ]
                                                                ]

------------------------------- Byte Arrays -------------------------------------

  NewByteArrayOp_Char               -> \[r] [l]         -> PrimInline (newByteArray r l)
  NewPinnedByteArrayOp_Char         -> \[r] [l]         -> PrimInline (newByteArray r l)
  NewAlignedPinnedByteArrayOp_Char  -> \[r] [l,_align]  -> PrimInline (newByteArray r l)
  MutableByteArrayIsPinnedOp        -> \[r] [_]         -> PrimInline $ r |= one_
  ByteArrayIsPinnedOp               -> \[r] [_]         -> PrimInline $ r |= one_
  ByteArrayContents_Char            -> \[a,o] [b]       -> PrimInline $ mconcat [a |= b, o |= zero_]
  MutableByteArrayContents_Char     -> \[a,o] [b]       -> PrimInline $ mconcat [a |= b, o |= zero_]
  ShrinkMutableByteArrayOp_Char     -> \[] [a,n]        -> PrimInline $ appS "h$shrinkMutableByteArray" [a,n]
  ResizeMutableByteArrayOp_Char     -> \[r] [a,n]       -> PrimInline $ r |= app "h$resizeMutableByteArray" [a,n]
  UnsafeFreezeByteArrayOp           -> \[a] [b]         -> PrimInline $ a |= b
  SizeofByteArrayOp                 -> \[r] [a]         -> PrimInline $ r |= a .^ "len"
  SizeofMutableByteArrayOp          -> \[r] [a]         -> PrimInline $ r |= a .^ "len"
  GetSizeofMutableByteArrayOp       -> \[r] [a]         -> PrimInline $ r |= a .^ "len"
  IndexByteArrayOp_Char             -> \[r] [a,i]       -> PrimInline $ r |= u8_ a i
  IndexByteArrayOp_WideChar         -> \[r] [a,i]       -> PrimInline $ r |= i32_ a i
  IndexByteArrayOp_Int              -> \[r] [a,i]       -> PrimInline $ r |= i32_ a i
  IndexByteArrayOp_Word             -> \[r] [a,i]       -> PrimInline $ r |= u32_ a i
  IndexByteArrayOp_Addr             -> \[r1,r2] [a,i]   ->
    PrimInline $ jVar \t -> mconcat
      [ t |= a .^ "arr"
      , ifBlockS (t .&&. t .! (i .<<. two_))
          [ r1 |= t .! (i .<<. two_) .! zero_
          , r2 |= t .! (i .<<. two_) .! one_
          ]
          [ r1 |= null_
          , r2 |= zero_
          ]
      ]

  IndexByteArrayOp_Float     -> \[r]     [a,i] -> PrimInline $ r |= f3_ a i
  IndexByteArrayOp_Double    -> \[r]     [a,i] -> PrimInline $ r |= f6_ a i
  IndexByteArrayOp_StablePtr -> \[r1,r2] [a,i] ->
    PrimInline $ mconcat
      [ r1 |= var "h$stablePtrBuf"
      , r2 |= i32_ a i
      ]
  IndexByteArrayOp_Int8  -> \[r] [a,i]      -> PrimInline $ r |= dv_i8 a i
  IndexByteArrayOp_Int16 -> \[r] [a,i]      -> PrimInline $ r |= dv_i16 a (i .<<. one_)
  IndexByteArrayOp_Int32 -> \[r] [a,i]      -> PrimInline $ r |= i32_ a i
  IndexByteArrayOp_Int64 -> \[h,l] [a,i]    -> PrimInline $ mconcat
                                                     [ h |= i32_ a (Add (i .<<. one_) one_)
                                                     , l |= u32_ a (i .<<. one_)
                                                     ]
  IndexByteArrayOp_Word8  -> \[r] [a,i]     -> PrimInline $ r |= u8_ a i
  IndexByteArrayOp_Word16 -> \[r] [a,i]     -> PrimInline $ r |= dv_u16 a (i .<<. one_)
  IndexByteArrayOp_Word32 -> \[r] [a,i]     -> PrimInline $ r |= u32_ a i
  IndexByteArrayOp_Word64 -> \[h,l] [a,i] -> PrimInline $ mconcat
                                                      [ h |= u32_ a (Add (i .<<. one_) one_)
                                                      , l |= u32_ a (i .<<. one_)
                                                      ]
  ReadByteArrayOp_Char     -> \[r]     [a,i] -> PrimInline $ r |= u8_ a i
  ReadByteArrayOp_WideChar -> \[r]     [a,i] -> PrimInline $ r |= i32_ a i
  ReadByteArrayOp_Int      -> \[r]     [a,i] -> PrimInline $ r |= i32_ a i
  ReadByteArrayOp_Word     -> \[r]     [a,i] -> PrimInline $ r |= u32_ a i
  ReadByteArrayOp_Addr     -> \[r1,r2] [a,i] ->
      PrimInline $ jVar \x -> mconcat
        [ x |= i .<<. two_
        , ifS (a .^ "arr" .&&. a .^ "arr" .! x)
               (mconcat [ r1 |= a .^ "arr" .! x .! zero_
                        , r2 |= a .^ "arr" .! x .! one_
                        ])
               (mconcat [r1 |= null_, r2 |= one_])
        ]
  ReadByteArrayOp_Float     -> \[r]     [a,i] -> PrimInline $ r |= f3_ a i
  ReadByteArrayOp_Double    -> \[r]     [a,i] -> PrimInline $ r |= f6_ a i
  ReadByteArrayOp_StablePtr -> \[r1,r2] [a,i] ->
      PrimInline $ mconcat
       [ r1 |= var "h$stablePtrBuf"
       , r2 |= i32_ a i
       ]
  ReadByteArrayOp_Int8  -> \[r]     [a,i] -> PrimInline $ r |= dv_i8 a i
  ReadByteArrayOp_Int16 -> \[r]     [a,i] -> PrimInline $ r |= dv_i16 a (i .<<. one_)
  ReadByteArrayOp_Int32 -> \[r]     [a,i] -> PrimInline $ r |= i32_ a i
  ReadByteArrayOp_Int64 -> \[h,l]   [a,i] ->
      PrimInline $ mconcat
        [ h |= i32_ a (Add (i .<<. one_) one_)
        , l |= u32_ a (i .<<. one_)
        ]
  ReadByteArrayOp_Word8  -> \[r]     [a,i] -> PrimInline $ r |= u8_ a i
  ReadByteArrayOp_Word16 -> \[r]     [a,i] -> PrimInline $ r |= u1_ a i
  ReadByteArrayOp_Word32 -> \[r]     [a,i] -> PrimInline $ r |= u32_ a i
  ReadByteArrayOp_Word64 -> \[h,l]   [a,i] ->
      PrimInline $ mconcat
        [ h |= u32_ a (Add (i .<<. one_) one_)
        , l |= u32_ a (i .<<. one_)
        ]
  WriteByteArrayOp_Char     -> \[] [a,i,e]     -> PrimInline $ u8_ a i |= e
  WriteByteArrayOp_WideChar -> \[] [a,i,e]     -> PrimInline $ i32_ a i |= e
  WriteByteArrayOp_Int      -> \[] [a,i,e]     -> PrimInline $ i32_ a i |= e
  WriteByteArrayOp_Word     -> \[] [a,i,e]     -> PrimInline $ i32_ a i |= i32 e
  WriteByteArrayOp_Addr     -> \[] [a,i,e1,e2] ->
    PrimInline $ mconcat
      [ ifS (Not (a .^ "arr")) (a .^ "arr" |= ValExpr (JList [])) mempty
      , a .^ "arr" .! (i .<<. two_) |= ValExpr (JList [e1, e2])
      ]
  WriteByteArrayOp_Float     -> \[] [a,i,e]      -> PrimInline $ f3_ a i |= e
  WriteByteArrayOp_Double    -> \[] [a,i,e]      -> PrimInline $ f6_ a i |= e
  WriteByteArrayOp_StablePtr -> \[] [a,i,_e1,e2] -> PrimInline $ i32_ a i |= e2

  WriteByteArrayOp_Int8  -> \[] [a,i,e]     -> PrimInline $ dv_s_i8 a i e
  WriteByteArrayOp_Int16 -> \[] [a,i,e]     -> PrimInline $ dv_s_i16 a (i .<<. one_) e
  WriteByteArrayOp_Int32 -> \[] [a,i,e]     -> PrimInline $ i32_ a i |= e
  WriteByteArrayOp_Int64 -> \[] [a,i,e1,e2] ->
      PrimInline $ mconcat
        [ i32_ a (Add (i .<<. one_) one_) |= e1
        , i32_ a (i .<<. one_)            |= i32 e2
        ]
  WriteByteArrayOp_Word8  -> \[] [a,i,e]     -> PrimInline $ u8_ a i |= e
  WriteByteArrayOp_Word16 -> \[] [a,i,e]     -> PrimInline $ u1_ a i |= e
  WriteByteArrayOp_Word32 -> \[] [a,i,e]     -> PrimInline $ i32_ a i |= i32 e
  WriteByteArrayOp_Word64 -> \[] [a,i,h,l] ->
      PrimInline $ mconcat
        [ i32_ a (Add (i .<<. one_) one_) |= i32 h
        , i32_ a (i .<<. one_)            |= i32 l
        ]
  CompareByteArraysOp -> \[r] [a1,o1,a2,o2,n] ->
      PrimInline $ r |= app "h$compareByteArrays" [a1,o1,a2,o2,n]

  CopyByteArrayOp -> \[] [a1,o1,a2,o2,n] ->
      PrimInline $ loopBlockS (Sub n one_) (.>=. zero_) \i ->
        [ u8_ a2 (Add i o2) |= u8_ a1 (Add i o1)
        , postDecrS i
        ]
  CopyMutableByteArrayOp       -> \[] xs@[_a1,_o1,_a2,_o2,_n] -> genPrim prof ty CopyByteArrayOp [] xs
  CopyByteArrayToAddrOp        -> \[] xs@[_a1,_o1,_a2,_o2,_n] -> genPrim prof ty CopyByteArrayOp [] xs
  CopyMutableByteArrayToAddrOp -> \[] xs@[_a1,_o1,_a2,_o2,_n] -> genPrim prof ty CopyByteArrayOp [] xs
  CopyAddrToByteArrayOp        -> \[] xs@[_ba,_bo,_aa,_ao,_n] -> genPrim prof ty CopyByteArrayOp [] xs

  SetByteArrayOp -> \[] [a,o,n,v] ->
      PrimInline $ loopBlockS zero_ (.<. n) \i ->
        [ u8_ a (Add o i) |= v
        , postIncrS i
        ]

  AtomicReadByteArrayOp_Int  -> \[r]   [a,i]         -> PrimInline $ r |= i32_ a i
  AtomicWriteByteArrayOp_Int -> \[]    [a,i,v]       -> PrimInline $ i32_ a i |= v
  CasByteArrayOp_Int         -> \[r]   [a,i,old,new] -> PrimInline $
      jVar \t -> mconcat
        [ t |= i32_ a i
        , r |= t
        , ifS (t .===. old) (i32_ a i |= new) mempty
        ]
  FetchAddByteArrayOp_Int    -> \[r]   [a,i,v] -> PrimInline $ fetchOpByteArray Add  r a i v
  FetchSubByteArrayOp_Int    -> \[r]   [a,i,v] -> PrimInline $ fetchOpByteArray Sub  r a i v
  FetchAndByteArrayOp_Int    -> \[r]   [a,i,v] -> PrimInline $ fetchOpByteArray BAnd r a i v
  FetchOrByteArrayOp_Int     -> \[r]   [a,i,v] -> PrimInline $ fetchOpByteArray BOr  r a i v
  FetchNandByteArrayOp_Int   -> \[r]   [a,i,v] -> PrimInline $ fetchOpByteArray (\x y -> BNot (BAnd x y)) r a i v
  FetchXorByteArrayOp_Int    -> \[r]   [a,i,v] -> PrimInline $ fetchOpByteArray BXor r a i v

------------------------------- Addr# ------------------------------------------

  AddrAddOp   -> \[a',o'] [a,o,i]         -> PrimInline $ mconcat [a' |= a, o' |= Add o i]
  AddrSubOp   -> \[i]     [_a1,o1,_a2,o2] -> PrimInline $ i |= Sub o1 o2
  AddrRemOp   -> \[r]     [_a,o,i]        -> PrimInline $ r |= Mod o i
  AddrToIntOp -> \[i]     [_a,o]          -> PrimInline $ i |= o -- only usable for comparisons within one range
  IntToAddrOp -> \[a,o]   [i]             -> PrimInline $ mconcat [a |= null_, o |= i]
  AddrGtOp -> \[r] [a1,o1,a2,o2] -> PrimInline $ r |= if10 (app "h$comparePointer" [a1,o1,a2,o2] .>. zero_)
  AddrGeOp -> \[r] [a1,o1,a2,o2] -> PrimInline $ r |= if10 (app "h$comparePointer" [a1,o1,a2,o2] .>=. zero_)
  AddrEqOp -> \[r] [a1,o1,a2,o2] -> PrimInline $ r |= if10 (app "h$comparePointer" [a1,o1,a2,o2] .===. zero_)
  AddrNeOp -> \[r] [a1,o1,a2,o2] -> PrimInline $ r |= if10 (app "h$comparePointer" [a1,o1,a2,o2] .!==. zero_)
  AddrLtOp -> \[r] [a1,o1,a2,o2] -> PrimInline $ r |= if10 (app "h$comparePointer" [a1,o1,a2,o2] .<. zero_)
  AddrLeOp -> \[r] [a1,o1,a2,o2] -> PrimInline $ r |= if10 (app "h$comparePointer" [a1,o1,a2,o2] .<=. zero_)

------------------------------- Addr Indexing: Unboxed Arrays -------------------

  IndexOffAddrOp_Char     -> \[c] [a,o,i] -> PrimInline $ c |= u8_ a    (off8  o i)
  IndexOffAddrOp_WideChar -> \[c] [a,o,i] -> PrimInline $ c |= dv_u32 a (off32 o i)
  IndexOffAddrOp_Int      -> \[c] [a,o,i] -> PrimInline $ c |= dv_i32 a (off32 o i)
  IndexOffAddrOp_Word     -> \[c] [a,o,i] -> PrimInline $ c |= dv_i32 a (off32 o i)
  IndexOffAddrOp_Addr     -> \[ca,co] [a,o,i] ->
      PrimInline $ ifBlockS (a .^ "arr " .&&. a .^ "arr" .! (i .<<. two_))
                       [ ca |= a .^ "arr" .! (off32 o i) .! zero_
                       ,  co |= a .^ "arr" .! (off32 o i) .! one_
                       ]
                       [ ca |= null_
                       , co |= zero_
                       ]
  IndexOffAddrOp_Float     -> \[c]     [a,o,i] -> PrimInline $ c |= dv_f32 a (off32 o i)
  IndexOffAddrOp_Double    -> \[c]     [a,o,i] -> PrimInline $ c |= dv_f64 a (off64 o i)
  IndexOffAddrOp_StablePtr -> \[c1,c2] [a,o,i] -> PrimInline $ mconcat
                                                          [ c1 |= var "h$stablePtrBuf"
                                                          , c2 |= dv_i32 a (off32 o i)
                                                          ]
  IndexOffAddrOp_Int8  -> \[c]     [a,o,i] -> PrimInline $ c |= u8_    a (off8  o i)
  IndexOffAddrOp_Int16 -> \[c]     [a,o,i] -> PrimInline $ c |= dv_i16 a (off16 o i)
  IndexOffAddrOp_Int32 -> \[c]     [a,o,i] -> PrimInline $ c |= dv_i32 a (off32 o i)
  IndexOffAddrOp_Int64 -> \[c1,c2] [a,o,i] ->
      PrimInline $ mconcat
       [ c1 |= dv_i32 a (Add (off64 o i) (Int 4))
       , c2 |= dv_i32 a (off64 o i)
       ]
  IndexOffAddrOp_Word8  -> \[c]     [a,o,i] -> PrimInline $ c |= u8_    a (off8  o i)
  IndexOffAddrOp_Word16 -> \[c]     [a,o,i] -> PrimInline $ c |= dv_u16 a (off16 o i)
  IndexOffAddrOp_Word32 -> \[c]     [a,o,i] -> PrimInline $ c |= dv_i32 a (off32 o i)
  IndexOffAddrOp_Word64 -> \[c1,c2] [a,o,i] ->
      PrimInline $ mconcat
       [ c1 |= dv_i32 a (Add (off64 o i) (Int 4))
       , c2 |= dv_i32 a (off64 o i)
       ]
  ReadOffAddrOp_Char     -> \[c] [a,o,i] -> PrimInline $ c |= u8_    a (off8  o i)
  ReadOffAddrOp_WideChar -> \[c] [a,o,i] -> PrimInline $ c |= dv_u32 a (off32 o i)
  ReadOffAddrOp_Int      -> \[c] [a,o,i] -> PrimInline $ c |= dv_i32 a (off32 o i)
  ReadOffAddrOp_Word     -> \[c] [a,o,i] -> PrimInline $ c |= dv_i32 a (off32 o i)
  ReadOffAddrOp_Addr     -> \[c1,c2] [a,o,i] ->
      PrimInline $ jVar \x -> mconcat
        [ x |= i .<<. two_
        , ifBlockS  (a .^ "arr" .&&. a .^ "arr" .! (Add o x))
              [ c1 |= a .^ "arr" .! (Add o x) .! zero_
              , c2 |= a .^ "arr" .! (Add o x) .! one_
              ]
              [ c1 |= null_
              , c2 |= zero_
              ]
        ]
  ReadOffAddrOp_Float     -> \[c]     [a,o,i] -> PrimInline $ c |= dv_f32 a (off32 o i)
  ReadOffAddrOp_Double    -> \[c]     [a,o,i] -> PrimInline $ c |= dv_f64 a (off64 o i)
  ReadOffAddrOp_StablePtr -> \[c1,c2] [a,o,i] -> PrimInline $ mconcat
                                                        [ c1 |= var "h$stablePtrBuf"
                                                        , c2 |= dv_u32 a (off32 o i)
                                                        ]
  ReadOffAddrOp_Int8   -> \[c]     [a,o,i] -> PrimInline $ AssignStat c $ dv_i8  a (off8  o i)
  ReadOffAddrOp_Int16  -> \[c]     [a,o,i] -> PrimInline $ AssignStat c $ dv_i16 a (off16 o i)
  ReadOffAddrOp_Int32  -> \[c]     [a,o,i] -> PrimInline $ AssignStat c $ dv_i32 a (off32 o i)
  ReadOffAddrOp_Int64  -> \[c1,c2] [a,o,i] ->
      PrimInline $ mconcat
        [ c1 |= dv_i32 a (Add (off64 o i) (Int 4))
        , c2 |= dv_i32 a (off64 o i)
        ]
  ReadOffAddrOp_Word8  -> \[c]     [a,o,i] -> PrimInline $ AssignStat c $ u8_    a (off8  o i)
  ReadOffAddrOp_Word16 -> \[c]     [a,o,i] -> PrimInline $ AssignStat c $ dv_u16 a (off16 o i)
  ReadOffAddrOp_Word32 -> \[c]     [a,o,i] -> PrimInline $ AssignStat c $ dv_i32 a (off32 o i)
  ReadOffAddrOp_Word64 -> \[c1,c2] [a,o,i] ->
      PrimInline $ mconcat
       [ c1 |= dv_i32 a (Add (off64 o i) (Int 4))
       , c2 |= dv_i32 a (off64 o i)
       ]
  WriteOffAddrOp_Char     -> \[] [a,o,i,v]     -> PrimInline $ u8_      a (off8  o i) |= v
  WriteOffAddrOp_WideChar -> \[] [a,o,i,v]     -> PrimInline $ dv_s_u32 a (off32 o i) v
  WriteOffAddrOp_Int      -> \[] [a,o,i,v]     -> PrimInline $ dv_s_i32 a (off32 o i) v
  WriteOffAddrOp_Word     -> \[] [a,o,i,v]     -> PrimInline $ dv_s_i32 a (off32 o i) v
  WriteOffAddrOp_Addr     -> \[] [a,o,i,va,vo] ->
      PrimInline $ mconcat
        [ ifS (Not (a .^ "arr")) (a .^ "arr" |= ValExpr (JList [])) mempty
        , AssignStat (a .^ "arr" .! (off32 o i)) $ ValExpr (JList [va, vo])
        ]
  WriteOffAddrOp_Float     -> \[] [a,o,i,v]      -> PrimInline $ dv_s_f32 a (off32 o i) v
  WriteOffAddrOp_Double    -> \[] [a,o,i,v]      -> PrimInline $ dv_s_f64 a (off64 o i) v
  WriteOffAddrOp_StablePtr -> \[] [a,o,i,_v1,v2] -> PrimInline $ dv_s_u32 a (off32 o i) v2
  WriteOffAddrOp_Int8      -> \[] [a,o,i,v]      -> PrimInline $ dv_s_i8  a (off8  o i) v
  WriteOffAddrOp_Int16     -> \[] [a,o,i,v]      -> PrimInline $ dv_s_i16 a (off16 o i) v
  WriteOffAddrOp_Int32     -> \[] [a,o,i,v]      -> PrimInline $ dv_s_i32 a (off32 o i) v
  WriteOffAddrOp_Int64     -> \[] [a,o,i,v1,v2]  -> PrimInline $ mconcat
                                                          [ dv_s_i32 a (Add (off64 o i) (Int 4)) v1
                                                          , dv_s_i32 a (off64 o i) v2
                                                          ]
  WriteOffAddrOp_Word8     -> \[] [a,o,i,v]      -> PrimInline $ u8_      a (off8  o i) |= v
  WriteOffAddrOp_Word16    -> \[] [a,o,i,v]      -> PrimInline $ dv_s_u16 a (off16 o i) v
  WriteOffAddrOp_Word32    -> \[] [a,o,i,v]      -> PrimInline $ dv_s_i32 a (off32 o i) v
  WriteOffAddrOp_Word64    -> \[] [a,o,i,v1,v2]  -> PrimInline $ mconcat
                                                          [ dv_s_i32 a (Add (off64 o i) (Int 4)) v1
                                                          , dv_s_i32 a (off64 o i) v2
                                                          ]
-- Mutable variables
  NewMutVarOp           -> \[r] [x]       -> PrimInline $ r |= New (app "h$MutVar" [x])
  ReadMutVarOp          -> \[r] [m]       -> PrimInline $ r |= m .^ "val"
  WriteMutVarOp         -> \[] [m,x]      -> PrimInline $ m .^ "val" |= x
  AtomicModifyMutVar2Op -> \[r1,r2] [m,f] -> PrimInline $ appT [r1,r2] "h$atomicModifyMutVar2" [m,f]
  AtomicModifyMutVar_Op -> \[r1,r2] [m,f] -> PrimInline $ appT [r1,r2] "h$atomicModifyMutVar" [m,f]

  CasMutVarOp -> \[status,r] [mv,o,n] -> PrimInline $ ifS (mv .^ "val" .===. o)
                   (mconcat [status |= zero_, r |= n, mv .^ "val" |= n])
                   (mconcat [status |= one_ , r |= mv .^ "val"])

------------------------------- Exceptions --------------------------------------

  CatchOp -> \[_r] [a,handler] -> PRPrimCall $ returnS (app "h$catch" [a, handler])

                             -- fully ignore the result arity as it can use 1 or 2
                             -- slots, depending on the return type.
  RaiseOp                 -> \_r [a] -> PRPrimCall $ returnS (app "h$throw" [a, false_])
  RaiseIOOp               -> \_r [a] -> PRPrimCall $ returnS (app "h$throw" [a, false_])
  RaiseUnderflowOp        -> \_r []  -> PRPrimCall $ returnS (app "h$throw" [var "h$baseZCGHCziExceptionziTypeziunderflowException", false_])
  RaiseOverflowOp         -> \_r []  -> PRPrimCall $ returnS (app "h$throw" [var "h$baseZCGHCziExceptionziTypezioverflowException", false_])
  RaiseDivZeroOp          -> \_r []  -> PRPrimCall $ returnS (app "h$throw" [var "h$baseZCGHCziExceptionziTypezidivZZeroException", false_])
  MaskAsyncExceptionsOp   -> \_r [a] -> PRPrimCall $ returnS (app "h$maskAsync" [a])
  MaskUninterruptibleOp   -> \_r [a] -> PRPrimCall $ returnS (app "h$maskUnintAsync" [a])
  UnmaskAsyncExceptionsOp -> \_r [a] -> PRPrimCall $ returnS (app "h$unmaskAsync" [a])

  MaskStatus -> \[r] [] -> PrimInline $ r |= app "h$maskStatus" []

------------------------------- STM-accessible Mutable Variables  --------------

  AtomicallyOp -> \[_r] [a]   -> PRPrimCall $ returnS (app "h$atomically" [a])
  RetryOp      -> \_r   []    -> PRPrimCall $ returnS (app "h$stmRetry" [])
  CatchRetryOp -> \[_r] [a,b] -> PRPrimCall $ returnS (app "h$stmCatchRetry" [a,b])
  CatchSTMOp   -> \[_r] [a,h] -> PRPrimCall $ returnS (app "h$catchStm" [a,h])
  NewTVarOp    -> \[tv] [v]   -> PrimInline $ tv |= app "h$newTVar" [v]
  ReadTVarOp   -> \[r] [tv]   -> PrimInline $ r |= app "h$readTVar" [tv]
  ReadTVarIOOp -> \[r] [tv]   -> PrimInline $ r |= app "h$readTVarIO" [tv]
  WriteTVarOp  -> \[] [tv,v]  -> PrimInline $ appS "h$writeTVar" [tv,v]

------------------------------- Synchronized Mutable Variables ------------------

  NewMVarOp     -> \[r]   []    -> PrimInline $ r |= New (app "h$MVar" [])
  TakeMVarOp    -> \[_r]  [m]   -> PRPrimCall $ returnS (app "h$takeMVar" [m])
  TryTakeMVarOp -> \[r,v] [m]   -> PrimInline $ appT [r,v] "h$tryTakeMVar" [m]
  PutMVarOp     -> \[]    [m,v] -> PRPrimCall $ returnS (app "h$putMVar" [m,v])
  TryPutMVarOp  -> \[r]   [m,v] -> PrimInline $ r |= app "h$tryPutMVar" [m,v]
  ReadMVarOp    -> \[_r]  [m]   -> PRPrimCall $ returnS (app "h$readMVar" [m])
  TryReadMVarOp -> \[r,v] [m]   -> PrimInline $ mconcat
                                                    [ v |= m .^ "val"
                                                    , r |= if01 (v .===. null_)
                                                    ]
  IsEmptyMVarOp -> \[r]   [m]   -> PrimInline $ r |= if10 (m .^ "val" .===. null_)

------------------------------- Delay/Wait Ops ---------------------------------

  DelayOp     -> \[] [t]  -> PRPrimCall $ returnS (app "h$delayThread" [t])
  WaitReadOp  -> \[] [fd] -> PRPrimCall $ returnS (app "h$waidRead" [fd])
  WaitWriteOp -> \[] [fd] -> PRPrimCall $ returnS (app "h$waitWrite" [fd])

------------------------------- Concurrency Primitives -------------------------

  ForkOp   -> \[_tid] [x]    -> PRPrimCall $ returnS (app "h$fork" [x, true_])
  ForkOnOp -> \[_tid] [_p,x] -> PRPrimCall $ returnS (app "h$fork" [x, true_]) -- ignore processor argument
  KillThreadOp  -> \[] [tid,ex]  -> PRPrimCall $ returnS (app "h$killThread" [tid,ex])
  YieldOp       -> \[] []        -> PRPrimCall $ returnS (app "h$yield" [])
  MyThreadIdOp  -> \[r] []       -> PrimInline $ r |= var "h$currentThread"
  IsCurrentThreadBoundOp -> \[r] [] -> PrimInline $ r |= one_
  NoDuplicateOp -> \[] [] -> PrimInline mempty -- don't need to do anything as long as we have eager blackholing
  ThreadStatusOp -> \[stat,cap,locked] [tid] -> PrimInline $ appT [stat, cap, locked] "h$threadStatus" [tid]

------------------------------- Weak Pointers -----------------------------------

  MkWeakOp              -> \[r] [o,b,c] -> PrimInline $ r |= app "h$makeWeak" [o,b,c]
  MkWeakNoFinalizerOp   -> \[r] [o,b]   -> PrimInline $ r |= app "h$makeWeakNoFinalizer" [o,b]
  AddCFinalizerToWeakOp -> \[r] [_a1,_a1o,_a2,_a2o,_i,_a3,_a3o,_w] -> PrimInline $ r |= one_
  DeRefWeakOp           -> \[f,v] [w] -> PrimInline $ mconcat
                                                        [ v |= w .^ "val"
                                                        , f |= if01 (v .===. null_)
                                                        ]
  FinalizeWeakOp     -> \[fl,fin] [w] -> PrimInline $ appT [fin, fl] "h$finalizeWeak" [w]
  TouchOp            -> \[] [_e]      -> PrimInline mempty
  KeepAliveOp        -> \[_r] [x, f]  -> PRPrimCall $ ReturnStat (app "h$keepAlive" [x, f])


------------------------------ Stable pointers and names ------------------------

  MakeStablePtrOp -> \[s1,s2] [a] -> PrimInline $ mconcat
      [ s1 |= var "h$stablePtrBuf"
      , s2 |= app "h$makeStablePtr" [a]
      ]
  DeRefStablePtrOp -> \[r] [_s1,s2]            -> PrimInline $ r |= app "h$deRefStablePtr" [s2]
  EqStablePtrOp    -> \[r] [_sa1,sa2,_sb1,sb2] -> PrimInline $ r |= if10 (sa2 .===. sb2)

  MakeStableNameOp  -> \[r] [a] -> PrimInline $ r |= app "h$makeStableName" [a]
  StableNameToIntOp -> \[r] [s] -> PrimInline $ r |= app "h$stableNameInt" [s]

------------------------------ Compact normal form -----------------------------

  CompactNewOp           -> \[c] [s]   -> PrimInline $ c |= app "h$compactNew" [s]
  CompactResizeOp        -> \[]  [c,s] -> PrimInline $ appS "h$compactResize" [c,s]
  CompactContainsOp      -> \[r] [c,v] -> PrimInline $ r |= app "h$compactContains" [c,v]
  CompactContainsAnyOp   -> \[r] [v]   -> PrimInline $ r |= app "h$compactContainsAny" [v]
  CompactGetFirstBlockOp -> \[ra,ro,s] [c] ->
    PrimInline $ appT [ra,ro,s] "h$compactGetFirstBlock" [c]
  CompactGetNextBlockOp -> \[ra,ro,s] [c,a,o] ->
    PrimInline $ appT [ra,ro,s] "h$compactGetNextBlock" [c,a,o]
  CompactAllocateBlockOp -> \[ra,ro] [size,sa,so] ->
    PrimInline $ appT [ra,ro] "h$compactAllocateBlock" [size,sa,so]
  CompactFixupPointersOp -> \[c,newroota, newrooto] [blocka,blocko,roota,rooto] ->
    PrimInline $ appT [c,newroota,newrooto] "h$compactFixupPointers" [blocka,blocko,roota,rooto]
  CompactAdd -> \[_r] [c,o] ->
    PRPrimCall $ returnS (app "h$compactAdd" [c,o])
  CompactAddWithSharing -> \[_r] [c,o] ->
    PRPrimCall $ returnS (app "h$compactAddWithSharing" [c,o])
  CompactSize -> \[s] [c] ->
    PrimInline $ s |= app "h$compactSize" [c]

------------------------------ Unsafe pointer equality --------------------------

  ReallyUnsafePtrEqualityOp -> \[r] [p1,p2] -> PrimInline $ r |= if10 (p1 .===. p2)

------------------------------ Parallelism --------------------------------------

  ParOp     -> \[r] [_a] -> PrimInline $ r |= zero_
  SparkOp   -> \[r] [a]  -> PrimInline $ r |= a
  SeqOp     -> \[_r] [e] -> PRPrimCall $ returnS (app "h$e" [e])
  NumSparks -> \[r] []   -> PrimInline $ r |= zero_

------------------------------ Tag to enum stuff --------------------------------

  DataToTagOp -> \[_r] [d] -> PRPrimCall $ mconcat
      [ stack .! PreInc sp |= var "h$dataToTag_e"
      , returnS (app "h$e" [d])
      ]
  TagToEnumOp -> \[r] [tag] -> if
    | isBoolTy ty -> PrimInline $ r |= IfExpr tag true_ false_
    | otherwise   -> PrimInline $ r |= app "h$tagToEnum" [tag]

------------------------------ Bytecode operations ------------------------------

  AddrToAnyOp -> \[r] [d,_o] -> PrimInline $ r |= d

------------------------------ Profiling (CCS)  ------------------------------

  GetCCSOfOp -> \[a, o] [obj] -> if
    | prof -> PrimInline $ mconcat
        [ a |= if_ (isObject obj)
                    (app "h$buildCCSPtr" [obj .^ "cc"])
                    null_
        , o |= zero_
        ]
    | otherwise -> PrimInline $ mconcat
                    [ a |= null_
                    , o |= zero_
                    ]

  GetCurrentCCSOp -> \[a, o] [_dummy_arg] ->
    let ptr = if prof then app "h$buildCCSPtr" [jCurrentCCS]
                      else null_
    in PrimInline $ mconcat
        [ a |= ptr
        , o |= zero_
        ]

  ClearCCSOp -> \[_r] [x] -> PRPrimCall $ ReturnStat (app "h$clearCCS" [x])

------------------------------ Eventlog -------------------

  TraceEventOp       -> \[] [ed,eo]     -> PrimInline $ appS "h$traceEvent" [ed,eo]
  TraceEventBinaryOp -> \[] [ed,eo,len] -> PrimInline $ appS "h$traceEventBinary" [ed,eo,len]
  TraceMarkerOp      -> \[] [ed,eo]     -> PrimInline $ appS "h$traceMarker" [ed,eo]

------------------------------ Unhandled primops -------------------

  BRevOp                            -> unhandledPrimop op
  BRev8Op                           -> unhandledPrimop op
  BRev16Op                          -> unhandledPrimop op
  BRev32Op                          -> unhandledPrimop op
  BRev64Op                          -> unhandledPrimop op

  DoubleExpM1Op                     -> unhandledPrimop op
  DoubleLog1POp                     -> unhandledPrimop op
  FloatExpM1Op                      -> unhandledPrimop op
  FloatLog1POp                      -> unhandledPrimop op

  ShrinkSmallMutableArrayOp_Char    -> unhandledPrimop op
  GetSizeofSmallMutableArrayOp      -> unhandledPrimop op

  IndexByteArrayOp_Word8AsChar      -> unhandledPrimop op
  IndexByteArrayOp_Word8AsWideChar  -> unhandledPrimop op
  IndexByteArrayOp_Word8AsAddr      -> unhandledPrimop op
  IndexByteArrayOp_Word8AsFloat     -> unhandledPrimop op
  IndexByteArrayOp_Word8AsDouble    -> unhandledPrimop op
  IndexByteArrayOp_Word8AsStablePtr -> unhandledPrimop op
  IndexByteArrayOp_Word8AsInt16     -> unhandledPrimop op
  IndexByteArrayOp_Word8AsInt32     -> unhandledPrimop op
  IndexByteArrayOp_Word8AsInt64     -> unhandledPrimop op
  IndexByteArrayOp_Word8AsInt       -> unhandledPrimop op
  IndexByteArrayOp_Word8AsWord16    -> unhandledPrimop op
  IndexByteArrayOp_Word8AsWord32    -> unhandledPrimop op
  IndexByteArrayOp_Word8AsWord64    -> unhandledPrimop op
  IndexByteArrayOp_Word8AsWord      -> unhandledPrimop op

  ReadByteArrayOp_Word8AsChar       -> unhandledPrimop op
  ReadByteArrayOp_Word8AsWideChar   -> unhandledPrimop op
  ReadByteArrayOp_Word8AsAddr       -> unhandledPrimop op
  ReadByteArrayOp_Word8AsFloat      -> unhandledPrimop op
  ReadByteArrayOp_Word8AsDouble     -> unhandledPrimop op
  ReadByteArrayOp_Word8AsStablePtr  -> unhandledPrimop op
  ReadByteArrayOp_Word8AsInt16      -> unhandledPrimop op
  ReadByteArrayOp_Word8AsInt32      -> unhandledPrimop op
  ReadByteArrayOp_Word8AsInt64      -> unhandledPrimop op
  ReadByteArrayOp_Word8AsInt        -> unhandledPrimop op
  ReadByteArrayOp_Word8AsWord16     -> unhandledPrimop op
  ReadByteArrayOp_Word8AsWord32     -> unhandledPrimop op
  ReadByteArrayOp_Word8AsWord64     -> unhandledPrimop op
  ReadByteArrayOp_Word8AsWord       -> unhandledPrimop op

  WriteByteArrayOp_Word8AsChar      -> unhandledPrimop op
  WriteByteArrayOp_Word8AsWideChar  -> unhandledPrimop op
  WriteByteArrayOp_Word8AsAddr      -> unhandledPrimop op
  WriteByteArrayOp_Word8AsFloat     -> unhandledPrimop op
  WriteByteArrayOp_Word8AsDouble    -> unhandledPrimop op
  WriteByteArrayOp_Word8AsStablePtr -> unhandledPrimop op
  WriteByteArrayOp_Word8AsInt16     -> unhandledPrimop op
  WriteByteArrayOp_Word8AsInt32     -> unhandledPrimop op
  WriteByteArrayOp_Word8AsInt64     -> unhandledPrimop op
  WriteByteArrayOp_Word8AsInt       -> unhandledPrimop op
  WriteByteArrayOp_Word8AsWord16    -> unhandledPrimop op
  WriteByteArrayOp_Word8AsWord32    -> unhandledPrimop op
  WriteByteArrayOp_Word8AsWord64    -> unhandledPrimop op
  WriteByteArrayOp_Word8AsWord      -> unhandledPrimop op

  CasByteArrayOp_Int8               -> unhandledPrimop op
  CasByteArrayOp_Int16              -> unhandledPrimop op
  CasByteArrayOp_Int32              -> unhandledPrimop op
  CasByteArrayOp_Int64              -> unhandledPrimop op

  InterlockedExchange_Addr          -> unhandledPrimop op
  InterlockedExchange_Word          -> unhandledPrimop op

  CasAddrOp_Addr                    -> unhandledPrimop op
  CasAddrOp_Word                    -> unhandledPrimop op
  CasAddrOp_Word8                   -> unhandledPrimop op
  CasAddrOp_Word16                  -> unhandledPrimop op
  CasAddrOp_Word32                  -> unhandledPrimop op
  CasAddrOp_Word64                  -> unhandledPrimop op

  FetchAddAddrOp_Word               -> unhandledPrimop op
  FetchSubAddrOp_Word               -> unhandledPrimop op
  FetchAndAddrOp_Word               -> unhandledPrimop op
  FetchNandAddrOp_Word              -> unhandledPrimop op
  FetchOrAddrOp_Word                -> unhandledPrimop op
  FetchXorAddrOp_Word               -> unhandledPrimop op

  AtomicReadAddrOp_Word             -> unhandledPrimop op
  AtomicWriteAddrOp_Word            -> unhandledPrimop op

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
  GetThreadLabelOp                  -> unhandledPrimop op
  ListThreadsOp                     -> unhandledPrimop op
  LabelThreadOp                     -> unhandledPrimop op -- \[] [t,la,lo] -> PrimInline $ t .^ "label" |= ValExpr (JList [la, lo])

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

  VecIndexScalarByteArrayOp _ _ _   -> unhandledPrimop op
  VecReadScalarByteArrayOp _ _ _    -> unhandledPrimop op
  VecWriteScalarByteArrayOp _ _ _   -> unhandledPrimop op
  VecIndexScalarOffAddrOp _ _ _     -> unhandledPrimop op
  VecReadScalarOffAddrOp _ _ _      -> unhandledPrimop op
  VecWriteScalarOffAddrOp _ _ _     -> unhandledPrimop op

  PrefetchByteArrayOp3              -> unhandledPrimop op
  PrefetchMutableByteArrayOp3       -> unhandledPrimop op
  PrefetchAddrOp3                   -> unhandledPrimop op
  PrefetchValueOp3                  -> unhandledPrimop op
  PrefetchByteArrayOp2              -> unhandledPrimop op
  PrefetchMutableByteArrayOp2       -> unhandledPrimop op
  PrefetchAddrOp2                   -> unhandledPrimop op
  PrefetchValueOp2                  -> unhandledPrimop op
  PrefetchByteArrayOp1              -> unhandledPrimop op
  PrefetchMutableByteArrayOp1       -> unhandledPrimop op
  PrefetchAddrOp1                   -> unhandledPrimop op
  PrefetchValueOp1                  -> unhandledPrimop op
  PrefetchByteArrayOp0              -> unhandledPrimop op
  PrefetchMutableByteArrayOp0       -> unhandledPrimop op
  PrefetchAddrOp0                   -> unhandledPrimop op
  PrefetchValueOp0                  -> unhandledPrimop op

unhandledPrimop :: PrimOp -> [JExpr] -> [JExpr] -> PrimRes
unhandledPrimop op rs as = PrimInline $ mconcat
  [ appS "h$log" [toJExpr $ mconcat
      [ "warning, unhandled primop: "
      , renderWithContext defaultSDocContext (ppr op)
      , " "
      , show (length rs, length as)
      ]]
  , appS (mkFastString $ "h$primop_" ++ zEncodeString (renderWithContext defaultSDocContext (ppr op))) as
    -- copyRes
  , mconcat $ zipWith (\r reg -> r |= toJExpr reg) rs (enumFrom Ret1)
  ]


-- tuple returns
appT :: [JExpr] -> FastString -> [JExpr] -> JStat
appT []     f xs = appS f xs
appT (r:rs) f xs = mconcat
  [ r |= app f xs
  , mconcat (zipWith (\r ret -> r |= toJExpr ret) rs (enumFrom Ret1))
  ]

i32_, u32_, u8_, f6_, f3_, u1_ :: JExpr -> JExpr -> JExpr
i32_ a i = IdxExpr (a .^ "i3") i
u32_ a i = (IdxExpr (a .^ "i3") i) .>>>. zero_
u8_ a i = IdxExpr (a .^ "u8") i
f6_ a i = IdxExpr (a .^ "f6") i
f3_ a i = IdxExpr (a .^ "f3") i
u1_ a i = IdxExpr (a .^ "u1") i

dv_s_i8, dv_s_i16, dv_s_u16, dv_s_i32, dv_s_u32, dv_s_f32, dv_s_f64 :: JExpr -> JExpr -> JExpr -> JStat
dv_s_i8  a i v = ApplStat (a .^ "dv" .^ "setInt8"   ) [i, v, true_]
dv_s_u16 a i v = ApplStat (a .^ "dv" .^ "setUint16" ) [i, v, true_]
dv_s_i16 a i v = ApplStat (a .^ "dv" .^ "setInt16"  ) [i, v, true_]
dv_s_i32 a i v = ApplStat (a .^ "dv" .^ "setInt32"  ) [i, v, true_]
dv_s_u32 a i v = ApplStat (a .^ "dv" .^ "setUint32" ) [i, v, true_]
dv_s_f32 a i v = ApplStat (a .^ "dv" .^ "setFloat32") [i, v, true_]
dv_s_f64 a i v = ApplStat (a .^ "dv" .^ "setFloat64") [i, v, true_]

dv_i8, dv_i16, dv_u16, dv_i32, dv_u32, dv_f32, dv_f64 :: JExpr -> JExpr -> JExpr
dv_i8  a i = ApplExpr (a .^ "dv" .^ "getInt8"   ) [i, true_]
dv_i16 a i = ApplExpr (a .^ "dv" .^ "getInt16"  ) [i, true_]
dv_u16 a i = ApplExpr (a .^ "dv" .^ "getUint16" ) [i, true_]
dv_i32 a i = ApplExpr (a .^ "dv" .^ "getInt32"  ) [i, true_]
dv_u32 a i = ApplExpr (a .^ "dv" .^ "getUint32" ) [i, true_]
dv_f32 a i = ApplExpr (a .^ "dv" .^ "getFloat32") [i, true_]
dv_f64 a i = ApplExpr (a .^ "dv" .^ "getFloat64") [i, true_]

fetchOpByteArray :: (JExpr -> JExpr -> JExpr) -> JExpr -> JExpr -> JExpr -> JExpr -> JStat
fetchOpByteArray op tgt src i v = mconcat
  [ tgt |= i32_ src i
  , i32_ src i |= op tgt v
  ]

--------------------------------------------------------------------------------
--                            Lifted Arrays
--------------------------------------------------------------------------------
-- | lifted arrays
cloneArray :: JExpr -> JExpr -> Maybe JExpr -> JExpr -> JStat
cloneArray tgt src mb_offset len = mconcat
  [ tgt |= ApplExpr (src .^ "slice") [start, end]
  , tgt .^ closureMeta_   |= zero_
  , tgt .^ "__ghcjsArray" |= true_
  ]
  where
    start = fromMaybe zero_ mb_offset
    end   = maybe len (Add len) mb_offset

newArray :: JExpr -> JExpr -> JExpr -> JStat
newArray tgt len elem =
    tgt |= app "h$newArray" [len, elem]

newByteArray :: JExpr -> JExpr -> JStat
newByteArray tgt len =
  tgt |= app "h$newByteArray" [len]


-- e|0  (32 bit signed integer truncation)
i32 :: JExpr -> JExpr
i32 e = BOr e zero_

-- e>>>0  (32 bit unsigned integer truncation)
u32 :: JExpr -> JExpr
u32 e = e .>>>. zero_

quotShortInt :: Int -> JExpr -> JExpr -> JExpr
quotShortInt bits x y = BAnd (signed x `Div` signed y) mask
  where
    signed z = (z .<<. shift) .>>. shift
    shift    = toJExpr (32 - bits)
    mask     = toJExpr (((2::Integer) ^ bits) - 1)

remShortInt :: Int -> JExpr -> JExpr -> JExpr
remShortInt bits x y = BAnd (signed x `Mod` signed y) mask
  where
    signed z = (z .<<. shift) .>>. shift
    shift    = toJExpr (32 - bits)
    mask     = toJExpr (((2::Integer) ^ bits) - 1)
