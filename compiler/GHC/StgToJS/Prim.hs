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

import GHC.JS.Unsat.Syntax hiding (JUOp (..))
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


genPrim :: Bool     -- ^ Profiling (cost-centres) enabled
        -> Bool     -- ^ Array bounds-checking enabled
        -> Type
        -> PrimOp   -- ^ the primitive operation
        -> [JExpr]  -- ^ where to store the result
        -> [JExpr]  -- ^ arguments
        -> PrimRes
genPrim prof bound ty op = case op of
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
  Word32ToInt32Op -> \[r] [x]   -> PrimInline $ r |= toI32 x

------------------------------ Int ----------------------------------------------

  IntAddOp        -> \[r] [x,y] -> PrimInline $ r |= toI32 (Add x y)
  IntSubOp        -> \[r] [x,y] -> PrimInline $ r |= toI32 (Sub x y)
  IntMulOp        -> \[r] [x,y] -> PrimInline $ r |= app "Math.imul" [x, y]
  IntMul2Op       -> \[c,hr,lr] [x,y] -> PrimInline $ appT [c,hr,lr] "h$hs_timesInt2" [x, y]
  IntMulMayOfloOp -> \[r] [x,y] -> PrimInline $ jVar \tmp -> mconcat
                                            [ tmp |= Mul x y
                                            , r   |= if01 (tmp .===. toI32 tmp)
                                            ]
  IntQuotOp       -> \[r]   [x,y] -> PrimInline $ r |= toI32 (Div x y)
  IntRemOp        -> \[r]   [x,y] -> PrimInline $ r |= Mod x y
  IntQuotRemOp    -> \[q,r] [x,y] -> PrimInline $ mconcat
                                            [ q |= toI32 (Div x y)
                                            , r |= x `Sub` (Mul y q)
                                            ]
  IntAndOp        -> \[r] [x,y]   -> PrimInline $ r |= BAnd x y
  IntOrOp         -> \[r] [x,y]   -> PrimInline $ r |= BOr  x y
  IntXorOp        -> \[r] [x,y]   -> PrimInline $ r |= BXor x y
  IntNotOp        -> \[r] [x]     -> PrimInline $ r |= BNot x

  IntNegOp        -> \[r] [x]   -> PrimInline $ r |= toI32 (Negate x)
-- add with carry: overflow == 0 iff no overflow
  IntAddCOp       -> \[r,overf] [x,y] ->
      PrimInline $ jVar \rt -> mconcat
        [ rt    |= Add x y
        , r     |= toI32 rt
        , overf |= if10 (r .!=. rt)
        ]
  IntSubCOp       -> \[r,overf] [x,y] ->
      PrimInline $ jVar \rt -> mconcat
        [ rt    |= Sub x y
        , r     |= toI32 rt
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
  IntSrlOp          -> \[r] [x,y] -> PrimInline $ r |= toI32 (x .>>>. y)

------------------------------ Int8 ---------------------------------------------

  Int8ToIntOp       -> \[r] [x]       -> PrimInline $ r |= x
  IntToInt8Op       -> \[r] [x]       -> PrimInline $ r |= signExtend8 x
  Int8NegOp         -> \[r] [x]       -> PrimInline $ r |= signExtend8 (Negate x)
  Int8AddOp         -> \[r] [x,y]     -> PrimInline $ r |= signExtend8 (Add x y)
  Int8SubOp         -> \[r] [x,y]     -> PrimInline $ r |= signExtend8 (Sub x y)
  Int8MulOp         -> \[r] [x,y]     -> PrimInline $ r |= signExtend8 (Mul x y)
  Int8QuotOp        -> \[r] [x,y]     -> PrimInline $ r |= signExtend8 (quotShortInt 8 x y)
  Int8RemOp         -> \[r] [x,y]     -> PrimInline $ r |= signExtend8 (remShortInt 8 x y)
  Int8QuotRemOp     -> \[r1,r2] [x,y] -> PrimInline $ mconcat
                                                [ r1 |= signExtend8 (quotShortInt 8 x y)
                                                , r2 |= signExtend8 (remShortInt 8 x y)
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
                                                  [ r1 |= toI32 (Div x y)
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
  Int16QuotOp        -> \[r] [x,y]     -> PrimInline $ r |= signExtend16 (quotShortInt 16 x y)
  Int16RemOp         -> \[r] [x,y]     -> PrimInline $ r |= signExtend16 (remShortInt 16 x y)
  Int16QuotRemOp     -> \[r1,r2] [x,y] -> PrimInline $ mconcat
                                                [ r1 |= signExtend16 (quotShortInt 16 x y)
                                                , r2 |= signExtend16 (remShortInt 16 x y)
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
                                                [ r1 |= toI32 (Div x y)
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

  Word32ToWordOp     -> \[r] [x]   -> PrimInline $ r |= x
  WordToWord32Op     -> \[r] [x]   -> PrimInline $ r |= x

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

  Int64ToIntOp      -> \[r] [_h,l] -> PrimInline $ r |= toI32 l

  Int64NegOp        -> \[r_h,r_l] [h,l] ->
      PrimInline $ mconcat
        [ r_l |= toU32 (BNot l + 1)
        , r_h |= toI32 (BNot h + Not r_l)
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
       [ r1 |= toU32 x1
       , r2 |= x2
       ]
  IntToInt64Op      -> \[r1,r2] [x] ->
      PrimInline $ mconcat
       [ r1 |= if_ (x .<. 0) (-1) 0 -- sign-extension
       , r2 |= toU32 x
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
     [ r1 |= toI32 x1
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
        [ hr |= toU32 (BOr h0 h1)
        , hl |= toU32 (BOr l0 l1)
        ]

  Word64AndOp -> \[hr,hl] [h0, l0, h1, l1] ->
      PrimInline $ mconcat
        [ hr |= toU32 (BAnd h0 h1)
        , hl |= toU32 (BAnd l0 l1)
        ]

  Word64XorOp -> \[hr,hl] [h0, l0, h1, l1] ->
      PrimInline $ mconcat
        [ hr |= toU32 (BXor h0 h1)
        , hl |= toU32 (BXor l0 l1)
        ]

  Word64NotOp -> \[hr,hl] [h, l] ->
      PrimInline $ mconcat
        [ hr |= toU32 (BNot h)
        , hl |= toU32 (BNot l)
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
        , r |= toU32 t
        , c |= if10 (t .!==. r)
        ]
  WordSubCOp  -> \[r,c] [x,y] ->
      PrimInline $ mconcat
        [ r |= toU32 (Sub x y)
        , c |= if10 (y .>. x)
        ]
  WordAdd2Op    -> \[h,l] [x,y] -> PrimInline $ appT [h,l] "h$wordAdd2" [x,y]
  WordSubOp     -> \  [r] [x,y] -> PrimInline $ r |= toU32 (Sub x y)
  WordMulOp     -> \  [r] [x,y] -> PrimInline $ r |= toU32 (app "Math.imul" [x, y])
  WordMul2Op    -> \[h,l] [x,y] -> PrimInline $ appT [h,l] "h$mul2Word32" [x,y]
  WordQuotOp    -> \  [q] [x,y] -> PrimInline $ q |= app "h$quotWord32" [x,y]
  WordRemOp     -> \  [r] [x,y] -> PrimInline $ r |= app "h$remWord32" [x,y]
  WordQuotRemOp -> \[q,r] [x,y] -> PrimInline $ appT [q,r] "h$quotRemWord32" [x,y]
  WordQuotRem2Op   -> \[q,r] [xh,xl,y] -> PrimInline $ appT [q,r] "h$quotRem2Word32" [xh,xl,y]
  WordAndOp        -> \[r] [x,y] -> PrimInline $ r |= toU32 (BAnd x y)
  WordOrOp         -> \[r] [x,y] -> PrimInline $ r |= toU32 (BOr  x y)
  WordXorOp        -> \[r] [x,y] -> PrimInline $ r |= toU32 (BXor x y)
  WordNotOp        -> \[r] [x]   -> PrimInline $ r |= toU32 (BNot x)
  WordSllOp        -> \[r] [x,y] -> PrimInline $ r |= toU32 (x .<<. y)
  WordSrlOp        -> \[r] [x,y] -> PrimInline $ r |= x .>>>. y
  WordToIntOp      -> \[r] [x]   -> PrimInline $ r |= toI32 x
  WordGtOp         -> \[r] [x,y] -> PrimInline $ r |= if10 (x .>.  y)
  WordGeOp         -> \[r] [x,y] -> PrimInline $ r |= if10 (x .>=. y)
  WordEqOp         -> \[r] [x,y] -> PrimInline $ r |= if10 (x .===. y)
  WordNeOp         -> \[r] [x,y] -> PrimInline $ r |= if10 (x .!==. y)
  WordLtOp         -> \[r] [x,y] -> PrimInline $ r |= if10 (x .<.  y)
  WordLeOp         -> \[r] [x,y] -> PrimInline $ r |= if10 (x .<=. y)
  WordToDoubleOp   -> \[r] [x]   -> PrimInline $ r |= x
  WordToFloatOp    -> \[r] [x]   -> PrimInline $ r |= math_fround [x]
  PopCnt8Op        -> \[r] [x]   -> PrimInline $ r |= var "h$popCntTab" .! (mask8 x)
  PopCnt16Op       -> \[r] [x]   -> PrimInline $ r |= Add (var "h$popCntTab" .! (mask8 x))
                                                      (var "h$popCntTab" .! (mask8 (x .>>>. Int 8)))

  PopCnt32Op  -> \[r] [x]     -> PrimInline $ r |= app "h$popCnt32" [x]
  PopCnt64Op  -> \[r] [x1,x2] -> PrimInline $ r |= app "h$popCnt64" [x1,x2]
  PopCntOp    -> \[r] [x]     -> genPrim prof bound ty PopCnt32Op [r] [x]
  Pdep8Op     -> \[r] [s,m]   -> PrimInline $ r |= app "h$pdep8"  [s,m]
  Pdep16Op    -> \[r] [s,m]   -> PrimInline $ r |= app "h$pdep16" [s,m]
  Pdep32Op    -> \[r] [s,m]   -> PrimInline $ r |= app "h$pdep32" [s,m]
  Pdep64Op    -> \[ra,rb] [sa,sb,ma,mb] -> PrimInline $ appT [ra,rb] "h$pdep64" [sa,sb,ma,mb]
  PdepOp      -> \rs xs                 -> genPrim prof bound ty Pdep32Op rs xs
  Pext8Op     -> \[r] [s,m] -> PrimInline $ r |= app "h$pext8" [s,m]
  Pext16Op    -> \[r] [s,m] -> PrimInline $ r |= app "h$pext16" [s,m]
  Pext32Op    -> \[r] [s,m] -> PrimInline $ r |= app "h$pext32" [s,m]
  Pext64Op    -> \[ra,rb] [sa,sb,ma,mb] -> PrimInline $ appT [ra,rb] "h$pext64" [sa,sb,ma,mb]
  PextOp      -> \rs xs     -> genPrim prof bound ty Pext32Op rs xs

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
      r |= toU32 ((x .<<. (Int 24))
            `BOr` ((BAnd x (Int 0xFF00)) .<<. (Int 8))
            `BOr` ((BAnd x (Int 0xFF0000)) .>>. (Int 8))
            `BOr` (x .>>>. (Int 24)))
  BSwap64Op   -> \[r1,r2] [x,y] -> PrimInline $ appT [r1,r2] "h$bswap64" [x,y]
  BSwapOp     -> \[r] [x]       -> genPrim prof bound ty BSwap32Op [r] [x]

  BRevOp      -> \[r] [w] -> genPrim prof bound ty BRev32Op [r] [w]
  BRev8Op     -> \[r] [w] -> PrimInline $ r |= (app "h$reverseWord" [w] .>>>. 24)
  BRev16Op    -> \[r] [w] -> PrimInline $ r |= (app "h$reverseWord" [w] .>>>. 16)
  BRev32Op    -> \[r] [w] -> PrimInline $ r |= app "h$reverseWord" [w]
  BRev64Op    -> \[rh,rl] [h,l] -> PrimInline $ mconcat [ rl |= app "h$reverseWord" [h]
                                                        , rh |= app "h$reverseWord" [l]
                                                        ]

------------------------------ Narrow -------------------------------------------

  Narrow8IntOp    -> \[r] [x] -> PrimInline $ r |= signExtend8  x
  Narrow16IntOp   -> \[r] [x] -> PrimInline $ r |= signExtend16 x
  Narrow32IntOp   -> \[r] [x] -> PrimInline $ r |= toI32  x
  Narrow8WordOp   -> \[r] [x] -> PrimInline $ r |= mask8  x
  Narrow16WordOp  -> \[r] [x] -> PrimInline $ r |= mask16 x
  Narrow32WordOp  -> \[r] [x] -> PrimInline $ r |= toU32  x

------------------------------ Double -------------------------------------------

  DoubleGtOp        -> \[r] [x,y] -> PrimInline $ r |= if10 (x .>.   y)
  DoubleGeOp        -> \[r] [x,y] -> PrimInline $ r |= if10 (x .>=.  y)
  DoubleEqOp        -> \[r] [x,y] -> PrimInline $ r |= if10 (x .===. y)
  DoubleNeOp        -> \[r] [x,y] -> PrimInline $ r |= if10 (x .!==. y)
  DoubleLtOp        -> \[r] [x,y] -> PrimInline $ r |= if10 (x .<.   y)
  DoubleLeOp        -> \[r] [x,y] -> PrimInline $ r |= if10 (x .<=.  y)
  DoubleAddOp       -> \[r] [x,y] -> PrimInline $ r |= Add x y
  DoubleSubOp       -> \[r] [x,y] -> PrimInline $ r |= Sub x y
  DoubleMulOp       -> \[r] [x,y] -> PrimInline $ r |= Mul x y
  DoubleDivOp       -> \[r] [x,y] -> PrimInline $ r |= Div x y
  DoubleNegOp       -> \[r] [x]   -> PrimInline $ r |= Negate x
  DoubleFabsOp      -> \[r] [x]   -> PrimInline $ r |= math_abs [x]
  DoubleToIntOp     -> \[r] [x]   -> PrimInline $ r |= toI32 x
  DoubleToFloatOp   -> \[r] [x]   -> PrimInline $ r |= math_fround [x]
  DoubleExpOp       -> \[r] [x]   -> PrimInline $ r |= math_exp  [x]
  DoubleExpM1Op     -> \[r] [x]   -> PrimInline $ r |= math_expm1 [x]
  DoubleLogOp       -> \[r] [x]   -> PrimInline $ r |= math_log  [x]
  DoubleLog1POp     -> \[r] [x]   -> PrimInline $ r |= math_log1p [x]
  DoubleSqrtOp      -> \[r] [x]   -> PrimInline $ r |= math_sqrt [x]
  DoubleSinOp       -> \[r] [x]   -> PrimInline $ r |= math_sin  [x]
  DoubleCosOp       -> \[r] [x]   -> PrimInline $ r |= math_cos  [x]
  DoubleTanOp       -> \[r] [x]   -> PrimInline $ r |= math_tan  [x]
  DoubleAsinOp      -> \[r] [x]   -> PrimInline $ r |= math_asin [x]
  DoubleAcosOp      -> \[r] [x]   -> PrimInline $ r |= math_acos [x]
  DoubleAtanOp      -> \[r] [x]   -> PrimInline $ r |= math_atan [x]
  DoubleSinhOp      -> \[r] [x]   -> PrimInline $ r |= math_sinh [x]
  DoubleCoshOp      -> \[r] [x]   -> PrimInline $ r |= math_cosh [x]
  DoubleTanhOp      -> \[r] [x]   -> PrimInline $ r |= math_tanh [x]
  DoubleAsinhOp     -> \[r] [x]   -> PrimInline $ r |= math_asinh [x]
  DoubleAcoshOp     -> \[r] [x]   -> PrimInline $ r |= math_acosh [x]
  DoubleAtanhOp     -> \[r] [x]   -> PrimInline $ r |= math_atanh [x]
  DoublePowerOp     -> \[r] [x,y] -> PrimInline $ r |= math_pow [x,y]
  DoubleDecode_2IntOp  -> \[s,h,l,e] [x] -> PrimInline $ appT [s,h,l,e] "h$decodeDouble2Int" [x]
  DoubleDecode_Int64Op -> \[s1,s2,e] [d] -> PrimInline $ appT [e,s1,s2] "h$decodeDoubleInt64" [d]

------------------------------ Float --------------------------------------------

  FloatGtOp         -> \[r] [x,y] -> PrimInline $ r |= if10 (x .>.   y)
  FloatGeOp         -> \[r] [x,y] -> PrimInline $ r |= if10 (x .>=.  y)
  FloatEqOp         -> \[r] [x,y] -> PrimInline $ r |= if10 (x .===. y)
  FloatNeOp         -> \[r] [x,y] -> PrimInline $ r |= if10 (x .!==. y)
  FloatLtOp         -> \[r] [x,y] -> PrimInline $ r |= if10 (x .<.   y)
  FloatLeOp         -> \[r] [x,y] -> PrimInline $ r |= if10 (x .<=.  y)
  FloatAddOp        -> \[r] [x,y] -> PrimInline $ r |= math_fround [Add x y]
  FloatSubOp        -> \[r] [x,y] -> PrimInline $ r |= math_fround [Sub x y]
  FloatMulOp        -> \[r] [x,y] -> PrimInline $ r |= math_fround [Mul x y]
  FloatDivOp        -> \[r] [x,y] -> PrimInline $ r |= math_fround [Div x y]
  FloatNegOp        -> \[r] [x]   -> PrimInline $ r |= Negate x
  FloatFabsOp       -> \[r] [x]   -> PrimInline $ r |= math_abs [x]
  FloatToIntOp      -> \[r] [x]   -> PrimInline $ r |= toI32 x
  FloatExpOp        -> \[r] [x]   -> PrimInline $ r |= math_fround [math_exp [x]]
  FloatExpM1Op      -> \[r] [x]   -> PrimInline $ r |= math_fround [math_expm1 [x]]
  FloatLogOp        -> \[r] [x]   -> PrimInline $ r |= math_fround [math_log [x]]
  FloatLog1POp      -> \[r] [x]   -> PrimInline $ r |= math_fround [math_log1p [x]]
  FloatSqrtOp       -> \[r] [x]   -> PrimInline $ r |= math_fround [math_sqrt [x]]
  FloatSinOp        -> \[r] [x]   -> PrimInline $ r |= math_fround [math_sin [x]]
  FloatCosOp        -> \[r] [x]   -> PrimInline $ r |= math_fround [math_cos [x]]
  FloatTanOp        -> \[r] [x]   -> PrimInline $ r |= math_fround [math_tan [x]]
  FloatAsinOp       -> \[r] [x]   -> PrimInline $ r |= math_fround [math_asin [x]]
  FloatAcosOp       -> \[r] [x]   -> PrimInline $ r |= math_fround [math_acos [x]]
  FloatAtanOp       -> \[r] [x]   -> PrimInline $ r |= math_fround [math_atan [x]]
  FloatSinhOp       -> \[r] [x]   -> PrimInline $ r |= math_fround [math_sinh [x]]
  FloatCoshOp       -> \[r] [x]   -> PrimInline $ r |= math_fround [math_cosh [x]]
  FloatTanhOp       -> \[r] [x]   -> PrimInline $ r |= math_fround [math_tanh [x]]
  FloatAsinhOp      -> \[r] [x]   -> PrimInline $ r |= math_fround [math_asinh [x]]
  FloatAcoshOp      -> \[r] [x]   -> PrimInline $ r |= math_fround [math_acosh [x]]
  FloatAtanhOp      -> \[r] [x]   -> PrimInline $ r |= math_fround [math_atanh [x]]
  FloatPowerOp      -> \[r] [x,y] -> PrimInline $ r |= math_fround [math_pow [x,y]]
  FloatToDoubleOp   -> \[r] [x]   -> PrimInline $ r |= x
  FloatDecode_IntOp -> \[s,e] [x] -> PrimInline $ appT [s,e] "h$decodeFloatInt" [x]

------------------------------ Arrays -------------------------------------------

  NewArrayOp           -> \[r] [l,e]   -> PrimInline $ r |= app "h$newArray" [l,e]
  ReadArrayOp          -> \[r] [a,i]   -> PrimInline $ bnd_arr bound a i (r |= a .! i)
  WriteArrayOp         -> \[]  [a,i,v] -> PrimInline $ bnd_arr bound a i (a .! i |= v)
  SizeofArrayOp        -> \[r] [a]     -> PrimInline $ r |= a .^ "length"
  SizeofMutableArrayOp -> \[r] [a]     -> PrimInline $ r |= a .^ "length"
  IndexArrayOp         -> \[r] [a,i]   -> PrimInline $ bnd_arr bound a i (r |= a .! i)
  UnsafeFreezeArrayOp  -> \[r] [a]     -> PrimInline $ r |= a
  UnsafeThawArrayOp    -> \[r] [a]     -> PrimInline $ r |= a
  CopyArrayOp          -> \[] [a,o1,ma,o2,n] ->
    PrimInline
      $ bnd_arr_range bound a o1 n
      $ bnd_arr_range bound ma o2 n
      $ loopBlockS (Int 0) (.<. n) \i ->
      [ ma .! (Add i o2) |= a .! (Add i o1)
      , preIncrS i
      ]
  CopyMutableArrayOp  -> \[]  [a1,o1,a2,o2,n] ->
    PrimInline
      $ bnd_arr_range bound a1 o1 n
      $ bnd_arr_range bound a2 o2 n
      $ appS "h$copyMutableArray" [a1,o1,a2,o2,n]

  CloneArrayOp        -> \[r] [a,start,n]     ->
    PrimInline
      $ bnd_arr_range bound a start n
      $ r |= app "h$sliceArray" [a,start,n]

  CloneMutableArrayOp -> \[r] [a,start,n]     ->
    PrimInline
      $ bnd_arr_range bound a start n
      $ r |= app "h$sliceArray" [a,start,n]

  FreezeArrayOp       -> \[r] [a,start,n]     ->
    PrimInline
      $ bnd_arr_range bound a start n
      $ r |= app "h$sliceArray" [a,start,n]

  ThawArrayOp         -> \[r] [a,start,n]     ->
    PrimInline
      $ bnd_arr_range bound a start n
      $ r |= app "h$sliceArray" [a,start,n]

  CasArrayOp          -> \[s,o] [a,i,old,new] ->
    PrimInline
      $ bnd_arr bound a i
      $ jVar \x -> mconcat
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
  ReadSmallArrayOp           -> \[r]   [a,i]         -> PrimInline $ bnd_arr bound a i (r |= a .! i)
  WriteSmallArrayOp          -> \[]    [a,i,e]       -> PrimInline $ bnd_arr bound a i (a .! i |= e)
  SizeofSmallArrayOp         -> \[r]   [a]           -> PrimInline $ r |= a .^ "length"
  SizeofSmallMutableArrayOp  -> \[r]   [a]           -> PrimInline $ r |= a .^ "length"
  IndexSmallArrayOp          -> \[r]   [a,i]         -> PrimInline $ bnd_arr bound a i (r |= a .! i)
  UnsafeFreezeSmallArrayOp   -> \[r]   [a]           -> PrimInline $ r |= a
  UnsafeThawSmallArrayOp     -> \[r]   [a]           -> PrimInline $ r |= a
  CopySmallArrayOp           -> \[]    [s,si,d,di,n] ->
    PrimInline
      $ bnd_arr_range bound s si n
      $ bnd_arr_range bound d di n
      $ loopBlockS (Sub n one_) (.>=. zero_) \i ->
          [ d .! (Add di i) |= s .! (Add si i)
          , postDecrS i
          ]
  CopySmallMutableArrayOp    -> \[]    [s,si,d,di,n] ->
    PrimInline
      $ bnd_arr_range bound s si n
      $ bnd_arr_range bound d di n
      $ appS "h$copyMutableArray" [s,si,d,di,n]

  CloneSmallArrayOp          -> \[r]   [a,o,n]       -> PrimInline $ cloneArray bound r a o n
  CloneSmallMutableArrayOp   -> \[r]   [a,o,n]       -> PrimInline $ cloneArray bound r a o n
  FreezeSmallArrayOp         -> \[r]   [a,o,n]       -> PrimInline $ cloneArray bound r a o n
  ThawSmallArrayOp           -> \[r]   [a,o,n]       -> PrimInline $ cloneArray bound r a o n

  CasSmallArrayOp            -> \[s,o] [a,i,old,new] ->
    PrimInline
      $ bnd_arr bound a i
      $ jVar \x -> mconcat
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

  NewByteArrayOp_Char               -> \[r]   [l]        -> PrimInline (newByteArray r l)
  NewPinnedByteArrayOp_Char         -> \[r]   [l]        -> PrimInline (newByteArray r l)
  NewAlignedPinnedByteArrayOp_Char  -> \[r]   [l,_align] -> PrimInline (newByteArray r l)
  MutableByteArrayIsPinnedOp        -> \[r]   [_]        -> PrimInline $ r |= one_
  ByteArrayIsPinnedOp               -> \[r]   [_]        -> PrimInline $ r |= one_
  ByteArrayContents_Char            -> \[a,o] [b]        -> PrimInline $ mconcat [a |= b, o |= zero_]
  MutableByteArrayContents_Char     -> \[a,o] [b]        -> PrimInline $ mconcat [a |= b, o |= zero_]
  ShrinkMutableByteArrayOp_Char     -> \[]    [a,n]      -> PrimInline $ appS "h$shrinkMutableByteArray" [a,n]
  ResizeMutableByteArrayOp_Char     -> \[r]   [a,n]      -> PrimInline $ r |= app "h$resizeMutableByteArray" [a,n]
  UnsafeFreezeByteArrayOp           -> \[a]   [b]        -> PrimInline $ a |= b
  SizeofByteArrayOp                 -> \[r]   [a]        -> PrimInline $ r |= a .^ "len"
  SizeofMutableByteArrayOp          -> \[r]   [a]        -> PrimInline $ r |= a .^ "len"
  GetSizeofMutableByteArrayOp       -> \[r]   [a]        -> PrimInline $ r |= a .^ "len"

  IndexByteArrayOp_Char      -> \[r]   [a,i] -> PrimInline $ bnd_ix8  bound a i $ r |= read_u8  a i
  IndexByteArrayOp_WideChar  -> \[r]   [a,i] -> PrimInline $ bnd_ix32 bound a i $ r |= read_i32 a i
  IndexByteArrayOp_Int       -> \[r]   [a,i] -> PrimInline $ bnd_ix32 bound a i $ r |= read_i32 a i
  IndexByteArrayOp_Word      -> \[r]   [a,i] -> PrimInline $ bnd_ix32 bound a i $ r |= read_u32 a i
  IndexByteArrayOp_Addr      -> \[r,o] [a,i] -> PrimInline $ bnd_ix32 bound a i $ read_addr a i r o
  IndexByteArrayOp_Float     -> \[r]   [a,i] -> PrimInline $ bnd_ix32 bound a i $ r |= read_f32 a i
  IndexByteArrayOp_Double    -> \[r]   [a,i] -> PrimInline $ bnd_ix64 bound a i $ r |= read_f64 a i
  IndexByteArrayOp_StablePtr -> \[r,o] [a,i] -> PrimInline $ bnd_ix32 bound a i $ read_stableptr a i r o
  IndexByteArrayOp_Int8      -> \[r]   [a,i] -> PrimInline $ bnd_ix8  bound a i $ r |= read_i8  a i
  IndexByteArrayOp_Int16     -> \[r]   [a,i] -> PrimInline $ bnd_ix16 bound a i $ r |= read_i16 a i
  IndexByteArrayOp_Int32     -> \[r]   [a,i] -> PrimInline $ bnd_ix32 bound a i $ r |= read_i32 a i
  IndexByteArrayOp_Int64     -> \[h,l] [a,i] -> PrimInline $ bnd_ix64 bound a i $ read_i64 a i h l
  IndexByteArrayOp_Word8     -> \[r]   [a,i] -> PrimInline $ bnd_ix8  bound a i $ r |= read_u8  a i
  IndexByteArrayOp_Word16    -> \[r]   [a,i] -> PrimInline $ bnd_ix16 bound a i $ r |= read_u16 a i
  IndexByteArrayOp_Word32    -> \[r]   [a,i] -> PrimInline $ bnd_ix32 bound a i $ r |= read_u32 a i
  IndexByteArrayOp_Word64    -> \[h,l] [a,i] -> PrimInline $ bnd_ix64 bound a i $ read_u64 a i h l

  ReadByteArrayOp_Char       -> \[r]   [a,i] -> PrimInline $ bnd_ix8 bound a i $ r |= read_u8  a i
  ReadByteArrayOp_WideChar   -> \[r]   [a,i] -> PrimInline $ bnd_ix32 bound a i $ r |= read_i32 a i
  ReadByteArrayOp_Int        -> \[r]   [a,i] -> PrimInline $ bnd_ix32 bound a i $ r |= read_i32 a i
  ReadByteArrayOp_Word       -> \[r]   [a,i] -> PrimInline $ bnd_ix32 bound a i $ r |= read_u32 a i
  ReadByteArrayOp_Addr       -> \[r,o] [a,i] -> PrimInline $ bnd_ix32 bound a i $ read_addr a i r o
  ReadByteArrayOp_Float      -> \[r]   [a,i] -> PrimInline $ bnd_ix32 bound a i $ r |= read_f32 a i
  ReadByteArrayOp_Double     -> \[r]   [a,i] -> PrimInline $ bnd_ix64 bound a i $ r |= read_f64 a i
  ReadByteArrayOp_StablePtr  -> \[r,o] [a,i] -> PrimInline $ bnd_ix32 bound a i $ read_stableptr a i r o
  ReadByteArrayOp_Int8       -> \[r]   [a,i] -> PrimInline $ bnd_ix8  bound a i $ r |= read_i8  a i
  ReadByteArrayOp_Int16      -> \[r]   [a,i] -> PrimInline $ bnd_ix16 bound a i $ r |= read_i16 a i
  ReadByteArrayOp_Int32      -> \[r]   [a,i] -> PrimInline $ bnd_ix32 bound a i $ r |= read_i32 a i
  ReadByteArrayOp_Int64      -> \[h,l] [a,i] -> PrimInline $ bnd_ix64 bound a i $ read_i64 a i h l
  ReadByteArrayOp_Word8      -> \[r]   [a,i] -> PrimInline $ bnd_ix8  bound a i $ r |= read_u8  a i
  ReadByteArrayOp_Word16     -> \[r]   [a,i] -> PrimInline $ bnd_ix16 bound a i $ r |= read_u16 a i
  ReadByteArrayOp_Word32     -> \[r]   [a,i] -> PrimInline $ bnd_ix32 bound a i $ r |= read_u32 a i
  ReadByteArrayOp_Word64     -> \[h,l] [a,i] -> PrimInline $ bnd_ix64 bound a i $ read_u64 a i h l

  WriteByteArrayOp_Char      -> \[] [a,i,e]   -> PrimInline $ bnd_ix8  bound a i $ write_u8  a i e
  WriteByteArrayOp_WideChar  -> \[] [a,i,e]   -> PrimInline $ bnd_ix32 bound a i $ write_i32 a i e
  WriteByteArrayOp_Int       -> \[] [a,i,e]   -> PrimInline $ bnd_ix32 bound a i $ write_i32 a i e
  WriteByteArrayOp_Word      -> \[] [a,i,e]   -> PrimInline $ bnd_ix32 bound a i $ write_u32 a i e
  WriteByteArrayOp_Addr      -> \[] [a,i,r,o] -> PrimInline $ bnd_ix32 bound a i $ write_addr a i r o
  WriteByteArrayOp_Float     -> \[] [a,i,e]   -> PrimInline $ bnd_ix32 bound a i $ write_f32 a i e
  WriteByteArrayOp_Double    -> \[] [a,i,e]   -> PrimInline $ bnd_ix64 bound a i $ write_f64 a i e
  WriteByteArrayOp_StablePtr -> \[] [a,i,r,o] -> PrimInline $ bnd_ix32 bound a i $ write_stableptr a i r o
  WriteByteArrayOp_Int8      -> \[] [a,i,e]   -> PrimInline $ bnd_ix8  bound a i $ write_i8  a i e
  WriteByteArrayOp_Int16     -> \[] [a,i,e]   -> PrimInline $ bnd_ix16 bound a i $ write_i16 a i e
  WriteByteArrayOp_Int32     -> \[] [a,i,e]   -> PrimInline $ bnd_ix32 bound a i $ write_i32 a i e
  WriteByteArrayOp_Int64     -> \[] [a,i,h,l] -> PrimInline $ bnd_ix64 bound a i $ write_i64 a i h l
  WriteByteArrayOp_Word8     -> \[] [a,i,e]   -> PrimInline $ bnd_ix8  bound a i $ write_u8  a i e
  WriteByteArrayOp_Word16    -> \[] [a,i,e]   -> PrimInline $ bnd_ix16 bound a i $ write_u16 a i e
  WriteByteArrayOp_Word32    -> \[] [a,i,e]   -> PrimInline $ bnd_ix32 bound a i $ write_u32 a i e
  WriteByteArrayOp_Word64    -> \[] [a,i,h,l] -> PrimInline $ bnd_ix64 bound a i $ write_u64 a i h l

  CompareByteArraysOp -> \[r] [a1,o1,a2,o2,n] ->
      PrimInline . bnd_ba_range bound a1 o1 n
                 . bnd_ba_range bound a2 o2 n
                 $ r |= app "h$compareByteArrays" [a1,o1,a2,o2,n]

  -- We assume the arrays aren't overlapping since they're of different types
  -- (ByteArray vs MutableByteArray, Addr# vs MutableByteArray#, [Mutable]ByteArray# vs Addr#)
  CopyByteArrayOp                      -> \[] [a1,o1,a2,o2,n] -> copyByteArray False bound a1 o1 a2 o2 n
  CopyAddrToByteArrayOp                -> \[] [a1,o1,a2,o2,n] -> copyByteArray False bound a1 o1 a2 o2 n
  CopyMutableByteArrayToAddrOp         -> \[] [a1,o1,a2,o2,n] -> copyByteArray False bound a1 o1 a2 o2 n
  CopyMutableByteArrayNonOverlappingOp -> \[] [a1,o1,a2,o2,n] -> copyByteArray False bound a1 o1 a2 o2 n
  CopyAddrToAddrNonOverlappingOp       -> \[] [a1,o1,a2,o2,n] -> copyByteArray False bound a1 o1 a2 o2 n
  CopyByteArrayToAddrOp                -> \[] [a1,o1,a2,o2,n] -> copyByteArray False bound a1 o1 a2 o2 n

  CopyMutableByteArrayOp               -> \[] [a1,o1,a2,o2,n] -> copyByteArray True  bound a1 o1 a2 o2 n
  CopyAddrToAddrOp                     -> \[] [a1,o1,a2,o2,n] -> copyByteArray True  bound a1 o1 a2 o2 n

  SetByteArrayOp -> \[] [a,o,n,v] ->
      PrimInline . bnd_ba_range bound a o n $ loopBlockS zero_ (.<. n) \i ->
        [ write_u8 a (Add o i) v
        , postIncrS i
        ]
  SetAddrRangeOp -> \[] xs@[_a,_o,_n,_v] -> genPrim prof bound ty SetByteArrayOp [] xs

  AtomicReadByteArrayOp_Int  -> \[r]   [a,i]   -> PrimInline $ bnd_ix32 bound a i $ r |= read_i32 a i
  AtomicWriteByteArrayOp_Int -> \[]    [a,i,v] -> PrimInline $ bnd_ix32 bound a i $ write_i32 a i v
  FetchAddByteArrayOp_Int    -> \[r]   [a,i,v] -> PrimInline $ bnd_ix32 bound a i $ fetchOpByteArray Add  r a i v
  FetchSubByteArrayOp_Int    -> \[r]   [a,i,v] -> PrimInline $ bnd_ix32 bound a i $ fetchOpByteArray Sub  r a i v
  FetchAndByteArrayOp_Int    -> \[r]   [a,i,v] -> PrimInline $ bnd_ix32 bound a i $ fetchOpByteArray BAnd r a i v
  FetchOrByteArrayOp_Int     -> \[r]   [a,i,v] -> PrimInline $ bnd_ix32 bound a i $ fetchOpByteArray BOr  r a i v
  FetchNandByteArrayOp_Int   -> \[r]   [a,i,v] -> PrimInline $ bnd_ix32 bound a i $ fetchOpByteArray (\x y -> BNot (BAnd x y)) r a i v
  FetchXorByteArrayOp_Int    -> \[r]   [a,i,v] -> PrimInline $ bnd_ix32 bound a i $ fetchOpByteArray BXor r a i v

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

  IndexOffAddrOp_Char      -> \[r]     [a,o,i] -> PrimInline $ r |= read_boff_u8  a (off8  o i)
  IndexOffAddrOp_WideChar  -> \[r]     [a,o,i] -> PrimInline $ r |= read_boff_i32 a (off32 o i)
  IndexOffAddrOp_Int       -> \[r]     [a,o,i] -> PrimInline $ r |= read_boff_i32 a (off32 o i)
  IndexOffAddrOp_Word      -> \[r]     [a,o,i] -> PrimInline $ r |= read_boff_u32 a (off32 o i)
  IndexOffAddrOp_Addr      -> \[ra,ro] [a,o,i] -> PrimInline $ read_boff_addr a (off32 o i) ra ro
  IndexOffAddrOp_Float     -> \[r]     [a,o,i] -> PrimInline $ r |= read_boff_f32 a (off32 o i)
  IndexOffAddrOp_Double    -> \[r]     [a,o,i] -> PrimInline $ r |= read_boff_f64 a (off64 o i)
  IndexOffAddrOp_StablePtr -> \[ra,ro] [a,o,i] -> PrimInline $ read_boff_stableptr a (off32 o i) ra ro
  IndexOffAddrOp_Int8      -> \[r]     [a,o,i] -> PrimInline $ r |= read_boff_i8  a (off8  o i)
  IndexOffAddrOp_Int16     -> \[r]     [a,o,i] -> PrimInline $ r |= read_boff_i16 a (off16 o i)
  IndexOffAddrOp_Int32     -> \[r]     [a,o,i] -> PrimInline $ r |= read_boff_i32 a (off32 o i)
  IndexOffAddrOp_Int64     -> \[h,l]   [a,o,i] -> PrimInline $ read_boff_i64 a (off64 o i) h l
  IndexOffAddrOp_Word8     -> \[r]     [a,o,i] -> PrimInline $ r |= read_boff_u8  a (off8  o i)
  IndexOffAddrOp_Word16    -> \[r]     [a,o,i] -> PrimInline $ r |= read_boff_u16 a (off16 o i)
  IndexOffAddrOp_Word32    -> \[r]     [a,o,i] -> PrimInline $ r |= read_boff_u32 a (off32 o i)
  IndexOffAddrOp_Word64    -> \[h,l]   [a,o,i] -> PrimInline $ read_boff_u64 a (off64 o i) h l

  ReadOffAddrOp_Char       -> \[r]     [a,o,i] -> PrimInline $ r |= read_boff_u8  a (off8  o i)
  ReadOffAddrOp_WideChar   -> \[r]     [a,o,i] -> PrimInline $ r |= read_boff_i32 a (off32 o i)
  ReadOffAddrOp_Int        -> \[r]     [a,o,i] -> PrimInline $ r |= read_boff_i32 a (off32 o i)
  ReadOffAddrOp_Word       -> \[r]     [a,o,i] -> PrimInline $ r |= read_boff_u32 a (off32 o i)
  ReadOffAddrOp_Addr       -> \[ra,ro] [a,o,i] -> PrimInline $ read_boff_addr a (off32 o i) ra ro
  ReadOffAddrOp_Float      -> \[r]     [a,o,i] -> PrimInline $ r |= read_boff_f32 a (off32 o i)
  ReadOffAddrOp_Double     -> \[r]     [a,o,i] -> PrimInline $ r |= read_boff_f64 a (off64 o i)
  ReadOffAddrOp_StablePtr  -> \[ra,ro] [a,o,i] -> PrimInline $ read_boff_stableptr a (off32 o i) ra ro
  ReadOffAddrOp_Int8       -> \[r]     [a,o,i] -> PrimInline $ r |= read_boff_i8  a (off8  o i)
  ReadOffAddrOp_Int16      -> \[r]     [a,o,i] -> PrimInline $ r |= read_boff_i16 a (off16 o i)
  ReadOffAddrOp_Int32      -> \[r]     [a,o,i] -> PrimInline $ r |= read_boff_i32 a (off32 o i)
  ReadOffAddrOp_Int64      -> \[h,l]   [a,o,i] -> PrimInline $ read_boff_i64 a (off64 o i) h l
  ReadOffAddrOp_Word8      -> \[r]     [a,o,i] -> PrimInline $ r |= read_boff_u8  a (off8  o i)
  ReadOffAddrOp_Word16     -> \[r]     [a,o,i] -> PrimInline $ r |= read_boff_u16 a (off16 o i)
  ReadOffAddrOp_Word32     -> \[r]     [a,o,i] -> PrimInline $ r |= read_boff_u32 a (off32 o i)
  ReadOffAddrOp_Word64     -> \[h,l]   [a,o,i] -> PrimInline $ read_boff_u64 a (off64 o i) h l

  WriteOffAddrOp_Char      -> \[] [a,o,i,v]     -> PrimInline $ write_boff_u8  a (off8  o i) v
  WriteOffAddrOp_WideChar  -> \[] [a,o,i,v]     -> PrimInline $ write_boff_i32 a (off32 o i) v
  WriteOffAddrOp_Int       -> \[] [a,o,i,v]     -> PrimInline $ write_boff_i32 a (off32 o i) v
  WriteOffAddrOp_Word      -> \[] [a,o,i,v]     -> PrimInline $ write_boff_u32 a (off32 o i) v
  WriteOffAddrOp_Addr      -> \[] [a,o,i,va,vo] -> PrimInline $ write_boff_addr a (off32 o i) va vo
  WriteOffAddrOp_Float     -> \[] [a,o,i,v]     -> PrimInline $ write_boff_f32 a (off32 o i) v
  WriteOffAddrOp_Double    -> \[] [a,o,i,v]     -> PrimInline $ write_boff_f64 a (off64 o i) v
  WriteOffAddrOp_StablePtr -> \[] [a,o,i,va,vo] -> PrimInline $ write_boff_stableptr a (off32 o i) va vo
  WriteOffAddrOp_Int8      -> \[] [a,o,i,v]     -> PrimInline $ write_boff_i8  a (off8  o i) v
  WriteOffAddrOp_Int16     -> \[] [a,o,i,v]     -> PrimInline $ write_boff_i16 a (off16 o i) v
  WriteOffAddrOp_Int32     -> \[] [a,o,i,v]     -> PrimInline $ write_boff_i32 a (off32 o i) v
  WriteOffAddrOp_Int64     -> \[] [a,o,i,h,l]   -> PrimInline $ write_boff_i64 a (off64 o i) h l
  WriteOffAddrOp_Word8     -> \[] [a,o,i,v]     -> PrimInline $ write_boff_u8  a (off8  o i) v
  WriteOffAddrOp_Word16    -> \[] [a,o,i,v]     -> PrimInline $ write_boff_u16 a (off16 o i) v
  WriteOffAddrOp_Word32    -> \[] [a,o,i,v]     -> PrimInline $ write_boff_u32 a (off32 o i) v
  WriteOffAddrOp_Word64    -> \[] [a,o,i,h,l]   -> PrimInline $ write_boff_u64 a (off64 o i) h l

------------------------------- Mutable varialbes --------------------------------------
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

  ForkOp                 -> \[_tid] [x]    -> PRPrimCall $ returnS (app "h$fork" [x, true_])
  ForkOnOp               -> \[_tid] [_p,x] -> PRPrimCall $ returnS (app "h$fork" [x, true_]) -- ignore processor argument
  KillThreadOp           -> \[] [tid,ex]   -> PRPrimCall $ returnS (app "h$killThread" [tid,ex])
  YieldOp                -> \[] []         -> PRPrimCall $ returnS (app "h$yield" [])
  MyThreadIdOp           -> \[r] []        -> PrimInline $ r |= var "h$currentThread"
  IsCurrentThreadBoundOp -> \[r] []        -> PrimInline $ r |= one_
  NoDuplicateOp          -> \[] []         -> PrimInline mempty -- don't need to do anything as long as we have eager blackholing
  ThreadStatusOp         -> \[stat,cap,locked] [tid] -> PrimInline $ appT [stat, cap, locked] "h$threadStatus" [tid]
  ListThreadsOp          -> \[r] [] -> PrimInline $ appT [r] "h$listThreads" []
  GetThreadLabelOp       -> \[r1, r2] [t]  -> PrimInline $ appT [r1, r2] "h$getThreadLabel" [t]
  LabelThreadOp          -> \[] [t,l]      -> PrimInline $ t .^ "label" |= l

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

------------------------------ ByteArray -------------------

  IndexByteArrayOp_Word8AsChar      -> \[r]   [a,i] -> PrimInline $ bnd_ba8  bound a i $ r |= read_boff_u8  a i
  IndexByteArrayOp_Word8AsWideChar  -> \[r]   [a,i] -> PrimInline $ bnd_ba32 bound a i $ r |= read_boff_i32 a i
  IndexByteArrayOp_Word8AsAddr      -> \[r,o] [a,i] -> PrimInline $ bnd_ba32 bound a i $ read_boff_addr a i r o
  IndexByteArrayOp_Word8AsFloat     -> \[r]   [a,i] -> PrimInline $ bnd_ba32 bound a i $ r |= read_boff_f32 a i
  IndexByteArrayOp_Word8AsDouble    -> \[r]   [a,i] -> PrimInline $ bnd_ba64 bound a i $ r |= read_boff_f64 a i
  IndexByteArrayOp_Word8AsStablePtr -> \[r,o] [a,i] -> PrimInline $ bnd_ba32 bound a i $ read_boff_stableptr a i r o
  IndexByteArrayOp_Word8AsInt16     -> \[r]   [a,i] -> PrimInline $ bnd_ba16 bound a i $ r |= read_boff_i16 a i
  IndexByteArrayOp_Word8AsInt32     -> \[r]   [a,i] -> PrimInline $ bnd_ba32 bound a i $ r |= read_boff_i32 a i
  IndexByteArrayOp_Word8AsInt64     -> \[h,l] [a,i] -> PrimInline $ bnd_ba64 bound a i $ read_boff_i64 a i h l
  IndexByteArrayOp_Word8AsInt       -> \[r]   [a,i] -> PrimInline $ bnd_ba32 bound a i $ r |= read_boff_i32  a i
  IndexByteArrayOp_Word8AsWord16    -> \[r]   [a,i] -> PrimInline $ bnd_ba16 bound a i $ r |= read_boff_u16  a i
  IndexByteArrayOp_Word8AsWord32    -> \[r]   [a,i] -> PrimInline $ bnd_ba32 bound a i $ r |= read_boff_u32  a i
  IndexByteArrayOp_Word8AsWord64    -> \[h,l] [a,i] -> PrimInline $ bnd_ba64 bound a i $ read_boff_u64 a i h l
  IndexByteArrayOp_Word8AsWord      -> \[r]   [a,i] -> PrimInline $ bnd_ba32 bound a i $ r |= read_boff_u32  a i

  ReadByteArrayOp_Word8AsChar       -> \[r]   [a,i] -> PrimInline $ bnd_ba8  bound a i $ r |= read_boff_u8  a i
  ReadByteArrayOp_Word8AsWideChar   -> \[r]   [a,i] -> PrimInline $ bnd_ba32 bound a i $ r |= read_boff_i32 a i
  ReadByteArrayOp_Word8AsAddr       -> \[r,o] [a,i] -> PrimInline $ bnd_ba32 bound a i $ read_boff_addr a i r o
  ReadByteArrayOp_Word8AsFloat      -> \[r]   [a,i] -> PrimInline $ bnd_ba32 bound a i $ r |= read_boff_f32 a i
  ReadByteArrayOp_Word8AsDouble     -> \[r]   [a,i] -> PrimInline $ bnd_ba64 bound a i $ r |= read_boff_f64 a i
  ReadByteArrayOp_Word8AsStablePtr  -> \[r,o] [a,i] -> PrimInline $ bnd_ba32 bound a i $ read_boff_stableptr a i r o
  ReadByteArrayOp_Word8AsInt16      -> \[r]   [a,i] -> PrimInline $ bnd_ba16 bound a i $ r |= read_boff_i16 a i
  ReadByteArrayOp_Word8AsInt32      -> \[r]   [a,i] -> PrimInline $ bnd_ba32 bound a i $ r |= read_boff_i32 a i
  ReadByteArrayOp_Word8AsInt64      -> \[h,l] [a,i] -> PrimInline $ bnd_ba64 bound a i $ read_boff_i64 a i h l
  ReadByteArrayOp_Word8AsInt        -> \[r]   [a,i] -> PrimInline $ bnd_ba32 bound a i $ r |= read_boff_i32  a i
  ReadByteArrayOp_Word8AsWord16     -> \[r]   [a,i] -> PrimInline $ bnd_ba16 bound a i $ r |= read_boff_u16  a i
  ReadByteArrayOp_Word8AsWord32     -> \[r]   [a,i] -> PrimInline $ bnd_ba32 bound a i $ r |= read_boff_u32  a i
  ReadByteArrayOp_Word8AsWord64     -> \[h,l] [a,i] -> PrimInline $ bnd_ba64 bound a i $ read_boff_u64 a i h l
  ReadByteArrayOp_Word8AsWord       -> \[r]   [a,i] -> PrimInline $ bnd_ba32 bound a i $ r |= read_boff_u32  a i

  WriteByteArrayOp_Word8AsChar      -> \[] [a,i,e]   -> PrimInline $ bnd_ba8  bound a i $ write_boff_i8  a i e
  WriteByteArrayOp_Word8AsWideChar  -> \[] [a,i,e]   -> PrimInline $ bnd_ba32 bound a i $ write_boff_i32 a i e
  WriteByteArrayOp_Word8AsAddr      -> \[] [a,i,r,o] -> PrimInline $ bnd_ba32 bound a i $ write_boff_addr a i r o
  WriteByteArrayOp_Word8AsFloat     -> \[] [a,i,e]   -> PrimInline $ bnd_ba32 bound a i $ write_boff_f32 a i e
  WriteByteArrayOp_Word8AsDouble    -> \[] [a,i,e]   -> PrimInline $ bnd_ba64 bound a i $ write_boff_f64 a i e
  WriteByteArrayOp_Word8AsStablePtr -> \[] [a,i,_,o] -> PrimInline $ bnd_ba32 bound a i $ write_boff_i32 a i o
  WriteByteArrayOp_Word8AsInt16     -> \[] [a,i,e]   -> PrimInline $ bnd_ba16 bound a i $ write_boff_i16 a i e
  WriteByteArrayOp_Word8AsInt32     -> \[] [a,i,e]   -> PrimInline $ bnd_ba32 bound a i $ write_boff_i32 a i e
  WriteByteArrayOp_Word8AsInt64     -> \[] [a,i,h,l] -> PrimInline $ bnd_ba64 bound a i $ write_boff_i64 a i h l
  WriteByteArrayOp_Word8AsInt       -> \[] [a,i,e]   -> PrimInline $ bnd_ba32 bound a i $ write_boff_i32 a i e
  WriteByteArrayOp_Word8AsWord16    -> \[] [a,i,e]   -> PrimInline $ bnd_ba16 bound a i $ write_boff_u16 a i e
  WriteByteArrayOp_Word8AsWord32    -> \[] [a,i,e]   -> PrimInline $ bnd_ba32 bound a i $ write_boff_u32 a i e
  WriteByteArrayOp_Word8AsWord64    -> \[] [a,i,h,l] -> PrimInline $ bnd_ba64 bound a i $ write_boff_u64 a i h l
  WriteByteArrayOp_Word8AsWord      -> \[] [a,i,e]   -> PrimInline $ bnd_ba32 bound a i $ write_boff_u32 a i e

  CasByteArrayOp_Int                -> \[r] [a,i,o,n] -> PrimInline $ bnd_ix32 bound a i $ casOp read_i32 write_i32 r a i o n
  CasByteArrayOp_Int8               -> \[r] [a,i,o,n] -> PrimInline $ bnd_ix8  bound a i $ casOp read_i8  write_i8  r a i o n
  CasByteArrayOp_Int16              -> \[r] [a,i,o,n] -> PrimInline $ bnd_ix16 bound a i $ casOp read_i16 write_i16 r a i o n
  CasByteArrayOp_Int32              -> \[r] [a,i,o,n] -> PrimInline $ bnd_ix32 bound a i $ casOp read_i32 write_i32 r a i o n

  CasByteArrayOp_Int64              -> \[rh,rl] [a,i,oh,ol,nh,nl] -> PrimInline $ bnd_ix64 bound a i $ casOp2 read_i64 write_i64 (rh,rl) a i (oh,ol) (nh,nl)

  CasAddrOp_Addr                    -> \[ra,ro] [a,o,oa,oo,na,no] -> PrimInline $ casOp2 read_boff_addr write_boff_addr (ra,ro) a o (oa,oo) (na,no)
  CasAddrOp_Word                    -> \[r] [a,o,old,new] -> PrimInline $ casOp read_u32 write_u32 r a o old new
  CasAddrOp_Word8                   -> \[r] [a,o,old,new] -> PrimInline $ casOp read_u8  write_u8  r a o old new
  CasAddrOp_Word16                  -> \[r] [a,o,old,new] -> PrimInline $ casOp read_u16 write_u16 r a o old new
  CasAddrOp_Word32                  -> \[r] [a,o,old,new] -> PrimInline $ casOp read_u32 write_u32 r a o old new
  CasAddrOp_Word64                  -> \[rh,rl] [a,o,oh,ol,nh,nl] -> PrimInline $ casOp2 read_u64 write_u64 (rh,rl) a o (oh,ol) (nh,nl)

  FetchAddAddrOp_Word               -> \[r] [a,o,v] -> PrimInline $ fetchOpAddr Add   r a o v
  FetchSubAddrOp_Word               -> \[r] [a,o,v] -> PrimInline $ fetchOpAddr Sub   r a o v
  FetchAndAddrOp_Word               -> \[r] [a,o,v] -> PrimInline $ fetchOpAddr BAnd  r a o v
  FetchNandAddrOp_Word              -> \[r] [a,o,v] -> PrimInline $ fetchOpAddr ((BNot .) . BAnd) r a o v
  FetchOrAddrOp_Word                -> \[r] [a,o,v] -> PrimInline $ fetchOpAddr BOr   r a o v
  FetchXorAddrOp_Word               -> \[r] [a,o,v] -> PrimInline $ fetchOpAddr BXor  r a o v

  InterlockedExchange_Addr          -> \[ra,ro] [a1,o1,a2,o2] -> PrimInline $ mconcat
                                          [ read_boff_addr a1 o1 ra ro
                                          , write_boff_addr a1 o1 a2 o2
                                          ]
  InterlockedExchange_Word          -> \[r] [a,o,w] -> PrimInline $ mconcat
                                          [ r |= read_boff_u32 a o
                                          , write_boff_u32 a o w
                                          ]

  ShrinkSmallMutableArrayOp_Char    -> \[]  [a,n] -> PrimInline $ appS "h$shrinkMutableCharArray" [a,n]
  GetSizeofSmallMutableArrayOp      -> \[r] [a]   -> PrimInline $ r |= a .^ "length"

  AtomicReadAddrOp_Word             -> \[r] [a,o]   -> PrimInline $ r |= read_boff_u32 a o
  AtomicWriteAddrOp_Word            -> \[]  [a,o,w] -> PrimInline $ write_boff_u32 a o w


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

  VecIndexScalarByteArrayOp _ _ _   -> unhandledPrimop op
  VecReadScalarByteArrayOp _ _ _    -> unhandledPrimop op
  VecWriteScalarByteArrayOp _ _ _   -> unhandledPrimop op
  VecIndexScalarOffAddrOp _ _ _     -> unhandledPrimop op
  VecReadScalarOffAddrOp _ _ _      -> unhandledPrimop op
  VecWriteScalarOffAddrOp _ _ _     -> unhandledPrimop op

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

-- | A No Op, used for primops the JS platform cannot or do not support. For
-- example, the prefetching primops do not make sense on the JS platform because
-- we do not have enough control over memory to provide any kind of prefetching
-- mechanism. Hence, these are NoOps.
noOp :: Foldable f => f a -> f a -> PrimRes
noOp = const . const $ PrimInline mempty

-- tuple returns
appT :: [JExpr] -> FastString -> [JExpr] -> JStat
appT []     f xs = appS f xs
appT (r:rs) f xs = mconcat
  [ r |= app f xs
  , mconcat (zipWith (\r ret -> r |= toJExpr ret) rs (enumFrom Ret1))
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

idx_i32, idx_u8, idx_u16, idx_f64, idx_f32 :: JExpr -> JExpr -> JExpr
idx_i32 a i = IdxExpr (a .^ "i3") i
idx_u8  a i = IdxExpr (a .^ "u8") i
idx_u16 a i = IdxExpr (a .^ "u1") i
idx_f64 a i = IdxExpr (a .^ "f6") i
idx_f32 a i = IdxExpr (a .^ "f3") i

read_u8 :: JExpr -> JExpr -> JExpr
read_u8 a i = idx_u8 a i

read_u16 :: JExpr -> JExpr -> JExpr
read_u16 a i = idx_u16 a i

read_u32 :: JExpr -> JExpr -> JExpr
read_u32 a i = toU32 (idx_i32 a i)

read_i8 :: JExpr -> JExpr -> JExpr
read_i8 a i = signExtend8 (idx_u8 a i)

read_i16 :: JExpr -> JExpr -> JExpr
read_i16 a i = signExtend16 (idx_u16 a i)

read_i32 :: JExpr -> JExpr -> JExpr
read_i32 a i = idx_i32 a i

read_f32 :: JExpr -> JExpr -> JExpr
read_f32 a i = idx_f32 a i

read_f64 :: JExpr -> JExpr -> JExpr
read_f64 a i = idx_f64 a i

read_u64 :: JExpr -> JExpr -> JExpr -> JExpr -> JStat
read_u64 a i rh rl = mconcat
  [ rl |= read_u32 a (i .<<. 1)
  , rh |= read_u32 a (Add 1 (i .<<. 1))
  ]

read_i64 :: JExpr -> JExpr -> JExpr -> JExpr -> JStat
read_i64 a i rh rl = mconcat
  [ rl |= read_u32 a (i .<<. 1)
  , rh |= read_i32 a (Add 1 (i .<<. 1))
  ]

--------------------------------------
-- Addr#
--------------------------------------

write_addr :: JExpr -> JExpr -> JExpr -> JExpr -> JStat
write_addr a i r o = mconcat
  [ write_i32 a i o
    -- create the hidden array for arrays if it doesn't exist
  , ifS (Not (a .^ "arr")) (a .^ "arr" |= ValExpr (JList [])) mempty
  , a .^ "arr" .! (i .<<. 2) |= r
  ]

read_addr :: JExpr -> JExpr -> JExpr -> JExpr -> JStat
read_addr a i r o = mconcat
  [ o |= read_i32 a i
  , r |= if_ ((a .^ "arr") .&&. (a .^ "arr" .! (i .<<. 2)))
            (a .^ "arr" .! (i .<<. 2))
            null_
  ]

read_boff_addr :: JExpr -> JExpr -> JExpr -> JExpr -> JStat
read_boff_addr a i r o = mconcat
  [ o |= read_boff_i32 a i
  , r |= if_ ((a .^ "arr") .&&. (a .^ "arr" .! i))
            (a .^ "arr" .! i)
            null_
  ]

write_boff_addr :: JExpr -> JExpr -> JExpr -> JExpr -> JStat
write_boff_addr a i r o = mconcat
  [ write_boff_i32 a i o
    -- create the hidden array for arrays if it doesn't exist
  , ifS (Not (a .^ "arr")) (a .^ "arr" |= ValExpr (JList [])) mempty
  , a .^ "arr" .! i |= r
  ]


--------------------------------------
-- StablePtr
--------------------------------------

read_stableptr :: JExpr -> JExpr -> JExpr -> JExpr -> JStat
read_stableptr a i r o = mconcat
  [ r |= var "h$stablePtrBuf" -- stable pointers are always in this array
  , o |= read_i32 a i
  ]

read_boff_stableptr :: JExpr -> JExpr -> JExpr -> JExpr -> JStat
read_boff_stableptr a i r o = mconcat
  [ r |= var "h$stablePtrBuf" -- stable pointers are always in this array
  , o |= read_boff_i32 a i
  ]

write_stableptr :: JExpr -> JExpr -> JExpr -> JExpr -> JStat
write_stableptr a i _r o = write_i32 a i o
  -- don't store "r" as it must be h$stablePtrBuf

write_boff_stableptr :: JExpr -> JExpr -> JExpr -> JExpr -> JStat
write_boff_stableptr a i _r o = write_boff_i32 a i o
  -- don't store "r" as it must be h$stablePtrBuf

write_u8 :: JExpr -> JExpr -> JExpr -> JStat
write_u8 a i v = idx_u8 a i |= v

write_u16 :: JExpr -> JExpr -> JExpr -> JStat
write_u16 a i v = idx_u16 a i |= v

write_u32 :: JExpr -> JExpr -> JExpr -> JStat
write_u32 a i v = idx_i32 a i |= v

write_i8 :: JExpr -> JExpr -> JExpr -> JStat
write_i8 a i v = idx_u8 a i |= v

write_i16 :: JExpr -> JExpr -> JExpr -> JStat
write_i16 a i v = idx_u16 a i |= v

write_i32 :: JExpr -> JExpr -> JExpr -> JStat
write_i32 a i v = idx_i32 a i |= v

write_f32 :: JExpr -> JExpr -> JExpr -> JStat
write_f32 a i v = idx_f32 a i |= v

write_f64 :: JExpr -> JExpr -> JExpr -> JStat
write_f64 a i v = idx_f64 a i |= v

write_u64 :: JExpr -> JExpr -> JExpr -> JExpr -> JStat
write_u64 a i h l = mconcat
  [ write_u32 a (i .<<. 1)         l
  , write_u32 a (Add 1 (i .<<. 1)) h
  ]

write_i64 :: JExpr -> JExpr -> JExpr -> JExpr -> JStat
write_i64 a i h l = mconcat
  [ write_u32 a (i .<<. 1)         l
  , write_i32 a (Add 1 (i .<<. 1)) h
  ]

-- Data View helper functions: byte indexed!
--
-- The argument list consists of the array @a@, the index @i@, and the new value
-- to set (in the case of a setter) @v@.

write_boff_i8, write_boff_u8, write_boff_i16, write_boff_u16, write_boff_i32, write_boff_u32, write_boff_f32, write_boff_f64 :: JExpr -> JExpr -> JExpr -> JStat
write_boff_i8  a i v = write_i8 a i v
write_boff_u8  a i v = write_u8 a i v
write_boff_i16 a i v = ApplStat (a .^ "dv" .^ "setInt16"  ) [i, v, true_]
write_boff_u16 a i v = ApplStat (a .^ "dv" .^ "setUint16" ) [i, v, true_]
write_boff_i32 a i v = ApplStat (a .^ "dv" .^ "setInt32"  ) [i, v, true_]
write_boff_u32 a i v = ApplStat (a .^ "dv" .^ "setUint32" ) [i, v, true_]
write_boff_f32 a i v = ApplStat (a .^ "dv" .^ "setFloat32") [i, v, true_]
write_boff_f64 a i v = ApplStat (a .^ "dv" .^ "setFloat64") [i, v, true_]

write_boff_i64, write_boff_u64 :: JExpr -> JExpr -> JExpr -> JExpr -> JStat
write_boff_i64 a i h l = mconcat
  [ write_boff_i32 a (Add i (Int 4)) h
  , write_boff_u32 a i l
  ]
write_boff_u64 a i h l = mconcat
  [ write_boff_u32 a (Add i (Int 4)) h
  , write_boff_u32 a i l
  ]

read_boff_i8, read_boff_u8, read_boff_i16, read_boff_u16, read_boff_i32, read_boff_u32, read_boff_f32, read_boff_f64 :: JExpr -> JExpr -> JExpr
read_boff_i8  a i = read_i8 a i
read_boff_u8  a i = read_u8 a i
read_boff_i16 a i = ApplExpr (a .^ "dv" .^ "getInt16"  ) [i, true_]
read_boff_u16 a i = ApplExpr (a .^ "dv" .^ "getUint16" ) [i, true_]
read_boff_i32 a i = ApplExpr (a .^ "dv" .^ "getInt32"  ) [i, true_]
read_boff_u32 a i = ApplExpr (a .^ "dv" .^ "getUint32" ) [i, true_]
read_boff_f32 a i = ApplExpr (a .^ "dv" .^ "getFloat32") [i, true_]
read_boff_f64 a i = ApplExpr (a .^ "dv" .^ "getFloat64") [i, true_]

read_boff_i64 :: JExpr -> JExpr -> JExpr -> JExpr -> JStat
read_boff_i64 a i rh rl = mconcat
  [ rh |= read_boff_i32 a (Add i (Int 4))
  , rl |= read_boff_u32 a i
  ]

read_boff_u64 :: JExpr -> JExpr -> JExpr -> JExpr -> JStat
read_boff_u64 a i rh rl = mconcat
  [ rh |= read_boff_u32 a (Add i (Int 4))
  , rl |= read_boff_u32 a i
  ]

fetchOpByteArray :: (JExpr -> JExpr -> JExpr) -> JExpr -> JExpr -> JExpr -> JExpr -> JStat
fetchOpByteArray op tgt src i v = mconcat
  [ tgt |= read_i32 src i
  , write_i32 src i (op tgt v)
  ]

fetchOpAddr :: (JExpr -> JExpr -> JExpr) -> JExpr -> JExpr -> JExpr -> JExpr -> JStat
fetchOpAddr op tgt src i v = mconcat
  [ tgt |= read_boff_u32 src i
  , write_boff_u32 src i (op tgt v)
  ]

casOp
  :: (JExpr -> JExpr -> JExpr)          -- read
  -> (JExpr -> JExpr -> JExpr -> JStat) -- write
  -> JExpr                     -- target register to store result
  -> JExpr                     -- source array
  -> JExpr                     -- index
  -> JExpr                     -- old value to compare
  -> JExpr                     -- new value to write
  -> JStat
casOp read write tgt src i old new = mconcat
  [ tgt |= read src i
  , ifS (tgt .===. old)
        (write src i new)
         mempty
  ]

casOp2
  :: (JExpr -> JExpr -> JExpr -> JExpr -> JStat) -- read
  -> (JExpr -> JExpr -> JExpr -> JExpr -> JStat) -- write
  -> (JExpr,JExpr)             -- target registers to store result
  -> JExpr                     -- source array
  -> JExpr                     -- index
  -> (JExpr,JExpr)             -- old value to compare
  -> (JExpr,JExpr)             -- new value to write
  -> JStat
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
cloneArray :: Bool -> JExpr -> JExpr -> JExpr -> JExpr -> JStat
cloneArray bound_check tgt src start len =
  bnd_arr_range bound_check src start len
  $ mconcat
      [ tgt |= ApplExpr (src .^ "slice") [start, Add len start]
      , tgt .^ closureMeta_   |= zero_
      , tgt .^ "__ghcjsArray" |= true_
      ]

newByteArray :: JExpr -> JExpr -> JStat
newByteArray tgt len =
  tgt |= app "h$newByteArray" [len]

-- | Check that index is positive and below a max value. Halt the process with
-- error code 134 otherwise. This is used to implement -fcheck-prim-bounds
check_bound
  :: JExpr -- ^ Max index expression
  -> Bool  -- ^ Should we do bounds checking?
  -> JExpr -- ^ Index
  -> JStat -- ^ Result
  -> JStat
check_bound _         False _ r = r
check_bound max_index True  i r = mconcat
  [ jwhenS ((i .<. zero_) .||. (i .>=. max_index)) $
      returnS (app "h$exitProcess" [Int 134])
  , r
  ]

-- | Bounds checking using ".length" property (Arrays)
bnd_arr
  :: Bool  -- ^ Should we do bounds checking?
  -> JExpr -- ^ Array
  -> JExpr -- ^ Index
  -> JStat -- ^ Result
  -> JStat
bnd_arr do_check arr = check_bound (arr .^ "length") do_check

-- | Range bounds checking using ".length" property (Arrays)
--
-- Empty ranges trivially pass the check
bnd_arr_range
  :: Bool  -- ^ Should we do bounds checking?
  -> JExpr -- ^ Array
  -> JExpr -- ^ Index
  -> JExpr -- ^ Range size
  -> JStat -- ^ Result
  -> JStat
bnd_arr_range False _arr _i _n r = r
bnd_arr_range True   arr  i  n r =
  ifS (n .<. zero_) (returnS $ app "h$exitProcess" [Int 134]) $
  -- Empty ranges trivially pass the check
  ifS (n .===. zero_)
      r
      (bnd_arr True arr i $ bnd_arr True arr (Add i (Sub n 1)) r)

-- | Bounds checking using ".len" property (ByteArrays)
bnd_ba
  :: Bool  -- ^ Should we do bounds checking?
  -> JExpr -- ^ Array
  -> JExpr -- ^ Index
  -> JStat -- ^ Result
  -> JStat
bnd_ba do_check arr = check_bound (arr .^ "len") do_check

-- | ByteArray bounds checking (byte offset, 8-bit value)
bnd_ba8 :: Bool -> JExpr -> JExpr -> JStat -> JStat
bnd_ba8 = bnd_ba

-- | ByteArray bounds checking (byte offset, 16-bit value)
bnd_ba16 :: Bool -> JExpr -> JExpr -> JStat -> JStat
bnd_ba16 do_check arr idx r =
  -- check that idx non incremented is in range:
  -- (idx + 1) may be in range while idx isn't
  bnd_ba do_check arr idx
  $ bnd_ba do_check arr (Add idx 1) r

-- | ByteArray bounds checking (byte offset, 32-bit value)
bnd_ba32 :: Bool -> JExpr -> JExpr -> JStat -> JStat
bnd_ba32 do_check arr idx r =
  -- check that idx non incremented is in range:
  -- (idx + 3) may be in range while idx isn't
  bnd_ba do_check arr idx
  $ bnd_ba do_check arr (Add idx 3) r

-- | ByteArray bounds checking (byte offset, 64-bit value)
bnd_ba64 :: Bool -> JExpr -> JExpr -> JStat -> JStat
bnd_ba64 do_check arr idx r =
  -- check that idx non incremented is in range:
  -- (idx + 7) may be in range while idx isn't
  bnd_ba do_check arr idx
  $ bnd_ba do_check arr (Add idx 7) r

-- | ByteArray bounds checking (8-bit offset, 8-bit value)
bnd_ix8 :: Bool -> JExpr -> JExpr -> JStat -> JStat
bnd_ix8 = bnd_ba8

-- | ByteArray bounds checking (16-bit offset, 16-bit value)
bnd_ix16 :: Bool -> JExpr -> JExpr -> JStat -> JStat
bnd_ix16 do_check arr idx r = bnd_ba16 do_check arr (idx .<<. 1) r

-- | ByteArray bounds checking (32-bit offset, 32-bit value)
bnd_ix32 :: Bool -> JExpr -> JExpr -> JStat -> JStat
bnd_ix32 do_check arr idx r = bnd_ba32 do_check arr (idx .<<. 2) r

-- | ByteArray bounds checking (64-bit offset, 64-bit value)
bnd_ix64 :: Bool -> JExpr -> JExpr -> JStat -> JStat
bnd_ix64 do_check arr idx r = bnd_ba64 do_check arr (idx .<<. 3) r

-- | Bounds checking on a range and using ".len" property (ByteArrays)
--
-- Empty ranges trivially pass the check
bnd_ba_range
  :: Bool  -- ^ Should we do bounds checking?
  -> JExpr -- ^ Array
  -> JExpr -- ^ Index
  -> JExpr -- ^ Range size
  -> JStat -- ^ Result
  -> JStat
bnd_ba_range False _  _ _ r = r
bnd_ba_range True  xs i n r =
  ifS (n .<. zero_) (returnS $ app "h$exitProcess" [Int 134]) $
  -- Empty ranges trivially pass the check
  ifS (n .===. zero_)
      r
      (bnd_ba True xs (Add i (Sub n 1)) (bnd_ba True xs i r))

checkOverlapByteArray
  :: Bool  -- ^ Should we do bounds checking?
  -> JExpr -- ^ First array
  -> JExpr -- ^ First offset
  -> JExpr -- ^ Second array
  -> JExpr -- ^ Second offset
  -> JExpr -- ^ Range size
  -> JStat -- ^ Result
  -> JStat
checkOverlapByteArray False _ _ _ _ _ r    = r
checkOverlapByteArray True a1 o1 a2 o2 n r =
  ifS (app "h$checkOverlapByteArray" [a1, o1, a2, o2, n])
    r
    (returnS $ app "h$exitProcess" [Int 134])

copyByteArray :: Bool -> Bool -> JExpr -> JExpr -> JExpr -> JExpr -> JExpr -> PrimRes
copyByteArray allow_overlap bound a1 o1 a2 o2 n = PrimInline $ check $ appS "h$copyMutableByteArray" [a1,o1,a2,o2,n]
  where
      check = bnd_ba_range bound a1 o1 n
              . bnd_ba_range bound a2 o2 n
              . (if not allow_overlap then checkOverlapByteArray bound a1 o1 a2 o2 n else id)

-- e|0 (32 bit signed integer truncation) required because of JS numbers. e|0
-- converts e to an Int32. Note that e|0 _is still a Double_ because JavaScript.
-- So (x|0) * (y|0) can still return values outside of the Int32 range. You have
-- been warned!
toI32 :: JExpr -> JExpr
toI32 e = BOr e zero_

-- e>>>0  (32 bit unsigned integer truncation)
-- required because of JS numbers. e>>>0 converts e to a Word32
-- so  (-2147483648)       >>> 0  = 2147483648
-- and ((-2147483648) >>>0) | 0   = -2147483648
toU32 :: JExpr -> JExpr
toU32 e = e .>>>. zero_

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
