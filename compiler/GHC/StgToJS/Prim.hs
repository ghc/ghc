{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

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

import GHC.Data.ShortText (ShortText)
import qualified GHC.Data.ShortText as ST
import GHC.Utils.Outputable (renderWithContext, defaultSDocContext, ppr)
import Data.Maybe


genPrim :: Bool     -- ^ Profiling (cost-centres) enabled
        -> Type
        -> PrimOp   -- ^ the primitive operation
        -> [JExpr]  -- ^ where to store the result
        -> [JExpr]  -- ^ arguments
        -> PrimRes
genPrim _ _ CharGtOp          [r] [x,y] = PrimInline $ r |= if10 (x .>. y)
genPrim _ _ CharGeOp          [r] [x,y] = PrimInline $ r |= if10 (x .>=. y)
genPrim _ _ CharEqOp          [r] [x,y] = PrimInline $ r |= if10 (x .===. y)
genPrim _ _ CharNeOp          [r] [x,y] = PrimInline $ r |= if10 (x .!==. y)
genPrim _ _ CharLtOp          [r] [x,y] = PrimInline $ r |= if10 (x .<. y)
genPrim _ _ CharLeOp          [r] [x,y] = PrimInline $ r |= if10 (x .<=. y)
genPrim _ _ OrdOp             [r] [x]   = PrimInline $ r |= x

genPrim _ _ IntAddOp          [r] [x,y] = PrimInline $ r |= trunc (Add x y)
genPrim _ _ IntSubOp          [r] [x,y] = PrimInline $ r |= trunc (Sub x y)
genPrim _ _ IntMulOp          [r] [x,y] =
    PrimInline $ r |= app "h$mulInt32" [x, y]
-- fixme may will give the wrong result in case of overflow
genPrim _ _ IntMulMayOfloOp   [r] [x,y] =
    PrimInline $ jVar \tmp -> mconcat
      [ tmp |= Mul x y
      , r   |= if01 (tmp .===. trunc tmp)
      ]
genPrim _ _ IntQuotOp         [r] [x,y] = PrimInline $ r |= trunc (Div x y)
genPrim _ _ IntRemOp          [r] [x,y] = PrimInline $ r |= Mod x y
genPrim _ _ IntQuotRemOp    [q,r] [x,y] = PrimInline $ mconcat
                                            [ q |= trunc (Div x y)
                                            , r |= x `Sub` (Mul y q)
                                            ]
genPrim _ _ IntAndOp [r] [x,y]          = PrimInline $ r |= BAnd x y
genPrim _ _ IntOrOp  [r] [x,y]          = PrimInline $ r |= BOr  x y
genPrim _ _ IntXorOp [r] [x,y]          = PrimInline $ r |= BXor x y
genPrim _ _ IntNotOp [r] [x]            = PrimInline $ r |= BNot x

genPrim _ _ IntNegOp          [r] [x]   = PrimInline $ r |= trunc (Negate x)
-- add with carry: overflow == 0 iff no overflow
genPrim _ _ IntAddCOp         [r,overf] [x,y] =
  PrimInline $ jVar \rt -> mconcat
    [ rt    |= Add x y
    , r     |= trunc rt
    , overf |= if10 (r .!=. rt)
    ]
genPrim _ _ IntSubCOp         [r,overf] [x,y] =
  PrimInline $ jVar \rt -> mconcat
    [ rt    |= Sub x y
    , r     |= trunc rt
    , overf |= if10 (r .!=. rt)
    ]
genPrim _ _ IntGtOp           [r] [x,y] = PrimInline $ r |= if10 (x .>. y)
genPrim _ _ IntGeOp           [r] [x,y] = PrimInline $ r |= if10 (x .>=. y)
genPrim _ _ IntEqOp           [r] [x,y] = PrimInline $ r |= if10 (x .===. y)
genPrim _ _ IntNeOp           [r] [x,y] = PrimInline $ r |= if10(x .!==. y)
genPrim _ _ IntLtOp           [r] [x,y] = PrimInline $ r |= if10 (x .<. y)
genPrim _ _ IntLeOp           [r] [x,y] = PrimInline $ r |= if10 (x .<=. y)
genPrim _ _ ChrOp             [r] [x]   = PrimInline $ r |= x
genPrim _ _ IntToWordOp       [r] [x]   = PrimInline $ r |= x
genPrim _ _ IntToFloatOp      [r] [x]   = PrimInline $ r |= x
genPrim _ _ IntToDoubleOp     [r] [x]   = PrimInline $ r |= x
genPrim _ _ IntSllOp          [r] [x,y] = PrimInline $ r |= x .<<. y
genPrim _ _ IntSraOp          [r] [x,y] = PrimInline $ r |= x .>>. y
genPrim _ _ IntSrlOp          [r] [x,y] = PrimInline $ r |= trunc (x .>>>. y)

genPrim _ _ Int8ToIntOp       [r] [x]   = PrimInline $ r |= mask8 x
genPrim _ _ IntToInt8Op       [r] [x]   = PrimInline $ r |= mask8 x -- fixme
genPrim _ _ Int8NegOp         [r] [x]   = PrimInline $ r |= mask8 (Sub (Int 0x100) x)
genPrim _ _ Int8AddOp         [r] [x,y] = PrimInline $ r |= mask8 (Add x y)
genPrim _ _ Int8SubOp         [r] [x,y] = PrimInline $ r |= mask8 (Sub x y)
genPrim _ _ Int8MulOp         [r] [x,y] = PrimInline $ r |= mask8 (Mul x y)
genPrim _ _ Int8QuotOp        [r] [x,y] = PrimInline $ r |= quotShortInt 8 x y
genPrim _ _ Int8RemOp         [r] [x,y] = PrimInline $ r |= remShortInt 8 x y
genPrim _ _ Int8QuotRemOp     [r1,r2] [x,y] = PrimInline $ mconcat
                                                [ r1 |= quotShortInt 8 x y
                                                , r2 |= remShortInt 8 x y
                                                ]
genPrim _ _ Int8EqOp          [r] [x,y] = PrimInline $ r |= if10 (x .===. y)
genPrim _ _ Int8GeOp          [r] [x,y] = PrimInline $ r |= if10 ((x .<<. (Int 24)) .>=. (y .<<. (Int 24)))
genPrim _ _ Int8GtOp          [r] [x,y] = PrimInline $ r |= if10 ((x .<<. (Int 24)) .>.  (y .<<. (Int 24)))
genPrim _ _ Int8LeOp          [r] [x,y] = PrimInline $ r |= if10 ((x .<<. (Int 24)) .<=. (y .<<. (Int 24)))
genPrim _ _ Int8LtOp          [r] [x,y] = PrimInline $ r |= if10 ((x .<<. (Int 24)) .<.  (y .<<. (Int 24)))
genPrim _ _ Int8NeOp          [r] [x,y] = PrimInline $ r |= if10 (x .!==. y)

genPrim _ _ Word8ToWordOp      [r] [x]   = PrimInline $ r |= mask8 x
genPrim _ _ WordToWord8Op      [r] [x]   = PrimInline $ r |= mask8 x
genPrim _ _ Word8NotOp         [r] [x]   = PrimInline $ r |= BXor x (Int 0xff)
genPrim _ _ Word8AddOp         [r] [x,y] = PrimInline $ r |= mask8 (Add x y)
genPrim _ _ Word8SubOp         [r] [x,y] = PrimInline $ r |= mask8 (Sub x y)
genPrim _ _ Word8MulOp         [r] [x,y] = PrimInline $ r |= mask8 (Mul x y)
genPrim _ _ Word8QuotOp        [r] [x,y] = PrimInline $ r |= trunc (Div x y)
genPrim _ _ Word8RemOp         [r] [x,y] = PrimInline $ r |= Mod x y
genPrim _ _ Word8QuotRemOp     [r1,r2] [x,y] = PrimInline $ mconcat
                                                  [ r1 |= trunc (Div x y)
                                                  , r2 |= Mod x y
                                                  ]
genPrim _ _ Word8EqOp          [r] [x,y] = PrimInline $ r |= if10 (x .===. y)
genPrim _ _ Word8GeOp          [r] [x,y] = PrimInline $ r |= if10 (x .>=. y)
genPrim _ _ Word8GtOp          [r] [x,y] = PrimInline $ r |= if10 (x .>. y)
genPrim _ _ Word8LeOp          [r] [x,y] = PrimInline $ r |= if10 (x .<=. y)
genPrim _ _ Word8LtOp          [r] [x,y] = PrimInline $ r |= if10 (x .<. y)
genPrim _ _ Word8NeOp          [r] [x,y] = PrimInline $ r |= if10 (x .!==. y)

genPrim _ _ Int16ToIntOp       [r] [x]   = PrimInline $ r |= mask16 x
genPrim _ _ IntToInt16Op       [r] [x]   = PrimInline $ r |= mask16 x -- fixme ?
genPrim _ _ Int16NegOp         [r] [x]   = PrimInline $ r |= mask16 (Sub (Int 0x10000) x)
genPrim _ _ Int16AddOp         [r] [x,y] = PrimInline $ r |= mask16 (Add x y)
genPrim _ _ Int16SubOp         [r] [x,y] = PrimInline $ r |= mask16 (Sub x y)
genPrim _ _ Int16MulOp         [r] [x,y] = PrimInline $ r |= mask16 (Mul x y)
genPrim _ _ Int16QuotOp        [r] [x,y] = PrimInline $ r |= quotShortInt 16 x y
genPrim _ _ Int16RemOp         [r] [x,y] = PrimInline $ r |= remShortInt 16 x y
genPrim _ _ Int16QuotRemOp     [r1,r2] [x,y] = PrimInline $ mconcat
                                                [ r1 |= quotShortInt 16 x y
                                                , r2 |= remShortInt 16 x y
                                                ]
genPrim _ _ Int16EqOp          [r] [x,y] = PrimInline $ r |= if10 (x .===. y)
genPrim _ _ Int16GeOp          [r] [x,y] = PrimInline $ r |= if10 ((x .<<. (Int 16)) .>=. (y .<<. (Int 16)))
genPrim _ _ Int16GtOp          [r] [x,y] = PrimInline $ r |= if10 ((x .<<. (Int 16)) .>.  (y .<<. (Int 16)))
genPrim _ _ Int16LeOp          [r] [x,y] = PrimInline $ r |= if10 ((x .<<. (Int 16)) .<=. (y .<<. (Int 16)))
genPrim _ _ Int16LtOp          [r] [x,y] = PrimInline $ r |= if10 ((x .<<. (Int 16)) .<.  (y .<<. (Int 16)))
genPrim _ _ Int16NeOp          [r] [x,y] = PrimInline $ r |= if10 (x .!==. y)

genPrim _ _ Word16ToWordOp     [r] [x]   = PrimInline $ r |= mask16 x
genPrim _ _ WordToWord16Op     [r] [x]   = PrimInline $ r |= mask16 x
genPrim _ _ Word16NotOp        [r] [x]   = PrimInline $ r |= BXor x (Int 0xffff)
genPrim _ _ Word16AddOp        [r] [x,y] = PrimInline $ r |= mask16 (Add x y)
genPrim _ _ Word16SubOp        [r] [x,y] = PrimInline $ r |= mask16 (Sub x y)
genPrim _ _ Word16MulOp        [r] [x,y] = PrimInline $ r |= mask16 (Mul x y)
genPrim _ _ Word16QuotOp       [r] [x,y] = PrimInline $ r |= trunc (Div x y)
genPrim _ _ Word16RemOp        [r] [x,y] = PrimInline $ r |= Mod x y
genPrim _ _ Word16QuotRemOp    [r1,r2] [x,y] = PrimInline $ mconcat
                                                [ r1 |= trunc (Div x y)
                                                , r2 |= Mod x y
                                                ]
genPrim _ _ Word16EqOp         [r] [x,y] = PrimInline $ r |= if10 (x .===. y)
genPrim _ _ Word16GeOp         [r] [x,y] = PrimInline $ r |= if10 (x .>=. y)
genPrim _ _ Word16GtOp         [r] [x,y] = PrimInline $ r |= if10 (x .>. y)
genPrim _ _ Word16LeOp         [r] [x,y] = PrimInline $ r |= if10 (x .<=. y)
genPrim _ _ Word16LtOp         [r] [x,y] = PrimInline $ r |= if10 (x .<. y)
genPrim _ _ Word16NeOp         [r] [x,y] = PrimInline $ r |= if10 (x .!==. y)

genPrim _ _ WordAddOp         [r] [x,y] = PrimInline $ r |= trunc (x `Add` y)
genPrim _ _ WordAddCOp      [r,c] [x,y] = PrimInline $
  jVar \t -> mconcat
    [ t |= (x .>>>. zero_) `Add` (y .>>>. zero_)
    , r |= trunc t
    , c |= if10 (t .>. Int 4294967295)
    ]
genPrim _ _ WordSubCOp        [r,c] [x,y] =
  PrimInline $ mconcat
    [ r |= trunc (Sub x y)
    , c |= if10 ((y .>>>. zero_) .>. (x .>>>. zero_))
    ]
genPrim _ _ WordAdd2Op      [h,l] [x,y] = PrimInline $ appT [h,l] "h$wordAdd2" [x,y]
genPrim _ _ WordSubOp         [r] [x,y] = PrimInline $ r |= trunc (Sub x y)
genPrim _ _ WordMulOp         [r] [x,y] = PrimInline $ r |= app "h$mulWord32" [x, y]
genPrim _ _ WordMul2Op      [h,l] [x,y] = PrimInline $ appT [h,l] "h$mul2Word32" [x,y]
genPrim _ _ WordQuotOp        [q] [x,y] = PrimInline $ q |= app "h$quotWord32" [x,y]
genPrim _ _ WordRemOp         [r] [x,y] = PrimInline $ r |= app "h$remWord32" [x,y]
genPrim _ _ WordQuotRemOp   [q,r] [x,y] = PrimInline $ mconcat
                                            [ q |= app "h$quotWord32" [x,y]
                                            , r |= app "h$remWord32" [x,y]
                                            ]
genPrim _ _ WordQuotRem2Op   [q,r] [xh,xl,y] = PrimInline $ appT [q,r] "h$quotRem2Word32" [xh,xl,y]
genPrim _ _ WordAndOp         [r] [x,y] = PrimInline $ r |= BAnd x y
genPrim _ _ WordOrOp          [r] [x,y] = PrimInline $ r |= BOr  x y
genPrim _ _ WordXorOp         [r] [x,y] = PrimInline $ r |= BXor x y
genPrim _ _ WordNotOp         [r] [x]   = PrimInline $ r |= BNot x
genPrim _ _ WordSllOp         [r] [x,y] = PrimInline $ r |= x .<<. y
genPrim _ _ WordSrlOp         [r] [x,y] = PrimInline $ r |= trunc (x .>>>. y)
genPrim _ _ WordToIntOp       [r] [x]   = PrimInline $ r |= x
genPrim _ _ WordGtOp          [r] [x,y] =
  PrimInline $ r |= if10 ((x .>>>. zero_) .>. (y .>>>. zero_))
genPrim _ _ WordGeOp          [r] [x,y] =
  PrimInline $ r |= if10 ((x .>>>. zero_) .>=. (y .>>>. zero_))
genPrim _ _ WordEqOp          [r] [x,y] = PrimInline $ r |= if10 (x .===. y)
genPrim _ _ WordNeOp          [r] [x,y] = PrimInline $ r |= if10 (x .!==. y)
genPrim _ _ WordLtOp          [r] [x,y] =
  PrimInline $ r |= if10 ((x .>>>. zero_) .<. (y .>>>. zero_))
genPrim _ _ WordLeOp          [r] [x,y] =
  PrimInline $ r |= if10 ((x .>>>. zero_) .<=. (y .>>>. zero_))
genPrim _ _ WordToDoubleOp    [r] [x] = PrimInline $ r |= (Add (BAnd x (Int 0x7FFFFFFF)) (x .>>>. (Int 31))) `Mul` Int 2147483648
genPrim _ _ WordToFloatOp     [r] [x] = PrimInline $ r |= (Add (BAnd x (Int 0x7FFFFFFF)) (x .>>>. (Int 31))) `Mul` Int 2147483648
genPrim _ _ PopCnt8Op         [r] [x] = PrimInline $ r |= var "h$popCntTab" .! (mask8 x)
genPrim _ _ PopCnt16Op        [r] [x] =
  PrimInline $ r |= Add (var "h$popCntTab" .! (mask8 x))
                        (var "h$popCntTab" .! (mask8 (x .>>>. Int 8)))

genPrim _ _ PopCnt32Op        [r] [x]   = PrimInline $ r |= app "h$popCnt32" [x]
genPrim _ _ PopCnt64Op        [r] [x1,x2] = PrimInline $ r |= app "h$popCnt64" [x1,x2]
genPrim d t PopCntOp          [r] [x]   = genPrim d t PopCnt32Op [r] [x]
genPrim _ _ Pdep8Op           [r] [s,m] = PrimInline $ r |= app "h$pdep8"  [s,m]
genPrim _ _ Pdep16Op          [r] [s,m] = PrimInline $ r |= app "h$pdep16" [s,m]
genPrim _ _ Pdep32Op          [r] [s,m] = PrimInline $ r |= app "h$pdep32" [s,m]
genPrim _ _ Pdep64Op          [ra,rb] [sa,sb,ma,mb] = PrimInline $ appT [ra,rb] "h$pdep64" [sa,sb,ma,mb]
genPrim d t PdepOp            rs xs = genPrim d t Pdep32Op rs xs
genPrim _ _ Pext8Op           [r] [s,m] = PrimInline $ r |= app "h$pext8" [s,m]
genPrim _ _ Pext16Op          [r] [s,m] = PrimInline $ r |= app "h$pext16" [s,m]
genPrim _ _ Pext32Op          [r] [s,m] = PrimInline $ r |= app "h$pext32" [s,m]
genPrim _ _ Pext64Op          [ra,rb] [sa,sb,ma,mb] = PrimInline $
  appT [ra,rb] "h$pext64" [sa,sb,ma,mb]
genPrim d t PextOp            rs xs     = genPrim d t Pext32Op rs xs

genPrim _ _ ClzOp                      [r]   [x]           = PrimInline $ r |= app "h$clz32" [x]
genPrim _ _ Clz8Op                     [r]   [x]           = PrimInline $ r |= app "h$clz8"  [x]
genPrim _ _ Clz16Op                    [r]   [x]           = PrimInline $ r |= app "h$clz16" [x]
genPrim _ _ Clz32Op                    [r]   [x]           = PrimInline $ r |= app "h$clz32" [x]
genPrim _ _ Clz64Op                    [r]   [x1,x2]       = PrimInline $ r |= app "h$clz64" [x1,x2]

genPrim _ _ CtzOp                      [r]   [x]           = PrimInline $ r |= app "h$ctz32" [x]
genPrim _ _ Ctz8Op                     [r]   [x]           = PrimInline $ r |= app "h$ctz8"  [x]
genPrim _ _ Ctz16Op                    [r]   [x]           = PrimInline $ r |= app "h$ctz16" [x]
genPrim _ _ Ctz32Op                    [r]   [x]           = PrimInline $ r |= app "h$ctz32" [x]
genPrim _ _ Ctz64Op                    [r]   [x1,x2]       = PrimInline $ r |= app "h$ctz64" [x1,x2]

genPrim _ _ BSwap16Op         [r] [x]   = PrimInline $
  r |= BOr ((mask8 x) .<<. (Int 8))
           (mask8 (x .>>>. (Int 8)))
genPrim _ _ BSwap32Op         [r] [x]   = PrimInline $
  r |= (x .<<. (Int 24))
        `BOr` ((BAnd x (Int 0xFF00)) .<<. (Int 8))
        `BOr` ((BAnd x (Int 0xFF0000)) .>>. (Int 8))
        `BOr` (x .>>>. (Int 24))
genPrim _ _ BSwap64Op     [r1,r2] [x,y] = PrimInline $
  appT [r1,r2] "h$bswap64" [x,y]
genPrim d t BSwapOp           [r] [x]   = genPrim d t BSwap32Op [r] [x]

genPrim _ _ Narrow8IntOp      [r] [x]   = PrimInline $ r |= (BAnd x (Int 0x7F)) `Sub` (BAnd x (Int 0x80))
genPrim _ _ Narrow16IntOp     [r] [x]   = PrimInline $ r |= (BAnd x (Int 0x7FFF)) `Sub` (BAnd x (Int 0x8000))
genPrim _ _ Narrow32IntOp     [r] [x]   = PrimInline $ r |= trunc x
genPrim _ _ Narrow8WordOp     [r] [x]   = PrimInline $ r |= mask8 x
genPrim _ _ Narrow16WordOp    [r] [x]   = PrimInline $ r |= mask16 x
genPrim _ _ Narrow32WordOp    [r] [x]   = PrimInline $ r |= trunc x

genPrim _ _ DoubleGtOp        [r] [x,y] = PrimInline $ r |= if10 (x .>. y)
genPrim _ _ DoubleGeOp        [r] [x,y] = PrimInline $ r |= if10 (x .>=. y)
genPrim _ _ DoubleEqOp        [r] [x,y] = PrimInline $ r |= if10 (x .===. y)
genPrim _ _ DoubleNeOp        [r] [x,y] = PrimInline $ r |= if10 (x .!==. y)
genPrim _ _ DoubleLtOp        [r] [x,y] = PrimInline $ r |= if10 (x .<. y)
genPrim _ _ DoubleLeOp        [r] [x,y] = PrimInline $ r |= if10 (x .<=. y)
genPrim _ _ DoubleAddOp       [r] [x,y] = PrimInline $ r |= Add x y
genPrim _ _ DoubleSubOp       [r] [x,y] = PrimInline $ r |= Sub x y
genPrim _ _ DoubleMulOp       [r] [x,y] = PrimInline $ r |= Mul x y
genPrim _ _ DoubleDivOp       [r] [x,y] = PrimInline $ r |= Div x y
genPrim _ _ DoubleNegOp       [r] [x]   = PrimInline $ r |= Negate x
genPrim _ _ DoubleFabsOp      [r] [x]   = PrimInline $ r |= math_abs [x]
genPrim _ _ DoubleToIntOp     [r] [x]   = PrimInline $ r |= trunc x
genPrim _ _ DoubleToFloatOp   [r] [x]   = PrimInline $ r |= app "h$fround" [x]
genPrim _ _ DoubleExpOp       [r] [x]   = PrimInline $ r |= math_exp [x]
genPrim _ _ DoubleLogOp       [r] [x]   = PrimInline $ r |= math_log [x]
genPrim _ _ DoubleSqrtOp      [r] [x]   = PrimInline $ r |= math_sqrt [x]
genPrim _ _ DoubleSinOp       [r] [x]   = PrimInline $ r |= math_sin [x]
genPrim _ _ DoubleCosOp       [r] [x]   = PrimInline $ r |= math_cos [x]
genPrim _ _ DoubleTanOp       [r] [x]   = PrimInline $ r |= math_tan [x]
genPrim _ _ DoubleAsinOp      [r] [x]   = PrimInline $ r |= math_asin [x]
genPrim _ _ DoubleAcosOp      [r] [x]   = PrimInline $ r |= math_acos [x]
genPrim _ _ DoubleAtanOp      [r] [x]   = PrimInline $ r |= math_atan [x]
genPrim _ _ DoubleSinhOp      [r] [x]   = PrimInline $ r |= (math_exp [x] `Sub` math_exp [Negate x]) `Div` two_
genPrim _ _ DoubleCoshOp      [r] [x]   = PrimInline $ r |= (math_exp [x] `Add` math_exp [Negate x]) `Div` two_
genPrim _ _ DoubleTanhOp      [r] [x]   = PrimInline $ r |= (math_exp [Mul two_ x] `Sub` one_) `Div` (math_exp [Mul two_ x] `Add` one_)
genPrim _ _ DoubleAsinhOp     [r] [x]   = PrimInline $ r |= math_asinh [x]
genPrim _ _ DoubleAcoshOp     [r] [x]   = PrimInline $ r |= math_acosh [x]
genPrim _ _ DoubleAtanhOp     [r] [x]   = PrimInline $ r |= math_atanh [x]
genPrim _ _ DoublePowerOp     [r] [x,y] = PrimInline $ r |= math_pow [x,y]
genPrim _ _ DoubleDecode_2IntOp [s,h,l,e] [x] = PrimInline $ appT [s,h,l,e] "h$decodeDouble2Int" [x]
genPrim _ _ DoubleDecode_Int64Op [s1,s2,e] [d] =
  PrimInline $ appT [e,s1,s2] "h$decodeDoubleInt64" [d]

genPrim _ _ FloatGtOp         [r] [x,y] = PrimInline $ r |= if10 (x .>. y)
genPrim _ _ FloatGeOp         [r] [x,y] = PrimInline $ r |= if10 (x .>=. y)
genPrim _ _ FloatEqOp         [r] [x,y] = PrimInline $ r |= if10 (x .===. y)
genPrim _ _ FloatNeOp         [r] [x,y] = PrimInline $ r |= if10 (x .!==. y)
genPrim _ _ FloatLtOp         [r] [x,y] = PrimInline $ r |= if10 (x .<. y)
genPrim _ _ FloatLeOp         [r] [x,y] = PrimInline $ r |= if10 (x .<=. y)
genPrim _ _ FloatAddOp        [r] [x,y] = PrimInline $ r |= Add x y
genPrim _ _ FloatSubOp        [r] [x,y] = PrimInline $ r |= Sub x y
genPrim _ _ FloatMulOp        [r] [x,y] = PrimInline $ r |= Mul x y
genPrim _ _ FloatDivOp        [r] [x,y] = PrimInline $ r |= Div x y
genPrim _ _ FloatNegOp        [r] [x]   = PrimInline $ r |= Negate x
genPrim _ _ FloatFabsOp       [r] [x]   = PrimInline $ r |= math_abs [x]
genPrim _ _ FloatToIntOp      [r] [x]   = PrimInline $ r |= trunc x
genPrim _ _ FloatExpOp        [r] [x]   = PrimInline $ r |= math_exp [x]
genPrim _ _ FloatLogOp        [r] [x]   = PrimInline $ r |= math_log [x]
genPrim _ _ FloatSqrtOp       [r] [x]   = PrimInline $ r |= math_sqrt [x]
genPrim _ _ FloatSinOp        [r] [x]   = PrimInline $ r |= math_sin [x]
genPrim _ _ FloatCosOp        [r] [x]   = PrimInline $ r |= math_cos [x]
genPrim _ _ FloatTanOp        [r] [x]   = PrimInline $ r |= math_tan [x]
genPrim _ _ FloatAsinOp       [r] [x]   = PrimInline $ r |= math_asin [x]
genPrim _ _ FloatAcosOp       [r] [x]   = PrimInline $ r |= math_acos [x]
genPrim _ _ FloatAtanOp       [r] [x]   = PrimInline $ r |= math_atan [x]
genPrim _ _ FloatSinhOp       [r] [x]   = PrimInline $ r |= (math_exp [x] `Sub` math_exp [Negate x]) `Div` two_
genPrim _ _ FloatCoshOp       [r] [x]   = PrimInline $ r |= (math_exp [x] `Add` math_exp [Negate x]) `Div` two_
genPrim _ _ FloatTanhOp       [r] [x]   = PrimInline $ r |= (math_exp [Mul two_ x] `Sub` one_) `Div` (math_exp [Mul two_ x] `Add` one_)
genPrim _ _ FloatAsinhOp      [r] [x]   = PrimInline $ r |= math_asinh [x]
genPrim _ _ FloatAcoshOp      [r] [x]   = PrimInline $ r |= math_acosh [x]
genPrim _ _ FloatAtanhOp      [r] [x]   = PrimInline $ r |= math_atanh [x]
genPrim _ _ FloatPowerOp      [r] [x,y] = PrimInline $ r |= math_pow [x,y]
genPrim _ _ FloatToDoubleOp   [r] [x]   = PrimInline $ r |= x
genPrim _ _ FloatDecode_IntOp [s,e] [x] = PrimInline $ appT [s,e] "h$decodeFloatInt" [x]

-- Arrays

genPrim _ _ NewArrayOp          [r] [l,e]   = PrimInline (newArray r l e)
genPrim _ _ ReadArrayOp         [r] [a,i]   = PrimInline $ r |= a .! i
genPrim _ _ WriteArrayOp        []  [a,i,v] = PrimInline $ a .! i |= v
genPrim _ _ SizeofArrayOp       [r] [a]     = PrimInline $ r |= a .^ "length"
genPrim _ _ SizeofMutableArrayOp [r] [a]    = PrimInline $ r |= a .^ "length"
genPrim _ _ IndexArrayOp        [r] [a,i]   = PrimInline $ r |= a .! i
genPrim _ _ UnsafeFreezeArrayOp [r] [a]     = PrimInline $ r |= a
genPrim _ _ UnsafeThawArrayOp   [r] [a]     = PrimInline $ r |= a
genPrim _ _ CopyArrayOp         [] [a,o1,ma,o2,n] =
  PrimInline $ loopBlockS (Int 0) (.<. n) \i ->
    [ ma .! (Add i o2) |= a .! (Add i o1)
    , preIncrS i
    ]
genPrim d t CopyMutableArrayOp  []  [a1,o1,a2,o2,n] = genPrim d t CopyArrayOp [] [a1,o1,a2,o2,n]
genPrim _ _ CloneArrayOp        [r] [a,start,n]     = PrimInline $ r |= app "h$sliceArray" [a,start,n]
genPrim d t CloneMutableArrayOp [r] [a,start,n]     = genPrim d t CloneArrayOp [r] [a,start,n]
genPrim _ _ FreezeArrayOp       [r] [a,start,n]     = PrimInline $ r |= app "h$sliceArray" [a,start,n]
genPrim _ _ ThawArrayOp         [r] [a,start,n]     = PrimInline $ r |= app "h$sliceArray" [a,start,n]
genPrim _ _ CasArrayOp        [s,o] [a,i,old,new]   = PrimInline $
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

-- Small Arrays

genPrim _ _ NewSmallArrayOp            [a]   [n,e]         = PrimInline $ a |= app "h$newArray" [n,e]
genPrim _ _ ReadSmallArrayOp           [r]   [a,i]         = PrimInline $ r |= a .! i
genPrim _ _ WriteSmallArrayOp          []    [a,i,e]       = PrimInline $ a .! i |= e
genPrim _ _ SizeofSmallArrayOp         [r]   [a]           = PrimInline $ r |= a .^ "length"
genPrim _ _ SizeofSmallMutableArrayOp  [r]   [a]           = PrimInline $ r |= a .^ "length"
genPrim _ _ IndexSmallArrayOp          [r]   [a,i]         = PrimInline $ r |= a .! i
genPrim _ _ UnsafeFreezeSmallArrayOp   [r]   [a]           = PrimInline $ r |= a
genPrim _ _ UnsafeThawSmallArrayOp     [r]   [a]           = PrimInline $ r |= a
genPrim _ _ CopySmallArrayOp           []    [s,si,d,di,n] = PrimInline $
  loopBlockS (Sub n one_) (.>=. zero_) \i ->
    [ d .! (Add di i) |= s .! (Add si i)
    , postDecrS i
    ]
genPrim _ _ CopySmallMutableArrayOp    []    [s,si,d,di,n] = PrimInline $
  loopBlockS (Sub n one_) (.>=. zero_) \i ->
    [ d .! (Add di i) |= s .! (Add si i)
    , postDecrS i
    ]
genPrim _ _ CloneSmallArrayOp          [r]   [a,o,n]       = PrimInline $ cloneArray r a (Just o) n
genPrim _ _ CloneSmallMutableArrayOp   [r]   [a,o,n]       = PrimInline $ cloneArray r a (Just o) n
genPrim _ _ FreezeSmallArrayOp         [r]   [a,o,n]       = PrimInline $ cloneArray r a (Just o) n
genPrim _ _ ThawSmallArrayOp           [r]   [a,o,n]       = PrimInline $ cloneArray r a (Just o) n
genPrim _ _ CasSmallArrayOp            [s,o] [a,i,old,new] = PrimInline $ jVar \x -> mconcat
                                                                [ x |= a .! i
                                                                , ifBlockS (x .===. old)
                                                                    [ o |= new
                                                                    , a .! i |= new
                                                                    , s |= zero_
                                                                    ]
                                                                    -- fixme both new?
                                                                    [ s |= one_
                                                                    , o |= x
                                                                    ]
                                                                ]

-- Byte Arrays

genPrim _ _ NewByteArrayOp_Char               [r] [l]         = PrimInline (newByteArray r l)
genPrim _ _ NewPinnedByteArrayOp_Char         [r] [l]         = PrimInline (newByteArray r l)
genPrim _ _ NewAlignedPinnedByteArrayOp_Char  [r] [l,_align]  = PrimInline (newByteArray r l)
genPrim _ _ MutableByteArrayIsPinnedOp        [r] [_]         = PrimInline $ r |= one_
genPrim _ _ ByteArrayIsPinnedOp               [r] [_]         = PrimInline $ r |= one_
genPrim _ _ ByteArrayContents_Char            [a,o] [b]       = PrimInline $ mconcat [a |= b, o |= zero_]
genPrim _ _ ShrinkMutableByteArrayOp_Char     [] [a,n]        = PrimInline $ appS "h$shrinkMutableByteArray" [a,n]
genPrim _ _ ResizeMutableByteArrayOp_Char     [r] [a,n]       = PrimInline $ r |= app "h$resizeMutableByteArray" [a,n]
genPrim _ _ UnsafeFreezeByteArrayOp           [a] [b]         = PrimInline $ a |= b
genPrim _ _ SizeofByteArrayOp                 [r] [a]         = PrimInline $ r |= a .^ "len"
genPrim _ _ SizeofMutableByteArrayOp          [r] [a]         = PrimInline $ r |= a .^ "len"
genPrim _ _ GetSizeofMutableByteArrayOp       [r] [a]         = PrimInline $ r |= a .^ "len"
genPrim _ _ IndexByteArrayOp_Char             [r] [a,i]       = PrimInline $ r |= u8_ a i
genPrim _ _ IndexByteArrayOp_WideChar         [r] [a,i]       = PrimInline $ r |= i3_ a i
genPrim _ _ IndexByteArrayOp_Int              [r] [a,i]       = PrimInline $ r |= i3_ a i
genPrim _ _ IndexByteArrayOp_Word             [r] [a,i]       = PrimInline $ r |= i3_ a i
genPrim _ _ IndexByteArrayOp_Addr             [r1,r2] [a,i]   = PrimInline $ jVar \t -> mconcat
                                                                [ t |= a .^ "arr"
                                                                , ifBlockS (t .&&. t .! (i .<<. two_))
                                                                    [ r1 |= t .! (i .<<. two_) .! zero_
                                                                    , r2 |= t .! (i .<<. two_) .! one_
                                                                    ]
                                                                    [ r1 |= null_
                                                                    , r2 |= zero_
                                                                    ]
                                                                ]

genPrim _ _ IndexByteArrayOp_Float [r] [a,i] =
  PrimInline $ r |= f3_ a i
genPrim _ _ IndexByteArrayOp_Double [r] [a,i] =
  PrimInline $ r |= f6_ a i
genPrim _ _ IndexByteArrayOp_StablePtr [r1,r2] [a,i] =
  PrimInline $ mconcat
    [ r1 |= var "h$stablePtrBuf"
    , r2 |= i3_ a i
    ]
genPrim _ _ IndexByteArrayOp_Int8  [r] [a,i]      = PrimInline $ r |= dv_i8 a i
genPrim _ _ IndexByteArrayOp_Int16 [r] [a,i]      = PrimInline $ r |= dv_i16 a (i .<<. one_)
genPrim _ _ IndexByteArrayOp_Int32 [r] [a,i]      = PrimInline $ r |= i3_ a i
genPrim _ _ IndexByteArrayOp_Int64 [r1,r2] [a,i]  = PrimInline $ mconcat
                                                     [ r1 |= i3_ a (Add (i .<<. one_) one_)
                                                     , r2 |= i3_ a (i .<<. one_)
                                                     ]
genPrim _ _ IndexByteArrayOp_Word8 [r] [a,i]      = PrimInline $ r |= u8_ a i
genPrim _ _ IndexByteArrayOp_Word16 [r] [a,i]     = PrimInline $ r |= dv_u16 a (i .<<. one_)
genPrim _ _ IndexByteArrayOp_Word32 [r] [a,i]     = PrimInline $ r |= i3_ a i
genPrim _ _ IndexByteArrayOp_Word64 [r1,r2] [a,i] = PrimInline $ mconcat
                                                      [ r1 |= i3_ a (Add (i .<<. one_) one_)
                                                      , r2 |= i3_ a (i .<<. one_)
                                                      ]
{- new ops in 8.6
   , IndexByteArrayOp_Word8AsChar
   , IndexByteArrayOp_Word8AsWideChar
   , IndexByteArrayOp_Word8AsAddr
   , IndexByteArrayOp_Word8AsFloat
   , IndexByteArrayOp_Word8AsDouble
   , IndexByteArrayOp_Word8AsStablePtr
   , IndexByteArrayOp_Word8AsInt16
   , IndexByteArrayOp_Word8AsInt32
   , IndexByteArrayOp_Word8AsInt64
   , IndexByteArrayOp_Word8AsInt
   , IndexByteArrayOp_Word8AsWord16
   , IndexByteArrayOp_Word8AsWord32
   , IndexByteArrayOp_Word8AsWord64
   , IndexByteArrayOp_Word8AsWord
 -}
genPrim _ _ ReadByteArrayOp_Char [r] [a,i] =
  PrimInline $ r |= u8_ a i
genPrim _ _ ReadByteArrayOp_WideChar [r] [a,i] =
  PrimInline $ r |= i3_ a i
genPrim _ _ ReadByteArrayOp_Int [r] [a,i] =
  PrimInline $ r |= i3_ a i
genPrim _ _ ReadByteArrayOp_Word [r] [a,i] =
  PrimInline $ r |= i3_ a i
genPrim _ _ ReadByteArrayOp_Addr [r1,r2] [a,i] =
  PrimInline $ jVar \x -> mconcat
    [ x |= i .<<. two_
    , ifS (a .^ "arr" .&&. a .^ "arr" .! x)
           (mconcat [ r1 |= a .^ "arr" .! x .! zero_
                    , r2 |= a .^ "arr" .! x .! one_
                    ])
           (mconcat [r1 |= null_, r2 |= one_])
    ]
genPrim _ _ ReadByteArrayOp_Float [r] [a,i] =
  PrimInline $ r |= f3_ a i
genPrim _ _ ReadByteArrayOp_Double [r] [a,i] =
  PrimInline $ r |= f6_ a i
genPrim _ _ ReadByteArrayOp_StablePtr [r1,r2] [a,i] =
   PrimInline $ mconcat
    [ r1 |= var "h$stablePtrBuf"
    , r2 |= i3_ a i
    ]
genPrim _ _ ReadByteArrayOp_Int8 [r] [a,i] =
  PrimInline $ r |= dv_i8 a i
genPrim _ _ ReadByteArrayOp_Int16 [r] [a,i] =
  PrimInline $ r |= dv_i16 a (i .<<. one_)
genPrim _ _ ReadByteArrayOp_Int32 [r] [a,i] =
  PrimInline $ r |= i3_ a i
genPrim _ _ ReadByteArrayOp_Int64 [r1,r2] [a,i] =
  PrimInline $ mconcat
    [ r1 |= i3_ a (Add (i .<<. one_) one_)
    , r2 |= i3_ a (i .<<. one_)
    ]
genPrim _ _ ReadByteArrayOp_Word8 [r] [a,i] = PrimInline $ r |= u8_ a i
genPrim _ _ ReadByteArrayOp_Word16 [r] [a,i] = PrimInline $ r |= u1_ a i
genPrim _ _ ReadByteArrayOp_Word32 [r] [a,i] = PrimInline $ r |= i3_ a i
genPrim _ _ ReadByteArrayOp_Word64 [r1,r2] [a,i] =
  PrimInline $ mconcat
    [ r1 |= i3_ a (Add (i .<<. one_) one_)
    , r2 |= i3_ a (i .<<. one_)
    ]
{- new ops in 8.6
   , ReadByteArrayOp_Word8AsChar
   , ReadByteArrayOp_Word8AsWideChar
   , ReadByteArrayOp_Word8AsAddr
   , ReadByteArrayOp_Word8AsFloat
   , ReadByteArrayOp_Word8AsDouble
   , ReadByteArrayOp_Word8AsStablePtr
   , ReadByteArrayOp_Word8AsInt16
   , ReadByteArrayOp_Word8AsInt32
   , ReadByteArrayOp_Word8AsInt64
   , ReadByteArrayOp_Word8AsInt
   , ReadByteArrayOp_Word8AsWord16
   , ReadByteArrayOp_Word8AsWord32
   , ReadByteArrayOp_Word8AsWord64
   , ReadByteArrayOp_Word8AsWord
 -}
genPrim _ _ WriteByteArrayOp_Char [] [a,i,e] = PrimInline $ u8_ a i |= e
genPrim _ _ WriteByteArrayOp_WideChar [] [a,i,e] = PrimInline $ i3_ a i |= e
genPrim _ _ WriteByteArrayOp_Int [] [a,i,e] = PrimInline $ i3_ a i |= e
genPrim _ _ WriteByteArrayOp_Word [] [a,i,e] = PrimInline $ i3_ a i |= e
genPrim _ _ WriteByteArrayOp_Addr [] [a,i,e1,e2] = PrimInline $ mconcat
                                                      [ ifS (Not (a .^ "arr")) (a .^ "arr" |= ValExpr (JList [])) mempty
                                                      , a .^ "arr" .! (i .<<. two_) |= ValExpr (JList [e1, e2])
                                                      ]
genPrim _ _ WriteByteArrayOp_Float [] [a,i,e] = PrimInline $ f3_ a i |= e
genPrim _ _ WriteByteArrayOp_Double [] [a,i,e] = PrimInline $ f6_ a i |= e
genPrim _ _ WriteByteArrayOp_StablePtr [] [a,i,_e1,e2]     = PrimInline $ i3_ a i |= e2

genPrim _ _ WriteByteArrayOp_Int8 [] [a,i,e] = PrimInline $ dv_s_i8 a i e
genPrim _ _ WriteByteArrayOp_Int16 [] [a,i,e]     = PrimInline $ dv_s_i16 a (i .<<. one_) e
genPrim _ _ WriteByteArrayOp_Int32 [] [a,i,e]     = PrimInline $ i3_ a i |= e
genPrim _ _ WriteByteArrayOp_Int64 [] [a,i,e1,e2] =
  PrimInline $ mconcat
    [ i3_ a (Add (i .<<. one_) one_) |= e1
    , i3_ a (i .<<. one_) |= e2
    ]
genPrim _ _ WriteByteArrayOp_Word8 [] [a,i,e]     = PrimInline $ u8_ a i |= e
genPrim _ _ WriteByteArrayOp_Word16 [] [a,i,e]     = PrimInline $ u1_ a i |= e
genPrim _ _ WriteByteArrayOp_Word32 [] [a,i,e]     = PrimInline $ i3_ a i |= e
genPrim _ _ WriteByteArrayOp_Word64 [] [a,i,e1,e2] =
  PrimInline $ mconcat
    [ i3_ a (Add (i .<<. one_) one_) |= e1
    , i3_ a (i .<<. one_) |= e2
    ]
{- implement new ops in 8.6
                  , WriteByteArrayOp_Word8AsChar
                  , WriteByteArrayOp_Word8AsWideChar
                  , WriteByteArrayOp_Word8AsAddr
                  , WriteByteArrayOp_Word8AsFloat
                  , WriteByteArrayOp_Word8AsDouble
                  , WriteByteArrayOp_Word8AsStablePtr
                  , WriteByteArrayOp_Word8AsInt16
                  , WriteByteArrayOp_Word8AsInt32
                  , WriteByteArrayOp_Word8AsInt64
                  , WriteByteArrayOp_Word8AsInt
                  , WriteByteArrayOp_Word8AsWord16
                  , WriteByteArrayOp_Word8AsWord32
                  , WriteByteArrayOp_Word8AsWord64
                  , WriteByteArrayOp_Word8AsWord
 -}

genPrim _ _ CompareByteArraysOp [r] [a1,o1,a2,o2,n] =
  PrimInline $ r |= app "h$compareByteArrays" [a1,o1,a2,o2,n]
-- fixme we can do faster by copying 32 bit ints or doubles
genPrim _ _ CopyByteArrayOp [] [a1,o1,a2,o2,n] =
  PrimInline $ loopBlockS (Sub n one_) (.>=. zero_) \i ->
    [ u8_ a2 (Add i o2) |= u8_ a1 (Add i o1)
    , postDecrS i
    ]
genPrim d t CopyMutableByteArrayOp [] xs@[_a1,_o1,_a2,_o2,_n] = genPrim d t CopyByteArrayOp [] xs
genPrim d t CopyByteArrayToAddrOp [] xs@[_a1,_o1,_a2,_o2,_n] = genPrim d t CopyByteArrayOp [] xs
genPrim d t CopyMutableByteArrayToAddrOp [] xs@[_a1,_o1,_a2,_o2,_n] = genPrim d t CopyByteArrayOp [] xs
genPrim d t CopyAddrToByteArrayOp [] xs@[_ba,_bo,_aa,_ao,_n] = genPrim d t CopyByteArrayOp [] xs

genPrim _ _ SetByteArrayOp [] [a,o,n,v] =
  PrimInline $ loopBlockS zero_ (.<. n) \i ->
    [ u8_ a (Add o i) |= v
    , postIncrS i
    ]

genPrim _ _ AtomicReadByteArrayOp_Int  [r]   [a,i]         = PrimInline $ r |= i3_ a i
genPrim _ _ AtomicWriteByteArrayOp_Int []    [a,i,v]       = PrimInline $ i3_ a i |= v
genPrim _ _ CasByteArrayOp_Int         [r]   [a,i,old,new] = PrimInline $
  jVar \t -> mconcat
    [ t |= i3_ a i
    , r |= t
    , ifS (t .===. old) (i3_ a i |= new) mempty
    ]
genPrim _ _ FetchAddByteArrayOp_Int    [r]   [a,i,v]       = PrimInline $ fetchOpByteArray Add  r a i v
genPrim _ _ FetchSubByteArrayOp_Int    [r]   [a,i,v]       = PrimInline $ fetchOpByteArray Sub  r a i v
genPrim _ _ FetchAndByteArrayOp_Int    [r]   [a,i,v]       = PrimInline $ fetchOpByteArray BAnd r a i v
genPrim _ _ FetchOrByteArrayOp_Int     [r]   [a,i,v]       = PrimInline $ fetchOpByteArray BOr  r a i v
genPrim _ _ FetchNandByteArrayOp_Int   [r]   [a,i,v]       = PrimInline $ fetchOpByteArray (\x y -> BNot (BAnd x y)) r a i v
genPrim _ _ FetchXorByteArrayOp_Int    [r]   [a,i,v]       = PrimInline $ fetchOpByteArray BXor r a i v

-- Addr#

genPrim _ _ AddrAddOp  [a',o'] [a,o,i]     = PrimInline $ mconcat [a' |= a, o' |= Add o i]
genPrim _ _ AddrSubOp  [i] [_a1,o1,_a2,o2] = PrimInline $ i |= Sub o1 o2
genPrim _ _ AddrRemOp  [r] [_a,o,i]        = PrimInline $ r |= Mod o i
genPrim _ _ AddrToIntOp [i]     [_a,o]     = PrimInline $ i |= o -- only usable for comparisons within one range
genPrim _ _ IntToAddrOp [a,o]   [i]        = PrimInline $ mconcat [a |= null_, o |= i] -- FIXME: unsupported
genPrim _ _ AddrGtOp   [r] [a1,o1,a2,o2] =
  PrimInline $ r |= if10 (app "h$comparePointer" [a1,o1,a2,o2] .>. zero_)
genPrim _ _ AddrGeOp   [r] [a1,o1,a2,o2] =
  PrimInline $ r |= if10 (app "h$comparePointer" [a1,o1,a2,o2] .>=. zero_)
genPrim _ _ AddrEqOp   [r] [a1,o1,a2,o2]   =
  PrimInline $ r |= if10 (app "h$comparePointer" [a1,o1,a2,o2] .===. zero_)
genPrim _ _ AddrNeOp   [r] [a1,o1,a2,o2]   =
  PrimInline $ r |= if10 (app "h$comparePointer" [a1,o1,a2,o2] .!==. zero_)
genPrim _ _ AddrLtOp   [r] [a1,o1,a2,o2] =
  PrimInline $ r |= if10 (app "h$comparePointer" [a1,o1,a2,o2] .<. zero_)
genPrim _ _ AddrLeOp   [r] [a1,o1,a2,o2] =
  PrimInline $ r |= if10 (app "h$comparePointer" [a1,o1,a2,o2] .<=. zero_)

-- addr indexing: unboxed arrays
genPrim _ _ IndexOffAddrOp_Char     [c] [a,o,i] = PrimInline $ c |= u8_ a    (off8  o i)
genPrim _ _ IndexOffAddrOp_WideChar [c] [a,o,i] = PrimInline $ c |= dv_u32 a (off32 o i)
genPrim _ _ IndexOffAddrOp_Int      [c] [a,o,i] = PrimInline $ c |= dv_i32 a (off32 o i)
genPrim _ _ IndexOffAddrOp_Word     [c] [a,o,i] = PrimInline $ c |= dv_i32 a (off32 o i)
genPrim _ _ IndexOffAddrOp_Addr [ca,co] [a,o,i] =
  PrimInline $ ifBlockS (a .^ "arr " .&&. a .^ "arr" .! (i .<<. two_))
                   [ ca |= a .^ "arr" .! (off32 o i) .! zero_
                   ,  co |= a .^ "arr" .! (off32 o i) .! one_
                   ]
                   [ ca |= null_
                   , co |= zero_
                   ]
genPrim _ _ IndexOffAddrOp_Float [c] [a,o,i] = PrimInline $ c |= dv_f32 a (off32 o i)
genPrim _ _ IndexOffAddrOp_Double [c] [a,o,i] = PrimInline $ c |= dv_f64 a (off64 o i)
genPrim _ _ IndexOffAddrOp_StablePtr [c1,c2] [a,o,i] = PrimInline $ mconcat
                                                          [ c1 |= var "h$stablePtrBuf"
                                                          , c2 |= dv_i32 a (off32 o i)
                                                          ]
genPrim _ _ IndexOffAddrOp_Int8  [c] [a,o,i] = PrimInline $ c |= u8_    a (off8  o i)
genPrim _ _ IndexOffAddrOp_Int16 [c] [a,o,i] = PrimInline $ c |= dv_i16 a (off16 o i)
genPrim _ _ IndexOffAddrOp_Int32 [c] [a,o,i] = PrimInline $ c |= dv_i32 a (off32 o i)
genPrim _ _ IndexOffAddrOp_Int64 [c1,c2] [a,o,i] =
   PrimInline $ mconcat
    [ c1 |= dv_i32 a (Add (off64 o i) (Int 4))
    , c2 |= dv_i32 a (off64 o i)
    ]
genPrim _ _ IndexOffAddrOp_Word8  [c] [a,o,i] = PrimInline $ c |= u8_    a (off8  o i)
genPrim _ _ IndexOffAddrOp_Word16 [c] [a,o,i] = PrimInline $ c |= dv_u16 a (off16 o i)
genPrim _ _ IndexOffAddrOp_Word32 [c] [a,o,i] = PrimInline $ c |= dv_i32 a (off32 o i)
genPrim _ _ IndexOffAddrOp_Word64 [c1,c2] [a,o,i] =
   PrimInline $ mconcat
    [ c1 |= dv_i32 a (Add (off64 o i) (Int 4))
    , c2 |= dv_i32 a (off64 o i)
    ]
genPrim _ _ ReadOffAddrOp_Char     [c] [a,o,i] = PrimInline $ c |= u8_    a (off8  o i)
genPrim _ _ ReadOffAddrOp_WideChar [c] [a,o,i] = PrimInline $ c |= dv_u32 a (off32 o i)
genPrim _ _ ReadOffAddrOp_Int      [c] [a,o,i] = PrimInline $ c |= dv_i32 a (off32 o i)
genPrim _ _ ReadOffAddrOp_Word     [c] [a,o,i] = PrimInline $ c |= dv_i32 a (off32 o i)
genPrim _ _ ReadOffAddrOp_Addr [c1,c2] [a,o,i] =
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
genPrim _ _ ReadOffAddrOp_Float  [c] [a,o,i] = PrimInline $ c |= dv_f32 a (off32 o i)
genPrim _ _ ReadOffAddrOp_Double [c] [a,o,i] = PrimInline $ c |= dv_f64 a (off64 o i)
genPrim _ _ ReadOffAddrOp_StablePtr [c1,c2] [a,o,i] = PrimInline $ mconcat
                                                        [ c1 |= var "h$stablePtrBuf"
                                                        , c2 |= dv_u32 a (off32 o i)
                                                        ]
genPrim _ _ ReadOffAddrOp_Int8   [c] [a,o,i] = PrimInline $ AssignStat c $ dv_i8  a (off8  o i)
genPrim _ _ ReadOffAddrOp_Int16  [c] [a,o,i] = PrimInline $ AssignStat c $ dv_i16 a (off16 o i)
genPrim _ _ ReadOffAddrOp_Int32  [c] [a,o,i] = PrimInline $ AssignStat c $ dv_i32 a (off32 o i)
genPrim _ _ ReadOffAddrOp_Int64  [c1,c2] [a,o,i] =
  PrimInline $ mconcat
    [ c1 |= dv_i32 a (Add (off64 o i) (Int 4))
    , c2 |= dv_i32 a (off64 o i)
    ]
genPrim _ _ ReadOffAddrOp_Word8  [c] [a,o,i] = PrimInline $ AssignStat c $ u8_    a (off8  o i)
genPrim _ _ ReadOffAddrOp_Word16 [c] [a,o,i] = PrimInline $ AssignStat c $ dv_u16 a (off16 o i)
genPrim _ _ ReadOffAddrOp_Word32 [c] [a,o,i] = PrimInline $ AssignStat c $ dv_i32 a (off32 o i)
genPrim _ _ ReadOffAddrOp_Word64 [c1,c2] [a,o,i] =
   PrimInline $ mconcat
    [ c1 |= dv_i32 a (Add (off64 o i) (Int 4))
    , c2 |= dv_i32 a (off64 o i)
    ]
genPrim _ _ WriteOffAddrOp_Char     [] [a,o,i,v]     = PrimInline $ u8_      a (off8  o i) |= v
genPrim _ _ WriteOffAddrOp_WideChar [] [a,o,i,v]     = PrimInline $ dv_s_u32 a (off32 o i) v
genPrim _ _ WriteOffAddrOp_Int      [] [a,o,i,v]     = PrimInline $ dv_s_i32 a (off32 o i) v
genPrim _ _ WriteOffAddrOp_Word     [] [a,o,i,v]     = PrimInline $ dv_s_i32 a (off32 o i) v
genPrim _ _ WriteOffAddrOp_Addr     [] [a,o,i,va,vo] =
  PrimInline $ mconcat
    [ ifS (Not (a .^ "arr")) (a .^ "arr" |= ValExpr (JList [])) mempty
    , AssignStat (a .^ "arr" .! (off32 o i)) $ ValExpr (JList [va, vo])
    ]
genPrim _ _ WriteOffAddrOp_Float     [] [a,o,i,v]      = PrimInline $ dv_s_f32 a (off32 o i) v
genPrim _ _ WriteOffAddrOp_Double    [] [a,o,i,v]      = PrimInline $ dv_s_f64 a (off64 o i) v
genPrim _ _ WriteOffAddrOp_StablePtr [] [a,o,i,_v1,v2] = PrimInline $ dv_s_u32 a (off32 o i) v2
genPrim _ _ WriteOffAddrOp_Int8      [] [a,o,i,v]      = PrimInline $ dv_s_i8  a (off8  o i) v
genPrim _ _ WriteOffAddrOp_Int16     [] [a,o,i,v]      = PrimInline $ dv_s_i16 a (off16 o i) v
genPrim _ _ WriteOffAddrOp_Int32     [] [a,o,i,v]      = PrimInline $ dv_s_i32 a (off32 o i) v
genPrim _ _ WriteOffAddrOp_Int64     [] [a,o,i,v1,v2]  = PrimInline $ mconcat
                                                          [ dv_s_i32 a (Add (off64 o i) (Int 4)) v1
                                                          , dv_s_i32 a (off64 o i) v2
                                                          ]
genPrim _ _ WriteOffAddrOp_Word8     [] [a,o,i,v]      = PrimInline $ u8_      a (off8  o i) |= v
genPrim _ _ WriteOffAddrOp_Word16    [] [a,o,i,v]      = PrimInline $ dv_s_u16 a (off16 o i) v
genPrim _ _ WriteOffAddrOp_Word32    [] [a,o,i,v]      = PrimInline $ dv_s_i32 a (off32 o i) v
genPrim _ _ WriteOffAddrOp_Word64    [] [a,o,i,v1,v2]  = PrimInline $ mconcat
                                                          [ dv_s_i32 a (Add (off64 o i) (Int 4)) v1
                                                          , dv_s_i32 a (off64 o i) v2
                                                          ]
-- Mutable variables
genPrim _ _ NewMutVarOp       [r] [x]   = PrimInline $ r |= New (app "h$MutVar" [x])
genPrim _ _ ReadMutVarOp      [r] [m]   = PrimInline $ r |= m .^ "val"
genPrim _ _ WriteMutVarOp     [] [m,x]  = PrimInline $ m .^ "val" |= x
genPrim _ _ AtomicModifyMutVar2Op [r1,r2] [m,f] = PrimInline $ appT [r1,r2] "h$atomicModifyMutVar2" [m,f]
genPrim _ _ AtomicModifyMutVar_Op [r1,r2] [m,f] = PrimInline $ appT [r1,r2] "h$atomicModifyMutVar" [m,f]

genPrim _ _ CasMutVarOp [status,r] [mv,o,n] = PrimInline $ ifS (mv .^ "val" .===. o)
                   (mconcat [status |= zero_, r |= n, mv .^ "val" |= n])
                   (mconcat [status |= one_ , r |= mv .^ "val"])

-- Exceptions

genPrim _ _ CatchOp [_r] [a,handler] = PRPrimCall $
  returnS (app "h$catch" [a, handler])
genPrim _ _ RaiseOp         [_r] [a] = PRPrimCall $ returnS (app "h$throw" [a, false_])
genPrim _ _ RaiseIOOp       [_r] [a] = PRPrimCall $ returnS (app "h$throw" [a, false_])

genPrim _ _ MaskAsyncExceptionsOp [_r] [a] =
  PRPrimCall $ returnS (app "h$maskAsync" [a])
genPrim _ _ MaskUninterruptibleOp [_r] [a] =
  PRPrimCall $ returnS (app "h$maskUnintAsync" [a])
genPrim _ _ UnmaskAsyncExceptionsOp [_r] [a] =
  PRPrimCall $ returnS (app "h$unmaskAsync" [a])

genPrim _ _ MaskStatus [r] [] = PrimInline $ r |= app "h$maskStatus" []

-- STM-accessible Mutable Variables

genPrim _ _ AtomicallyOp [_r] [a] = PRPrimCall $ returnS (app "h$atomically" [a])
genPrim _ _ RetryOp [_r] [] = PRPrimCall $ returnS (app "h$stmRetry" [])
genPrim _ _ CatchRetryOp [_r] [a,b] = PRPrimCall $ returnS (app "h$stmCatchRetry" [a,b])
genPrim _ _ CatchSTMOp [_r] [a,h] = PRPrimCall $ returnS (app "h$catchStm" [a,h])
genPrim _ _ NewTVarOp [tv] [v] = PrimInline $ tv |= app "h$newTVar" [v]
genPrim _ _ ReadTVarOp [r] [tv] = PrimInline $ r |= app "h$readTVar" [tv]
genPrim _ _ ReadTVarIOOp [r] [tv] = PrimInline $ r |= app "h$readTVarIO" [tv]
genPrim _ _ WriteTVarOp [] [tv,v] = PrimInline $ appS "h$writeTVar" [tv,v]

-- Synchronized Mutable Variables

genPrim _ _ NewMVarOp     [r]   []    = PrimInline $ r |= New (app "h$MVar" [])
genPrim _ _ TakeMVarOp    [_r]  [m]   = PRPrimCall $ returnS (app "h$takeMVar" [m])
genPrim _ _ TryTakeMVarOp [r,v] [m]   = PrimInline $ appT [r,v] "h$tryTakeMVar" [m]
genPrim _ _ PutMVarOp     []    [m,v] = PRPrimCall $ returnS (app "h$putMVar" [m,v])
genPrim _ _ TryPutMVarOp  [r]   [m,v] = PrimInline $ r |= app "h$tryPutMVar" [m,v]
genPrim _ _ ReadMVarOp    [_r]  [m]   = PRPrimCall $ returnS (app "h$readMVar" [m])
genPrim _ _ TryReadMVarOp [r,v] [m]   = PrimInline $ mconcat
                                                    [ v |= m .^ "val"
                                                    , r |= if01 (v .===. null_)
                                                    ]
genPrim _ _ IsEmptyMVarOp [r]   [m]   = PrimInline $ r |= if10 (m .^ "val" .===. null_)

-- Delay/wait operations

genPrim _ _ DelayOp [] [t] = PRPrimCall $ returnS (app "h$delayThread" [t])
genPrim _ _ WaitReadOp [] [fd] = PRPrimCall $ returnS (app "h$waidRead" [fd])
genPrim _ _ WaitWriteOp [] [fd] = PRPrimCall $ returnS (app "h$waitWrite" [fd])

-- Concurrency primitives

genPrim _ _ ForkOp [_tid] [x] = PRPrimCall $ returnS (app "h$fork" [x, true_])
genPrim _ _ ForkOnOp [_tid] [_p,x] = PRPrimCall $ returnS (app "h$fork" [x, true_]) -- ignore processor argument
genPrim _ _ KillThreadOp [] [tid,ex] =
  PRPrimCall $ returnS (app "h$killThread" [tid,ex])
genPrim _ _ YieldOp [] [] = PRPrimCall $ returnS (app "h$yield" [])
genPrim _ _ MyThreadIdOp [r] [] = PrimInline $ r |= var "h$currentThread"
genPrim _ _ LabelThreadOp [] [t,la,lo] = PrimInline $ t .^ "label" |= ValExpr (JList [la, lo])
genPrim _ _ IsCurrentThreadBoundOp [r] [] = PrimInline $ r |= one_
genPrim _ _ NoDuplicateOp [] [] = PrimInline mempty -- don't need to do anything as long as we have eager blackholing
genPrim _ _ ThreadStatusOp [stat,cap,locked] [tid] = PrimInline $
  appT [stat, cap, locked] "h$threadStatus" [tid]

-- Weak pointers

genPrim _ _ MkWeakOp [r] [o,b,c] = PrimInline $ r |= app "h$makeWeak" [o,b,c]
genPrim _ _ MkWeakNoFinalizerOp [r] [o,b] = PrimInline $ r |= app "h$makeWeakNoFinalizer" [o,b]
genPrim _ _ AddCFinalizerToWeakOp [r] [_a1,_a1o,_a2,_a2o,_i,_a3,_a3o,_w] =
  PrimInline $ r |= one_ -- fixme?
genPrim _ _ DeRefWeakOp        [f,v] [w] = PrimInline $ mconcat
                                                        [ v |= w .^ "val"
                                                        , f |= if01 (v .===. null_)
                                                        ]
genPrim _ _ FinalizeWeakOp     [fl,fin] [w] =
  PrimInline $ appT [fin, fl] "h$finalizeWeak" [w]
genPrim _ _ TouchOp [] [_e] = PrimInline mempty -- fixme what to do?

-- Stable pointers and names

genPrim _ _ MakeStablePtrOp [s1,s2] [a] = PrimInline $ mconcat
  [ s1 |= var "h$stablePtrBuf"
  , s2 |= app "h$makeStablePtr" [a]
  ]
genPrim _ _ DeRefStablePtrOp [r] [_s1,s2] = PrimInline $
  r |= app "h$deRefStablePtr" [s2]
genPrim _ _ EqStablePtrOp [r] [_sa1,sa2,_sb1,sb2] = PrimInline $
  r |= if10 (sa2 .===. sb2)

genPrim _ _ MakeStableNameOp [r] [a] = PrimInline $ r |= app "h$makeStableName" [a]
genPrim _ _ StableNameToIntOp [r] [s] = PrimInline $ r |= app "h$stableNameInt" [s]

-- Compact normal form

genPrim _ _ CompactNewOp [c] [s] = PrimInline $ c |= app "h$compactNew" [s]
genPrim _ _ CompactResizeOp [] [s] = PrimInline $ appS "h$compactResize" [s]
genPrim _ _ CompactContainsOp [r] [c,v] = PrimInline $ r |= app "h$compactContains" [c,v]
genPrim _ _ CompactContainsAnyOp [r] [v] = PrimInline $ r |= app "h$compactContainsAny" [v]
genPrim _ _ CompactGetFirstBlockOp [ra,ro,s] [c] =
  PrimInline $ appT [ra,ro,s] "h$compactGetFirstBlock" [c]
genPrim _ _ CompactGetNextBlockOp [ra,ro,s] [c,a,o] =
  PrimInline $ appT [ra,ro,s] "h$compactGetNextBlock" [c,a,o]
genPrim _ _ CompactAllocateBlockOp [ra,ro] [size,sa,so] =
  PrimInline $ appT [ra,ro] "h$compactAllocateBlock" [size,sa,so]
genPrim _ _ CompactFixupPointersOp [newroota, newrooto] [blocka,blocko,roota,rooto] =
  PrimInline $ appT [newroota,newrooto] "h$compactFixupPointers" [blocka,blocko,roota,rooto]
genPrim _ _ CompactAdd [_r] [c,o] =
  PRPrimCall $ returnS (app "h$compactAdd" [c,o])
genPrim _ _ CompactAddWithSharing [_r] [c,o] =
  PRPrimCall $ returnS (app "h$compactAddWithSharing" [c,o])
genPrim _ _ CompactSize [s] [c] =
  PrimInline $ s |= app "h$compactSize" [c]

-- Unsafe pointer equality

genPrim _ _ ReallyUnsafePtrEqualityOp [r] [p1,p2] = PrimInline $ r |= if10 (p1 .===. p2)

-- Parallelism

genPrim _ _ ParOp   [r] [_a] = PrimInline $ r |= zero_
genPrim _ _ SparkOp [r] [a]  = PrimInline $ r |= a
genPrim _ _ SeqOp   [_r] [e] = PRPrimCall $ returnS (app "h$e" [e])
{-
GetSparkOp
-}
genPrim _ _ NumSparks [r] [] = PrimInline $ r |= zero_

-- Tag to enum stuff

genPrim _ _t DataToTagOp [_r] [d] = PRPrimCall $ mconcat
  [ stack .! PreInc sp |= var "h$dataToTag_e"
  , returnS (app "h$e" [d])
  ]
genPrim _ t TagToEnumOp [r] [tag]
  | isBoolTy t = PrimInline $ r |= IfExpr tag true_ false_
  | otherwise  = PrimInline $ r |= app "h$tagToEnum" [tag]

-- Bytecode operations

genPrim _ _ AddrToAnyOp [r] [d,_o] = PrimInline $ r |= d

{-
AnyToAddrOp
MkApUpd0_Op
NewBCOOp
UnpackClosureOp
GetApStackValOp
-}

-- Misc

genPrim prof _ GetCCSOfOp [a, o] [obj]
  | prof = PrimInline $ mconcat
      [ a |= if_ (isObject obj)
                  (app "h$buildCCSPtr" [obj .^ "cc"])
                  null_
      , o |= zero_
      ]
  | otherwise = PrimInline $ mconcat
                  [ a |= null_
                  , o |= zero_
                  ]

genPrim prof _ GetCurrentCCSOp [a, o] [_dummy_arg] =
  let ptr = if prof then app "h$buildCCSPtr" [jCurrentCCS]
                    else null_
  in PrimInline $ mconcat
      [ a |= ptr
      , o |= zero_
      ]

genPrim _ _ ClearCCSOp [_r] [x] = PRPrimCall $ ReturnStat (app "h$clearCCS" [x])

-- Etc (Miscellaneous built-ins)

genPrim _ _ TraceEventOp       [] [ed,eo]     = PrimInline $ appS "h$traceEvent" [ed,eo]
genPrim _ _ TraceEventBinaryOp [] [ed,eo,len] = PrimInline $ appS "h$traceEventBinary" [ed,eo,len]
genPrim _ _ TraceMarkerOp      [] [ed,eo]     = PrimInline $ appS "h$traceMarker" [ed,eo]

genPrim _ _ op rs as = PrimInline $ mconcat
  [ appS "h$log" [toJExpr $ mconcat
      [ "warning, unhandled primop: "
      , renderWithContext defaultSDocContext (ppr op)
      , " "
      , show (length rs, length as)
      ]]
  , appS (ST.pack $ "h$primop_" ++ renderWithContext defaultSDocContext (ppr op)) as
    -- copyRes
  , mconcat $ zipWith (\r reg -> r |= toJExpr reg) rs (enumFrom Ret1)
  ]

-- tuple returns
appT :: [JExpr] -> ShortText -> [JExpr] -> JStat
appT []     f xs = appS f xs
appT (r:rs) f xs = mconcat
  [ r |= app f xs
  , mconcat (zipWith (\r ret -> r |= toJExpr ret) rs (enumFrom Ret1))
  ]

i3_, u8_, f6_, f3_, u1_ :: JExpr -> JExpr -> JExpr
i3_ a i = IdxExpr (a .^ "i3") i
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
  [ tgt |= i3_ src i
  , i3_ src i |= op tgt v
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
trunc :: JExpr -> JExpr
trunc e = BOr e zero_

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
