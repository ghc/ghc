{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Main where

-- base
import GHC.Exts

--------------------------------------------------------------------------------

data DoubleX2 = DX2# DoubleX2#

instance Show DoubleX2 where
  show ( DX2# d ) = case unpackDoubleX2# d of
    (# a, b #) -> show ( D# a, D# b )

type T = (# DoubleX2#, DoubleX2#, DoubleX2#, DoubleX2#, DoubleX2#
          , DoubleX2#, DoubleX2#, DoubleX2#, DoubleX2#, DoubleX2#
          , DoubleX2#, DoubleX2#, DoubleX2#, DoubleX2#, DoubleX2#
          , DoubleX2#, DoubleX2#, DoubleX2#, DoubleX2#, DoubleX2#
          , DoubleX2#, DoubleX2#, DoubleX2#, DoubleX2#, DoubleX2#
          , DoubleX2#, DoubleX2#, DoubleX2#, DoubleX2#, DoubleX2#
          , DoubleX2#, DoubleX2#, DoubleX2#, DoubleX2#, DoubleX2#
          , DoubleX2#, DoubleX2#, DoubleX2#, DoubleX2#, DoubleX2#
          , DoubleX2#, DoubleX2#, DoubleX2#, DoubleX2#, DoubleX2#
          , DoubleX2#, DoubleX2#, DoubleX2#, DoubleX2#, DoubleX2# #)

type F =  DoubleX2# -> DoubleX2# -> DoubleX2# -> DoubleX2# -> DoubleX2#
       -> DoubleX2# -> DoubleX2# -> DoubleX2# -> DoubleX2# -> DoubleX2#
       -> DoubleX2# -> DoubleX2# -> DoubleX2# -> DoubleX2# -> DoubleX2#
       -> DoubleX2# -> DoubleX2# -> DoubleX2# -> DoubleX2# -> DoubleX2#
       -> DoubleX2# -> DoubleX2# -> DoubleX2# -> DoubleX2# -> DoubleX2#
       -> DoubleX2# -> DoubleX2# -> DoubleX2# -> DoubleX2# -> DoubleX2#
       -> DoubleX2# -> DoubleX2# -> DoubleX2# -> DoubleX2# -> DoubleX2#
       -> DoubleX2# -> DoubleX2# -> DoubleX2# -> DoubleX2# -> DoubleX2#
       -> DoubleX2# -> DoubleX2# -> DoubleX2# -> DoubleX2# -> DoubleX2#
       -> DoubleX2# -> DoubleX2# -> DoubleX2# -> DoubleX2# -> DoubleX2#
       -> T

f :: T -> T
f (# x00, x01, x02, x03, x04, x05, x06, x07, x08, x09
   , x10, x11, x12, x13, x14, x15, x16, x17, x18, x19
   , x20, x21, x22, x23, x24, x25, x26, x27, x28, x29
   , x30, x31, x32, x33, x34, x35, x36, x37, x38, x39
   , x40, x41, x42, x43, x44, x45, x46, x47, x48, x49 #)
 = (# x01, x02, x03, x04, x05, x06, x07, x08, x09
    , x10, x11, x12, x13, x14, x15, x16, x17, x18, x19
    , x20, x21, x22, x23, x24, x25, x26, x27, x28, x29
    , x30, x31, x32, x33, x34, x35, x36, x37, x38, x39
    , x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x00 #)
{-# OPAQUE f #-}

f2 :: T -> T -> T
f2 a b = f a
{-# OPAQUE f2 #-}

w :: Int -> T -> T
w 0 t = t
w i t =
  -- This recursion is designed to trigger stack overflow,
  -- so that we run into the register save/restore logic in the RTS
  -- when we reach the corresponding stack underflow frame.
  case w ( i - 1 ) t of
    r -> f2 r t
{-# OPAQUE w #-}

main :: IO ()
main = do
  let !( DX2# x00 ) = z00
      !( DX2# x01 ) = z01
      !( DX2# x02 ) = z02
      !( DX2# x03 ) = z03
      !( DX2# x04 ) = z04
      !( DX2# x05 ) = z05
      !( DX2# x06 ) = z06
      !( DX2# x07 ) = z07
      !( DX2# x08 ) = z08
      !( DX2# x09 ) = z09
      !( DX2# x10 ) = z10
      !( DX2# x11 ) = z11
      !( DX2# x12 ) = z12
      !( DX2# x13 ) = z13
      !( DX2# x14 ) = z14
      !( DX2# x15 ) = z15
      !( DX2# x16 ) = z16
      !( DX2# x17 ) = z17
      !( DX2# x18 ) = z18
      !( DX2# x19 ) = z19
      !( DX2# x20 ) = z20
      !( DX2# x21 ) = z21
      !( DX2# x22 ) = z22
      !( DX2# x23 ) = z23
      !( DX2# x24 ) = z24
      !( DX2# x25 ) = z25
      !( DX2# x26 ) = z26
      !( DX2# x27 ) = z27
      !( DX2# x28 ) = z28
      !( DX2# x29 ) = z29
      !( DX2# x30 ) = z30
      !( DX2# x31 ) = z31
      !( DX2# x32 ) = z32
      !( DX2# x33 ) = z33
      !( DX2# x34 ) = z34
      !( DX2# x35 ) = z35
      !( DX2# x36 ) = z36
      !( DX2# x37 ) = z37
      !( DX2# x38 ) = z38
      !( DX2# x39 ) = z39
      !( DX2# x40 ) = z40
      !( DX2# x41 ) = z41
      !( DX2# x42 ) = z42
      !( DX2# x43 ) = z43
      !( DX2# x44 ) = z44
      !( DX2# x45 ) = z45
      !( DX2# x46 ) = z46
      !( DX2# x47 ) = z47
      !( DX2# x48 ) = z48
      !( DX2# x49 ) = z49
      !t = (# x00, x01, x02, x03, x04, x05, x06, x07, x08, x09
            , x10, x11, x12, x13, x14, x15, x16, x17, x18, x19
            , x20, x21, x22, x23, x24, x25, x26, x27, x28, x29
            , x30, x31, x32, x33, x34, x35, x36, x37, x38, x39
            , x40, x41, x42, x43, x44, x45, x46, x47, x48, x49 #)
  let !u = w ( 2 * 50 + 16 ) t
  case u of
    (# r00, r01, r02, r03, r04, r05, r06, r07, r08, r09
     , r10, r11, r12, r13, r14, r15, r16, r17, r18, r19
     , r20, r21, r22, r23, r24, r25, r26, r27, r28, r29
     , r30, r31, r32, r33, r34, r35, r36, r37, r38, r39
     , r40, r41, r42, r43, r44, r45, r46, r47, r48, r49 #)
       -> do putStrLn "Should start listing pairs from 16:\n"
             putStrLn $ unlines $ map show $
               [ DX2# r00, DX2# r01, DX2# r02, DX2# r03, DX2# r04, DX2# r05, DX2# r06, DX2# r07, DX2# r08, DX2# r09
               , DX2# r10, DX2# r11, DX2# r12, DX2# r13, DX2# r14, DX2# r15, DX2# r16, DX2# r17, DX2# r18, DX2# r19
               , DX2# r20, DX2# r21, DX2# r22, DX2# r23, DX2# r24, DX2# r25, DX2# r26, DX2# r27, DX2# r28, DX2# r29
               , DX2# r30, DX2# r31, DX2# r32, DX2# r33, DX2# r34, DX2# r35, DX2# r36, DX2# r37, DX2# r38, DX2# r39
               , DX2# r40, DX2# r41, DX2# r42, DX2# r43, DX2# r44, DX2# r45, DX2# r46, DX2# r47, DX2# r48, DX2# r49 ]
  let !v = w ( 2 * 50 + 23 - 16 ) $ u
  case v of
    (# r00, r01, r02, r03, r04, r05, r06, r07, r08, r09
     , r10, r11, r12, r13, r14, r15, r16, r17, r18, r19
     , r20, r21, r22, r23, r24, r25, r26, r27, r28, r29
     , r30, r31, r32, r33, r34, r35, r36, r37, r38, r39
     , r40, r41, r42, r43, r44, r45, r46, r47, r48, r49 #)
       -> do putStrLn "\nShould start listing pairs from 23:\n"
             putStrLn $ unlines $ map show $
               [ DX2# r00, DX2# r01, DX2# r02, DX2# r03, DX2# r04, DX2# r05, DX2# r06, DX2# r07, DX2# r08, DX2# r09
               , DX2# r10, DX2# r11, DX2# r12, DX2# r13, DX2# r14, DX2# r15, DX2# r16, DX2# r17, DX2# r18, DX2# r19
               , DX2# r20, DX2# r21, DX2# r22, DX2# r23, DX2# r24, DX2# r25, DX2# r26, DX2# r27, DX2# r28, DX2# r29
               , DX2# r30, DX2# r31, DX2# r32, DX2# r33, DX2# r34, DX2# r35, DX2# r36, DX2# r37, DX2# r38, DX2# r39
               , DX2# r40, DX2# r41, DX2# r42, DX2# r43, DX2# r44, DX2# r45, DX2# r46, DX2# r47, DX2# r48, DX2# r49 ]

z00 :: DoubleX2; z00 = DX2# ( packDoubleX2# (#  0.1##,  0.2## #) ); {-# OPAQUE z00 #-}
z01 :: DoubleX2; z01 = DX2# ( packDoubleX2# (#  1.1##,  1.2## #) ); {-# OPAQUE z01 #-}
z02 :: DoubleX2; z02 = DX2# ( packDoubleX2# (#  2.1##,  2.2## #) ); {-# OPAQUE z02 #-}
z03 :: DoubleX2; z03 = DX2# ( packDoubleX2# (#  3.1##,  3.2## #) ); {-# OPAQUE z03 #-}
z04 :: DoubleX2; z04 = DX2# ( packDoubleX2# (#  4.1##,  4.2## #) ); {-# OPAQUE z04 #-}
z05 :: DoubleX2; z05 = DX2# ( packDoubleX2# (#  5.1##,  5.2## #) ); {-# OPAQUE z05 #-}
z06 :: DoubleX2; z06 = DX2# ( packDoubleX2# (#  6.1##,  6.2## #) ); {-# OPAQUE z06 #-}
z07 :: DoubleX2; z07 = DX2# ( packDoubleX2# (#  7.1##,  7.2## #) ); {-# OPAQUE z07 #-}
z08 :: DoubleX2; z08 = DX2# ( packDoubleX2# (#  8.1##,  8.2## #) ); {-# OPAQUE z08 #-}
z09 :: DoubleX2; z09 = DX2# ( packDoubleX2# (#  9.1##,  9.2## #) ); {-# OPAQUE z09 #-}
z10 :: DoubleX2; z10 = DX2# ( packDoubleX2# (# 10.1##, 10.2## #) ); {-# OPAQUE z10 #-}
z11 :: DoubleX2; z11 = DX2# ( packDoubleX2# (# 11.1##, 11.2## #) ); {-# OPAQUE z11 #-}
z12 :: DoubleX2; z12 = DX2# ( packDoubleX2# (# 12.1##, 12.2## #) ); {-# OPAQUE z12 #-}
z13 :: DoubleX2; z13 = DX2# ( packDoubleX2# (# 13.1##, 13.2## #) ); {-# OPAQUE z13 #-}
z14 :: DoubleX2; z14 = DX2# ( packDoubleX2# (# 14.1##, 14.2## #) ); {-# OPAQUE z14 #-}
z15 :: DoubleX2; z15 = DX2# ( packDoubleX2# (# 15.1##, 15.2## #) ); {-# OPAQUE z15 #-}
z16 :: DoubleX2; z16 = DX2# ( packDoubleX2# (# 16.1##, 16.2## #) ); {-# OPAQUE z16 #-}
z17 :: DoubleX2; z17 = DX2# ( packDoubleX2# (# 17.1##, 17.2## #) ); {-# OPAQUE z17 #-}
z18 :: DoubleX2; z18 = DX2# ( packDoubleX2# (# 18.1##, 18.2## #) ); {-# OPAQUE z18 #-}
z19 :: DoubleX2; z19 = DX2# ( packDoubleX2# (# 19.1##, 19.2## #) ); {-# OPAQUE z19 #-}
z20 :: DoubleX2; z20 = DX2# ( packDoubleX2# (# 20.1##, 20.2## #) ); {-# OPAQUE z20 #-}
z21 :: DoubleX2; z21 = DX2# ( packDoubleX2# (# 21.1##, 21.2## #) ); {-# OPAQUE z21 #-}
z22 :: DoubleX2; z22 = DX2# ( packDoubleX2# (# 22.1##, 22.2## #) ); {-# OPAQUE z22 #-}
z23 :: DoubleX2; z23 = DX2# ( packDoubleX2# (# 23.1##, 23.2## #) ); {-# OPAQUE z23 #-}
z24 :: DoubleX2; z24 = DX2# ( packDoubleX2# (# 24.1##, 24.2## #) ); {-# OPAQUE z24 #-}
z25 :: DoubleX2; z25 = DX2# ( packDoubleX2# (# 25.1##, 25.2## #) ); {-# OPAQUE z25 #-}
z26 :: DoubleX2; z26 = DX2# ( packDoubleX2# (# 26.1##, 26.2## #) ); {-# OPAQUE z26 #-}
z27 :: DoubleX2; z27 = DX2# ( packDoubleX2# (# 27.1##, 27.2## #) ); {-# OPAQUE z27 #-}
z28 :: DoubleX2; z28 = DX2# ( packDoubleX2# (# 28.1##, 28.2## #) ); {-# OPAQUE z28 #-}
z29 :: DoubleX2; z29 = DX2# ( packDoubleX2# (# 29.1##, 29.2## #) ); {-# OPAQUE z29 #-}
z30 :: DoubleX2; z30 = DX2# ( packDoubleX2# (# 30.1##, 30.2## #) ); {-# OPAQUE z30 #-}
z31 :: DoubleX2; z31 = DX2# ( packDoubleX2# (# 31.1##, 31.2## #) ); {-# OPAQUE z31 #-}
z32 :: DoubleX2; z32 = DX2# ( packDoubleX2# (# 32.1##, 32.2## #) ); {-# OPAQUE z32 #-}
z33 :: DoubleX2; z33 = DX2# ( packDoubleX2# (# 33.1##, 33.2## #) ); {-# OPAQUE z33 #-}
z34 :: DoubleX2; z34 = DX2# ( packDoubleX2# (# 34.1##, 34.2## #) ); {-# OPAQUE z34 #-}
z35 :: DoubleX2; z35 = DX2# ( packDoubleX2# (# 35.1##, 35.2## #) ); {-# OPAQUE z35 #-}
z36 :: DoubleX2; z36 = DX2# ( packDoubleX2# (# 36.1##, 36.2## #) ); {-# OPAQUE z36 #-}
z37 :: DoubleX2; z37 = DX2# ( packDoubleX2# (# 37.1##, 37.2## #) ); {-# OPAQUE z37 #-}
z38 :: DoubleX2; z38 = DX2# ( packDoubleX2# (# 38.1##, 38.2## #) ); {-# OPAQUE z38 #-}
z39 :: DoubleX2; z39 = DX2# ( packDoubleX2# (# 39.1##, 39.2## #) ); {-# OPAQUE z39 #-}
z40 :: DoubleX2; z40 = DX2# ( packDoubleX2# (# 40.1##, 40.2## #) ); {-# OPAQUE z40 #-}
z41 :: DoubleX2; z41 = DX2# ( packDoubleX2# (# 41.1##, 41.2## #) ); {-# OPAQUE z41 #-}
z42 :: DoubleX2; z42 = DX2# ( packDoubleX2# (# 42.1##, 42.2## #) ); {-# OPAQUE z42 #-}
z43 :: DoubleX2; z43 = DX2# ( packDoubleX2# (# 43.1##, 43.2## #) ); {-# OPAQUE z43 #-}
z44 :: DoubleX2; z44 = DX2# ( packDoubleX2# (# 44.1##, 44.2## #) ); {-# OPAQUE z44 #-}
z45 :: DoubleX2; z45 = DX2# ( packDoubleX2# (# 45.1##, 45.2## #) ); {-# OPAQUE z45 #-}
z46 :: DoubleX2; z46 = DX2# ( packDoubleX2# (# 46.1##, 46.2## #) ); {-# OPAQUE z46 #-}
z47 :: DoubleX2; z47 = DX2# ( packDoubleX2# (# 47.1##, 47.2## #) ); {-# OPAQUE z47 #-}
z48 :: DoubleX2; z48 = DX2# ( packDoubleX2# (# 48.1##, 48.2## #) ); {-# OPAQUE z48 #-}
z49 :: DoubleX2; z49 = DX2# ( packDoubleX2# (# 49.1##, 49.2## #) ); {-# OPAQUE z49 #-}
