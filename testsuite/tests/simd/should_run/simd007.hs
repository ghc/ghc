{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RecordWildCards #-}
-- tests for spilling of vector registers

import GHC.Exts

data FloatX4  = FX4# FloatX4#

instance Show FloatX4 where
  show (FX4# f) = case (unpackFloatX4# f) of
    (# a, b, c, d #) -> show ((F# a), (F# b), (F# c), (F# d))


instance Eq FloatX4 where
  (FX4# a) == (FX4# b)
    = case (unpackFloatX4# a) of
        (# a1, a2, a3, a4 #) ->
          case (unpackFloatX4# b) of
            (# b1, b2, b3, b4 #) -> (F# a1) == (F# b1) &&
                                    (F# a2) == (F# b2) &&
                                    (F# a3) == (F# b3) &&
                                    (F# a4) == (F# b4)

data DoubleX2 = DX2# DoubleX2#

instance Show DoubleX2 where
  show (DX2# d) = case (unpackDoubleX2# d) of
    (# a, b #) -> show ((D# a), (D# b))


instance Eq DoubleX2 where
  (DX2# a) == (DX2# b)
    = case (unpackDoubleX2# a) of
        (# a1, a2 #) ->
          case (unpackDoubleX2# b) of
            (# b1, b2 #) -> (D# a1) == (D# b1) &&
                            (D# a2) == (D# b2)


data F = F { x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16
           , x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32
             :: FloatX4# }
instance Show F where
  show ( F { .. } )
    = show [ FX4# x1, FX4# x2, FX4# x3, FX4# x4, FX4# x5, FX4# x6
           , FX4# x7, FX4# x8, FX4# x9, FX4# x10, FX4# x11, FX4# x12
           , FX4# x13, FX4# x14, FX4# x15, FX4# x16, FX4# x17, FX4# x18
           , FX4# x19, FX4# x20, FX4# x21, FX4# x22, FX4# x23, FX4# x24
           , FX4# x25, FX4# x26, FX4# x27, FX4# x28, FX4# x29, FX4# x30
           , FX4# x31, FX4# x32 ]
data D = D { y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16
           , y17, y18, y19, y20, y21, y22, y23, y24, y25, y26, y27, y28, y29, y30, y31, y32
             :: DoubleX2# }
instance Show D where
  show ( D { .. } )
    = show [ DX2# y1, DX2# y2, DX2# y3, DX2# y4, DX2# y5, DX2# y6
           , DX2# y7, DX2# y8, DX2# y9, DX2# y10, DX2# y11, DX2# y12
           , DX2# y13, DX2# y14, DX2# y15, DX2# y16, DX2# y17, DX2# y18
           , DX2# y19, DX2# y20, DX2# y21, DX2# y22, DX2# y23, DX2# y24
           , DX2# y25, DX2# y26, DX2# y27, DX2# y28, DX2# y29, DX2# y30
           , DX2# y31, DX2# y32 ]

setF :: FloatX4# -> F -> F
setF f ( F { .. }) = F { x17 = f, .. }
{-# NOINLINE setF #-}
setD :: DoubleX2# -> D -> D
setD d ( D { .. } ) = D { y17 = d, .. }
{-# NOINLINE setD #-}

main :: IO ()
main = do

  print $ setF (packFloatX4# (# 17.5#, 17.6#, 17.7#, 17.8# #))
   (F
     ( packFloatX4# (#  1.1#,  1.2#,  1.3#,  1.4# #) )
     ( packFloatX4# (#  2.1#,  2.2#,  2.3#,  2.4# #) )
     ( packFloatX4# (#  3.1#,  3.2#,  3.3#,  3.4# #) )
     ( packFloatX4# (#  4.1#,  4.2#,  4.3#,  4.4# #) )
     ( packFloatX4# (#  5.1#,  5.2#,  5.3#,  5.4# #) )
     ( packFloatX4# (#  6.1#,  6.2#,  6.3#,  6.4# #) )
     ( packFloatX4# (#  7.1#,  7.2#,  7.3#,  7.4# #) )
     ( packFloatX4# (#  8.1#,  8.2#,  8.3#,  8.4# #) )
     ( packFloatX4# (#  9.1#,  9.2#,  9.3#,  9.4# #) )
     ( packFloatX4# (# 10.1#, 10.2#, 10.3#, 10.4# #) )
     ( packFloatX4# (# 11.1#, 11.2#, 11.3#, 11.4# #) )
     ( packFloatX4# (# 12.1#, 12.2#, 12.3#, 12.4# #) )
     ( packFloatX4# (# 13.1#, 13.2#, 13.3#, 13.4# #) )
     ( packFloatX4# (# 14.1#, 14.2#, 14.3#, 14.4# #) )
     ( packFloatX4# (# 15.1#, 15.2#, 15.3#, 15.4# #) )
     ( packFloatX4# (# 16.1#, 16.2#, 16.3#, 16.4# #) )
     ( packFloatX4# (# 17.1#, 17.2#, 17.3#, 17.4# #) )
     ( packFloatX4# (# 18.1#, 18.2#, 18.3#, 18.4# #) )
     ( packFloatX4# (# 19.1#, 19.2#, 19.3#, 19.4# #) )
     ( packFloatX4# (# 20.1#, 20.2#, 20.3#, 20.4# #) )
     ( packFloatX4# (# 21.1#, 21.2#, 21.3#, 21.4# #) )
     ( packFloatX4# (# 22.1#, 22.2#, 22.3#, 22.4# #) )
     ( packFloatX4# (# 23.1#, 23.2#, 23.3#, 23.4# #) )
     ( packFloatX4# (# 24.1#, 24.2#, 24.3#, 24.4# #) )
     ( packFloatX4# (# 25.1#, 25.2#, 25.3#, 25.4# #) )
     ( packFloatX4# (# 26.1#, 26.2#, 26.3#, 26.4# #) )
     ( packFloatX4# (# 27.1#, 27.2#, 27.3#, 27.4# #) )
     ( packFloatX4# (# 28.1#, 28.2#, 28.3#, 28.4# #) )
     ( packFloatX4# (# 28.1#, 28.2#, 28.3#, 28.4# #) )
     ( packFloatX4# (# 30.1#, 30.2#, 30.3#, 30.4# #) )
     ( packFloatX4# (# 31.1#, 31.2#, 31.3#, 31.4# #) )
     ( packFloatX4# (# 32.1#, 32.2#, 32.3#, 32.4# #) )
    )

  print $ setD (packDoubleX2# (# 17.5##, 17.6## #))
   (D
     ( packDoubleX2# (#  1.1##,  1.2## #) )
     ( packDoubleX2# (#  2.1##,  2.2## #) )
     ( packDoubleX2# (#  3.1##,  3.2## #) )
     ( packDoubleX2# (#  4.1##,  4.2## #) )
     ( packDoubleX2# (#  5.1##,  5.2## #) )
     ( packDoubleX2# (#  6.1##,  6.2## #) )
     ( packDoubleX2# (#  7.1##,  7.2## #) )
     ( packDoubleX2# (#  8.1##,  8.2## #) )
     ( packDoubleX2# (#  9.1##,  9.2## #) )
     ( packDoubleX2# (# 10.1##, 10.2## #) )
     ( packDoubleX2# (# 11.1##, 11.2## #) )
     ( packDoubleX2# (# 12.1##, 12.2## #) )
     ( packDoubleX2# (# 13.1##, 13.2## #) )
     ( packDoubleX2# (# 14.1##, 14.2## #) )
     ( packDoubleX2# (# 15.1##, 15.2## #) )
     ( packDoubleX2# (# 16.1##, 16.2## #) )
     ( packDoubleX2# (# 17.1##, 17.2## #) )
     ( packDoubleX2# (# 18.1##, 18.2## #) )
     ( packDoubleX2# (# 19.1##, 19.2## #) )
     ( packDoubleX2# (# 20.1##, 20.2## #) )
     ( packDoubleX2# (# 21.1##, 21.2## #) )
     ( packDoubleX2# (# 22.1##, 22.2## #) )
     ( packDoubleX2# (# 23.1##, 23.2## #) )
     ( packDoubleX2# (# 24.1##, 24.2## #) )
     ( packDoubleX2# (# 25.1##, 25.2## #) )
     ( packDoubleX2# (# 26.1##, 26.2## #) )
     ( packDoubleX2# (# 27.1##, 27.2## #) )
     ( packDoubleX2# (# 28.1##, 28.2## #) )
     ( packDoubleX2# (# 28.1##, 28.2## #) )
     ( packDoubleX2# (# 30.1##, 30.2## #) )
     ( packDoubleX2# (# 31.1##, 31.2## #) )
     ( packDoubleX2# (# 32.1##, 32.2## #) )
    )
