{-# LANGUAGE MagicHash #-}

import GHC.Exts hiding
  ( (+#)
  , (-#)
  , (*#)
  )

import GHC.Word
import GHC.Int

(+&) = plusWord32#
(-&) = subWord32#
(*&) = timesWord32#

(+#) = plusInt32#
(-#) = subInt32#
(*#) = timesInt32#

{-# NOINLINE testsW #-}
-- NOINLINE otherwise basic constant folding rules (without
-- variables) are applied
testsW :: Word32# -> Word32# -> [Word32]
testsW x y =
   [ W32# (w43 +& (w37 +& x))
   , W32# (w43 +& (w37 -& x))
   , W32# (w43 +& (x -& w37))
   , W32# (w43 -& (w37 +& x))
   , W32# (w43 -& (w37 -& x))
   , W32# (w43 -& (x -& w37))
   , W32# ((w43 +& x) -& w37)
   , W32# ((x +& w43) -& w37)
   , W32# ((w43 -& x) -& w37)
   , W32# ((x -& w43) -& w37)

   , W32# ((x +& w43) +& (y +& w37))
   , W32# ((x +& w43) +& (y -& w37))
   , W32# ((x +& w43) +& (w37 -& y))
   , W32# ((x -& w43) +& (w37 -& y))
   , W32# ((x -& w43) +& (y -& w37))
   , W32# ((w43 -& x) +& (w37 -& y))
   , W32# ((w43 -& x) +& (y -& w37))
   ]
  where
   W32# w37 = 37
   W32# w43 = 43

{-# NOINLINE testsI #-}
testsI :: Int32# -> Int32# -> [Int32]
testsI x y =
   [ I32# (i43 +# (i37 +# x))
   , I32# (i43 +# (i37 -# x))
   , I32# (i43 +# (x -# i37))
   , I32# (i43 -# (i37 +# x))
   , I32# (i43 -# (i37 -# x))
   , I32# (i43 -# (x -# i37))
   , I32# ((i43 +# x) -# i37)
   , I32# ((x +# i43) -# i37)
   , I32# ((i43 -# x) -# i37)
   , I32# ((x -# i43) -# i37)

   , I32# ((x +# i43) +# (y +# i37))
   , I32# ((x +# i43) +# (y -# i37))
   , I32# ((x +# i43) +# (i37 -# y))
   , I32# ((x -# i43) +# (i37 -# y))
   , I32# ((x -# i43) +# (y -# i37))
   , I32# ((i43 -# x) +# (i37 -# y))
   , I32# ((i43 -# x) +# (y -# i37))

   , I32# ((x +# i43) -# (y +# i37))
   , I32# ((x +# i43) -# (y -# i37))
   , I32# ((x +# i43) -# (i37 -# y))
   , I32# ((x -# i43) -# (y +# i37))
   , I32# ((i43 -# x) -# (i37 +# y))
   , I32# ((x -# i43) -# (y -# i37))
   , I32# ((x -# i43) -# (i37 -# y))
   , I32# ((i43 -# x) -# (y -# i37))
   , I32# ((i43 -# x) -# (i37 -# y))

   , I32# (i43 *# (i37 *# y))
   , I32# (i43 *# (y *# i37))
   , I32# ((i43 *# x) *# (y *# i37))

   , I32# (i43 *# (i37 +# y))
   , I32# (i43 *# (i37 -# y))
   , I32# (i43 *# (y -# i37))

   , I32# (x +# x)
   , I32# ((i43 *# x) +# x)
   , I32# (x +# (i43 *# x))
   , I32# ((i43 *# x) +# (i37 *# x))
   , I32# ((i43 *# x) +# (x *# i37))

   , I32# (x -# x)
   , I32# ((i43 *# x) -# x)
   , I32# (x -# (i43 *# x))
   , I32# ((i43 *# x) -# (i37 *# x))
   , I32# ((i43 *# x) -# (x *# i37))

   , I32# (x +# (i37 +# y))
   , I32# (x +# (y +# i37))
   , I32# (x +# (i37 -# y))
   , I32# (x +# (y -# i37))
   , I32# (x -# (i37 +# y))
   , I32# (x -# (y +# i37))
   , I32# (x -# (i37 -# y))
   , I32# (x -# (y -# i37))
   , I32# ((i37 +# y) -# x)
   , I32# ((y +# i37) -# x)
   , I32# ((i37 -# y) -# x)
   , I32# ((y -# i37) -# x)

   , I32# (y *# y)
   ]
  where
   I32# i37 = 37
   I32# i43 = 43


main :: IO ()
main = do
   print (testsW w7 w13)
   print (testsI i7 i13)
  where
   W32# w7 = 7
   W32# w13 = 13
   I32# i7 = 7
   I32# i13 = 13
