{-# LANGUAGE MagicHash #-}

import GHC.Exts hiding
  ( (+#)
  , (-#)
  , (*#)
  )

import GHC.Word
import GHC.Int

(+&) = plusWord16#
(-&) = subWord16#
(*&) = timesWord16#

(+#) = plusInt16#
(-#) = subInt16#
(*#) = timesInt16#

{-# NOINLINE testsW #-}
-- NOINLINE otherwise basic constant folding rules (without
-- variables) are applied
testsW :: Word16# -> Word16# -> [Word16]
testsW x y =
   [ W16# (w43 +& (w37 +& x))
   , W16# (w43 +& (w37 -& x))
   , W16# (w43 +& (x -& w37))
   , W16# (w43 -& (w37 +& x))
   , W16# (w43 -& (w37 -& x))
   , W16# (w43 -& (x -& w37))
   , W16# ((w43 +& x) -& w37)
   , W16# ((x +& w43) -& w37)
   , W16# ((w43 -& x) -& w37)
   , W16# ((x -& w43) -& w37)

   , W16# ((x +& w43) +& (y +& w37))
   , W16# ((x +& w43) +& (y -& w37))
   , W16# ((x +& w43) +& (w37 -& y))
   , W16# ((x -& w43) +& (w37 -& y))
   , W16# ((x -& w43) +& (y -& w37))
   , W16# ((w43 -& x) +& (w37 -& y))
   , W16# ((w43 -& x) +& (y -& w37))
   ]
  where
   W16# w37 = 37
   W16# w43 = 43

{-# NOINLINE testsI #-}
testsI :: Int16# -> Int16# -> [Int16]
testsI x y =
   [ I16# (i43 +# (i37 +# x))
   , I16# (i43 +# (i37 -# x))
   , I16# (i43 +# (x -# i37))
   , I16# (i43 -# (i37 +# x))
   , I16# (i43 -# (i37 -# x))
   , I16# (i43 -# (x -# i37))
   , I16# ((i43 +# x) -# i37)
   , I16# ((x +# i43) -# i37)
   , I16# ((i43 -# x) -# i37)
   , I16# ((x -# i43) -# i37)

   , I16# ((x +# i43) +# (y +# i37))
   , I16# ((x +# i43) +# (y -# i37))
   , I16# ((x +# i43) +# (i37 -# y))
   , I16# ((x -# i43) +# (i37 -# y))
   , I16# ((x -# i43) +# (y -# i37))
   , I16# ((i43 -# x) +# (i37 -# y))
   , I16# ((i43 -# x) +# (y -# i37))

   , I16# ((x +# i43) -# (y +# i37))
   , I16# ((x +# i43) -# (y -# i37))
   , I16# ((x +# i43) -# (i37 -# y))
   , I16# ((x -# i43) -# (y +# i37))
   , I16# ((i43 -# x) -# (i37 +# y))
   , I16# ((x -# i43) -# (y -# i37))
   , I16# ((x -# i43) -# (i37 -# y))
   , I16# ((i43 -# x) -# (y -# i37))
   , I16# ((i43 -# x) -# (i37 -# y))

   , I16# (i43 *# (i37 *# y))
   , I16# (i43 *# (y *# i37))
   , I16# ((i43 *# x) *# (y *# i37))

   , I16# (i43 *# (i37 +# y))
   , I16# (i43 *# (i37 -# y))
   , I16# (i43 *# (y -# i37))

   , I16# (x +# x)
   , I16# ((i43 *# x) +# x)
   , I16# (x +# (i43 *# x))
   , I16# ((i43 *# x) +# (i37 *# x))
   , I16# ((i43 *# x) +# (x *# i37))

   , I16# (x -# x)
   , I16# ((i43 *# x) -# x)
   , I16# (x -# (i43 *# x))
   , I16# ((i43 *# x) -# (i37 *# x))
   , I16# ((i43 *# x) -# (x *# i37))

   , I16# (x +# (i37 +# y))
   , I16# (x +# (y +# i37))
   , I16# (x +# (i37 -# y))
   , I16# (x +# (y -# i37))
   , I16# (x -# (i37 +# y))
   , I16# (x -# (y +# i37))
   , I16# (x -# (i37 -# y))
   , I16# (x -# (y -# i37))
   , I16# ((i37 +# y) -# x)
   , I16# ((y +# i37) -# x)
   , I16# ((i37 -# y) -# x)
   , I16# ((y -# i37) -# x)

   , I16# (y *# y)
   ]
  where
   I16# i37 = 37
   I16# i43 = 43


main :: IO ()
main = do
   print (testsW w7 w13)
   print (testsI i7 i13)
  where
   W16# w7 = 7
   W16# w13 = 13
   I16# i7 = 7
   I16# i13 = 13
