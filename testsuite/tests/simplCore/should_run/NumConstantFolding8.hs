{-# LANGUAGE MagicHash #-}

import GHC.Exts hiding
  ( (+#)
  , (-#)
  , (*#)
  )

import GHC.Word
import GHC.Int

(+&) = plusWord8#
(-&) = subWord8#
(*&) = timesWord8#

(+#) = plusInt8#
(-#) = subInt8#
(*#) = timesInt8#

{-# NOINLINE testsW #-}
-- NOINLINE otherwise basic constant folding rules (without
-- variables) are applied
testsW :: Word8# -> Word8# -> [Word8]
testsW x y =
   [ W8# (w43 +& (w37 +& x))
   , W8# (w43 +& (w37 -& x))
   , W8# (w43 +& (x -& w37))
   , W8# (w43 -& (w37 +& x))
   , W8# (w43 -& (w37 -& x))
   , W8# (w43 -& (x -& w37))
   , W8# ((w43 +& x) -& w37)
   , W8# ((x +& w43) -& w37)
   , W8# ((w43 -& x) -& w37)
   , W8# ((x -& w43) -& w37)

   , W8# ((x +& w43) +& (y +& w37))
   , W8# ((x +& w43) +& (y -& w37))
   , W8# ((x +& w43) +& (w37 -& y))
   , W8# ((x -& w43) +& (w37 -& y))
   , W8# ((x -& w43) +& (y -& w37))
   , W8# ((w43 -& x) +& (w37 -& y))
   , W8# ((w43 -& x) +& (y -& w37))
   ]
  where
   W8# w37 = 37
   W8# w43 = 43

{-# NOINLINE testsI #-}
testsI :: Int8# -> Int8# -> [Int8]
testsI x y =
   [ I8# (i43 +# (i37 +# x))
   , I8# (i43 +# (i37 -# x))
   , I8# (i43 +# (x -# i37))
   , I8# (i43 -# (i37 +# x))
   , I8# (i43 -# (i37 -# x))
   , I8# (i43 -# (x -# i37))
   , I8# ((i43 +# x) -# i37)
   , I8# ((x +# i43) -# i37)
   , I8# ((i43 -# x) -# i37)
   , I8# ((x -# i43) -# i37)

   , I8# ((x +# i43) +# (y +# i37))
   , I8# ((x +# i43) +# (y -# i37))
   , I8# ((x +# i43) +# (i37 -# y))
   , I8# ((x -# i43) +# (i37 -# y))
   , I8# ((x -# i43) +# (y -# i37))
   , I8# ((i43 -# x) +# (i37 -# y))
   , I8# ((i43 -# x) +# (y -# i37))

   , I8# ((x +# i43) -# (y +# i37))
   , I8# ((x +# i43) -# (y -# i37))
   , I8# ((x +# i43) -# (i37 -# y))
   , I8# ((x -# i43) -# (y +# i37))
   , I8# ((i43 -# x) -# (i37 +# y))
   , I8# ((x -# i43) -# (y -# i37))
   , I8# ((x -# i43) -# (i37 -# y))
   , I8# ((i43 -# x) -# (y -# i37))
   , I8# ((i43 -# x) -# (i37 -# y))

   , I8# (i43 *# (i37 *# y))
   , I8# (i43 *# (y *# i37))
   , I8# ((i43 *# x) *# (y *# i37))

   , I8# (i43 *# (i37 +# y))
   , I8# (i43 *# (i37 -# y))
   , I8# (i43 *# (y -# i37))

   , I8# (x +# x)
   , I8# ((i43 *# x) +# x)
   , I8# (x +# (i43 *# x))
   , I8# ((i43 *# x) +# (i37 *# x))
   , I8# ((i43 *# x) +# (x *# i37))

   , I8# (x -# x)
   , I8# ((i43 *# x) -# x)
   , I8# (x -# (i43 *# x))
   , I8# ((i43 *# x) -# (i37 *# x))
   , I8# ((i43 *# x) -# (x *# i37))

   , I8# (x +# (i37 +# y))
   , I8# (x +# (y +# i37))
   , I8# (x +# (i37 -# y))
   , I8# (x +# (y -# i37))
   , I8# (x -# (i37 +# y))
   , I8# (x -# (y +# i37))
   , I8# (x -# (i37 -# y))
   , I8# (x -# (y -# i37))
   , I8# ((i37 +# y) -# x)
   , I8# ((y +# i37) -# x)
   , I8# ((i37 -# y) -# x)
   , I8# ((y -# i37) -# x)

   , I8# (y *# y)
   ]
  where
   I8# i37 = 37
   I8# i43 = 43


main :: IO ()
main = do
   print (testsW w7 w13)
   print (testsI i7 i13)
  where
   W8# w7 = 7
   W8# w13 = 13
   I8# i7 = 7
   I8# i13 = 13
