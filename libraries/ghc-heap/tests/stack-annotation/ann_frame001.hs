
import TestUtils

hello :: Int -> Int -> Int
hello x y = annotateStackShow (x,y) $
  unsafeDecodeAndPrintAnnotationFrames $
    x + y + 42
{-# OPAQUE hello #-}

main :: IO ()
main = do
  print $ hello 2 3
  print $ tailCallEx 4 5

{-# INLINE tailCallEx #-}
tailCallEx :: Int -> Int -> Int
tailCallEx a b = annotateStackShow "tailCallEx" $ foo a b

{-# INLINE foo #-}
foo :: Int -> Int -> Int
foo a b = annotateStackShow "foo" $ bar $ a * b

bar c = annotateStackShow "bar" $
  unsafeDecodeAndPrintAnnotationFrames $
    c + unsafeDecodeAndPrintAnnotationFrames c

