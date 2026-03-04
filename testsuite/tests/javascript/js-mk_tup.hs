module Main where

import qualified GHC.Exts as Exts
import           Unsafe.Coerce

foreign import javascript "test_mk_tup2"  js_mk_tup2  :: Exts.Any -- (Int, Int)
foreign import javascript "test_mk_tup3"  js_mk_tup3  :: Exts.Any -- (Int, Int, Int)
foreign import javascript "test_mk_tup4"  js_mk_tup4  :: Exts.Any -- (Int, Int, Int, Int)
foreign import javascript "test_mk_tup5"  js_mk_tup5  :: Exts.Any -- (Int, Int, Int, Int, ...)
foreign import javascript "test_mk_tup6"  js_mk_tup6  :: Exts.Any -- (Int, Int, Int, Int, ...)
foreign import javascript "test_mk_tup7"  js_mk_tup7  :: Exts.Any -- (Int, Int, Int, Int, ...)
foreign import javascript "test_mk_tup8"  js_mk_tup8  :: Exts.Any -- (Int, Int, Int, Int, ...)
foreign import javascript "test_mk_tup9"  js_mk_tup9  :: Exts.Any -- (Int, Int, Int, Int, ...)
foreign import javascript "test_mk_tup10" js_mk_tup10 :: Exts.Any -- (Int, Int, Int, Int, ...)

mkTup2 :: (Int, Int)
mkTup2 = unsafeCoerce js_mk_tup2

mkTup3 :: (Int, Int, Int)
mkTup3 = unsafeCoerce js_mk_tup3

mkTup4 :: (Int, Int, Int, Int)
mkTup4 = unsafeCoerce js_mk_tup4

mkTup5 :: (Int, Int, Int, Int, Int)
mkTup5 = unsafeCoerce js_mk_tup5

mkTup6 :: (Int, Int, Int, Int, Int, Int)
mkTup6 = unsafeCoerce js_mk_tup6

mkTup7 :: (Int, Int, Int, Int, Int, Int, Int)
mkTup7 = unsafeCoerce js_mk_tup7

mkTup8 :: (Int, Int, Int, Int, Int, Int, Int, Int)
mkTup8 = unsafeCoerce js_mk_tup8

mkTup9 :: (Int, Int, Int, Int, Int, Int, Int, Int, Int)
mkTup9 = unsafeCoerce js_mk_tup9

mkTup10 :: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)
mkTup10 = unsafeCoerce js_mk_tup10

main :: IO ()
main = do
  putStr "mkTup2: "
  print mkTup2

  putStr "mkTup3: "
  print mkTup3

  putStr "mkTup4: "
  print mkTup4

  putStr "mkTup5: "
  print mkTup5

  putStr "mkTup6: "
  print mkTup6

  putStr "mkTup7: "
  print mkTup7

  putStr "mkTup8: "
  print mkTup8

  putStr "mkTup9: "
  print mkTup9

  putStr "mkTup10: "
  print mkTup10

