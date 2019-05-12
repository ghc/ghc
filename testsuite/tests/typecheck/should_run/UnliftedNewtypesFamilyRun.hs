{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

import GHC.Int (Int(I#))
import GHC.Word (Word(W#))
import GHC.Exts (Int#,Word#)
import GHC.Types

main :: IO ()
main = do
  print (method 5 (BarIntC 6#))
  print (method 13 (BarWordC 9#))

class Foo a where
  data Bar a :: TYPE 'IntRep
  method :: a -> Bar a -> a

instance Foo Int where
  newtype Bar Int = BarIntC Int#
  method x (BarIntC y) = x + I# y

instance Foo Word where
  newtype Bar Word = BarWordC Int#
  method x (BarWordC y) = x - fromIntegral (I# y)
