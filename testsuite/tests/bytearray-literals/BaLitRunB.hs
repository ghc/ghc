{-# language MagicHash #-}
{-# language QuasiQuotes #-}

import BaLitCmplTH (ascii,asciiToByteArray#)
import GHC.Exts

main :: IO ()
main = do
  print (func (asciiToByteArray# "gadget"))
  print (func (asciiToByteArray# "foo"))
  print (func (asciiToByteArray# "baz"))
  print (func (asciiToByteArray# "widget"))
  print (func (asciiToByteArray# "bin"))
  print (func (asciiToByteArray# "bar"))
  print (func (asciiToByteArray# "exe"))
  print (func (asciiToByteArray# "oop"))
  print (func (asciiToByteArray# "narwal"))
  print (func (asciiToByteArray# "scone"))

func :: ByteArray# -> Int
func x = case x of
  [ascii|gadget|] -> 41
  [ascii|foo|] -> 42
  [ascii|baz|] -> 43
  [ascii|widget|] -> 44
  [ascii|bin|] -> 45
  [ascii|bar|] -> 46
  [ascii|exe|] -> 47
  _ -> 50

