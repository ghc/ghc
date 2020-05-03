{-# language MagicHash #-}
{-# language QuasiQuotes #-}

import BaLitCmplTH (ascii,asciiToByteArray#)
import GHC.Exts

main :: IO ()
main = do
  print (func (asciiToByteArray# "hello"))
  print (func (asciiToByteArray# "abcd"))
  print (func (asciiToByteArray# "foo"))
  print (func (asciiToByteArray# "ab"))
  print (func (asciiToByteArray# "baz"))

func :: ByteArray# -> Int
func x = case x of
  [ascii|hello|] -> 42
  [ascii|foo|] -> 44
  _ -> 45
