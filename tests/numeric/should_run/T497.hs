import Data.Word

-- #497: using -O should not make these == 1
main = do
  print $ q * 2 + 1
  print $ q' * 2 + 1

q :: Word32
q = 0x7FFFFFFF

q' :: Word64
q' = 0x7FFFFFFFFFFFFFFF
