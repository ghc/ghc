{-# LANGUAGE MagicHash #-}

import GHC.Exts

{-# NOINLINE lt8 #-}
lt8 :: Int -> Word -> Int    -- ltWord8# 254 255: must be 1
lt8 (I# m) (W# n) = I# (ltWord8# (int8ToWord8# (intToInt8# m)) (wordToWord8# n))

{-# NOINLINE eq8 #-}
eq8 :: Int -> Word -> Int    -- eqWord8# 254 254: must be 1
eq8 (I# m) (W# n) = I# (eqWord8# (int8ToWord8# (intToInt8# m)) (wordToWord8# n))

{-# NOINLINE eqi16 #-}
eqi16 :: Int -> Int -> Int   -- eqInt16# (-2) (-2): must be 1
eqi16 (I# m) (I# n) = I# (eqInt16# (intToInt16# m) (word16ToInt16# (wordToWord16# (int2Word# n))))

{-# NOINLINE rem8 #-}
rem8 :: Int -> Word -> Word  -- remWord8# 254 100: must be 54
rem8 (I# m) (W# n) = W# (word8ToWord# (remWord8# (int8ToWord8# (intToInt8# m)) (wordToWord8# n)))

main :: IO ()
main = do
  print (lt8   (-2) 255)
  print (eq8   (-2) 254)
  print (eqi16 (-2) 65534)
  print (rem8  (-2) 100)
