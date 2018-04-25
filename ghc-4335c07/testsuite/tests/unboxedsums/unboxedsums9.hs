{-# LANGUAGE UnboxedSums, UnboxedTuples, MagicHash #-}

module Main where

type UbxBool = (# (# #) | (# #) #)

{-# NOINLINE packBool #-}
packBool :: UbxBool -> Bool
packBool (# _ | #) = True
packBool (# | _ #) = False

{-# NOINLINE unpackBool #-}
unpackBool :: Bool -> UbxBool
unpackBool True  = (# (# #) | #)
unpackBool False = (# | (# #) #)

{-# NOINLINE showUbxBool #-}
showUbxBool :: UbxBool -> String
showUbxBool b = show (packBool b)

main :: IO ()
main = do
  putStrLn (showUbxBool (unpackBool True))
  putStrLn (showUbxBool (unpackBool False))
  putStrLn (show (packBool (# (# #) | #)))
  putStrLn (show (packBool (# | (# #) #)))
