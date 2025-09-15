{-# LANGUAGE ScopedTypeVariables #-}

module Main (main, one32) where

import Data.Int
import Data.Word
import Foreign.C.String
import Foreign.Ptr

{-# NOINLINE all64 #-}
all64 :: Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> IO ()
all64 = \p1 p2 p3 p4 p5 p6 p7 p8 -> do
    putStrLn "Callback with only 64-bit parameters..."
    putStrLn $ "P1: " ++ show p1
    putStrLn $ "P2: " ++ show p2
    putStrLn $ "P3: " ++ show p3
    putStrLn $ "P4: " ++ show p4
    putStrLn $ "P5: " ++ show p5
    putStrLn $ "P6: " ++ show p6
    putStrLn $ "P7: " ++ show p7
    putStrLn $ "P8: " ++ show p8

{-# NOINLINE one32 #-}
one32 :: One32
one32 = \p1 p2 p3 p4 p5 p6 p7 p8 -> do
    putStrLn "Callback with one 32-bit parameter and the rest 64-bit..."
    putStrLn $ "P1: " ++ show p1
    putStrLn $ "P2: " ++ show p2
    putStrLn $ "P3: " ++ show p3
    putStrLn $ "P4: " ++ show p4
    putStrLn $ "P5: " ++ show p5
    putStrLn $ "P6: " ++ show p6
    putStrLn $ "P7: " ++ show p7
    putStrLn $ "P8: " ++ show p8

{-# NOINLINE oneF #-}
oneF :: OneF
oneF = \p1 p2 p3 p4 p5 p6 p7 p8 -> do
    putStrLn "Callback with one float parameter and the rest 64-bit..."
    putStrLn $ "P1: " ++ show p1
    putStrLn $ "P2: " ++ show p2
    putStrLn $ "P3: " ++ show p3
    putStrLn $ "P4: " ++ show p4
    putStrLn $ "P5: " ++ show p5
    putStrLn $ "P6: " ++ show p6
    putStrLn $ "P7: " ++ show p7
    putStrLn $ "P8: " ++ show p8

two32 :: Two32
two32 = \p1 p2 p3 p4 p5 p6 p7 p8 -> do
    putStrLn "Callback with two 32-bit parameters and the rest 64-bit..."
    putStrLn $ "P1: " ++ show p1
    putStrLn $ "P2: " ++ show p2
    putStrLn $ "P3: " ++ show p3
    putStrLn $ "P4: " ++ show p4
    putStrLn $ "P5: " ++ show p5
    putStrLn $ "P6: " ++ show p6
    putStrLn $ "P7: " ++ show p7
    putStrLn $ "P8: " ++ show p8

allKinds :: AllKinds
allKinds = \p1 p2 p3 p4 p5 p6
            p11 p12 p13 p14 p15 p16
            p21 p22 p23 p24 p25 p26
            p31 p32 p33 p34 p35 p36 -> do
    putStrLn "Callback with all kinds of arguments"
    putStrLn $ show (p1, p2, p3, p4, p5, p6)
    putStrLn $ show (p11, p12, p13, p14, p15, p16)
    putStrLn $ show (p21, p22, p23, p24, p25, p26)
    putStrLn $ show (p31, p32, p33, p34, p35, p36)



main :: IO ()
main = do
  (all64Ptr :: FunPtr All64) <- wrapAll64 all64
  (one32Ptr  :: FunPtr One32) <- wrapOne32 one32
  (oneFPtr  :: FunPtr OneF) <- wrapOneF oneF
  (two32Ptr  :: FunPtr Two32) <- wrapTwo32 two32
  (allKindsPtr  :: FunPtr AllKinds) <- wrapAllKinds allKinds
  callMe all64Ptr one32Ptr oneFPtr two32Ptr allKindsPtr
  freeHaskellFunPtr all64Ptr
  freeHaskellFunPtr one32Ptr
  freeHaskellFunPtr oneFPtr
  freeHaskellFunPtr two32Ptr
  freeHaskellFunPtr allKindsPtr

type DynamicWrapper f = f -> IO (FunPtr f)
type All64 = Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> IO ()
type One32 = Word64 -> Word64 -> Word32 -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> IO ()
type OneF = Word64 -> Word64 -> Float -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> IO ()
type Two32 = Word64 -> Word32 -> Word32 -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> IO ()
type AllKinds = Word8 -> Word16 -> Word32 -> Word64 -> Float -> Double
                -> Word8 -> Word16 -> Word32 -> Word64 -> Float -> Double
                -> Word8 -> Word16 -> Word32 -> Word64 -> Float -> Double
                -> Word8 -> Word16 -> Word32 -> Word64 -> Float -> Double -> IO ()

foreign import ccall "callMe" callMe :: FunPtr All64 -> FunPtr One32 -> FunPtr OneF -> FunPtr Two32 -> FunPtr AllKinds -> IO ()
foreign import ccall "wrapper" wrapAll64 :: DynamicWrapper All64
foreign import ccall "wrapper" wrapOne32 :: DynamicWrapper One32
foreign import ccall "wrapper" wrapOneF :: DynamicWrapper OneF
foreign import ccall "wrapper" wrapTwo32 :: DynamicWrapper Two32
foreign import ccall "wrapper" wrapAllKinds :: DynamicWrapper AllKinds
