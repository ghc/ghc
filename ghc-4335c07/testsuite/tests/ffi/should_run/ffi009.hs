import Foreign
import System.Random

--------------------------------------------------------------------------------

type FunType5I = Int -> Int -> Int -> Int -> Int -> Int

foreign import ccall "dynamic" callFun5I :: FunPtr FunType5I -> FunType5I
foreign import ccall "wrapper" mkFun5I   :: FunType5I -> IO (FunPtr FunType5I)

manyArgs5I :: FunType5I
manyArgs5I a1 a2 a3 a4 a5 = (((a1 * 31 + a2) * 31 + a3) * 31 + a4) * 31 + a5

test5I :: IO ()
test5I = do
  a1 <- randomIO
  a2 <- randomIO
  a3 <- randomIO
  a4 <- randomIO
  a5 <- randomIO
  funAddr <- mkFun5I manyArgs5I
  print (callFun5I funAddr a1 a2 a3 a4 a5 ==
         manyArgs5I        a1 a2 a3 a4 a5)
  freeHaskellFunPtr funAddr

--------------------------------------------------------------------------------

type FunType11D = Double -> Double -> Double -> Double -> Double -> Double
               -> Double -> Double -> Double -> Double -> Double -> Double

foreign import ccall "dynamic" callFun11D :: FunPtr FunType11D -> FunType11D
foreign import ccall "wrapper" mkFun11D   :: FunType11D -> IO (FunPtr FunType11D)

manyArgs11D :: FunType11D
manyArgs11D a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 = 
  ((((a1 * 31 + a2) * 31 + a3) * 31 + a4) * 31 + a5) * 31 + a6
  + a7 + a8 + a9 + a10 + a11

test11D :: IO ()
test11D = do
  a1 <- randomIO
  a2 <- randomIO
  a3 <- randomIO
  a4 <- randomIO
  a5 <- randomIO
  a6 <- randomIO
  a7 <- randomIO
  a8 <- randomIO
  a9 <- randomIO
  a10 <- randomIO
  a11 <- randomIO
  funAddr <- mkFun11D manyArgs11D
  let x = callFun11D funAddr a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11
      y = manyArgs11D        a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 
  if x /= y then
        print x >> print y
     else
       print True
  freeHaskellFunPtr funAddr

--------------------------------------------------------------------------------

type FunType11M = Int -> Double -> Float -> Char -> Bool -> Int -> Float -> Int
               -> Char -> Double -> Bool -> Double

foreign import ccall "dynamic" callFun11M :: FunPtr FunType11M -> FunType11M
foreign import ccall "wrapper" mkFun11M   :: FunType11M -> IO (FunPtr FunType11M)

manyArgs11M :: FunType11M
manyArgs11M a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 =
   (((((((((fromIntegral           a1   * 31 +                         a2)  * 31 +
            realToFrac             a3)  * 31 + fromIntegral (fromEnum  a4)) * 31 +
            fromIntegral (fromEnum a5)) * 31 + fromIntegral            a6)  * 31 +
            realToFrac             a7)  * 31 + fromIntegral            a8)  * 31 +
            fromIntegral (fromEnum a9)) * 31 +                         a10) * 31 +
            fromIntegral (fromEnum a11)

test11M :: IO ()
test11M = do
  a1  <- randomIO
  a2  <- randomIO
  a3  <- randomIO
  a4  <- randomIO
  a5  <- randomIO
  a6  <- randomIO
  a7  <- randomIO
  a8  <- randomIO
  a9  <- randomIO
  a10 <- randomIO
  a11 <- randomIO
  funAddr <- mkFun11M manyArgs11M
  print (callFun11M funAddr a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 ==
         manyArgs11M        a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11)
  freeHaskellFunPtr funAddr

--------------------------------------------------------------------------------

type FunTypeM1 = Double -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
              -> Int -> Int -> Int -> Double

foreign import ccall "dynamic" callFunM1 :: FunPtr FunTypeM1 -> FunTypeM1
foreign import ccall "wrapper" mkFunM1   :: FunTypeM1 -> IO (FunPtr FunTypeM1)

manyArgsM1 :: FunTypeM1
manyArgsM1 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 =
   ((((((((((              a1  * 31 + fromIntegral  a2) * 31 +
             fromIntegral  a3) * 31 + fromIntegral  a4) * 31 +
             fromIntegral  a5) * 31 + fromIntegral  a6) * 31 +
             fromIntegral  a7) * 31 + fromIntegral  a8) * 31 +
             fromIntegral  a9) * 31 + fromIntegral a10) * 31 +
             fromIntegral a11) * 31 + fromIntegral a12

testM1 :: IO ()
testM1 = do
  a1  <- randomIO
  a2  <- randomIO
  a3  <- randomIO
  a4  <- randomIO
  a5  <- randomIO
  a6  <- randomIO
  a7  <- randomIO
  a8  <- randomIO
  a9  <- randomIO
  a10 <- randomIO
  a11 <- randomIO
  a12 <- randomIO
  funAddr <- mkFunM1 manyArgsM1
  print (callFunM1 funAddr a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 ==
         manyArgsM1        a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12)
  freeHaskellFunPtr funAddr

--------------------------------------------------------------------------------

type FunTypeM2 = Int -> Double -> Int -> Int -> Int -> Int -> Int -> Int -> Int
              -> Int -> Int -> Int -> Double

foreign import ccall "dynamic" callFunM2 :: FunPtr FunTypeM2 -> FunTypeM2
foreign import ccall "wrapper" mkFunM2   :: FunTypeM2 -> IO (FunPtr FunTypeM2)

manyArgsM2 :: FunTypeM2
manyArgsM2 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 =
   ((((((((((fromIntegral  a1  * 31 +               a2) * 31 +
             fromIntegral  a3) * 31 + fromIntegral  a4) * 31 +
             fromIntegral  a5) * 31 + fromIntegral  a6) * 31 +
             fromIntegral  a7) * 31 + fromIntegral  a8) * 31 +
             fromIntegral  a9) * 31 + fromIntegral a10) * 31 +
             fromIntegral a11) * 31 + fromIntegral a12

testM2 :: IO ()
testM2 = do
  a1  <- randomIO
  a2  <- randomIO
  a3  <- randomIO
  a4  <- randomIO
  a5  <- randomIO
  a6  <- randomIO
  a7  <- randomIO
  a8  <- randomIO
  a9  <- randomIO
  a10 <- randomIO
  a11 <- randomIO
  a12 <- randomIO
  funAddr <- mkFunM2 manyArgsM2
  print (callFunM2 funAddr a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 ==
         manyArgsM2        a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12)
  freeHaskellFunPtr funAddr

--------------------------------------------------------------------------------

type FunTypeM3 = Int -> Int -> Double -> Int -> Int -> Int -> Int -> Int -> Int
              -> Int -> Int -> Int -> Double

foreign import ccall "dynamic" callFunM3 :: FunPtr FunTypeM3 -> FunTypeM3
foreign import ccall "wrapper" mkFunM3   :: FunTypeM3 -> IO (FunPtr FunTypeM3)

manyArgsM3 :: FunTypeM3
manyArgsM3 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 =
   ((((((((((fromIntegral  a1  * 31 + fromIntegral  a2) * 31 +
                           a3) * 31 + fromIntegral  a4) * 31 +
             fromIntegral  a5) * 31 + fromIntegral  a6) * 31 +
             fromIntegral  a7) * 31 + fromIntegral  a8) * 31 +
             fromIntegral  a9) * 31 + fromIntegral a10) * 31 +
             fromIntegral a11) * 31 + fromIntegral a12

testM3 :: IO ()
testM3 = do
  a1  <- randomIO
  a2  <- randomIO
  a3  <- randomIO
  a4  <- randomIO
  a5  <- randomIO
  a6  <- randomIO
  a7  <- randomIO
  a8  <- randomIO
  a9  <- randomIO
  a10 <- randomIO
  a11 <- randomIO
  a12 <- randomIO
  funAddr <- mkFunM3 manyArgsM3
  print (callFunM3 funAddr a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 ==
         manyArgsM3        a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12)
  freeHaskellFunPtr funAddr

--------------------------------------------------------------------------------

type FunTypeM4 = Int -> Int -> Int -> Double -> Int -> Int -> Int -> Int -> Int
              -> Int -> Int -> Int -> Double

foreign import ccall "dynamic" callFunM4 :: FunPtr FunTypeM4 -> FunTypeM4
foreign import ccall "wrapper" mkFunM4   :: FunTypeM4 -> IO (FunPtr FunTypeM4)

manyArgsM4 :: FunTypeM4
manyArgsM4 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 =
   ((((((((((fromIntegral  a1  * 31 + fromIntegral  a2) * 31 +
             fromIntegral  a3) * 31 +               a4) * 31 +
             fromIntegral  a5) * 31 + fromIntegral  a6) * 31 +
             fromIntegral  a7) * 31 + fromIntegral  a8) * 31 +
             fromIntegral  a9) * 31 + fromIntegral a10) * 31 +
             fromIntegral a11) * 31 + fromIntegral a12

testM4 :: IO ()
testM4 = do
  a1  <- randomIO
  a2  <- randomIO
  a3  <- randomIO
  a4  <- randomIO
  a5  <- randomIO
  a6  <- randomIO
  a7  <- randomIO
  a8  <- randomIO
  a9  <- randomIO
  a10 <- randomIO
  a11 <- randomIO
  a12 <- randomIO
  funAddr <- mkFunM4 manyArgsM4
  print (callFunM4 funAddr a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 ==
         manyArgsM4        a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12)
  freeHaskellFunPtr funAddr

--------------------------------------------------------------------------------

type FunTypeM5 = Int -> Int -> Int -> Int -> Double -> Int -> Int -> Int -> Int
              -> Int -> Int -> Int -> Double

foreign import ccall "dynamic" callFunM5 :: FunPtr FunTypeM5 -> FunTypeM5
foreign import ccall "wrapper" mkFunM5   :: FunTypeM5 -> IO (FunPtr FunTypeM5)

manyArgsM5 :: FunTypeM5
manyArgsM5 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 =
   ((((((((((fromIntegral  a1  * 31 + fromIntegral  a2) * 31 +
             fromIntegral  a3) * 31 + fromIntegral  a4) * 31 +
                           a5) * 31 + fromIntegral  a6) * 31 +
             fromIntegral  a7) * 31 + fromIntegral  a8) * 31 +
             fromIntegral  a9) * 31 + fromIntegral a10) * 31 +
             fromIntegral a11) * 31 + fromIntegral a12

testM5 :: IO ()
testM5 = do
  a1  <- randomIO
  a2  <- randomIO
  a3  <- randomIO
  a4  <- randomIO
  a5  <- randomIO
  a6  <- randomIO
  a7  <- randomIO
  a8  <- randomIO
  a9  <- randomIO
  a10 <- randomIO
  a11 <- randomIO
  a12 <- randomIO
  funAddr <- mkFunM5 manyArgsM5
  print (callFunM5 funAddr a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 ==
         manyArgsM5        a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12)
  freeHaskellFunPtr funAddr

--------------------------------------------------------------------------------

type FunTypeM6 = Int -> Int -> Int -> Int -> Int -> Double -> Int -> Int -> Int
              -> Int -> Int -> Int -> Double

foreign import ccall "dynamic" callFunM6 :: FunPtr FunTypeM6 -> FunTypeM6
foreign import ccall "wrapper" mkFunM6   :: FunTypeM6 -> IO (FunPtr FunTypeM6)

manyArgsM6 :: FunTypeM6
manyArgsM6 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 =
   ((((((((((fromIntegral  a1  * 31 + fromIntegral  a2) * 31 +
             fromIntegral  a3) * 31 + fromIntegral  a4) * 31 +
             fromIntegral  a5) * 31 +               a6) * 31 +
             fromIntegral  a7) * 31 + fromIntegral  a8) * 31 +
             fromIntegral  a9) * 31 + fromIntegral a10) * 31 +
             fromIntegral a11) * 31 + fromIntegral a12

testM6 :: IO ()
testM6 = do
  a1  <- randomIO
  a2  <- randomIO
  a3  <- randomIO
  a4  <- randomIO
  a5  <- randomIO
  a6  <- randomIO
  a7  <- randomIO
  a8  <- randomIO
  a9  <- randomIO
  a10 <- randomIO
  a11 <- randomIO
  a12 <- randomIO
  funAddr <- mkFunM6 manyArgsM6
  print (callFunM6 funAddr a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 ==
         manyArgsM6        a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12)
  freeHaskellFunPtr funAddr

--------------------------------------------------------------------------------

type FunTypeM7 = Int -> Int -> Int -> Int -> Int -> Int -> Double -> Int -> Int
              -> Int -> Int -> Int -> Double

foreign import ccall "dynamic" callFunM7 :: FunPtr FunTypeM7 -> FunTypeM7
foreign import ccall "wrapper" mkFunM7   :: FunTypeM7 -> IO (FunPtr FunTypeM7)

manyArgsM7 :: FunTypeM7
manyArgsM7 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 =
   ((((((((((fromIntegral  a1  * 31 + fromIntegral  a2) * 31 +
             fromIntegral  a3) * 31 + fromIntegral  a4) * 31 +
             fromIntegral  a5) * 31 + fromIntegral  a6) * 31 +
                           a7) * 31 + fromIntegral  a8) * 31 +
             fromIntegral  a9) * 31 + fromIntegral a10) * 31 +
             fromIntegral a11) * 31 + fromIntegral a12

testM7 :: IO ()
testM7 = do
  a1  <- randomIO
  a2  <- randomIO
  a3  <- randomIO
  a4  <- randomIO
  a5  <- randomIO
  a6  <- randomIO
  a7  <- randomIO
  a8  <- randomIO
  a9  <- randomIO
  a10 <- randomIO
  a11 <- randomIO
  a12 <- randomIO
  funAddr <- mkFunM7 manyArgsM7
  print (callFunM7 funAddr a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 ==
         manyArgsM7        a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12)
  freeHaskellFunPtr funAddr

--------------------------------------------------------------------------------

type FunTypeM8 = Int -> Int -> Int -> Int -> Int -> Int -> Int -> Double -> Int
              -> Int -> Int -> Int -> Double

foreign import ccall "dynamic" callFunM8 :: FunPtr FunTypeM8 -> FunTypeM8
foreign import ccall "wrapper" mkFunM8   :: FunTypeM8 -> IO (FunPtr FunTypeM8)

manyArgsM8 :: FunTypeM8
manyArgsM8 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 =
   ((((((((((fromIntegral  a1  * 31 + fromIntegral  a2) * 31 +
             fromIntegral  a3) * 31 + fromIntegral  a4) * 31 +
             fromIntegral  a5) * 31 + fromIntegral  a6) * 31 +
             fromIntegral  a7) * 31 +               a8) * 31 +
             fromIntegral  a9) * 31 + fromIntegral a10) * 31 +
             fromIntegral a11) * 31 + fromIntegral a12

testM8 :: IO ()
testM8 = do
  a1  <- randomIO
  a2  <- randomIO
  a3  <- randomIO
  a4  <- randomIO
  a5  <- randomIO
  a6  <- randomIO
  a7  <- randomIO
  a8  <- randomIO
  a9  <- randomIO
  a10 <- randomIO
  a11 <- randomIO
  a12 <- randomIO
  funAddr <- mkFunM8 manyArgsM8
  print (callFunM8 funAddr a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 ==
         manyArgsM8        a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12)
  freeHaskellFunPtr funAddr

--------------------------------------------------------------------------------

type FunTypeM9 = Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Double
              -> Int -> Int -> Int -> Double

foreign import ccall "dynamic" callFunM9 :: FunPtr FunTypeM9 -> FunTypeM9
foreign import ccall "wrapper" mkFunM9   :: FunTypeM9 -> IO (FunPtr FunTypeM9)

manyArgsM9 :: FunTypeM9
manyArgsM9 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 =
   ((((((((((fromIntegral  a1  * 31 + fromIntegral  a2) * 31 +
             fromIntegral  a3) * 31 + fromIntegral  a4) * 31 +
             fromIntegral  a5) * 31 + fromIntegral  a6) * 31 +
             fromIntegral  a7) * 31 + fromIntegral  a8) * 31 +
                           a9) * 31 + fromIntegral a10) * 31 +
             fromIntegral a11) * 31 + fromIntegral a12

testM9 :: IO ()
testM9 = do
  a1  <- randomIO
  a2  <- randomIO
  a3  <- randomIO
  a4  <- randomIO
  a5  <- randomIO
  a6  <- randomIO
  a7  <- randomIO
  a8  <- randomIO
  a9  <- randomIO
  a10 <- randomIO
  a11 <- randomIO
  a12 <- randomIO
  funAddr <- mkFunM9 manyArgsM9
  print (callFunM9 funAddr a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 ==
         manyArgsM9        a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12)
  freeHaskellFunPtr funAddr

--------------------------------------------------------------------------------

type FunTypeM10 = Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
               -> Double -> Int -> Int -> Double

foreign import ccall "dynamic" callFunM10 :: FunPtr FunTypeM10 -> FunTypeM10
foreign import ccall "wrapper" mkFunM10   :: FunTypeM10 -> IO (FunPtr FunTypeM10)

manyArgsM10 :: FunTypeM10
manyArgsM10 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 =
   ((((((((((fromIntegral  a1  * 31 + fromIntegral  a2) * 31 +
             fromIntegral  a3) * 31 + fromIntegral  a4) * 31 +
             fromIntegral  a5) * 31 + fromIntegral  a6) * 31 +
             fromIntegral  a7) * 31 + fromIntegral  a8) * 31 +
             fromIntegral  a9) * 31 +              a10) * 31 +
             fromIntegral a11) * 31 + fromIntegral a12

testM10 :: IO ()
testM10 = do
  a1  <- randomIO
  a2  <- randomIO
  a3  <- randomIO
  a4  <- randomIO
  a5  <- randomIO
  a6  <- randomIO
  a7  <- randomIO
  a8  <- randomIO
  a9  <- randomIO
  a10 <- randomIO
  a11 <- randomIO
  a12 <- randomIO
  funAddr <- mkFunM10 manyArgsM10
  print (callFunM10 funAddr a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 ==
         manyArgsM10        a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12)
  freeHaskellFunPtr funAddr

--------------------------------------------------------------------------------

type FunTypeM11 = Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
               -> Int -> Double -> Int -> Double

foreign import ccall "dynamic" callFunM11 :: FunPtr FunTypeM11 -> FunTypeM11
foreign import ccall "wrapper" mkFunM11   :: FunTypeM11 -> IO (FunPtr FunTypeM11)

manyArgsM11 :: FunTypeM11
manyArgsM11 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 =
   ((((((((((fromIntegral  a1  * 31 + fromIntegral  a2) * 31 +
             fromIntegral  a3) * 31 + fromIntegral  a4) * 31 +
             fromIntegral  a5) * 31 + fromIntegral  a6) * 31 +
             fromIntegral  a7) * 31 + fromIntegral  a8) * 31 +
             fromIntegral  a9) * 31 + fromIntegral a10) * 31 +
                          a11) * 31 + fromIntegral a12

testM11 :: IO ()
testM11 = do
  a1  <- randomIO
  a2  <- randomIO
  a3  <- randomIO
  a4  <- randomIO
  a5  <- randomIO
  a6  <- randomIO
  a7  <- randomIO
  a8  <- randomIO
  a9  <- randomIO
  a10 <- randomIO
  a11 <- randomIO
  a12 <- randomIO
  funAddr <- mkFunM11 manyArgsM11
  print (callFunM11 funAddr a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 ==
         manyArgsM11        a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12)
  freeHaskellFunPtr funAddr

--------------------------------------------------------------------------------

type FunTypeM12 = Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
               -> Int -> Int -> Double -> Double

foreign import ccall "dynamic" callFunM12 :: FunPtr FunTypeM12 -> FunTypeM12
foreign import ccall "wrapper" mkFunM12   :: FunTypeM12 -> IO (FunPtr FunTypeM12)

manyArgsM12 :: FunTypeM12
manyArgsM12 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 =
   ((((((((((fromIntegral  a1  * 31 + fromIntegral  a2) * 31 +
             fromIntegral  a3) * 31 + fromIntegral  a4) * 31 +
             fromIntegral  a5) * 31 + fromIntegral  a6) * 31 +
             fromIntegral  a7) * 31 + fromIntegral  a8) * 31 +
             fromIntegral  a9) * 31 + fromIntegral a10) * 31 +
             fromIntegral a11) * 31 +              a12

testM12 :: IO ()
testM12 = do
  a1  <- randomIO
  a2  <- randomIO
  a3  <- randomIO
  a4  <- randomIO
  a5  <- randomIO
  a6  <- randomIO
  a7  <- randomIO
  a8  <- randomIO
  a9  <- randomIO
  a10 <- randomIO
  a11 <- randomIO
  a12 <- randomIO
  funAddr <- mkFunM12 manyArgsM12
  print (callFunM12 funAddr a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 ==
         manyArgsM12        a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12)
  freeHaskellFunPtr funAddr

--------------------------------------------------------------------------------

rep :: String -> IO () -> IO ()
rep msg tst = do
   putStrLn ("Testing " ++ msg ++ "...")
   sequence_ (replicate 10 tst)

main :: IO ()
main = do
  setStdGen (mkStdGen 4711)
  rep "5 Int arguments" test5I
  rep "11 Double arguments" test11D
  rep "11 mixed arguments" test11M
  rep "Double as 1st argument, rest Int" testM1
  rep "Double as 2nd argument, rest Int" testM2
  rep "Double as 3rd argument, rest Int" testM3
  rep "Double as 4th argument, rest Int" testM4
  rep "Double as 5th argument, rest Int" testM5
  rep "Double as 6th argument, rest Int" testM6
  rep "Double as 7th argument, rest Int" testM7
  rep "Double as 8th argument, rest Int" testM8
  rep "Double as 9th argument, rest Int" testM9
  rep "Double as 10th argument, rest Int" testM10
  rep "Double as 11th argument, rest Int" testM11
  rep "Double as 12th argument, rest Int" testM12
