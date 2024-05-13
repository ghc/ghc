{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -msse #-}
{-# OPTIONS_GHC -msse2 #-}
{-# OPTIONS_GHC -msse4 #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}

-- QuickCheck testing for SIMD operations

module Main
    ( main
    ) where

import Data.Word
import Data.Int
import GHC.Natural
import Data.Coerce
import Data.Typeable
import Data.Proxy
import GHC.Int
import GHC.Word
import Data.Function
import GHC.Prim
import Control.Monad.Reader
import System.IO
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Ptr
import Data.List (intercalate)
import Data.IORef
import Unsafe.Coerce
import GHC.Exts
import GHC.Float
  ( castFloatToWord32 , castWord32ToFloat
  , castDoubleToWord64, castWord64ToDouble
  )


#include "MachDeps.h"

newtype Gen a = Gen { runGen :: (ReaderT LCGGen IO a) }
  deriving newtype (Functor, Applicative, Monad)

class Arbitrary a where
  arbitrary :: Gen a

class IsProperty p where
    property :: p -> Property

data PropertyCheck = PropertyBinaryOp Bool String String String
                   | PropertyAnd PropertyCheck PropertyCheck

instance IsProperty PropertyCheck where
    property check = Prop $ pure (PropertyEOA check)

data PropertyTestArg = PropertyEOA PropertyCheck
                     | PropertyArg String PropertyTestArg

getCheck :: PropertyTestArg -> ([String], PropertyCheck)
getCheck (PropertyEOA pc) = ([], pc)
getCheck (PropertyArg s pta ) = let (ss, pc) = getCheck pta in (s:ss, pc)

data Property = Prop { unProp :: Gen PropertyTestArg }

instance (Show a, Arbitrary a, IsProperty prop) => IsProperty (a -> prop) where
    property p = forAll arbitrary p

-- | Running a generator for a specific type under a property
forAll :: (Show a, IsProperty prop) => Gen a -> (a -> prop) -> Property
forAll generator tst = Prop $ do
    a <- generator
    augment a <$> unProp (property (tst a))
  where
    augment a arg = PropertyArg (show a) arg

-- | A property that check for equality of its 2 members.
propertyCompare :: (Show a) => String -> (a -> a -> Bool) -> a -> a -> PropertyCheck
propertyCompare s f a b =
    let sa = show a
        sb = show b
     in PropertyBinaryOp (a `f` b) s sa sb

(===) :: (Show a, Eq a) => a -> a -> PropertyCheck
(===) = propertyCompare "==" (==)
infix 4 ===

propertyAnd = PropertyAnd


data Test where
  Group :: String -> [Test] -> Test
  Property :: IsProperty prop => String -> prop -> Test


arbitraryInt64 :: Gen Int64
arbitraryInt64 = Gen $ do
    h <- ask
    W64# w <- liftIO (randomWord64 h)
    return (I64# (unsafeCoerce# w))

integralDownsize :: (Integral a) => Int64 -> a
integralDownsize = fromIntegral

wordDownsize :: (Integral a) => Word64 -> a
wordDownsize = fromIntegral

arbitraryWord64 :: Gen Word64
arbitraryWord64 = Gen $ do
    h <- ask
    liftIO (randomWord64 h)


instance Arbitrary Word64 where
    arbitrary = arbitraryWord64
instance Arbitrary Word32 where
    arbitrary = wordDownsize <$> arbitraryWord64

newtype FloatNT = FloatNT Float
  deriving newtype (Show, Num)
instance Eq FloatNT where
  FloatNT f1 == FloatNT f2 =
    castFloatToWord32 f1 == castFloatToWord32 f2
instance Arbitrary FloatNT where
  arbitrary = FloatNT . castWord32ToFloat <$> arbitrary
newtype DoubleNT = DoubleNT Double
  deriving newtype (Show, Num)
instance Eq DoubleNT where
  DoubleNT d1 == DoubleNT d2 =
    castDoubleToWord64 d1 == castDoubleToWord64 d2
instance Arbitrary DoubleNT where
  arbitrary = DoubleNT . castWord64ToDouble <$> arbitrary


data FloatX4 = FX4# FloatX4#
instance Show FloatX4 where
  show (FX4# f) = case (unpackFloatX4# f) of
    (# a, b, c, d #) -> show ((F# a), (F# b), (F# c), (F# d))
instance Eq FloatX4 where
  (FX4# a) == (FX4# b)
    = case (unpackFloatX4# a) of
        (# a1, a2, a3, a4 #) ->
          case (unpackFloatX4# b) of
            (# b1, b2, b3, b4 #) -> FloatNT (F# a1) == FloatNT (F# b1) &&
                                    FloatNT (F# a2) == FloatNT (F# b2) &&
                                    FloatNT (F# a3) == FloatNT (F# b3) &&
                                    FloatNT (F# a4) == FloatNT (F# b4)
instance Arbitrary FloatX4 where
  arbitrary = do
    FloatNT (F# f1) <- arbitrary
    FloatNT (F# f2) <- arbitrary
    FloatNT (F# f3) <- arbitrary
    FloatNT (F# f4) <- arbitrary
    return $ FX4# (packFloatX4# (# f1, f2, f3, f4 #))
instance Num FloatX4 where
  FX4# x + FX4# y =
    FX4# ( x `plusFloatX4#` y )
  FX4# x - FX4# y =
    FX4# ( x `minusFloatX4#` y )
  negate ( FX4# x ) = FX4# ( negateFloatX4# x )
  FX4# x * FX4# y =
    FX4# ( x `timesFloatX4#` y )
  abs = error "no"
  signum = error "no"
  fromInteger = error "no"

data DoubleX2 = DX2# DoubleX2#
instance Show DoubleX2 where
  show (DX2# d) = case (unpackDoubleX2# d) of
    (# a, b #) -> show ((D# a), (D# b))
instance Eq DoubleX2 where
  (DX2# a) == (DX2# b)
    = case (unpackDoubleX2# a) of
        (# a1, a2 #) ->
          case (unpackDoubleX2# b) of
            (# b1, b2 #) -> DoubleNT (D# a1) == DoubleNT (D# b1) &&
                            DoubleNT (D# a2) == DoubleNT (D# b2)
instance Arbitrary DoubleX2 where
  arbitrary = do
    DoubleNT (D# d1) <- arbitrary
    DoubleNT (D# d2) <- arbitrary
    return $ DX2# (packDoubleX2# (# d1, d2 #))
instance Num DoubleX2 where
  DX2# x + DX2# y =
    DX2# ( x `plusDoubleX2#` y )
  DX2# x - DX2# y =
    DX2# ( x `minusDoubleX2#` y )
  negate ( DX2# x ) = DX2# ( negateDoubleX2# x )
  DX2# x * DX2# y =
    DX2# ( x `timesDoubleX2#` y )
  abs = error "no"
  signum = error "no"
  fromInteger = error "no"

data Expr a where
  Lit :: a -> Expr a
  Add :: Expr a -> Expr a -> Expr a
  Sub :: Expr a -> Expr a -> Expr a
  Neg :: Expr a -> Expr a
  Mul :: Expr a -> Expr a -> Expr a
  deriving (Show, Eq)
fmapExpr :: (a -> b) -> Expr a -> Expr b
fmapExpr f (Lit a) = Lit (f a)
fmapExpr f (Add a b) = Add (fmapExpr f a) (fmapExpr f b)
fmapExpr f (Sub a b) = Sub (fmapExpr f a) (fmapExpr f b)
fmapExpr f (Neg a) = Neg (fmapExpr f a)
fmapExpr f (Mul a b) = Mul (fmapExpr f a) (fmapExpr f b)

instance Arbitrary a => Arbitrary (Expr a) where
  arbitrary = do
    (tag :: Word64) <- arbitrary
    case tag `mod` 20 of
      1 -> Add <$> arbitrary <*> arbitrary
      2 -> Sub <$> arbitrary <*> arbitrary
      3 -> Neg <$> arbitrary
      4 -> Mul <$> arbitrary <*> arbitrary
      _ -> Lit <$> arbitrary

eval :: Num a => Expr a -> a
eval (Lit a) = a
eval (Add a b) = eval a + eval b
eval (Sub a b) = eval a - eval b
eval (Neg a) = negate (eval a)
eval (Mul a b) = eval a * eval b


int64ToInt :: Int64 -> Int
#if WORD_SIZE_IN_BITS == 64
#if __GLASGOW_HASKELL__ >= 904
int64ToInt (I64# i) = I# (int64ToInt# i)
#else
int64ToInt (I64# i) = I# i
#endif
#else
int64ToInt (I64# i) = I# (int64ToInt# i)
#endif


word64ToWord :: Word64 -> Word
#if WORD_SIZE_IN_BITS == 64
#if __GLASGOW_HASKELL__ >= 904
word64ToWord (W64# i) = W# (GHC.Prim.word64ToWord# i)
#else
word64ToWord (W64# i) = W# i
#endif
#else
word64ToWord (W64# i) = W# (word64ToWord# i)
#endif


data RunS = RunS { depth :: Int, rg :: LCGGen  }

newtype LCGGen = LCGGen { randomWord64 :: IO Word64 }

data LCGParams = LCGParams { seed :: Word64, a :: Word64, c :: Word64, m :: Word64 }

newLCGGen :: LCGParams -> IO LCGGen
newLCGGen LCGParams{..}  = do
  var <- newIORef (fromIntegral seed)
  return $ LCGGen $ do
    atomicModifyIORef' var (\old_v -> let new_val = (old_v * a + c) `mod` m in (new_val, new_val))


runPropertyCheck (PropertyBinaryOp res desc s1 s2) =
  if res then return True else (putMsg ("Failure: " ++ s1 ++ desc ++ s2) >> return False)
runPropertyCheck (PropertyAnd a1 a2) = (&&) <$> runPropertyCheck a1 <*> runPropertyCheck a2

runProperty :: Property -> ReaderT RunS IO ()
runProperty (Prop p) = do
  let iterations = 100
  loop iterations iterations
  where
    loop iterations 0 = putMsg ("Passed " ++ show iterations ++ " iterations")
    loop iterations n = do
      h <- rg <$> ask
      p <- liftIO (runReaderT (runGen p) h)
      let (ss, pc) = getCheck p
      res <- runPropertyCheck pc
      if res then loop iterations (n-1)
             else putMsg ("With arguments " ++ intercalate ", " ss)

putMsg s = do
  n <- depth <$> ask
  liftIO . putStrLn $ replicate (n * 2) ' ' ++ s

nest = local (\s -> s { depth = depth s + 1 })

runTestInternal :: Test -> ReaderT RunS IO ()
runTestInternal (Group name tests) = do
  putMsg ("Group " ++ name)
  nest (mapM_ runTestInternal tests)
runTestInternal (Property name p) = do
  putMsg ("Running " ++ name)
  nest $ runProperty (property p)


runTests :: Test -> IO ()
runTests t = do
  -- These params are the same ones as glibc uses.
  h <- newLCGGen (LCGParams { seed = 1238123213, m = 2^31, a = 1103515245, c = 12345 })
  runReaderT  (runTestInternal t) (RunS 0 h)

-------------------------------------------------------------------------------

testFloatX4 :: Test
testFloatX4 = Group "FloatX4"
  [ Property "FloatX4" $ \ ( expr :: Expr FloatX4 ) ->
      unpack ( eval expr ) ===
        ( eval ( fmapExpr get1 expr )
        , eval ( fmapExpr get2 expr )
        , eval ( fmapExpr get3 expr )
        , eval ( fmapExpr get4 expr )
        )
  ]
  where
    unpack :: FloatX4 -> ( FloatNT, FloatNT, FloatNT, FloatNT )
    unpack (FX4# f) = case unpackFloatX4# f of
      (# f1, f2, f3, f4 #) -> coerce ( F# f1, F# f2, F# f3, F# f4 )
    get1, get2, get3, get4 :: FloatX4 -> FloatNT
    get1 (FX4# f) = case unpackFloatX4# f of
      (# f1, _, _, _ #) -> FloatNT (F# f1)
    get2 (FX4# f) = case unpackFloatX4# f of
      (# _, f2, _, _ #) -> FloatNT (F# f2)
    get3 (FX4# f) = case unpackFloatX4# f of
      (# _, _, f3, _ #) -> FloatNT (F# f3)
    get4 (FX4# f) = case unpackFloatX4# f of
      (# _, _, _, f4 #) -> FloatNT (F# f4)

testDoubleX2 :: Test
testDoubleX2 = Group "DoubleX2"
  [ Property "DoubleX2" $ \ ( expr :: Expr DoubleX2 ) ->
      unpack ( eval expr ) ===
        ( eval ( fmapExpr get1 expr )
        , eval ( fmapExpr get2 expr )
        )
  ]
  where
    unpack :: DoubleX2 -> ( DoubleNT, DoubleNT )
    unpack (DX2# d) = case unpackDoubleX2# d of
      (# d1, d2 #) -> coerce ( D# d1, D# d2 )
    get1, get2 :: DoubleX2 -> DoubleNT
    get1 (DX2# d) = case unpackDoubleX2# d of
      (# d1, _ #) -> DoubleNT (D# d1)
    get2 (DX2# d) = case unpackDoubleX2# d of
      (# _, d2 #) -> DoubleNT (D# d2)

testSIMD :: Test
testSIMD = Group "ALL"
    [ testFloatX4
    , testDoubleX2
    ]

main = runTests testSIMD
