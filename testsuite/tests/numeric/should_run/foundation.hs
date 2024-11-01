{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
module Main
    ( main
    ) where

import Data.Word
import Data.Int
import GHC.Natural
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

instance Arbitrary Natural where
    arbitrary = integralDownsize . (`mod` 10000) . abs <$> arbitraryInt64

-- Bounded by Int64
instance Arbitrary Integer where
    arbitrary = fromIntegral <$> arbitraryInt64

instance Arbitrary Int where
    arbitrary = int64ToInt <$> arbitraryInt64
instance Arbitrary Word where
    arbitrary = word64ToWord <$> arbitraryWord64
instance Arbitrary Word64 where
    arbitrary = arbitraryWord64
instance Arbitrary Word32 where
    arbitrary = wordDownsize <$> arbitraryWord64
instance Arbitrary Word16 where
    arbitrary = wordDownsize <$> arbitraryWord64
instance Arbitrary Word8 where
    arbitrary = wordDownsize <$> arbitraryWord64
instance Arbitrary Int64 where
    arbitrary = arbitraryInt64
instance Arbitrary Int32 where
    arbitrary = integralDownsize <$> arbitraryInt64
instance Arbitrary Int16 where
    arbitrary = integralDownsize <$> arbitraryInt64
instance Arbitrary Int8 where
    arbitrary = integralDownsize <$> arbitraryInt64

int64ToInt :: Int64 -> Int
int64ToInt (I64# i) = I# (int64ToInt# i)


word64ToWord :: Word64 -> Word
word64ToWord (W64# i) = W# (word64ToWord# i)


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

testIntegral :: forall a . (Arbitrary a, Show a, Integral a, Typeable a)
             => Proxy a -> Test
testIntegral _ = Group "Integral"
    [ Property "FromIntegral(Integer(a)) == a" $ \(a :: a) -> fromInteger (toInteger a) === a
    ]

testEqOrd :: forall a . (Arbitrary a, Show a, Eq a, Ord a, Integral a, Typeable a)
          => Proxy a -> Test
testEqOrd _ = Group "Property"
    [ Property "Eq" $ \(a :: a) -> a === a
    -- , Property "Ne" $ \(a :: a) (b :: a) -> if a === w
    , Property "Show" $ \(a :: a) -> show a === show (toInteger a)
    , Property "Ord" $ \(a :: a) (b :: a) -> compare a b === (compare `on` toInteger) a b
    , Property "<" $ \(a :: a) (b :: a) -> case compare a b of
                                                LT -> propertyCompare "<" (<) a b
                                                GT -> propertyCompare "<" (<) b a
                                                EQ -> propertyCompare "not <" ((not .) . (<)) a b `propertyAnd`
                                                      propertyCompare "not <" ((not .) . (<)) b a
    ]

testAdditive :: forall a . (Show a, Eq a, Num a, Arbitrary a, Typeable a)
             => Proxy a -> Test
testAdditive _ = Group "Additive"
    [ Property "a + azero == a" $ \(a :: a) -> a + 0 === a
    , Property "azero + a == a" $ \(a :: a) -> 0 + a === a
    , Property "a + b == b + a" $ \(a :: a) (b :: a) -> a + b === b + a
    ]

testMultiplicative :: forall a . (Show a, Eq a, Integral a, Arbitrary a, Typeable a)
                   => Proxy a -> Test
testMultiplicative _ = Group "Multiplicative"
    [ Property "a * 1 == a" $ \(a :: a) -> a * 1 === a
    , Property "1 * a == a" $ \(a :: a) -> 1 * a === a
    , Property "multiplication commutative" $ \(a :: a) (b :: a) -> a * b === b * a
    , Property "a * b == Integer(a) * Integer(b)" $ \(a :: a) (b :: a) -> a * b === fromInteger (toInteger a * toInteger b)
    ]

testDividible :: forall a . (Show a, Eq a, Integral a, Num a, Arbitrary a, Typeable a)
              => Proxy a -> Test
testDividible _ = Group "Divisible"
    [ Property "(x `div` y) * y + (x `mod` y) == x" $ \(a :: a) b ->
            if b == 0 then True === True
                      else a === (a `div` b) * b + (a `mod` b)
    ]

testOperatorPrecedence :: forall a . (Show a, Eq a, Prelude.Num a, Integral a, Num a,  Arbitrary a, Typeable a)
                       => Proxy a -> Test
testOperatorPrecedence _ = Group "Precedence"
    [ Property "+ and - (1)" $ \(a :: a) (b :: a) (c :: a) -> (a + b - c) === ((a + b) - c)
    , Property "+ and - (2)" $ \(a :: a) (b :: a) (c :: a) -> (a - b + c) === ((a - b) + c)
    , Property "+ and * (1)" $ \(a :: a) (b :: a) (c :: a) -> (a + b * c) === (a + (b * c))
    , Property "+ and * (2)" $ \(a :: a) (b :: a) (c :: a) -> (a * b + c) === ((a * b) + c)
    , Property "- and * (1)" $ \(a :: a) (b :: a) (c :: a) -> (a - b * c) === (a - (b * c))
    , Property "- and * (2)" $ \(a :: a) (b :: a) (c :: a) -> (a * b - c) === ((a * b) - c)
    , Property "* and ^ (1)" $ \(a :: a) (b :: Natural) (c :: a) -> (a ^ b * c) === ((a ^ b) * c)
    , Property "* and ^ (2)" $ \(a :: a) (c :: Natural) (b :: a) -> (a * b ^ c) === (a * (b ^ c))
    ]


testNumber :: (Show a, Eq a, Prelude.Num a, Integral a, Num a, Arbitrary a, Typeable a)
           => String -> Proxy a -> Test
testNumber name proxy = Group name
    [ testIntegral proxy
    , testEqOrd proxy
    , testAdditive proxy
    , testMultiplicative proxy
    , testDividible proxy
    , testOperatorPrecedence proxy
    ]

testNumberRefs :: Test
testNumberRefs = Group "ALL"
    [ testNumber "Int" (Proxy :: Proxy Int)
    , testNumber "Int8" (Proxy :: Proxy Int8)
    , testNumber "Int16" (Proxy :: Proxy Int16)
    , testNumber "Int32" (Proxy :: Proxy Int32)
    , testNumber "Int64" (Proxy :: Proxy Int64)
    , testNumber "Integer" (Proxy :: Proxy Integer)
    , testNumber "Word" (Proxy :: Proxy Word)
    , testNumber "Word8" (Proxy :: Proxy Word8)
    , testNumber "Word16" (Proxy :: Proxy Word16)
    , testNumber "Word32" (Proxy :: Proxy Word32)
    , testNumber "Word64" (Proxy :: Proxy Word64)
    ]


main = runTests testNumberRefs
