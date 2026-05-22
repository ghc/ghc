{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

-- | A minimal QuickCheck-like property testing framework for use in the GHC
-- test suite.
--
-- We vendor this package to avoid depending on the real QuickCheck package,
-- as the latter (or one of its dependencies) may not build with the GHC version
-- being tested.
module MiniQuickCheck
  ( -- * QuickCheck generator
    Gen(..)

    -- * QuickCheck typeclasses
  , Arbitrary(..)
  , IsProperty(..)

    -- * QuickCheck properties
  , PropertyCheck(..)
  , PropertyTestArg(..)
  , Property(..)
  , forAll
  , (===)
  , propertyCompare
  , propertyAnd
  , propertyTrue
  , getCheck

    -- * QuickCheck test tree
  , Test(..)

    -- * Running QuickCheck tests
  , Result(..)
  , Iterations(..)
  , runTestsMain
  , runTests
  , runTestInternal

    -- * QuickCheck primitive generators
  , arbitraryInt64
  , arbitraryWord64
  , integralDownsize
  , wordDownsize

    -- * QuickCheck newtypes
  , NonZero(..)
  , nonZero
  , BoundedShiftAmount(..)
  , BoundedBy(..)
  ) where

-- base
import Control.Exception
  ( SomeException, displayException, evaluate, try )
import Control.Monad.IO.Class
  ( liftIO )
import Data.Bits
  ( (.|.), shiftL, shiftR
  , FiniteBits, finiteBitSize
  )
import Data.Int
  ( Int8, Int16, Int32, Int64 )
import Data.IORef
  ( newIORef, atomicModifyIORef' )
import Data.Kind
  ( Type )
import Data.List
  ( intercalate )
import Data.Proxy
  ( Proxy(..) )
import Data.Word
  ( Word8, Word16, Word32, Word64 )
import GHC.TypeNats
  ( Nat, KnownNat, natVal )
import Numeric.Natural
  ( Natural )
import System.Environment
  ( getArgs )
import System.Exit
  ( die, exitFailure )
import Text.Read
  ( readMaybe )

-- transformers
import Control.Monad.Trans.Reader
  ( ReaderT, runReaderT, ask, local )
import Control.Monad.Trans.State.Strict
  ( State, state, runState )

--------------------------------------------------------------------------------
-- Core framework

newtype Gen a = Gen { runGen :: State Word64 a }
  deriving newtype ( Functor, Applicative, Monad )

class Arbitrary a where
  arbitrary :: Gen a

class IsProperty p where
  property :: p -> Property

data PropertyCheck
  = PropertyBinaryOp Bool String String String
  | PropertyAnd PropertyCheck PropertyCheck

instance IsProperty PropertyCheck where
  property check = Prop (pure (PropertyEOA check))

data PropertyTestArg
  = PropertyEOA PropertyCheck
  | PropertyArg String PropertyTestArg

getCheck :: PropertyTestArg -> ([String], PropertyCheck)
getCheck (PropertyEOA pc)    = ([], pc)
getCheck (PropertyArg s pta) = let (ss, pc) = getCheck pta in (s:ss, pc)

data Property = Prop { unProp :: Gen PropertyTestArg }

instance (Show a, Arbitrary a, IsProperty prop) => IsProperty (a -> prop) where
  property p = forAll arbitrary p

-- | Run a generator for a value of the given type and add it as an argument
-- to the property test.
forAll :: (Show a, IsProperty prop) => Gen a -> (a -> prop) -> Property
forAll generator tst = Prop $ do
  a <- generator
  augment a <$> unProp (property (tst a))
  where
    augment a arg = PropertyArg (show a) arg

-- | Build a @PropertyCheck@ by comparing two values with a named predicate.
propertyCompare :: Show a => String -> (a -> a -> Bool) -> a -> a -> PropertyCheck
propertyCompare s f a b = PropertyBinaryOp (f a b) s (show a) (show b)

-- | Check that two values are equal (by '==').
(===) :: (Show a, Eq a) => a -> a -> PropertyCheck
(===) = propertyCompare "==" (==)
infix 4 ===

-- | Conjunction of two property checks.
propertyAnd :: PropertyCheck -> PropertyCheck -> PropertyCheck
propertyAnd = PropertyAnd

-- | A property check that trivially holds. Useful to mark a case to skip.
propertyTrue :: PropertyCheck
propertyTrue = PropertyBinaryOp True "trivially" "" ""

--------------------------------------------------------------------------------
-- Test tree

-- | A named test or group of tests.
data Test where
  Group    :: String -> [Test] -> Test
  Property :: IsProperty prop => String -> prop -> Test

--------------------------------------------------------------------------------
-- Test runner

newtype Iterations = Iterations { nbIterations :: Word }
  deriving newtype ( Show, Eq, Ord )

-- | Outcome of running a test suite.
data Result = Success | Failure [[String]]

instance Semigroup Result where
  Success    <> y          = y
  x          <> Success    = x
  Failure xs <> Failure ys = Failure (xs ++ ys)

instance Monoid Result where
  mempty = Success

data RunS = RunS
  { depth       :: Int
  , currentSeed :: Word64
  , context     :: [String]
  }

putMsg :: String -> ReaderT RunS IO ()
putMsg s = do
  n <- depth <$> ask
  liftIO . putStrLn $ replicate (n * 2) ' ' ++ s

nest :: String -> ReaderT RunS IO a -> ReaderT RunS IO a
nest c = local (\s -> s { depth = depth s + 1, context = c : context s })

runPropertyCheck :: PropertyCheck -> ReaderT RunS IO Result
runPropertyCheck pcThunk = do
  -- See Note [Catching exceptions in property evaluation].
  pcRes <- liftIO $ try @SomeException (evaluate pcThunk)
  case pcRes of
    Left  e -> reportFailure ("Failure: exception: " ++ displayException e)
    Right (PropertyAnd a b) ->
      (<>) <$> runPropertyCheck a <*> runPropertyCheck b
    Right (PropertyBinaryOp ok desc s1 s2) -> do
      okRes <- liftIO $ try @SomeException (evaluate ok)
      case okRes of
        Right True  -> return Success
        Right False -> reportFailure ("Failure: " ++ s1 ++ " " ++ desc ++ " " ++ s2)
        Left  e     -> reportFailure ("Failure: exception: " ++ displayException e)

reportFailure :: String -> ReaderT RunS IO Result
reportFailure msg = do
  ctx <- context <$> ask
  putMsg msg
  return (Failure [msg : ctx])

-- Note [Catching exceptions in property evaluation]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- A property like `\a b -> let !r = a `div` 0 in r === b` builds a
-- `PropertyCheck` thunk whose forcing raises an exception -- in this case
-- already at the `PropertyBinaryOp` constructor, before its `ok` field is
-- ever inspected. Other properties may force `ok = (s1 == s2)` instead and
-- raise from there.
--
-- To handle both, we `evaluate` first the `PropertyCheck` thunk and then
-- the `ok` field, each inside `try`, and report any exception through the
-- normal `reportFailure` path. The surrounding loop then still prints
-- "With arguments ... (Seed: ...)" and the test driver continues with
-- subsequent properties instead of aborting.

runProperty :: Iterations -> Property -> ReaderT RunS IO Result
runProperty (Iterations iters) (Prop p) = do
  startingSeed <- currentSeed <$> ask
  loop iters startingSeed
  where
    loop 0 _ = do
      putMsg ("Passed " ++ show iters ++ " iterations")
      return Success
    loop n s = do
      let (pt, s') = runState (runGen p) s
          (ss, pc) = getCheck pt
      res <- runPropertyCheck pc
      case res of
        Success      -> loop (n - 1) s'
        Failure msgs -> do
          let msg = "With arguments " ++ intercalate ", " ss ++ " (Seed: " ++ show s ++ ")"
          putMsg msg
          return (Failure (map (msg :) msgs))

-- | Run a single 'Test', accumulating all failures.
runTestInternal :: Iterations -> Test -> ReaderT RunS IO Result
runTestInternal iters (Group name tests) = do
  let label = "Group " ++ name
  putMsg label
  env <- ask
  nest label $ do
    -- Compute initial seed for each test in the group, based on the
    -- index of the test in the group.
    let runOne idx t = do
          let !s = snd $ stepLCG (currentSeed env + fromIntegral idx)
          local (\e -> e { currentSeed = s }) (runTestInternal iters t)
    mconcat <$> traverse (uncurry runOne) (zip [1..] tests)

runTestInternal iters (Property name p) = do
  let label = "Running " ++ name
  putMsg label
  nest label (runProperty iters (property p))

showStack :: Int -> [String] -> String
showStack _  []     = ""
showStack n (s:ss) = replicate n ' ' ++ s ++ "\n" ++ showStack (n + 2) ss

-- | Standard @main@ entry point for tests using 'MiniQuickCheck'.
--
-- Reads a 'Word64' seed from the first command-line argument, then
-- delegates to 'runTests'.
runTestsMain :: Iterations -> Test -> IO ()
runTestsMain iters t = do
  args <- getArgs
  seed <- case args of
    [arg] -> case readMaybe arg of
      Just s  -> pure s
      Nothing -> die $ "Invalid seed: " ++ show arg
    _ -> die "Usage: <test-name> <seed>"
  runTests iters seed t

runTests :: Iterations -> Word64 -> Test -> IO ()
runTests iters seed t = do
  res <- runReaderT (runTestInternal iters t) (RunS 0 seed [])
  case res of
    Success      -> return ()
    Failure tests -> do
      putStrLn $ "Seed: " ++ show seed
      putStrLn $ "These tests failed:\n"
              ++ intercalate "\n" (map (showStack 0 . reverse) tests)
      exitFailure

--------------------------------------------------------------------------------
-- Random number generation (linear congruences)

-- Constants from Knuth's MMIX

lcgMultiplier :: Word64
lcgMultiplier = 6364136223846793005
lcgIncrement :: Word64
lcgIncrement = 1442695040888963407

-- | Pure step function for the linear congruential generator
stepLCG :: Word64 -> (Word64, Word64)
stepLCG s =
  let s' = s * lcgMultiplier + lcgIncrement
  in (s', s')

--------------------------------------------------------------------------------
-- Primitive generators

-- | Generate a uniformly random 'Word64'.
arbitraryWord64 :: Gen Word64
arbitraryWord64 = Gen $ state stepLCG

-- | Generate a uniformly random 'Int64' (bit-reinterpretation of a Word64).
arbitraryInt64 :: Gen Int64
arbitraryInt64 = fromIntegral <$> arbitraryWord64

-- | Shrink a random 'Int64' down to a smaller integral type.
integralDownsize :: (Integral a, FiniteBits a) => Int64 -> a
integralDownsize = wordDownsize . fromIntegral

-- | Shrink a random 'Word64' down to a smaller integral type.
wordDownsize :: forall a. (Integral a, FiniteBits a) => Word64 -> a
wordDownsize w =
  fromIntegral (w `shiftR` (64 - finiteBitSize (undefined :: a)))
    -- take the higher bits (more random with our LCG)

--------------------------------------------------------------------------------
-- Basic Arbitrary instances

instance Arbitrary Bool where
  arbitrary = ( == 1 ) . ( `shiftR` 63 ) <$> arbitraryWord64

instance Arbitrary Word64 where
  arbitrary = arbitraryWord64
instance Arbitrary Word32 where
  arbitrary = wordDownsize <$> arbitraryWord64
instance Arbitrary Word16 where
  arbitrary = wordDownsize <$> arbitraryWord64
instance Arbitrary Word8 where
  arbitrary = wordDownsize <$> arbitraryWord64
instance Arbitrary Word where
  arbitrary = fromIntegral <$> arbitraryWord64

instance Arbitrary Int64 where
  arbitrary = arbitraryInt64
instance Arbitrary Int32 where
  arbitrary = integralDownsize <$> arbitraryInt64
instance Arbitrary Int16 where
  arbitrary = integralDownsize <$> arbitraryInt64
instance Arbitrary Int8 where
  arbitrary = integralDownsize <$> arbitraryInt64
instance Arbitrary Int where
  arbitrary = fromIntegral <$> arbitraryInt64

-- | Generates a natural number with at most 192 bits set.
instance Arbitrary Natural where
  arbitrary = do
    cx <- ( `shiftR` 62 ) <$> arbitraryWord64
    n1 <- fromIntegral <$> arbitraryWord64
    n2 <- fromIntegral <$> arbitraryWord64
    n3 <- fromIntegral <$> arbitraryWord64

    pure $ case cx of
      0 -> n1
      1 -> (n1 `shiftL` 64) .|. n2
      _ -> (n1 `shiftL` 128) .|. (n2 `shiftL` 64) .|. n3

-- | Generates an integer with at most 192 bits set.
instance Arbitrary Integer where
  arbitrary = do
    nat <- arbitrary @Natural
    neg <- arbitrary @Bool

    pure $
      if neg
      then negate (fromIntegral nat)
      else fromIntegral nat

instance Arbitrary Char where
  arbitrary = do
    let high = fromIntegral (fromEnum (maxBound :: Char)) :: Word
    x <- arbitrary
    return (toEnum . fromIntegral $ x `mod` (high + 1))

--------------------------------------------------------------------------------
-- Useful newtypes for different Arbitrary instances

-- | Wrapper for non-zero values.
newtype NonZero a = NonZero { getNonZero :: a }
  deriving (Eq, Ord, Bounded, Show)

-- | Generator that rejects zero values.
nonZero :: (Arbitrary a, Num a, Eq a) => Gen (NonZero a)
nonZero = do
  x <- arbitrary
  if x == 0 then nonZero else pure (NonZero x)

instance (Arbitrary a, Num a, Eq a) => Arbitrary (NonZero a) where
  arbitrary = nonZero

-- | Shift amount bounded to @[0, finiteBitSize - 1]@.
newtype BoundedShiftAmount a = BoundedShiftAmount { getBoundedShiftAmount :: Int }
  deriving (Eq, Ord, Show)

instance FiniteBits a => Arbitrary (BoundedShiftAmount a) where
  arbitrary = do
    x <- arbitrary
    let w = finiteBitSize (undefined :: a)
    pure $ BoundedShiftAmount (abs x `mod` w)

-- | @a `BoundedBy` n@ represents numbers with maximum absolute value @n@ (inclusive).
type BoundedBy :: Type -> Nat -> Type
newtype BoundedBy a n = BoundedBy { getBoundedBy :: a }
  deriving (Eq, Ord, Show)

instance
  forall n a
  .  ( KnownNat n, Integral a, Arbitrary a )
  => Arbitrary ( a `BoundedBy` n ) where
  arbitrary = BoundedBy . (`rem` (n + 1)) <$> arbitrary
    where
      n :: a
      n = fromIntegral $ natVal @n Proxy
