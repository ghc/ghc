{-# LANGUAGE CPP, MultiParamTypeClasses,
             FlexibleInstances, TypeSynonymInstances #-}
--
-- Uses multi-param type classes
--
module QuickCheckUtils where

import Test.QuickCheck
import Text.Show.Functions

import Control.Monad        ( liftM2 )
import Control.Monad.Instances
import Data.Char
import Data.List
import Data.Word
import Data.Int
import System.Random
import System.IO
import Foreign.C (CChar)

import qualified Data.ByteString      as P
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Internal as L (checkInvariant,ByteString(..))

import qualified Data.ByteString.Char8      as PC
import qualified Data.ByteString.Lazy.Char8 as LC

------------------------------------------------------------------------

integralRandomR :: (Integral a, RandomGen g) => (a,a) -> g -> (a,g)
integralRandomR  (a,b) g = case randomR (fromIntegral a :: Integer,
                                         fromIntegral b :: Integer) g of
                            (x,g) -> (fromIntegral x, g)

sizedByteString n = do m <- choose(0, n)
                       fmap P.pack $ vectorOf m arbitrary

instance Arbitrary P.ByteString where
  arbitrary = do
    bs <- sized sizedByteString
    n  <- choose (0, 2)
    return (P.drop n bs) -- to give us some with non-0 offset

instance CoArbitrary P.ByteString where
  coarbitrary s = coarbitrary (P.unpack s)

instance Arbitrary L.ByteString where
  arbitrary = sized $ \n -> do numChunks <- choose (0, n)
                               if numChunks == 0
                                   then return L.empty
                                   else fmap (L.checkInvariant .
                                              L.fromChunks .
                                              filter (not . P.null)) $
                                            vectorOf numChunks
                                                     (sizedByteString
                                                          (n `div` numChunks))

instance CoArbitrary L.ByteString where
  coarbitrary s = coarbitrary (L.unpack s)

newtype CByteString = CByteString P.ByteString
  deriving Show

instance Arbitrary CByteString where
  arbitrary = fmap (CByteString . P.pack . map fromCChar) arbitrary
    where
      fromCChar :: CChar -> Word8
      fromCChar = fromIntegral

instance Arbitrary CChar where
  arbitrary = fmap (fromIntegral :: Int -> CChar)
            $ oneof [choose (-128,-1), choose (1,127)]

------------------------------------------------------------------------
--
-- We're doing two forms of testing here. Firstly, model based testing.
-- For our Lazy and strict bytestring types, we have model types:
--
--  i.e.    Lazy    ==   Byte
--              \\      //
--                 List 
--
-- That is, the Lazy type can be modeled by functions in both the Byte
-- and List type. For each of the 3 models, we have a set of tests that
-- check those types match.
--
-- The Model class connects a type and its model type, via a conversion
-- function. 
--
--
class Model a b where
  model :: a -> b  -- get the abstract vale from a concrete value

--
-- Connecting our Lazy and Strict types to their models. We also check
-- the data invariant on Lazy types.
--
-- These instances represent the arrows in the above diagram
--
instance Model B P      where model = abstr . checkInvariant
instance Model P [W]    where model = P.unpack
instance Model P [Char] where model = PC.unpack
instance Model B [W]    where model = L.unpack  . checkInvariant
instance Model B [Char] where model = LC.unpack . checkInvariant
instance Model Char Word8 where model = fromIntegral . ord

-- Types are trivially modeled by themselves
instance Model Bool  Bool         where model = id
instance Model Int   Int          where model = id
instance Model P     P            where model = id
instance Model B     B            where model = id
instance Model Int64 Int64        where model = id
instance Model Word8 Word8        where model = id
instance Model Ordering Ordering  where model = id
instance Model Char Char  where model = id

-- More structured types are modeled recursively, using the NatTrans class from Gofer.
class (Functor f, Functor g) => NatTrans f g where
    eta :: f a -> g a

-- The transformation of the same type is identity
instance NatTrans [] []             where eta = id
instance NatTrans Maybe Maybe       where eta = id
instance NatTrans ((->) X) ((->) X) where eta = id
instance NatTrans ((->) Char) ((->) Char) where eta = id

instance NatTrans ((->) W) ((->) W) where eta = id

-- We have a transformation of pairs, if the pairs are in Model
instance Model f g => NatTrans ((,) f) ((,) g) where eta (f,a) = (model f, a)

-- And finally, we can take any (m a) to (n b), if we can Model m n, and a b
instance (NatTrans m n, Model a b) => Model (m a) (n b) where model x = fmap model (eta x)

------------------------------------------------------------------------

-- In a form more useful for QC testing (and it's lazy)
checkInvariant :: L.ByteString -> L.ByteString
checkInvariant = L.checkInvariant

abstr :: L.ByteString -> P.ByteString
abstr = P.concat . L.toChunks 

-- Some short hand.
type X = Int
type W = Word8
type P = P.ByteString
type B = L.ByteString

------------------------------------------------------------------------
--
-- These comparison functions handle wrapping and equality.
--
-- A single class for these would be nice, but note that they differe in
-- the number of arguments, and those argument types, so we'd need HList
-- tricks. See here: http://okmij.org/ftp/Haskell/vararg-fn.lhs
--

eq1 f g = \a         ->
    model (f a)         == g (model a)
eq2 f g = \a b       ->
    model (f a b)       == g (model a) (model b)
eq3 f g = \a b c     ->
    model (f a b c)     == g (model a) (model b) (model c)

--
-- And for functions that take non-null input
--
eqnotnull1 f g = \x     -> (not (isNull x)) ==> eq1 f g x
eqnotnull2 f g = \x y   -> (not (isNull y)) ==> eq2 f g x y
eqnotnull3 f g = \x y z -> (not (isNull z)) ==> eq3 f g x y z

class    IsNull t            where isNull :: t -> Bool
instance IsNull L.ByteString where isNull = L.null
instance IsNull P.ByteString where isNull = P.null
