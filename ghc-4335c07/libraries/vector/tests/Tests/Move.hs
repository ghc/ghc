module Tests.Move (tests) where

import Test.QuickCheck
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck.Property (Property(..))

import Utilities ()

import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as M

import qualified Data.Vector as V
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Unboxed as U

basicMove :: G.Vector v a => v a -> Int -> Int -> Int -> v a
basicMove v dstOff srcOff len
  | len > 0 = G.modify (\ mv -> G.copy (M.slice dstOff len mv) (G.slice srcOff len v)) v
  | otherwise = v

testMove :: (G.Vector v a, Show (v a), Eq (v a)) => v a -> Property
testMove v = G.length v > 0 ==> (MkProperty $ do
  dstOff <- choose (0, G.length v - 1)
  srcOff <- choose (0, G.length v - 1)
  len <- choose (1, G.length v - max dstOff srcOff)
  expected <- return $ basicMove v dstOff srcOff len
  actual <- return $  G.modify (\ mv -> M.move (M.slice dstOff len mv) (M.slice srcOff len mv)) v
  unProperty $ counterexample ("Move: " ++ show (v, dstOff, srcOff, len)) (expected == actual))

tests =
    [testProperty "Data.Vector.Mutable (Move)" (testMove :: V.Vector Int -> Property),
     testProperty "Data.Vector.Primitive.Mutable (Move)" (testMove :: P.Vector Int -> Property),
     testProperty "Data.Vector.Unboxed.Mutable (Move)" (testMove :: U.Vector Int -> Property),
     testProperty "Data.Vector.Storable.Mutable (Move)" (testMove :: S.Vector Int -> Property)]