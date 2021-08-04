{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}

module Regress (regressions) where

import qualified Test.Framework as F
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@=?))
import GHC.Generics (Generic)
import Data.List (nub)
import Data.Fixed (Pico)

#ifdef HAVE_MMAP
import qualified Regress.Mmap as Mmap
#endif

import Data.Hashable

regressions :: [F.Test]
regressions = [] ++
#ifdef HAVE_MMAP
    Mmap.regressions ++
    [ testCase "Fixed" $ do
        (hash (1 :: Pico) == hash (2 :: Pico)) @=? False
    ] ++
#endif
    [ F.testGroup "Generic: sum of nullary constructors"
        [ testCase "0" $ nullaryCase 0 S0
        , testCase "1" $ nullaryCase 1 S1
        , testCase "2" $ nullaryCase 2 S2
        , testCase "3" $ nullaryCase 3 S3
        , testCase "4" $ nullaryCase 4 S4
        ]
    , testCase "Generic: Peano https://github.com/tibbe/hashable/issues/135" $ do
        let ns = take 20 $ iterate S Z
        let hs = map hash ns
        hs @=? nub hs
    ]
  where
    nullaryCase :: Int -> SumOfNullary -> IO ()
    nullaryCase n s = do
        let salt = 42
        let expected = salt `hashWithSalt` n `hashWithSalt` ()
        let actual = hashWithSalt salt s
        expected @=? actual

data SumOfNullary = S0 | S1 | S2 | S3 | S4 deriving (Generic)
instance Hashable SumOfNullary

data Nat = Z | S Nat deriving (Generic)
instance Hashable Nat
