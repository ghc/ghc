-- | Test generators.
--
module TestGenerators (
        emptyDocGen,
        emptyDocListGen
    ) where

import PrettyTestVersion
import TestStructures

import Control.Monad

import Test.QuickCheck

instance Arbitrary CDoc where
   arbitrary = sized arbDoc
    where
      -- TODO: finetune frequencies
      arbDoc k | k <= 1 = frequency [
               (1,return CEmpty)
             , (2,return (CText . unText) `ap` arbitrary)
             ]
      arbDoc n = frequency [
             (1, return CList `ap` arbitrary `ap`  (liftM unDocList $ resize (pred n) arbitrary))
            ,(1, binaryComb n CBeside)
            ,(1, binaryComb n CAbove)
            ,(1, choose (0,10) >>= \k -> return (CNest k) `ap` (resize (pred n) arbitrary)) 
            ]
      binaryComb n f = 
        split2 (n-1) >>= \(n1,n2) ->
        return f `ap` arbitrary `ap` (resize n1 arbitrary) `ap` (resize n2 arbitrary)
      split2 n = flip liftM ( choose (0,n) ) $ \sz -> (sz, n - sz)

instance CoArbitrary CDoc where
   coarbitrary CEmpty = variant 0
   coarbitrary (CText t) = variant 1 . coarbitrary (length t)
   coarbitrary (CList f list) = variant 2 . coarbitrary f . coarbitrary list
   coarbitrary (CBeside b d1 d2) = variant 3 . coarbitrary b . coarbitrary d1 . coarbitrary d2
   coarbitrary (CAbove b d1 d2) = variant 4 . coarbitrary b . coarbitrary d1 . coarbitrary d2
   coarbitrary (CNest k d) = variant 5 . coarbitrary k . coarbitrary d
   
instance Arbitrary CList where
    arbitrary = oneof $ map return [ CCat, CSep, CFCat, CFSep ]

instance CoArbitrary CList where
    coarbitrary cl = variant (case cl of CCat -> 0; CSep -> 1; CFCat -> 2; CFSep -> 3)

-- we assume that the list itself has no size, so that 
-- sizeof (a $$ b) = sizeof (sep [a,b]) = sizeof(a) + sizeof(b)+1
instance Arbitrary CDocList where
    arbitrary = liftM CDocList $ sized $ \n -> arbDocList n where
        arbDocList 0 = return []
        arbDocList n = do
          listSz <- choose (1,n)
          let elems = take listSz $ repeat (n `div` listSz) -- approximative
          mapM (\sz -> resize sz arbitrary) elems

instance CoArbitrary CDocList where
    coarbitrary (CDocList ds) = coarbitrary ds

instance Arbitrary Text where
    arbitrary = liftM Text $ sized $ \n -> mapM (const arbChar) [1..n]
        where arbChar = oneof (map return ['a'..'c'])

instance CoArbitrary Text where
    coarbitrary (Text str) = coarbitrary (length str)

emptyDocGen :: Gen CDoc
emptyDocGen = return CEmpty

emptyDocListGen :: Gen CDocList
emptyDocListGen = do
    ls <- listOf emptyDocGen
    return $ CDocList ls

