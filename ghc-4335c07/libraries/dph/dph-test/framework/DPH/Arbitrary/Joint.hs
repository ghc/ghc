{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

-- | Generation of nested arrays with identical structure.
module DPH.Arbitrary.Joint where
import Data.Array.Parallel.Pretty
import Test.QuickCheck                          hiding (NonEmpty)
import Data.Array.Parallel.Array      as A
import qualified Data.Vector.Unboxed            as U
import qualified Data.Vector                    as V
import Control.Monad


-- Joint21 ---------------------------------------------------------------------
-- | Generate two arrays with the same length.
data Joint21 a b
        = Joint21 a b
        deriving Show
        
instance ( Array c1 a, Arbitrary a
         , Array c2 b, Arbitrary b)
        => Arbitrary (Joint21 (c1 a) (c2 b)) where

 arbitrary
  = sized $ \s ->
  do    NonNegative n   <- arbitrary
        xs      <- vector n
        ys      <- vector n
        return  $ Joint21 (fromList xs) (fromList ys)


instance (PprPhysical a, PprPhysical b)
        => PprPhysical (Joint21 a b) where
 pprp (Joint21 x y)
        = vcat [text "Joint21", nest 4 $ pprp x, nest 4 $ pprp y]


-- Joint2 ---------------------------------------------------------------------
-- | Generate two nested arrays with the same lengths at the two outermost
--   levels. Also adjust the length of the inner elements, so the the total
--   number of elements is proportional to the size parameter.
data Joint22 a b
        = Joint22 a b
        deriving Show

instance ( Array c11 (c12 a), Array c12 a, Arbitrary a
         , Array c21 (c22 b), Array c22 b, Arbitrary b)
        => Arbitrary (Joint22 (c11 (c12 a)) (c21 (c22 b))) where

 arbitrary
  = sized $ \s ->
  do    let s'   =  truncate $ sqrt $ fromIntegral s

        lens     <- liftM (map (\(NonNegative n) -> n)) 
                 $  listOf $ resize s' arbitrary

        xs      <- liftM (fromList . map fromList) $ mapM vector lens
        ys      <- liftM (fromList . map fromList) $ mapM vector lens
        return  $ Joint22 xs ys


instance (PprPhysical a, PprPhysical b) => PprPhysical (Joint22 a b) where
 pprp (Joint22 x y)
        = vcat [text "Joint22", nest 4 $ pprp x, nest 4 $ pprp y]

instance (PprVirtual a,  PprVirtual b)  => PprVirtual  (Joint22 a b) where
 pprv (Joint22 x y)
        = text "Joint22" <+> parens (pprv x) <+> parens (pprv y)

-- Joint2N ---------------------------------------------------------------------
-- | Generate two nested arrays with the same lengths at the outermost level.
--   Also adjust the length of the inner elements, so the the total
--   number of elements is proportional to the size parameter.
data Joint2n a b
        = Joint2n a b
        deriving Show

instance ( Array c11 (c12 a), Array c12 a, Arbitrary a
         , Array c21 (c22 b), Array c22 b, Arbitrary b)
        => Arbitrary (Joint2n (c11 (c12 a)) (c21 (c22 b))) where

 arbitrary
  = sized $ \s ->
  do    let s'   =  truncate $ sqrt $ fromIntegral s

        lens1    <- liftM (map (\(NonNegative n) -> n)) 
                 $  listOf $ resize s' arbitrary
        -- for each element in first lengths, generate list with new number
        lens2    <- mapM (\_ -> do
                NonNegative n <- resize s' arbitrary
                return n
                ) lens1

        xs      <- liftM (fromList . map fromList) $ mapM vector lens1
        ys      <- liftM (fromList . map fromList) $ mapM vector lens2
        return  $ Joint2n xs ys


instance (PprPhysical a, PprPhysical b) => PprPhysical (Joint2n a b) where
 pprp (Joint2n x y)
        = vcat [text "Joint2n", nest 4 $ pprp x, nest 4 $ pprp y]

instance (PprVirtual a,  PprVirtual b)  => PprVirtual  (Joint2n a b) where
 pprv (Joint2n x y)
        = text "Joint2n" <+> parens (pprv x) <+> parens (pprv y)
