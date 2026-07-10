{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -O0 #-} -- doing unsafe things, so be on the safe side

module Test(
    (~~), (=%=), K, V,
    Key(..),
    ValidRule(..), ValidRuleOnce(..),
    Property
    ) where

import Control.Monad.State
import Data.IORef
import Data.List.Extra
import System.IO.Unsafe
import Util
import Test.QuickCheck


(~~) :: Testable a => String -> a -> (String, Property)
(~~) a b = (a, property b)

(=%=) :: V -> K -> Bool
(=%=) (V k1 _) k2 = k1 == k2

newtype K = K Int deriving (Eq,Show)

ks :: [K]
ks = map K [1..4]

newtype Key = Key K deriving (Show,Arbitrary)

instance Arbitrary K where
    arbitrary = elements ks

-- Each V has a K it is associated with and a Bool value
data V = V K Bool deriving (Eq,Show)

data Op = Skip | Not

data Rule = Rule Bool [(K, Op)]

instance Arbitrary Op where
    arbitrary = elements [Skip,Not]

instance Arbitrary Rule where
    arbitrary = Rule <$> arbitrary <*> arbitrary

fixRule :: K -> Rule -> Rule
fixRule (K k) (Rule v ops) = Rule v $ filter (good . fst) ops
    where good (K x) = x < k

evalRules :: [(K,Rule)] -> (K -> Action K V)
evalRules rs k | Just (Rule v ops) <- lookup k rs = f v ops
    where
        f v [] = Done $ V k v
        f v ((k,o):xs) = Need k $ \(V k' v') -> case (v',o) of
            _ | k /= k' -> error $ "Asked for " ++ show k ++ " but got " ++ show k'
            (True, Skip) -> f v $ drop1 xs
            (True, Not ) -> f (not v) xs
            _              -> f v xs


-- | Rule associates each K with a list of dependencies, and an argument for a min operation
--   so we get mixing
data ValidRule = ValidRule [(K,Rule)] (K -> Action K V)

data ValidRuleOnce = ValidRuleOnce [(K,Rule)] (K -> Action K V)

instance Show ValidRule where show _ = "ValidRule"
instance Show ValidRuleOnce where show _ = "ValidRuleOnce"

instance Arbitrary ValidRule where
    arbitrary = do
        rs <- forM ks $ \k -> do
            r <- arbitrary
            pure (k, fixRule k r)
        pure $ ValidRule rs $ evalRules rs

instance Arbitrary ValidRuleOnce where
    arbitrary = do
        ValidRule a b <- arbitrary
        pure $ ValidRuleOnce a $ assertOnce b


{-# NOINLINE assertOnce #-}
assertOnce :: (Show k, Eq k) => (k -> v) -> (k -> v)
assertOnce f = unsafePerformIO $ do
    ref <- newIORef []
    pure $ \k -> unsafePerformIO $ do
        ks <- readIORef ref
        when (k `elem` ks) $ error $ "Key used twice: " ++ show k
        modifyIORef ref (k:)
        pure $ f k
