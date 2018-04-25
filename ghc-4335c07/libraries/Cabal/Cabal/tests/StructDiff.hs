{- |

At the ZuriHac 2016 I worked on the new parsec-based parser for the *.cabal files.
The obvious test case is to compare new and old parser results for all of Hackage.
Traversing the Hackage is quite trivial. The difficult part is inspecting
the result 'GenericPackageDescription's to spot the difference.

In the same event, Andres Löh showed his library @generics-sop@. Obvious choice
to quickly put something together for the repetetive task. After all you can
compare records field-wise. And if sum constructors are different, that's
enough for our case as well!

Generic programming ftw.
-}

{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
-- | TODO: package as a library? is this useful elsewhere?
module StructDiff where

import           Control.Applicative  (liftA2)
import           Data.Align.Key       (AlignWithKey (..))
import           Data.Foldable        (Foldable, fold, traverse_)
import           Data.Key             (Key)
import           Data.List            (intercalate)
import           Data.Map             (Map)
import           Data.Monoid          (Monoid (..), (<>))
import           Data.Singletons.Bool (SBool (..), SBoolI (..), eqToRefl)
import           Data.These           (These (..))
import           Data.Type.Equality
import           Generics.SOP hiding (fieldName)

-- | Because @'Data.Proxy.Proxy' :: 'Data.Proxy.Proxy' a@ is so long.
data P a = P

-------------------------------------------------------------------------------
-- Structure diffs
-------------------------------------------------------------------------------

-- | Each thunk has a path, removed and added "stuff"
data DiffThunk = DiffThunk { dtPath :: [String], dtA :: String, dtB :: String }
    deriving Show

-- | Diff result is a collection of thunks
data DiffResult = DiffResult [DiffThunk]
    deriving Show

prefixThunk :: String -> DiffThunk -> DiffThunk
prefixThunk pfx (DiffThunk path a b) = DiffThunk (pfx : path) a b

prefixResult :: String -> DiffResult -> DiffResult
prefixResult name (DiffResult thunks) = DiffResult $ map (prefixThunk name) thunks

-- | Pretty print a result
prettyResultIO :: DiffResult -> IO ()
prettyResultIO (DiffResult []) = putStrLn "Equal"
prettyResultIO (DiffResult xs) = traverse_ p xs
  where
    p (DiffThunk paths a b) = do
        putStrLn $ intercalate " " paths ++ " : "
        putStrLn $ "- " ++ a
        putStrLn $ "+ " ++ b

-- | We can join diff results
instance Monoid DiffResult where
    mempty = DiffResult mempty
    mappend (DiffResult x) (DiffResult y) = DiffResult (mappend x y)

-- | And we have a class for things we can diff
class Diff a where
    diff :: a -> a -> DiffResult
    default diff
        :: (Generic a, HasDatatypeInfo a, All2 Diff (Code a))
        => a -> a -> DiffResult
    diff = gdiff

-- | And generic implementation!
gdiff :: forall a. (Generic a, HasDatatypeInfo a, All2 Diff (Code a)) => a -> a -> DiffResult
gdiff x y  = gdiffS (constructorInfo (datatypeInfo (P :: P a))) (unSOP $ from x) (unSOP $ from y)

gdiffS :: All2 Diff xss => NP ConstructorInfo xss -> NS (NP I) xss -> NS (NP I) xss -> DiffResult
gdiffS (c :* _) (Z xs) (Z ys) = mconcat $ hcollapse $ hczipWith3 (P :: P Diff) f (fieldNames c) xs ys
  where
    f :: Diff a => K FieldName a -> I a -> I a -> K DiffResult a
    f (K fieldName) x y = K . prefixResult fieldName . unI $ liftA2 diff x y
gdiffS (_ :* cs) (S xss) (S yss) = gdiffS cs xss yss
gdiffS cs xs ys = DiffResult [DiffThunk [] (constructorNameOf cs xs) (constructorNameOf cs ys)]

eqDiff :: (Eq a, Show a) => a -> a -> DiffResult
eqDiff x y
    | x == y    = DiffResult []
    | otherwise = DiffResult [DiffThunk [] (show x) (show y)]

alignDiff
    :: (Show (Key f), Show a, Diff a, AlignWithKey f, Foldable f)
    => f a -> f a -> DiffResult
alignDiff x y = fold $ alignWithKey (\k -> prefixResult (show k) . f) x y
  where
    f (These a b) = diff a b
    f (This a)    = DiffResult [DiffThunk [] (show a) "<none>"]
    f (That b)    = DiffResult [DiffThunk [] "<none>" (show b)]

instance Diff Char where diff = eqDiff
instance Diff Bool
instance Diff a => Diff (Maybe a)
instance Diff Int where diff = eqDiff
instance (Diff a, Diff b) => Diff (Either a b)

instance (Diff a, Diff b) => Diff (a, b) where
    diff (a, b) (a', b') =
        prefixResult "_1" (diff a a') <>
        prefixResult "_2" (diff b b')

instance (Diff a, Diff b, Diff c) => Diff (a, b, c) where
    diff (a, b, c) (a', b', c') =
        prefixResult "_1" (diff a a') <>
        prefixResult "_2" (diff b b') <>
        prefixResult "_3" (diff c c')

instance (SBoolI (a == Char), Show a, Diff a) => Diff [a] where
    diff = case sbool :: SBool (a == Char) of
        STrue  -> case eqToRefl :: a :~: Char of
            Refl -> eqDiff
        SFalse -> alignDiff

instance (Ord k, Show k, Diff v, Show v) => Diff (Map k v) where diff = alignDiff

-------------------------------------------------------------------------------
-- SOP helpers
-------------------------------------------------------------------------------

constructorNameOf :: NP ConstructorInfo xss -> NS f xss -> ConstructorName
constructorNameOf (c :* _)  (Z _)  = constructorName c
constructorNameOf (_ :* cs) (S xs) = constructorNameOf cs xs
constructorNameOf _ _ = error "Should never happen"

-- | This is a little lie.
fieldNames :: ConstructorInfo xs -> NP (K FieldName) xs
fieldNames (Constructor name) = hpure (K name) -- TODO: add .1 .2 etc.
fieldNames (Infix name _ _) = K ("-(" ++ name ++ ")") :* K ("(" ++ name ++ ")-") :* Nil
fieldNames (Record _ fis) = hmap (\(FieldInfo fieldName) -> K fieldName) fis
