{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Exception (SomeException, evaluate, try)
import Data.Kind (Type)

-- | A Boolean singleton with the same index discipline as singletons.
data SBool (b :: Bool) where
    STrue :: SBool True
    SFalse :: SBool False

-- | Select a lifted result type.
type family Select (b :: Bool) :: Type where
    Select True = Int
    Select False = Bool

-- | Refine the result type by matching the retained witness.
choose :: forall b. SBool b -> Select b
choose STrue = 1
choose SFalse = False

-- | Preserve an index without evaluating its witness.
keep :: forall b. SBool b -> Select b -> Select b
keep _ x = x

-- | Introduce one index for a runtime Boolean.
withSBool :: Bool -> (forall b. SBool b -> r) -> r
withSBool True k = k STrue
withSBool False k = k SFalse

-- | Share one witness across dependent calls.
shared :: Bool -> Bool
shared b = withSBool b $ \s -> case s of
    STrue -> keep s (choose s) == 1
    SFalse -> keep s (choose s) == False

-- | Demonstrate that automatic reification can add strictness.
naiveBridge :: Bool -> ()
naiveBridge b = withSBool b $ \s -> keep s `seq` ()

-- | Check the translation and its strictness boundary.
main :: IO ()
main = do
    print (choose STrue, choose SFalse, shared True, shared False)
    print (keep @True undefined `seq` ())
    bridge <- try (evaluate (naiveBridge undefined)) :: IO (Either SomeException ())
    putStrLn $ case bridge of
        Left _ -> "naive-bridge-forces-input"
        Right _ -> "unexpected-lazy-bridge"
