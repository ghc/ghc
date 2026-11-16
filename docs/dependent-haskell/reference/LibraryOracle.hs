{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Exception (SomeException, evaluate, try)
import Data.Kind (Type)
import Data.Singletons

-- | A Boolean witness for the upstream singleton API.
data SBool (b :: Bool) where
    STrue :: SBool True
    SFalse :: SBool False

type instance Sing @Bool = SBool

instance SingKind Bool where
    type Demote Bool = Bool
    fromSing STrue = True
    fromSing SFalse = False
    toSing True = SomeSing STrue
    toSing False = SomeSing SFalse

-- | Select a lifted result type.
type family Select (b :: Bool) :: Type where
    Select True = Int
    Select False = Bool

-- | Refine the result type by matching the witness.
choose :: forall b. Sing b -> Select b
choose STrue = 1
choose SFalse = False

-- | Preserve an index without evaluating its witness.
keep :: forall b. Sing b -> Select b -> Select b
keep _ x = x

-- | Share one reified runtime index across dependent calls.
shared :: Bool -> Bool
shared b = withSomeSing @Bool b $ \s -> case s of
    STrue -> keep s (choose s) == 1
    SFalse -> keep s (choose s) == False

-- | Check the upstream API and its strict reification boundary.
main :: IO ()
main = do
    print (choose STrue, choose SFalse, shared True, shared False)
    print (keep @True undefined `seq` ())
    strict <-
        try (evaluate (withSomeSing @Bool undefined (\_ -> ()))) ::
            IO (Either SomeException ())
    putStrLn $ case strict of
        Left _ -> "library-reification-is-strict"
        Right _ -> "unexpected-lazy-library-reification"
