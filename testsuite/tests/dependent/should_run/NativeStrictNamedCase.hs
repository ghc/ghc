{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Type.Equality ((:~:) (Refl))

data Counter = Z | S Counter

data Token (n :: Counter) = Token

observe :: (Counter -> Bool) -> foreach (n :: Counter) -> Token n -> Bool
observe f n _ = case f (S n) of
    False ->
        let witness :: f (S n) :~: 'False
            witness = Refl
         in case witness of Refl -> False
    True ->
        let witness :: f (S n) :~: 'True
            witness = Refl
         in case witness of Refl -> True

{-# NOINLINE always #-}
always :: Counter -> Bool
always _ = True

main :: IO ()
main = do
    let delayed :: Counter
        delayed = error "constructor field must stay lazy"
    print (observe always delayed (error "token must stay lazy"))
