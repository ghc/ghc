{-# LANGUAGE DataKinds, GADTs, RankNTypes, TypeAbstractions, TypeApplications #-}

module LambdaMain where

import Builder (Builder, empty, shadowed, singleton, throwing)
import Control.Exception (ErrorCall, evaluate, try)
import Data.List (isInfixOf)
import Vector

observe :: Builder -> Bool -> [Bool]
observe builder input = consume builder input
  where
    consume :: (Bool -> Vec n Bool) -> Bool -> [Bool]
    consume run b = vecToList (run b)

    vecToList :: Vec n Bool -> [Bool]
    vecToList Nil = []
    vecToList (Cons b rest) = b : vecToList rest

ignored :: Builder -> Bool
ignored builder = forget builder
  where
    forget :: (Bool -> Vec n Bool) -> Bool
    forget _ = True

forward :: Builder -> Builder
forward = id

instantiate :: (forall (n :: Nat). Builder) -> Builder
instantiate builder = builder @'Z

main :: IO ()
main = do
    print (observe singleton True, observe singleton False,
           observe empty (error "empty builder forced its argument"))
    print (throwing `seq` True, ignored (error "opening forced the package"))
    print (observe (forward (\b -> Cons b Nil)) True,
           observe ((\_ -> Nil) :: Builder) False, observe (shadowed @'Z) False,
           observe (instantiate (\ @n b -> Cons b Nil)) True)
    outcome <- try @ErrorCall (evaluate (observe throwing True))
    case outcome of
        Left ex | "builder body ran" `isInfixOf` show ex ->
            putStrLn "forcing the result entered the builder body"
        _ -> error "expected the builder body exception"
