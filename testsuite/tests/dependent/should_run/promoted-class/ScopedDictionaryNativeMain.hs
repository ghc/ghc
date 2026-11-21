{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, ScopedTypeVariables #-}
module Main (main) where

import System.Environment (getArgs)

class Choose a where
    choose :: a -> Bool

instance Choose Bool where
    choose x = x

data Inverted = Inverted Bool

instance Choose Inverted where
    choose (Inverted x) = not x

data Tagged (b :: Bool) where
    Tagged :: Bool -> Tagged b

tagBool :: foreach (b :: Bool) -> Tagged b
tagBool b = Tagged @b b

chooseTaggedBoth :: forall a b. (Choose b, Choose a)
                 => foreach (x :: a) -> foreach (y :: b)
                 -> (Tagged (choose x), Tagged (choose y))
chooseTaggedBoth x y = (tagBool (choose x), tagBool (choose y))

chooseTagged :: forall a. Choose a => foreach (x :: a) -> Tagged (choose x)
chooseTagged x = tagBool (choose x)

forward :: forall a. Choose a => foreach (x :: a) -> Tagged (choose x)
forward x = chooseTagged x

forwardBoth :: forall a b. (Choose a, Choose b)
            => foreach (x :: a) -> foreach (y :: b)
            -> (Tagged (choose x), Tagged (choose y))
forwardBoth x y = chooseTaggedBoth x y

observe :: Choose a => a -> Bool
observe x = case forward x of
    Tagged value -> value

observeBoth :: (Choose a, Choose b) => a -> b -> (Bool, Bool)
observeBoth x y = case forwardBoth x y of
    (Tagged lhs, Tagged rhs) -> (lhs, rhs)

observeGround :: Bool -> Bool
observeGround x = case chooseTagged x of
    Tagged value -> value

data Ignored = Ignored
instance Choose Ignored where
    choose _ = True

data Unobserved = Unobserved
instance Choose Unobserved where
    choose _ = error "the method was forced"

ignorePayload :: Choose a => a -> ()
ignorePayload x = case forward x of
    Tagged _ -> ()

main :: IO ()
main = do
    args <- getArgs
    if args == ["force"]
        then print (observe Unobserved)
        else do
            let input = args == ["true"]
            print (observe input)
            print (observeBoth input (Inverted input))
            print (observeGround input)
            print (observe True)
            print (observe (error "the ignored operand was forced" :: Ignored))
            print (ignorePayload Unobserved)
