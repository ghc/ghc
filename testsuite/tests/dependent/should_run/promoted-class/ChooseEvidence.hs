{-# LANGUAGE CPP, ScopedTypeVariables #-}
#ifdef PROMOTED
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs #-}
#ifdef APPLICATIONS
{-# LANGUAGE BangPatterns #-}
#endif
#else
{-# LANGUAGE NoExperimentalDependentTypes #-}
#endif
module ChooseEvidence where

class Choose a where
    choose :: a -> Bool

#ifdef MISSING_GIVEN
chooseBoth :: forall a b. Choose a => a -> b -> (Bool, Bool)
#else
chooseBoth :: forall a b. (Choose b, Choose a) => a -> b -> (Bool, Bool)
#endif
chooseBoth x y = (choose x, choose y)

instance Choose Bool where
    choose x = x

newtype Inverted = Inverted Bool

instance Choose Inverted where
    choose (Inverted x) = not x

#ifdef PROMOTED
data Tagged (b :: Bool) where
    TaggedTrue :: Tagged 'True
    TaggedFalse :: Tagged 'False

tagBool :: foreach (b :: Bool) -> Tagged b
tagBool True = TaggedTrue
tagBool False = TaggedFalse

chooseTaggedBoth :: forall a b. (Choose b, Choose a)
                 => foreach (x :: a) -> foreach (y :: b)
                 -> (Tagged (choose x), Tagged (choose y))
chooseTaggedBoth x y = (tagBool (choose x), tagBool (choose y))

#ifdef APPLICATIONS
chooseTagged :: forall a. Choose a => foreach (x :: a) -> Tagged (choose x)
chooseTagged x = tagBool (choose x)

forward :: forall a. Choose a => foreach (x :: a) -> Tagged (choose x)
forward x = chooseTagged x

forwardBoth :: forall a b. (Choose a, Choose b)
            => foreach (x :: a) -> foreach (y :: b)
            -> (Tagged (choose x), Tagged (choose y))
forwardBoth x y = chooseTaggedBoth x y

concrete :: Bool -> ()
concrete x = case chooseTagged x of !_ -> ()
#endif
#endif
