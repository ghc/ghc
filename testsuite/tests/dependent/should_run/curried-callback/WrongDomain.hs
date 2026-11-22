{-# LANGUAGE CPP, DataKinds, GADTs, NoExperimentalDependentTypes #-}
module WrongDomain where
import CompareAt
import Data.Proxy
newtype Bit = Bit Bool

#ifdef WRONG_INPUT
bad :: (Bit -> Bool -> Ordering) -> Bool -> Bool -> IO ()
bad f x y = print (keepFamilyCompare f x y)
#elif defined(WRONG_RESULT)
bad :: (Bool -> Bool -> Bool) -> Bool -> Bool -> IO ()
bad f x y = print (keepFamilyCompare f x y)
#elif defined(ABSTRACT_TAG)
bad :: forall tag. Proxy tag -> (Bool -> Result tag) -> Bool -> Bool -> IO ()
bad _ f x y = print (keepFamilyCompare f x y)
#endif
