{-# LANGUAGE AllowAmbiguousTypes, TypeFamilies, TypeOperators #-}

import Data.Kind
import Data.Typeable

unsafeCoerce :: a -> b
unsafeCoerce x = case eqT :: Maybe (Type :~: Constraint) of
                   Nothing -> error "No more bug!"
                   Just r  -> error "Bug!"

main :: IO ()
main = let x :: Int
           x = 42
       in print (unsafeCoerce x :: Double)
