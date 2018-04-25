{-# LANGUAGE ScopedTypeVariables, TypeOperators, GADTs #-}
module Main
       ( main -- :: IO ()
       ) where
import Data.Data
import Data.Ratio

main :: IO ()
main =
  let bad = gmapT (\(x :: b) ->
              case eqT :: Maybe (b :~: Integer) of
                Nothing -> x;
                Just Refl -> x * 2) (1 % 2) :: Rational
  in print (bad == numerator bad % denominator bad)
