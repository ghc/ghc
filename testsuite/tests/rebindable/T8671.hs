{-# LANGUAGE RebindableSyntax #-}

module T8671 where

import Data.Void
import Prelude ((.), ($), Int, id, Num(..))


(>>) :: (b -> c) -> (a -> b) -> (a -> c)
(>>) = (.)


return :: Void -> Void
return = absurd


run :: a -> (a -> b) -> b
run x f = f x


result :: Int
result = run 8 $ do
    \n -> n * n
    id
    (+ 7)
    (* 2)
