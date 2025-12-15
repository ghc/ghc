{-# OPTIONS_GHC -ddump-simpl #-}
module T22097 where
import T22097a ( isEven )

main :: IO ()
main = print $ isEven @Int 10
