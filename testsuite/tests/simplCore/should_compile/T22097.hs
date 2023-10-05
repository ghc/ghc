{-# OPTIONS_GHC -ddump-simpl #-}
{-# LANGUAGE TypeApplications #-}
module T22097 where
import T22097a ( isEven )

main :: IO ()
main = print $ isEven @Int 10
