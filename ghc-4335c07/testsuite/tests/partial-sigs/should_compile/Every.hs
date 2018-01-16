{-# LANGUAGE PartialTypeSignatures #-}
module Every where

every :: _ -> _ -> Bool
every _ [] = True
every p (x:xs) = p x && every p xs
