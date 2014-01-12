{-# LANGUAGE TemplateHaskell #-}
module T5566 where

class C a where
     c :: a

$([d| instance C Int where c = 0 |] )

data D = D

$([d| instance C D where c = D |] )

$([d| instance Show D where show _ = "D" |] )
