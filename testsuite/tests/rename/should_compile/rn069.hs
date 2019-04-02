{-# OPTIONS -Wno-name-shadowing #-}

module Rn069 where

f = {-# OPTIONS_LOCAL -Wname-shadowing #-}
 let a = () in
 let a = 5 in
  a + a

f' =
 let a = () in
 let a = 5 in
  a + a

