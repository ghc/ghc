{-# LANGUAGE PartialTypeSignatures #-}

module Tc272 where

x =
  let a :: _
      a = ()
   in ()

x' = {-# OPTIONS_LOCAL -Wno-partial-type-signatures #-}
  let a :: _
      a = ()
   in ()

