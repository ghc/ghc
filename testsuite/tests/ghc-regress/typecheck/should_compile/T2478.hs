{-# OPTIONS -fglasgow-exts #-}

module ShouldCompile where

  data Eq t => TrafoE t = forall env2 . TrafoE Int t

  newSRef () = TrafoE 
