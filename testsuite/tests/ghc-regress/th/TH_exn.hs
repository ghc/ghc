{-# OPTIONS -fth #-}

-- A (head []) failure within a TH splice
-- This one generates an exception in the TH 
-- code which should be caught in a civilised way

module TH where

$( do { ds <- [d| |] 
      ; return (tail ds) }
 )

