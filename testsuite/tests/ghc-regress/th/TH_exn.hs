{-# OPTIONS -fth #-}

-- This one generates an exception in the TH 
-- code which should be caught in a civilised way

module TH where

$( [d| |] >>= return.tail)

