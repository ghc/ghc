-- an options pragma that needs more than 1k of buffer to read
{-# OPTIONS_GHC
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                -w #-}

module Pragma002 () where

-- This will make a warning if the pragam isn't picked up
foo _ = ()
foo _ = ()

