{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Defaulting1MROff where

-- Even without the MR, this signature forces monomorphism,
-- because of the partial signature with no '=>'
alpha :: _
alpha = 3
