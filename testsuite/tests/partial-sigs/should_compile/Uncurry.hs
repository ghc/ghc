{-# LANGUAGE PartialTypeSignatures #-}
module Uncurry where

unc :: (_ -> _ -> _) -> (_, _) -> _
unc = uncurry
