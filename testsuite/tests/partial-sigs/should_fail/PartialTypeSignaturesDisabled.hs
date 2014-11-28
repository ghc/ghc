module PartialTypeSignaturesEnabled where

-- The PartialTypeSignatures extension should be enabled
foo :: _ -> _
foo x = not x
