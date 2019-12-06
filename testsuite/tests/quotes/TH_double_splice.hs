{-# LANGUAGE TemplateHaskellQuotes #-}
module TH_double_splice where

-- Should be a compile time error as TemplateHaskell is not enabled.

foo = [| $($(error "should not happen")) |]

