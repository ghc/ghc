{-# LANGUAGE TemplateHaskellQuotes #-}
module TTH_top_splice where

-- Should be a compile time error as TemplateHaskell is not enabled.

qux = $$([|| 1 ||])


