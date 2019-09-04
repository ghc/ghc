{-# LANGUAGE TypeApplications #-}

module T11313 where

x = fmap @(*)

-- test error message output, which was quite silly before
