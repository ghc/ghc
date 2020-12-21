{-# LANGUAGE NoIncomplete #-}
-- {-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -fdefer-incomplete-record-updates #-}

module A where

data PartialRec = No
                | Yes { a :: Int, b :: Bool }

update r = r { b = False }
