{-# OPTIONS_GHC -Wincomplete-record-updates #-}

module Bug where

data PartialRec = No
                | Yes { a :: Int, b :: Bool }

update No = No
update r@(Yes {}) = r { b = False }
