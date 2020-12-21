{-# LANGUAGE NoIncomplete #-}
-- {-# LANGUAGE NoFieldSelectors #-}

module A where

data PartialRec = No
                | Yes { a :: Int, b :: Bool }

update r = r { b = False }
