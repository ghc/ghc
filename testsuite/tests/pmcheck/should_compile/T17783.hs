module Bug where

data PartialRec = No
                | Yes { a :: Int, b :: Bool }

update No = No
update r@(Yes {}) = r { b = False }


data T = A { x :: Int } | B

f r@A{} = r { x = 3 }
f _     = B
