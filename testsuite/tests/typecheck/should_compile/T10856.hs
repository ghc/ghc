{-# LANGUAGE ExistentialQuantification #-}
module T10856 where

data Rec a b = Show a => Mk { a :: a, b :: b }

update :: Show c => c -> Rec a b -> Rec c b
update c r = r { a = c }
