{-# LANGUAGE OverloadedRecordFields, ExistentialQuantification, RankNTypes #-}

-- x is existential (naughty)
data T a = forall e . MkT { x :: e }

-- x and y are higher-rank
data U = MkU { x :: forall a . a -> a }
       | MkU2 { y :: (forall b . b) -> () }

-- Should generate sensible unsolved constraint errors
a = x (MkT True) :: Bool
b = x (MkU id)
c = y (MkU2 (\ _ -> ()))
d = x ((\ x -> x) :: Int -> Int) :: Bool

e :: (T Int) { foo :: t } => t
e = x (MkT True)

main = return ()
