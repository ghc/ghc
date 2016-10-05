module T12124 where

data Whoops = Whoops Int Int

foo :: Maybe Int
foo = return (case Whoops 1 2 of
                 Whoops a -> a
                 _ -> 0)
