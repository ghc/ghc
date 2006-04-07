class  EMonad m  where
    aaaaa       :: m a -> (a -> m b) -> m b
    bbbbb        :: m a -> m b -> m b

    bbbbb m k      =  aaaaa m (\_ -> k)
                  -- = \M \A \B -> \m:(M A) -> \k:(M B) -> aaaaa M A B m (\_:A -> k: M B)
                  --   Free types must include "A"!!!

main = putStr "hello world\n"


