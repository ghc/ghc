class  NewFunctor f  where
    new_fmap         :: (a -> b) -> f a -> f b

data N a = Z a | S (N a)

nmap f (Z x) = Z (f x)
nmap f (S n) = S (nmap f n)

tag (Z x) = x
tag (S n) = tag n

instance NewFunctor N where
    new_fmap = nmap

--class  Strange f  where
--    zero         :: a -> f a
--    suc         :: f a -> f a
--    tag         :: f a -> a


--class  FMonad m  where
--    (>>=)       :: m a -> (a -> m b) -> m b
--    (>>)        :: m a -> m b -> m b
--    return      :: a -> m a
--    fail	:: String -> m a
--
--    m >> k      =  m >>= \_ -> k
--    fail s      = error s




--instance Strange N
--  where
--   zero x = Z x
--   suc y = S y
--   tag n = gettag n

twice :: NewFunctor f => (a -> a) -> f a -> f a
twice f x = new_fmap f (new_fmap f x)

main = putStr (tag (nmap (\x -> x) (Z "hello world\n")))
--main = putStr (tag (nmap (\x -> x) (Z "hello world\n")))
-- main = putStr (tag {- (twice (\x -> x) -}  (Z "hello world\n"))
