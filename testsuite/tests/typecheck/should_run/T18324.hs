{-# LANGUAGE ImpredicativeTypes #-}

type Id = forall a. a -> a

t :: IO Id
t = return id

p :: Id -> (Bool, Int)
p f = (f True, f 3)

foo1 = t >>= \x -> return (p x)

foo2 = do { x <- t ; return (p x) }

blah x y = return (3::Int)

main = do x <- foo2
          y <- foo1
          putStrLn $ show x
          putStrLn $ show y
