class  NewFunctor f  where
    inj         :: a -> f a
    surj         :: f a -> a

data N a = Z a 

ninj x = (Z x) 
nsurj (Z x) = x

instance NewFunctor N where
    inj = ninj
    surj = nsurj

twice :: NewFunctor f => a -> f (f a)
twice x = inj(inj x)

undo :: NewFunctor f => f (f a) -> a
undo x = surj(surj x)

main = putStr (undo (Z (Z "hello world\n")))
