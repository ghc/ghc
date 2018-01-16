{-# LANGUAGE GADTs #-}

module Main where

data Rep t where
  Rint :: Rep Int
  Rchar :: Rep Char
  Runit :: Rep ()
  Rpair :: Rep a -> Rep b -> Rep (a,b)
  Rsum  :: Rep a -> Rep b -> Rep (Either a b)
  Rcon  :: String -> Rep t -> Rep t
  Rdata :: Rep i -> (t -> i) -> (i -> t) -> Rep t


int :: Rep Int
int = Rint

list :: Rep a -> Rep [a]
list x = Rdata (Rsum (Rcon "[]" Runit) (Rcon ":" (Rpair x (list x)))) h g
  where 
    h [] = Left ()
    h (x:xs) = Right (x,xs)

    g (Left ()) = []
    g (Right (x,xs)) = x:xs


addUp :: Rep a -> a -> Int
addUp Rint          n         = n
addUp (Rpair r s)   (x,y)     = addUp r x + addUp s y
addUp (Rsum r s)    (Left x)  = addUp r x
addUp (Rsum r s)    (Right x) = addUp s x
addUp (Rdata i f g) x         = addUp i (f x)
addUp (Rcon s r)    x         = addUp r x
addUp v             x         = 0


main = print (addUp (list int) [1,2,4])
