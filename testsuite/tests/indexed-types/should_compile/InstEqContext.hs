{-# LANGUAGE TypeFamilies #-}

module InstEqContext  where 


{- encoding of 
 -	class C a | -> a
 -}
class a ~ Int => C a 

instance C Int

unC :: (C a) => a -> Int
unC i = undefined

test :: Int
test = unC undefined
