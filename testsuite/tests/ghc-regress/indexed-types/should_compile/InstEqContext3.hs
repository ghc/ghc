{-# LANGUAGE TypeFamilies #-}

module InstEqContext  where 


{- encoding of 
 -	class C a | -> a
 - with extra indirection
 -}
class a ~ Int => D a 
instance D Int

class D a => C a 
instance C Int

unC :: (C a) => a -> Int
unC i = undefined

test :: Int
test = unC undefined
