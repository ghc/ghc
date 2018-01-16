{-# LANGUAGE IncoherentInstances, MultiParamTypeClasses, FlexibleInstances #-}

-- Liberated IncoherentInstances behavior (#8141)

class C a b where foo :: (a,b)

instance C Int b where foo = undefined
instance C a Int where foo = undefined

x :: (Int, Int)
x = foo

main = return ()
