{-# LANGUAGE FunctionalDependencies, FlexibleContexts #-}

class C a where 

class D a where 
 dop :: a -> a    

instance C a => D [a] where 
 dop = undefined

class J a b | a -> b 
 where j :: a -> b -> () 

instance J Bool Int where 
 j = undefined
   
foo :: D [Int] => ()
foo = j True (head (dop [undefined]))

main = return ()

