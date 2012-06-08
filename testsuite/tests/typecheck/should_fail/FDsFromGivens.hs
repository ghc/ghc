{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts, GADTs #-}

module FDsFromGivens where 

class C a b | a -> b where 
   cop :: a -> b -> ()


data KCC where 
  KCC :: C Char Char => () -> KCC


{- Failing, as it righteously should! 
g1 :: (C Char [a], C Char Bool) => a -> () 
g1 x = ()
-}

f :: C Char [a] => a -> a
f = undefined

bar (KCC _) = f

   
   