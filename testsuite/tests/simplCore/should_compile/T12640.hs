{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module T12640 where

class Ex2 a b c | a b -> c where
  (+++) :: a -> b -> c

instance Ex2 Bool Bool Bool where
  (+++) = ex_or

{-# INLINE [2] ex_or #-}
ex_or = (||)

{-# RULES
"force-inline" forall a b . ex_or a b = a || b
 #-}

main = print (True +++ True)

