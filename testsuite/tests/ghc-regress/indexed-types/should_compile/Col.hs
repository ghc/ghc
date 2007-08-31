{-# LANGUAGE TypeFamilies #-}

module Col  where 

type family Elem c

type instance Elem [e] = e

class Col c where
 singleton :: Elem c -> c
 add :: c -> Elem c -> c

instance Col [e] where
 singleton = \x -> [x]
 add = flip (:)

