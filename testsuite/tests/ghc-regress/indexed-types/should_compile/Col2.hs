{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Col  where 

type family Elem c

type instance Elem [e] = e

class (Eq (Elem c)) => Col c where
  count     :: Elem c -> c -> Int

instance Eq e => Col [e] where
  count	    x = length . filter (==x)
