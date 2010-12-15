{-# LANGUAGE TypeFamilies #-}

-- This should fail

module T3208b where
  
class SUBST s where
    type STerm s

class OBJECT o where
    type OTerm o
    apply :: (SUBST s, OTerm o ~ STerm s) => s -> o

fce' :: (OTerm a ~ STerm a, OBJECT a, SUBST a) => a -> c
fce' f = fce (apply f)
-- f :: a
-- apply f :: (OBJECT a, SUBST a, OTerm o ~ STerm a) => o
-- fce called with a=o, gives wanted (OTerm o ~ STerm o, OBJECT o, SUBST o)


fce :: (OTerm a ~ STerm a, OBJECT a, SUBST a) => a -> c
fce f = fce' f
