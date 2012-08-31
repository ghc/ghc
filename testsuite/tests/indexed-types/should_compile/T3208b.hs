{-# LANGUAGE TypeFamilies #-}

-- This should fail

module T3208b where
  
class SUBST s where
    type STerm s

class OBJECT o where
    type OTerm o
    apply :: (SUBST a, OTerm o ~ STerm a) => a -> o

fce' :: (OTerm a ~ STerm a, OBJECT a, SUBST a) => a -> c
fce' f = fce (apply f)
-- f :: a
-- apply f :: (SUBST a, OBJECT o, OTerm o ~ STerm a) => o
-- fce called with a=o, gives wanted (OTerm o ~ STerm o, OBJECT o, SUBST o)
-- o is a unif var.

fce :: (OTerm o ~ STerm o, OBJECT o, SUBST o) => o -> c
fce f = fce' f


{-  (OTerm a ~ STerm a, OBJECT a, SUBST a)
 => (SUBST a, OBJECT o, OTerm o ~ STerm a, OTerm o ~ STerm o, OBJECT o, SUBST o)
    
 =     OTerm o ~ u1
       STerm o ~ u1
       Sterm a ~ u1

 =  (SUBST a, OBJECT o, STerm o ~ STerm a, OTerm o ~ STerm o, OBJECT o, SUBST o)
-}