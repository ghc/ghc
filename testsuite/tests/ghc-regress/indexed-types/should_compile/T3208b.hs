{-# LANGUAGE TypeFamilies #-}

module T3208b where
  
class SUBST s where
    type STerm s

class OBJECT o where
    type OTerm o
    apply :: (SUBST s, OTerm o ~ STerm s) => s -> o

fce' :: (OTerm a ~ STerm a, OBJECT a, SUBST a) => a -> c
fce' f = fce . apply $ f

fce :: (OTerm a ~ STerm a, OBJECT a, SUBST a) => a -> c
fce f = fce' f
