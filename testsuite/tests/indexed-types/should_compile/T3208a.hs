{-# LANGUAGE TypeFamilies #-}

module T3208a where
  
class SUBST s where
    type STerm s

class OBJECT o where
    type OTerm o
    apply :: (SUBST s, OTerm o ~ STerm s) => s -> o

fce' f = fce . apply $ f

fce f = fce' f
