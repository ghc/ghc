{-# LANGUAGE TypeFamilies, ScopedTypeVariables #-}

module T2102 where

type family Cat ts0 ts
type instance Cat ()      ts' = ts'
type instance Cat (s, ts) ts' = (s, Cat ts ts')

class (Cat ts () ~ ts) => Valid ts
instance Valid () -- compiles OK
instance Valid ts => Valid (s, ts) -- fails to compile

-- need to prove Cat (s, ts) () ~ (s, Cat ts ())
-- for the superclass of class Valid.
-- (1) From Valid ts: Cat ts () ~ ts
-- (2) Therefore:     (s, Cat ts ()) ~ (s, ts)

coerce :: forall f ts. Valid ts => f (Cat ts ()) -> f ts
coerce x = x
