{-# LANGUAGE OverloadedRecordFields, NoMonomorphismRestriction, ExistentialQuantification #-}

data T = forall e . MkT { x :: e -> e }

-- Without the monomorphism restriction, this could be given type
--     v :: T { x :: t } => t
-- but it is inferred as T { x :: GetResult T "x" }, which doesn't get
-- quantified over because it has no free variables.
v = x (MkT id)

main = print ()
