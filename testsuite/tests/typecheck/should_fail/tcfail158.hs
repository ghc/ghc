{-# LANGUAGE ExplicitForAll #-}

-- This one actually crashed in 6.4.1
-- There's a kind error in the signature for bar, 
-- and we were recovering, and then crashing when we found
-- a scoped type variable not in scope

 data Val v sm = Val
 foo :: forall v sm. Val v sm
 foo = undefined
   where foo1 :: Val v sm
         foo1 = bar
 -- Correct type signature: bar :: forall v sm. Val v sm
 bar :: forall v. Val v
 bar = undefined foo
