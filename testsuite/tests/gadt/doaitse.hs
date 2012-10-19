{-# LANGUAGE GADTs, ExistentialQuantification, ScopedTypeVariables,
             RankNTypes #-}

-- Here's an example from Doaitse Swiestra (Sept 06)
-- which requires use of scoped type variables
-- 
-- It's a cut-down version of a larger program
--
-- It's also one which was sensitive to syntactic order
-- in GHC 6.4; but not in 6.6

module ShouldCompile where

data Exists f = forall a . Exists (f a)

data Ref env a where
  Zero :: Ref (a,env') a
  Suc  :: Ref env' a -> Ref (x,env') a

data Find env final = Find (forall a . Ref env a -> Maybe (Ref final a))

data Equal a b where
  Eq :: Equal a a

sym :: Equal a b -> Equal b a
sym Eq = Eq

match' :: Ref env' a -> Ref env'' a -> Bool
match' _ _ = True

match :: Ref env a -> Ref env b -> Maybe (Equal a b)
match Zero Zero      = Just Eq
match (Suc x)(Suc y) = match x y
match _ _            = Nothing

-- Notice the essential type sig for the argument to Exists
f1 :: forall env.  (Exists (Ref env)) -> Exists (Find  env)
f1 (Exists (ref1 :: Ref env b))
  = Exists ( Find (\ ref2 -> case match ref2 ref1 of
                                Just Eq -> Just Zero
                                _       -> Nothing
                  ):: Find env (b,())
   )


-- same as 'f1' except that 'ref1' and 'ref2' are swapped in the  
-- application of 'match'
f2 :: forall env.  (Exists (Ref env)) -> Exists (Find  env)
f2 (Exists (ref1 :: Ref env b))
   = Exists (Find (\ ref2 -> case match ref1 ref2 of
                                    Just Eq -> Just Zero
                                    _       -> Nothing
                    ) :: Find env (b,())
     )

