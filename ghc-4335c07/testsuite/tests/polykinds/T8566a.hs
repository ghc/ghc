{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
module T8566a where

data Field = forall k. APP k [Field]

data InField (u :: Field) :: * where
  A :: AppVars t (ExpandField args) -> InField (APP t args)

type family ExpandField (args :: [Field]) :: [*]
type family AppVars (t :: k) (vs :: [*]) :: *

-- This function fails to compile, because we discard
-- 'given' kind equalities.  See comment 7 in Trac #8566
-- This is really a bug, I claim
unA :: InField (APP t args) -> AppVars t (ExpandField args)
unA (A x) = x

