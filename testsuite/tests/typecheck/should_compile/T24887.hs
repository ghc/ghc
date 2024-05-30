{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables, TypeAbstractions #-}

module Data.Array.Nested.Internal where

import Data.Coerce (coerce)


type family Id (b :: Bool) where
  Id b = b

newtype Ranked b a = MkRanked (Mixed b a)


data family Mixed (b :: Bool) a

newtype instance Mixed b1 (Mixed b2 a) = M_Nest (Mixed False a)

newtype instance Mixed b (Ranked c a) = M_Ranked (Mixed b (Mixed (Id c) a))
--
-- newtype MixedNT b c a = M_Ranked (Mixed b (Mixed (Id c) a))
-- And we    Mixed b (Ranked c a) ~N MixedNT b c a

idMixed :: Mixed b a -> Mixed b a
idMixed = undefined

bar :: forall a b c. Mixed b (Ranked c a) -> Mixed b (Ranked c a)
bar (M_Ranked @_ @c @a arr)
  = coerce (idMixed arr)                          -- fails
  -- = coerce (idMixed @_ @(Mixed (Id c) a) arr)  -- ok
  -- = coerce (id arr)                            -- ok
  -- = let r = idMixed arr in coerce r            -- ok

{-

arr :: Mixed b (Mixed (Id c) a)

idMixed arr :: Mixed b (Mixed (Id c) a)

coerce does
  [W] (Mixed b (Mixed (Id c) a)) ~R (Mixed b (Ranked c a))
--> Unwrap lHS
  [W] Mixed False a ~R  (Mixed b (Ranked c a))
--> Unwrap RHS
  [W] Mixed False a ~R  Mixed b (Mixed (Id c) a)
--> Unwrap RHS again
  [W] Mixed False a ~R  Mixed False a


That is true if
  Mixed (Id c) a  ~N  Ranked c a

Also
  Mixed b (Ranked c a) ~N MixedNT b c a

  [W] (Mixed b (Mixed (Id c) a)) ~R (Mixed b (Ranked c a))
-->
  [W] (Mixed b (Mixed (Id c) a)) ~R (MixedNT b c a)
--> unwrap NT
  [W] (Mixed b (Mixed (Id c) a)) ~R (Mixed b (Mixed (Id c) a))
-}
