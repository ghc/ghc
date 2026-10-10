{-# LANGUAGE ImpredicativeTypes #-}
module T27917 where

pair :: forall p q. Maybe p -> q -> (p, q)
pair _ _ = undefined

ids :: [forall a. a -> a]
ids = []

use :: forall s. s -> ()
use _ = ()

isMaybe :: Maybe a -> ()
isMaybe (Just {}) = ()

-- #27917 reported that foo1 was accepted but foo2 was rejected!
foo1 = \y -> (isMaybe y, use (pair y ids))
foo2 = \y -> (use (pair y ids), isMaybe y)

{-   y :: Maybe alpha
     pair @k1 @k2 y ids

Maybe k1 ~ Maybe alpha
k2 ~ [ forall a. a->a ]

use @ss (pair ...)

ss ~ (k1, [forall a. a->a])
-}
