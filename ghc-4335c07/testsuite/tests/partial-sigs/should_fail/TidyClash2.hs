{-# LANGUAGE NamedWildCards, ScopedTypeVariables #-}
module TidyClash2 where

{-
The bug that TidyClash2 tries to tickle is the following: when GHC generalises
over wild cards it uses t, t1, t2, ... as type variables. When the user already
used the t type variable in the same signature, the tidier should respect this
and use t1, t2, ... for generalised wild cards. So _ -> -> t should result
in t1 -> t2 -> t. Before the bug fix, the actual output would have been
something like t -> t1 -> t2.
-}

barry :: forall t. _ -> _ -> t
barry (x :: _) (y :: _) = undefined :: _
