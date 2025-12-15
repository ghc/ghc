{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE ImplicitParams #-}

-- #1445

module Bug where

f :: () -> (?p :: ()) => () -> ()
f _ _ = ()

g :: (?p :: ()) => ()
g = f () ()
