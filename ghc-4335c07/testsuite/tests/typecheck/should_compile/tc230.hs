{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE ImplicitParams, RankNTypes #-}

-- Trac #1445

module Bug where

f :: () -> (?p :: ()) => () -> ()
f _ _ = ()

g :: (?p :: ()) => ()
g = f () ()
