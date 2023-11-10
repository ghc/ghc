{-# LANGUAGE MonoLocalBinds          #-}
{-# LANGUAGE QuantifiedConstraints   #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module T22912 where


class    c => Exactly c
instance c => Exactly c
class    c => Implies c

data Dict c = c => Dict

anythingDict :: forall c. Dict c
anythingDict = go
  where
    go :: (Exactly (Implies c) => Implies c) => Dict c
    go = Dict

-- This is clearly wrong: we shouldn't be able to produce evidence
-- for any constraint whatsoever! However, GHC can be tricked into
-- producing a bottom dictionary.
-- This test checks that it emits an appropriate warning when doing so,
-- to allow users to adapt their code before we plug the hole completely.
