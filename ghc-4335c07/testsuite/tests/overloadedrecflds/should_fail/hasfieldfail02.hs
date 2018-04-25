{-# LANGUAGE DataKinds, ExistentialQuantification, MagicHash, RankNTypes,
             TypeApplications #-}

import GHC.Records (HasField(..))

data T = MkT { foo :: forall a . a -> a }
data U = forall b . MkU { bar :: b }

-- This should fail because foo is higher-rank.
x = getField @"foo" (MkT id)

-- This should fail because bar is a naughty record selector (it
-- involves an existential).
y = getField @"bar" (MkU True)

main = return ()
