{-# LANGUAGE DataKinds, ExistentialQuantification, MagicHash, RankNTypes,
             TypeApplications #-}

import GHC.Prim (Int#)
import GHC.Records (HasField, getField)

data T = MkT { foo :: forall a . a -> a }
data U = forall b . MkU { bar :: b }
data V = MkV { baz :: Int# }

-- This should fail because foo is higher-rank.
x = getField @"foo" (MkT id)

-- This should fail because bar is a naughty record selector (it
-- involves an existential).
y = getField @"bar" (MkU True)

-- This should fail because baz is not of kind Type.
z = getField @"baz" (MkV 3#)

main = return ()
