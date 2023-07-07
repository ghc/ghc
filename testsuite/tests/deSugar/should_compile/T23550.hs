{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

-- We didn't need -XMonoLocalBinds to trigger #T23550. But it is sufficient. And
-- since it is the simpler case, to avoid confusing the issue, the test uses
-- -XMonoLocalBinds.
{-# LANGUAGE MonoLocalBinds #-}

module T23550 where

emptyGraph :: Int -> ()
emptyGraph stms = undefined
  where
    -- The wildcard is important: it's a PatBind, and it's the case we're
    -- testing. For some reason it also seems that binding no variable matters.
    -- Otherwise the (mutually recursive) dictionaries are bound at toplevel
    -- instead of locally.
    _ = analyseStms stms


class Aliased rep where
instance AliasedOp (SOAC Aliases) => Aliased Aliases where

class AliasedOp op where
instance Aliased rep => AliasedOp (SOAC rep) where

analyseStms :: AliasedOp (SOAC Aliases) => Int -> ()
analyseStms = undefined

data Aliases
data SOAC rep
