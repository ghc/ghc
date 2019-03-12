{-# LANGUAGE RankNTypes, NamedWildCards #-}

-- See #11098

module NamedWildcardExplicitForall where

foo :: forall _a . _a -> _a                -- _a is a type variable
foo = not

bar :: _a -> _a                            -- _a is a named wildcard
bar = not

baz :: forall _a . _a -> _b -> (_a, _b)    -- _a is a variable, _b is a wildcard
baz x y = (not x, not y)

qux :: _a -> (forall _a . _a -> _a) -> _a  -- the _a bound by forall is a tyvar
qux x f = let _ = f 7 in not x             -- the other _a are wildcards
