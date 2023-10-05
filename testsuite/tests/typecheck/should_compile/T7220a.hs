{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module T7220a where

class C a b | b -> a

data X = X
data Y = Y

type family TF b

f :: (forall b. (C a b, TF b ~ Y) => b) -> X
-- This type is really ambiguous
-- GHC 7.8 didn't detect that, and accepted the type, but would fail
-- when given    g :: <the same type>
--               g x = f x
-- But it would succeed if you said just
--               g = f
-- Now we fail in all ways!

-- But with simple subsumption (#17775) we
-- no longer get an ambiguity check here

f _ = undefined
