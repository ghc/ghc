
{-# LANGUAGE Arrows, ExistentialQuantification #-}

-- Crashed GHC 6.4 with a lint error
-- because of the existential

-- Esa Pulkkinen <esa.pulkkinen@kotiposti.net>
-- Thomas Jäger <ThJaeger@gmail.com>

module ShouldFail where

class Foo a where foo :: a -> ()
data Bar = forall a. Foo a => Bar a

get :: Bar -> ()
get = proc x -> case x of Bar a -> foo -< a

-- This should be rejected because the left side of -< (here foo)
-- should be treated as being outside the scope of the proc: it can't
-- refer to the local variables x and a (this is enforced), nor the
-- existentially quantified type variable introduced by unwrapping x.
