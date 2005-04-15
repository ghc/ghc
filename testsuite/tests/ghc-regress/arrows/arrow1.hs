{-# OPTIONS -farrows -fglasgow-exts #-}

-- Crashed GHC 6.4 with a lint error
-- because of the existential

-- Esa Pulkkinen <esa.pulkkinen@kotiposti.net>
-- Thomas Jäger <ThJaeger@gmail.com>

module ShouldCompile where

class Foo a where foo :: a -> ()
data Bar = forall a. Foo a => Bar a

get :: Bar -> ()
get = proc x -> case x of Bar a -> do foo -< a
