{-# LANGUAGE MultiParamTypeClasses #-}

-- Tests top-level ambiguity resolution
-- This made a post-6.4 GHC fall over in TcSimplify

module ShouldFail where 

class Foo a
instance Foo (a -> b)

foo :: Foo a => a -> ()
foo = undefined

class Bar a r
-- The same happens if we use fundeps:
-- class Bar a r | r -> a

bar :: Bar a r => r -> ()
bar = undefined

test = foo bar
