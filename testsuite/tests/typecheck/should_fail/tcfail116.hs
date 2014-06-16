module ShouldFail where

-- in Haskell 98, methods must mention the class variable
-- (SourceForge bug #756454).
class Foo a where
    bug :: ()
