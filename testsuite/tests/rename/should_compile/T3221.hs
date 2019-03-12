{-# OPTIONS_GHC -Werror -fwarn-unused-binds #-}

-- Test #3221: the constructors are used by the deriving
--                  clause, even though they are not exported

module T3221( Foo ) where

data Foo = Bar | Baz  deriving (Show,Read)
