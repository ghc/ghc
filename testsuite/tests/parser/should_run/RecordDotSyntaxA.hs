module RecordDotSyntaxA where

data Foo = Foo { foo :: Int } deriving Show

n :: Foo
n = Foo {foo = 2}
