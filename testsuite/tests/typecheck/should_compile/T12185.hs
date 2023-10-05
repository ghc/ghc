{-# LANGUAGE FlexibleContexts, RankNTypes, TypeFamilies #-}

module T12185 where

class Foo a

newtype Bar r = Pow r deriving (Eq)

instance (Foo r) => Foo (Bar r)

type family Ctx a where Ctx t = (Foo (Bar t), Eq (Bar t))

run :: (forall t . (Ctx t) => t -> Int) -> Int
run g = undefined

foo :: (Foo (Bar t)) => t -> Int
foo = undefined

main :: IO ()
main = print $ run foo
