-- In case of types with nested type synonyms, all synonyms should be expanded

{-# LANGUAGE RankNTypes #-}

import Control.Monad.ST

type Foo = Int
type Bar = Bool

type MyFooST s = ST s Foo
type MyBarST s = ST s Bar

fooGen :: forall s . MyFooST s
fooGen = undefined

barGen :: forall s . MyBarST s
barGen = undefined

main = print (runST fooGen == runST barGen)
