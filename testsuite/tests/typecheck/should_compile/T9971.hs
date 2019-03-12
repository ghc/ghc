{-# LANGUAGE FunctionalDependencies #-}
module T9971 where

type Vertex v = v Double

class C a b | b->a  where
  op :: a -> b

foo :: Vertex x
foo = error "urk"

bar x = [op foo, op foo]
 -- This gives rise to a [D] Vertex a1 ~ Vertex a2
 -- And that made the canonicaliser go into a loop (#9971)

