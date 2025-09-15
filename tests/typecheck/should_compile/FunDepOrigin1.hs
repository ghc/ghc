{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts #-}

module FunDepOrigin1 where

class C a b | a -> b where
  op :: a -> b -> b

foo :: C Bool (Maybe a) => x -> Maybe a
foo _ = op True Nothing

bar :: C Bool [a] => x -> [a]
bar _ = op False []
