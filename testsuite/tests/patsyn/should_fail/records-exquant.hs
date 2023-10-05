{-# LANGUAGE PatternSynonyms, ExistentialQuantification #-}
module ExQuant where

data Showable = forall a . Show a => Showable a

pattern Nasty{a} = Showable a

qux = a (Showable True)

foo = (Showable ()) { a = True }
