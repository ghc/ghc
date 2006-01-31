
-- Tests that subFunTys works when the arugment is a type of form (a ty1 ty2)

module ShouldCompile where

newtype StreamArrow a b c = Str (a [b] [c])

foo = Str $ (\x -> x)
