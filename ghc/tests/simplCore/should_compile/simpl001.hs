-- !!! Desugaring sections with function-type arguments
--  Although this is really a desugaring test, the problem is
-- only tickled by the simplifier

-- type Foo a b = a -> (b -> a) -> b
module ShouldCompile where

(++++) :: (a -> (b -> a) -> b) -> (a -> (b -> a) -> b) -> a -> (b -> a) -> b
x ++++ y = y

g a xs = map (++++ a) xs

h b xs = map (b ++++) xs
