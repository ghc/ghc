--!!! Desugaring sections with function-type arguments
--

-- type Foo a b = a -> (b -> a) -> b

(++++) :: (a -> (b -> a) -> b) -> (a -> (b -> a) -> b) -> a -> (b -> a) -> b
x ++++ y = y

g a xs = map (++++ a) xs

h b xs = map (b ++++) xs
