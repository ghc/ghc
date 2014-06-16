-- !!! simple overloading example

class Foo a where
    foo :: a -> a -> Bool

class (Foo a) => Bar a where
    bar :: a -> a -> Bool

instance Foo Int where
    foo a b = a /= b

instance Foo Bool where
    foo a b = a /= b

instance Bar Int where
    bar a b = a < b

instance Bar Bool where
    bar a b = a < b

foO = if bar (2::Int) (3::Int) then
	    if bar False True then
	    	(42::Int)
	    else
		(888::Int)
       else
	  (999::Int)

main = print foO
