-- Tests some simple deriving stuff, and built-in instances

module Main( main ) where

data Command = Commit (Maybe String) | Foo | Baz Bool | Boz Int
	     deriving (Read,Show)

type T = ([Command], [Command], [Command])
val :: T
val = ([Commit Nothing, Commit (Just "foo")], 
       [Foo, Baz True], 
       [Boz 3, Boz (-2)])

main = do { print val  ;
	    print ((read (show val)) :: T) }

