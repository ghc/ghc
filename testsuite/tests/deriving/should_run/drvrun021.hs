module Main where

class Show a => Foo a where
  op :: a -> a

newtype Moose = MkMoose () deriving (Show, Eq, Ord)

newtype Noose = MkNoose () deriving (Ord)

instance Eq Noose where
  a == b = False	-- Non-standard!

f :: Ord a => a -> Bool
f x = x==x

main = do print (MkNoose () == MkNoose ())	-- Eq Noose
	  print (f (MkNoose ()))		-- via Ord Noose
	  print (MkMoose () == MkMoose ())	-- Eq Moose
	  print (f (MkMoose ()))		-- via Ord Moose
	  putStrLn (show (MkMoose ()))		-- Should not use the show () method
