-- !!! Show/Read deriving example given in the Haskell Report.
module Main(main) where

infix 4 :^:
data Tree a 
  =  Leaf a  | (Tree a) :^: (Tree a)
     deriving (Show, Read)

val1 :: Tree Int
val1 = Leaf 2

val2 :: Tree Int
val2 = Leaf 2 :^: Leaf (-1)

main = do
  print val1
  print val2

  print ((read (show val1))::Tree Int)
  print ((read (show val2))::Tree Int)
  print ((read (show val1))::Tree Integer)
  print ((read (show val2))::Tree Integer)

{- What you'll want
instance (Show a) => Show (Tree a) where

        showsPrec d (Leaf m) = showParen (d >= 10) showStr
    	  where
             showStr = showString "Leaf " . showsPrec 10 m

        showsPrec d (u :^: v) = showParen (d > 4) showStr
    	  where
             showStr = showsPrec 5 u . 
                       showString " :^: " .
                       showsPrec 5 v

instance (Read a) => Read (Tree a) where

	readsPrec d r =  readParen (d > 4)
			 (\r -> [(u:^:v,w) |
				 (u,s) <- readsPrec 5 r,
				 (":^:",t) <- lex s,
				 (v,w) <- readsPrec 5 t]) r

		      ++ readParen (d > 9)
			 (\r -> [(Leaf m,t) |
				 ("Leaf",s) <- lex r,
				 (m,t) <- readsPrec 10 s]) r
-}
