{-# LANGUAGE Rank2Types #-}

-- !!! Check that record selectors for polymorphic fields work right

module Main where

class Foo a where
  bar :: a -> [a]

instance Foo Int where
  bar x = replicate x x

instance Foo Bool where
  bar x = [x, not x]

data Record = R {
     blub :: Foo a => a -> [a]
    }

main = do { let r = R {blub = bar}
	  ; print (blub r (3::Int)) 
	  ; print (blub r True)
	  }



