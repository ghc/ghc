{-# OPTIONS -fglasgow-exts #-}

module ShouldCompile where

data Rep t where
  Rint :: Rep Int
  Rchar :: Rep Char
  Runit :: Rep ()
  Rpair :: Rep a -> Rep b -> Rep (a,b)
  Rsum  :: Rep a -> Rep b -> Rep (Either a b)
  Rcon  :: String -> Rep t -> Rep t

data Equal a b where
   Eq :: Equal c c 

test :: Rep a -> Rep b -> Maybe (Equal a b)
test Rint  Rint  = return Eq
test Rchar Rchar = return Eq
test Runit Runit = return Eq
test (Rpair x y) (Rpair a b)
  = case test x a of {
	Nothing -> Nothing ;
	Just Eq -> 
    case test y b of
	Nothing -> Nothing
	Just Eq -> Just Eq 
    }
	
--  = do { Eq <- test x a; Eq <- test y b; return Eq }
test (Rsum x y) (Rsum a b)
  = case test x a of {
	Nothing -> Nothing ;
	Just Eq -> 
    case test y b of
	Nothing -> Nothing
	Just Eq -> Just Eq 
    }
--  = do { Eq f g <- test x a; Eq h i <- test y b; return Eq }
test (Rcon s1 x) (Rcon s2 y)
  = if s1==s2 then test x y else Nothing
test _ _ = Nothing

