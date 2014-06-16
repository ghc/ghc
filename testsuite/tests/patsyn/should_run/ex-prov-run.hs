-- Pattern synonyms

{-# LANGUAGE PatternSynonyms, GADTs #-}
module Main where

data T a where
	MkT :: (Eq b) => a -> b -> T a

pattern P x y <- MkT x y

f :: T Bool -> Bool
f (P x y) = x && y == y

data Crazy = Crazy

instance Eq Crazy where
    _ == _ = False

main = do
    print (f $ MkT True True)
    print (f $ MkT True Crazy)
