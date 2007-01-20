{-# OPTIONS -foverloaded-strings #-}
module T where

newtype MyString = MyString String deriving (Eq, Show)
instance IsString MyString where
    fromString = MyString

greet1 :: MyString -> MyString
greet1 "hello" = "world"
greet1 other = other

greet2 :: String -> String
greet2 "hello" = "world"
greet2 other = other

greet3 :: (Eq s, IsString s) => s -> s
greet3 "hello" = "world"
greet3 other = other

test = do
    print $ greet1 "hello"
    print $ greet2 "fool"
    print $ greet3 ("foo" :: String)
    print $ greet3 ("bar" :: MyString)
