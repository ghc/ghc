module Semis where

-- Make sure we get all the semicolons in statements

foo :: IO ()
foo = do
  do { ;;;; a }
  a

bar :: IO ()
bar = do
  { ;
    a ;;
    b
  }

baz :: IO ()
baz = do { ;; s ; s ; ; s ;; }
