-- should fail, Main must *export* main (#414)
module Main () where
main = return ()
