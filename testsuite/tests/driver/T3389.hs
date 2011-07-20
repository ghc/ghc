
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -optP -C #-}

#define A 'a'

main :: IO ()
main = putStrLn [A,
-- /*
                 'b',
-- */
                 'c']
