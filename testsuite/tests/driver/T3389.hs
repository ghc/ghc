
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -optP -C -optP -ffreestanding #-}

#define A 'a'

main :: IO ()
main = putStrLn [A,
-- /*
                 'b',
-- */
                 'c']
