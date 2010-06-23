
{-# LANGUAGE CPP #-}

#define A 'a'

main :: IO ()
main = putStrLn [A,
-- /*
                 'b',
-- */
                 'c']
