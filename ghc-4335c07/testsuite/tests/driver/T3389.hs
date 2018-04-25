
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -optP -C -optP -ffreestanding #-}

#define A 'a'

main :: IO ()
main = putStrLn [A,
-- CPP treats this as the beginning of a multiline C-style comment: /usr/local/*
                 'b',
-- And */ close it. By passing `-optP -C` we tell CPP not to delete it.
                 'c']
