
Test program for reading a binary file containing a sequence of
vectors of possibly different dimension.  The format of the file is 

        Block size                              Data
        --------------------------------        ---------------------

        sizeof(int) bytes                       dimension of vector 1
        sizeof(float) x dimension1 bytes        vector 1
        sizeof(int) bytes                       dimension of vector 2
        sizeof(float) x dimension2 bytes        vector 2
        sizeof(int) bytes                       dimension of vector 3
        sizeof(float) x dimension3 bytes        vector 3

                :                                  :

This program will print the dimension, then the vector, then a blank
line, then the dimension of the next vector, the next vector, then a
blank line, etc.

----------------------------------------------------------------------

> module Main where

> import Native
> import MaybeStateT
> import System

> main = getArgs           >>= \ args ->
>       case args of
>
>       [file]  -> readFile file   >>= \ bs ->
>                  let
>                    vs = readVectors bs
>                  in
>                  putStr (display vs)
>
>       _       -> error " need a binary file name"



> type Vector           = [Float]


> readVectors           :: Bytes -> [Vector]
> readVectors bs =
>       case readVector bs of
>       Nothing         -> []   -- assume there are no more vectors to read
>       Just (v, bs')   -> v : readVectors bs'


> readVector            :: MST Bytes Vector
> readVector =
>       readBytes                               `bindMST` \dimension ->
>       listReadBytes dimension                 `bindMST` \v ->
>       returnMST v



> display :: [Vector] -> String
> display (v:vs) = displayVector v ++ display vs
> display []     = "\n"

> displayVector :: Vector -> String
> displayVector v = shows (length v) "\n" ++
>                   shows v "\n\n"

