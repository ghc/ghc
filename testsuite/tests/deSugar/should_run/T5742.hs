{-# LANGUAGE DoRec #-}

module Main where

main = do { x <- foo; print x }
foo :: IO Int
foo = do rec 
             a1 <- return a2
             a2 <- return a3
             a3 <- return a4
             a4 <- return a5
             a5 <- return b1

             b1 <- return b2
             b2 <- return b3
             b3 <- return b4
             b4 <- return b5
             b5 <- return c1

             c1 <- return c2
             c2 <- return c3
             c3 <- return c4
             c4 <- return c5
             c5 <- return d1

             d1 <- return d2
             d2 <- return d3
             d3 <- return d4
             d4 <- return d5
             d5 <- return a1x

             a1x <- return a2x
             a2x <- return a3x
             a3x <- return a4x
             a4x <- return a5x
             a5x <- return b1x

             b1x <- return b2x
             b2x <- return b3x
             b3x <- return b4x
             b4x <- return b5x
             b5x <- return c1x

             c1x <- return c2x
             c2x <- return c3x
             c3x <- return c4x
             c4x <- return c5x
             c5x <- return d1x

             d1x <- return d2x
             d2x <- return d3x
             d3x <- return d4x
             d4x <- return d5x
             d5x <- return a1y

             a1y <- return a2y
             a2y <- return a3y
             a3y <- return a4y
             a4y <- return a5y
             a5y <- return b1y

             b1y <- return b2y
             b2y <- return b3y
             b3y <- return b4y
             b4y <- return b5y
             b5y <- return c1y

             c1y <- return c2y
             c2y <- return c3y
             c3y <- return c4y
             c4y <- return c5y
             c5y <- return d1y

             d1y <- return d2y
             d2y <- return d3y
             d3y <- return d4y
             d4y <- return d5y
             d5y <- return a1z

             a1z <- return a2z
             a2z <- return a3z
             a3z <- return a4z
             a4z <- return a5z
             a5z <- return b1z

             b1z <- return b2z
             b2z <- return b3z
             b3z <- return b4z
             b4z <- return b5z
             b5z <- return c1z

             c1z <- return c2z
             c2z <- return c3z
             c3z <- return c4z
             c4z <- return c5z
             c5z <- return d1z

             d1z <- return d2z
             d2z <- return d3z
             d3z <- return d4z
             d4z <- return d5z
             d5z <- return 1

         return a4

