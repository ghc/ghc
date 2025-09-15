
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}

module Main (main) where

import GHC.Base
import GHC.Num.Integer
import Control.Monad
import System.Exit

main :: IO ()
main = do

   let test a b = do
         putStrLn $ "GCDE " ++ show a ++ " " ++ show b
         let r@(g,x,y) = integerGcde a b
         putStrLn $ "   -> g = " ++ show g
         putStrLn $ "   -> x = " ++ show x
         putStrLn $ "   -> y = " ++ show y
         let sign a | a >= 0    = 1
                    | otherwise = -1
         let assert text cond term
               | not cond  = return ()
               | term      = return ()
               | otherwise = do
                  putStrLn $ "FAILED: " ++ text
                  putStrLn $ "a*x + b*y = g"
                  putStrLn $ "a = " ++ show a
                  putStrLn $ "b = " ++ show b
                  putStrLn $ "x = " ++ show x
                  putStrLn $ "y = " ++ show y
                  putStrLn $ "g = " ++ show g
                  putStrLn $ "expected g = " ++ show (abs (integerGcd a b))
                  exitFailure

         -- check properties
         assert "g >= 0"            True (g >= 0)
         assert "a*x + b*y = g"     True (a*x + b*y == g)
         assert "g = abs (gcd a b)" True (g == abs (integerGcd a b))

         if -- special cases
            | a == 0 && b == 0 -> do
               assert "a == 0 && b ==0 ==> g == 0" (a == 0 && b == 0) (g == 0)

            | abs a == abs b -> do
               assert "abs a == abs b ==> x == 0 && y == sign b && g == abs a"
                  (abs a == abs b) (x == 0 && y == sign b && g == abs a)

            -- non special cases
            | otherwise -> do
               assert "b == 0 ==> x=sign a"
                  (b == 0)
                  (x == sign a)

               assert "abs b == 2g ==> x=sign a"
                  (abs b == 2*g)
                  (x == sign a)

               assert "b /= 0 ==> abs x <= abs b / 2*g"
                  (b /= 0)
                  (abs x <= abs b `div` 2 * g)

               assert "a /= 0 ==> abs y <= abs a / 2*g"
                  (a /= 0)
                  (abs y <= abs a `div` 2 * g)

               assert "a == 0 ==> y=sign b"
                  (a == 0)
                  (y == sign b)

               assert "abs a == 2g ==> y==sign b"
                  (abs a == 2*g)
                  (y == sign b)

               assert "x == 0 ==> g == abs b"
                  (x == 0)
                  (g == abs b)

       nums =
            [ 0
            , 1
            , 7
            , 14
            , 123
            , 1230
            , 123456789456789456789456789456789456789465789465456789465454645789
            , 4 * 123456789456789456789456789456789456789465789465456789465454645789
            , -1
            , -123
            , -123456789456789456789456789456789456789465789465456789465454645789
            , 4567897897897897899789897897978978979789
            , 2988348162058574136915891421498819466320163312926952423791023078876139
            , 2351399303373464486466122544523690094744975233415544072992656881240319
            , 5328841272400314897981163497728751426
            , 32052182750761975518649228050096851724
            ]

   forM_ nums $ \a ->
      forM_ nums $ \b ->
         test a b

   -- see #15350
   do
       let a = 2
           b = 2^65 + 1
       test a b
       test a (-b)
       test (-a) b
       test (-a) (-b)
       test b a
       test b (-a)
       test (-b) a
       test (-b) (-a)
