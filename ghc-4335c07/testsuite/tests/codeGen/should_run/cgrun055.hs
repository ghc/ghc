-- This program broke GHC 6.3, because dataToTag was called with
-- an unevaluated argument

module Main where

import System.Environment (getArgs)

-- NOTE: When if you remove Eight (or any other constructor) everything works
-- Having at least 9 constructors has something to do with the bug
data Digit = Zero | One | Two | Three | Four | Five | Six | Seven | Eight
    deriving (Eq,Enum)

instance Show Digit where
    show Five = "Five"
    show Six = "Six"
    show _ = undefined

-- Use either of these instances (instead of derived) and everything works
{-instance Enum Digit where
    fromEnum Five = 5
    fromEnum _ = undefined
    toEnum 6 = Six
    toEnum _ = undefined-}

{-instance Eq Digit where
    Five == Five = True
    Six == Six = True
    _ == _ = undefined-}

isFive :: Digit -> Bool
isFive a = succ a == Six

main :: IO()
main = do
    putStrLn ("======")
    -- These next two lines are just here to keep ghc from optimizing away stuff
    args <- getArgs
    let x = if length args == -1 then undefined else Five
    putStrLn ("x: " ++ show x)
    let y = succ x
    putStrLn ("let y = succ x")
    putStrLn ("y: " ++ show y)
    putStrLn ("y == Six: " ++ show (y == Six))
    putStrLn ("succ x == Six: " ++ show (succ x == Six))
    putStrLn ("isFive x: " ++ show (isFive x))

