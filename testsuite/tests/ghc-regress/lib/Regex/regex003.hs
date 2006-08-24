{-# OPTIONS_GHC -fglasgow-exts #-}
import Text.Regex.Base
import Text.Regex.Posix(Regex,(=~),(=~~)) -- or DFA or PCRE or PosixRE
import qualified Data.ByteString.Char8 as B(ByteString,pack)

-- Show mixing of ByteString and String as well as polymorphism:

main = let x :: (RegexContext Regex String target) => target
           x = ("abaca" =~ B.pack "(.)a")
           x' :: (RegexContext Regex String target,Monad m) => m target
           x' = ("abaca" =~~ "(.)a")
           y :: (RegexContext Regex B.ByteString target) => target
           y = (B.pack "abaca" =~ "(.)a")
           y' :: (RegexContext Regex B.ByteString target,Monad m) => m target
           y' = (B.pack "abaca" =~~ B.pack "(.)a")
       in do print (x :: Bool)
             print (x :: Int)
             print (x :: [MatchArray])
             print (x' :: Maybe (String,String,String,[String]))
             print (y :: Bool)
             print (y :: Int)
             print (y :: [MatchArray])
             print (y' :: Maybe (B.ByteString,B.ByteString,B.ByteString,[B.ByteString]))

{- Output is, except for replacing Full with DFA (which has no capture)
True
2
[array (0,1) [(0,(1,2)),(1,(1,1))],array (0,1) [(0,(3,2)),(1,(3,1))]]
Just ("a","ba","ca",["b"])
True
2
[array (0,1) [(0,(1,2)),(1,(1,1))],array (0,1) [(0,(3,2)),(1,(3,1))]]
Just ("a","ba","ca",["b"])
-}
{- The output for DFA is
True
2
[array (0,0) [(0,(1,2))],array (0,0) [(0,(3,2))]]
Just ("a","ba","ca",[])
True
2
[array (0,0) [(0,(1,2))],array (0,0) [(0,(3,2))]]
Just ("a","ba","ca",[])
-}
