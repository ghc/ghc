{-# OPTIONS_GHC -fglasgow-exts #-}
import Text.Regex.Base
import Text.Regex.Posix((=~),(=~~)) -- or DFA or PCRE or PosixRE
import qualified Data.ByteString.Char8 as B(ByteString,pack)

main = let b :: Bool
           b = ("abaca" =~ "(.)a")
           c :: [MatchArray]
           c = ("abaca" =~ "(.)a")
           d :: Maybe (String,String,String,[String])
           d = ("abaca" =~~ "(.)a")
       in do print b
             print c
             print d
