module ParserCoreUtils where

import IO 

data ParseResult a = OkP a | FailP String
type P a = String -> Int -> ParseResult a

thenP :: P a -> (a -> P b) -> P b
m `thenP`  k = \ s l -> 
  case m s l of 
    OkP a -> k a s l
    FailP s -> FailP s

returnP :: a -> P a
returnP m _ _ = OkP m

failP :: String -> P a
failP s s' _ = FailP (s ++ ":" ++ s')

getCoreModuleName :: FilePath -> IO String
getCoreModuleName fpath = 
   catch (do 
     h  <- openFile fpath ReadMode
     ls <- hGetContents h
     let mo = findMod (words ls)
      -- make sure we close up the file right away.
     (length mo) `seq` return ()
     hClose h
     return mo)
    (\ _ -> return "Main")
 where
   findMod [] = "Main"
   -- TODO: this should just return the module name, without the package name
   findMod ("%module":m:_) = m
   findMod (_:xs) = findMod xs


data Token =
   TKmodule
 | TKdata
 | TKnewtype
 | TKforall
 | TKrec
 | TKlet
 | TKin
 | TKcase
 | TKof
 | TKcast
 | TKnote
 | TKexternal
 | TKlocal
 | TKwild
 | TKoparen
 | TKcparen
 | TKobrace
 | TKcbrace
 | TKhash
 | TKeq
 | TKcolon
 | TKcoloncolon
 | TKcoloneqcolon
 | TKstar
 | TKrarrow
 | TKlambda
 | TKat
 | TKdot
 | TKquestion
 | TKsemicolon
 | TKname String
 | TKcname String
 | TKinteger Integer
 | TKrational Rational
 | TKstring String
 | TKchar Char
 | TKEOF

