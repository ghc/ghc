module Parse
      (Parse, Parses,                       --  data types
       thenP, returnP, eachP, consP,                  --  sequencing and success
       elseP, failP, guardP, filterP,                 --  alternation and failure
       starP, plusP, cutP,                            --  repetition and cut
       endP, itemP, litP, litsP, exactlyP,            --  end, next item, and literals
       spacesP, lexP, lexicalP, lexactlyP,            --  spaces and lexemes
       asciiP, controlP, printP, spaceP,              --  character parsers
       alphaP, upperP, lowerP, digitP, alphanumP,
       surroundP, plusSepP, starSepP, parenP, listP,  --  surrounds and separators
       useP)                                          --  using a parser      
      where

import Data.Char -- 1.3

infixr 1      `elseP`
infix  2      `thenP`
infix  2      `eachP`
infixr 3      `filterP`
infixr 3      `guardP`
type Parse a x  =  a -> [(x, a)]
type Parses x  =  Parse String x
thenP         :: Parse a x -> (x -> Parse a y) -> Parse a y
xP `thenP` kP =  \a -> [ (y,c) | (x,b) <- xP a, (y,c) <- kP x b ]
returnP       :: x -> Parse a x
returnP x     =  \a -> [ (x,a) ]
eachP         :: Parse a x -> (x -> y) -> Parse a y
xP `eachP` f  =  xP `thenP` (\x -> returnP (f x))
consP           :: Parse a x -> Parse a [x] -> Parse a [x]
xP `consP` xsP  =  xP   `thenP` (\x ->
                   xsP  `thenP` (\xs ->
                        returnP (x:xs)))
elseP         :: Parse a x -> Parse a x -> Parse a x
xP `elseP` yP =  \a -> xP a ++ yP a
failP         :: Parse a x
failP         =  \a -> []
guardP        :: Bool -> Parse a x -> Parse a x
guardP b xP   =  if  b  then  xP  else  failP
filterP       :: (x -> Bool) -> Parse a x -> Parse a x
filterP p xP  =  xP `thenP` (\x -> p x `guardP` returnP x)
starP         :: Parse a x -> Parse a [x]
starP xP      =  cutP (plusP xP `elseP` returnP [])
plusP         :: Parse a x -> Parse a [x]
plusP xP      =  xP `consP` starP xP
cutP          :: Parse a x -> Parse a x
cutP xP       =  \a -> case  xP a  of  { ~(~(x,b):_) -> [(x,b)] }
endP          :: Parse [x] ()
endP          =  \xs -> if  null xs  then  returnP () xs  else  failP xs
itemP         :: Parse [x] x
itemP         =  \xs -> if  null xs  then  failP xs
                                     else  returnP (head xs) (tail xs)
litP          :: (Eq x) => x -> Parse [x] x
litP c        =  (\x -> c==x) `filterP` itemP
litsP         :: (Eq x) => [x] -> Parse [x] [x]
litsP []      =  returnP []
litsP (c:cs)  =  litP c `consP` litsP cs
exactlyP      :: Parse [y] x -> Parse [y] x
exactlyP xP   =  xP `thenP` (\x -> endP `thenP` (\() -> returnP x))
spacesP       :: Parses String
spacesP       =  starP spaceP
lexicalP      :: Parses x -> Parses x
lexicalP xP   =  xP `thenP` (\x -> spacesP `thenP` (\_ -> returnP x))
lexP          :: String -> Parses String
lexP cs       =  lexicalP (litsP cs)
lexactlyP     :: Parses x -> Parses x
lexactlyP xP  =  spacesP `thenP` (\_ -> exactlyP xP)
asciiP, controlP, printP, spaceP, upperP      :: Parses Char
lowerP, alphaP, digitP, alphanumP             :: Parses Char
asciiP        =  isAscii    `filterP` itemP
controlP      =  isControl  `filterP` itemP
printP        =  isPrint    `filterP` itemP
spaceP        =  isSpace    `filterP` itemP
upperP        =  isUpper    `filterP` itemP
lowerP        =  isLower    `filterP` itemP
alphaP        =  isAlpha    `filterP` itemP
digitP        =  isDigit    `filterP` itemP
alphanumP     =  isAlphaNum `filterP` itemP
surroundP             :: String -> Parses x -> String -> Parses x
surroundP l xP r      =  lexP l       `thenP` (\_ ->
                         xP           `thenP` (\x ->
                         lexP r       `thenP` (\_ ->
                                      returnP x)))
plusSepP              :: String -> Parses x -> Parses [x]
plusSepP s xP         =  xP `consP` starP (lexP s `thenP` (\_ -> xP))
starSepP              :: String -> Parses x -> Parses [x]
starSepP s xP         =  plusSepP s xP `elseP` returnP []
parenP                :: Parses x -> Parses x
parenP xP             =  surroundP "(" xP ")"
listP                 :: Parses x -> Parses [x]
listP xP              =  surroundP "[" (starSepP "," xP) "]"
useP          :: x -> Parse a x -> (a -> x)
useP failx xP =  \a -> case  xP a  of { [] -> failx; ((x,_):_) -> x }
