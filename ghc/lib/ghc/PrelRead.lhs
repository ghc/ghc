%
% (c) The AQUA Project, Glasgow University, 1994-1996
%

\section[PrelRead]{Module @Prelread@}

The @Read@ class and many of its instances.

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module PrelRead where

import {#- SOURCE #-}	IOBase	( error )
import PrelNum
import PrelList
import PrelTup
import PrelBase
\end{code}

%*********************************************************
%*							*
\subsection{The @Read@ class}
%*							*
%*********************************************************

\begin{code}
type  ReadS a   = String -> [(a,String)]

class  Read a  where
    readsPrec :: Int -> ReadS a

    readList  :: ReadS [a]
    readList   = readList__ reads
\end{code}

%*********************************************************
%*							*
\subsection{Instances of @Read@}
%*							*
%*********************************************************

\begin{code}
instance  Read Char  where
    readsPrec p      = readParen False
    	    	    	    (\r -> [(c,t) | ('\'':s,t)<- lex r,
					    (c,_)     <- readLitChar s])

    readList = readParen False (\r -> [(l,t) | ('"':s, t) <- lex r,
					       (l,_)      <- readl s ])
	       where readl ('"':s)	= [("",s)]
		     readl ('\\':'&':s)	= readl s
		     readl s		= [(c:cs,u) | (c ,t) <- readLitChar s,
						      (cs,u) <- readl t	      ]

instance Read Bool where
    readsPrec p = readParen False
			(\r ->	let lr = lex r
				in
				[(True, rest) | ("True", rest) <- lr] ++
				[(False,rest) | ("False",rest) <- lr])
		

instance Read Ordering where
    readsPrec p = readParen False
			(\r ->	let lr = lex r
				in
				[(LT, rest) | ("LT", rest) <- lr] ++
				[(EQ, rest) | ("EQ", rest) <- lr] ++
				[(GT, rest) | ("GT", rest) <- lr])

instance Read a => Read (Maybe a) where
    readsPrec p = readParen False
			(\r ->	let lr = lex r
				in
				[(Nothing, rest) | ("Nothing", rest) <- lr] ++
				[(Just x, rest2) | ("Just", rest1) <- lr,
					           (x, rest2) <- reads rest1])

instance (Read a, Read b) => Read (Either a b) where
    readsPrec p = readParen False
			(\r ->	let lr = lex r
				in
				[(Left x, rest2)  | ("Left", rest1) <- lr,
					            (x, rest2) <- reads rest1] ++
				[(Right x, rest2) | ("Right", rest1) <- lr,
					            (x, rest2) <- reads rest1])

instance  Read Int  where
    readsPrec p x = readSigned readDec x

instance  Read Integer  where
    readsPrec p x = readSigned readDec x

instance  Read Float  where
    readsPrec p x = readSigned readFloat x

instance  Read Double  where
    readsPrec p x = readSigned readFloat x

instance  (Integral a, Read a)  => Read (Ratio a)  where
    readsPrec p  =  readParen (p > ratio_prec)
			      (\r -> [(x%y,u) | (x,s)   <- reads r,
					        ("%",t) <- lex s,
						(y,u)   <- reads t ])

instance  (Read a) => Read [a]  where
    readsPrec p         = readList

instance Read () where
    readsPrec p    = readParen False
                            (\r -> [((),t) | ("(",s) <- lex r,
                                             (")",t) <- lex s ] )

instance  (Read a, Read b) => Read (a,b)  where
    readsPrec p = readParen False
                            (\r -> [((x,y), w) | ("(",s) <- lex r,
                                                 (x,t)   <- reads s,
                                                 (",",u) <- lex t,
                                                 (y,v)   <- reads u,
                                                 (")",w) <- lex v ] )

instance (Read a, Read b, Read c) => Read (a, b, c) where
    readsPrec p = readParen False
			(\a -> [((x,y,z), h) | ("(",b) <- lex a,
					       (x,c)   <- readsPrec 0 b,
					       (",",d) <- lex c,
					       (y,e)   <- readsPrec 0 d,
					       (",",f) <- lex e,
					       (z,g)   <- readsPrec 0 f,
					       (")",h) <- lex g ] )

instance (Read a, Read b, Read c, Read d) => Read (a, b, c, d) where
    readsPrec p = readParen False
		    (\a -> [((w,x,y,z), j) | ("(",b) <- lex a,
					     (w,c)   <- readsPrec 0 b,
					     (",",d) <- lex c,
					     (x,e)   <- readsPrec 0 d,
					     (",",f) <- lex e,
					     (y,g)   <- readsPrec 0 f,
					     (",",h) <- lex g,
					     (z,i)   <- readsPrec 0 h,
					     (")",j) <- lex i ] )

instance (Read a, Read b, Read c, Read d, Read e) => Read (a, b, c, d, e) where
    readsPrec p = readParen False
		    (\a -> [((w,x,y,z,v), l) | ("(",b) <- lex a,
					       (w,c)   <- readsPrec 0 b,
					       (",",d) <- lex c,
					       (x,e)   <- readsPrec 0 d,
					       (",",f) <- lex e,
					       (y,g)   <- readsPrec 0 f,
					       (",",h) <- lex g,
					       (z,i)   <- readsPrec 0 h,
					       (",",j) <- lex i,
					       (v,k)   <- readsPrec 0 j,
					       (")",l) <- lex k ] )
\end{code}



%*********************************************************
%*							*
\subsection{Utility functions}
%*							*
%*********************************************************

\begin{code}
reads           :: (Read a) => ReadS a
reads           =  readsPrec 0

read            :: (Read a) => String -> a
read s          =  case [x | (x,t) <- reads s, ("","") <- lex t] of
                        [x] -> x
                        []  -> error "PreludeText.read: no parse"
                        _   -> error "PreludeText.read: ambiguous parse"

readParen       :: Bool -> ReadS a -> ReadS a
readParen b g   =  if b then mandatory else optional
                   where optional r  = g r ++ mandatory r
                         mandatory r = [(x,u) | ("(",s) <- lex r,
                                                (x,t)   <- optional s,
                                                (")",u) <- lex t    ]

{-# GENERATE_SPECS readList__ a #-}
readList__ :: ReadS a -> ReadS [a]

readList__ readx
  = readParen False (\r -> [pr | ("[",s)  <- lex r, pr <- readl s])
  where readl  s = [([],t)   | ("]",t)  <- lex s] ++
		   [(x:xs,u) | (x,t)    <- readx s,
			       (xs,u)   <- readl2 t]
	readl2 s = [([],t)   | ("]",t)  <- lex s] ++
		   [(x:xs,v) | (",",t)  <- lex s,
			       (x,u)    <- readx t,
			       (xs,v)   <- readl2 u]
\end{code}



%*********************************************************
%*							*
\subsection{Reading characters}
%*							*
%*********************************************************

\begin{code}
readLitChar 		:: ReadS Char

readLitChar ('\\':s)	=  readEsc s
	where
	readEsc ('a':s)	 = [('\a',s)]
	readEsc ('b':s)	 = [('\b',s)]
	readEsc ('f':s)	 = [('\f',s)]
	readEsc ('n':s)	 = [('\n',s)]
	readEsc ('r':s)	 = [('\r',s)]
	readEsc ('t':s)	 = [('\t',s)]
	readEsc ('v':s)	 = [('\v',s)]
	readEsc ('\\':s) = [('\\',s)]
	readEsc ('"':s)	 = [('"',s)]
	readEsc ('\'':s) = [('\'',s)]
	readEsc ('^':c:s) | c >= '@' && c <= '_'
			 = [(chr (ord c - ord '@'), s)]
	readEsc s@(d:_) | isDigit d
			 = [(chr n, t) | (n,t) <- readDec s]
	readEsc ('o':s)  = [(chr n, t) | (n,t) <- readOct s]
	readEsc ('x':s)	 = [(chr n, t) | (n,t) <- readHex s]
	readEsc s@(c:_) | isUpper c
			 = let table = ('\DEL', "DEL") : zip ['\NUL'..] asciiTab
			   in case [(c,s') | (c, mne) <- table,
					     ([],s') <- [match mne s]]
			      of (pr:_) -> [pr]
				 []	-> []
	readEsc _	 = []
readLitChar (c:s)	=  [(c,s)]

match			:: (Eq a) => [a] -> [a] -> ([a],[a])
match (x:xs) (y:ys) | x == y  =  match xs ys
match xs     ys		      =  (xs,ys)

\end{code}


%*********************************************************
%*							*
\subsection{Reading numbers}
%*							*
%*********************************************************

\begin{code}
{-# GENERATE_SPECS readDec a{Int#,Int,Integer} #-}
readDec :: (Integral a) => ReadS a
readDec = readInt 10 isDigit (\d -> ord d - ord_0)

{-# GENERATE_SPECS readOct a{Int#,Int,Integer} #-}
readOct :: (Integral a) => ReadS a
readOct = readInt 8 isOctDigit (\d -> ord d - ord_0)

{-# GENERATE_SPECS readHex a{Int#,Int,Integer} #-}
readHex :: (Integral a) => ReadS a
readHex = readInt 16 isHexDigit hex
	    where hex d = ord d - (if isDigit d then ord_0
				   else ord (if isUpper d then 'A' else 'a') - 10)

{-# GENERATE_SPECS readInt a{Int#,Int,Integer} #-}
readInt :: (Integral a) => a -> (Char -> Bool) -> (Char -> Int) -> ReadS a
readInt radix isDig digToInt s =
    [(foldl1 (\n d -> n * radix + d) (map (fromInt . digToInt) ds), r)
	| (ds,r) <- nonnull isDig s ]

{-# GENERATE_SPECS readSigned a{Int#,Double#,Int,Integer,Double} #-}
readSigned :: (Real a) => ReadS a -> ReadS a
readSigned readPos = readParen False read'
		     where read' r  = read'' r ++
				      [(-x,t) | ("-",s) <- lex r,
						(x,t)   <- read'' s]
			   read'' r = [(n,s)  | (str,s) <- lex r,
		      				(n,"")  <- readPos str]
\end{code}

The functions readFloat below uses rational arithmetic
to insure correct conversion between the floating-point radix and
decimal.  It is often possible to use a higher-precision floating-
point type to obtain the same results.

\begin{code}
{-# GENERATE_SPECS readFloat a{Double#,Double} #-}
readFloat :: (RealFloat a) => ReadS a
readFloat r = [(fromRational x, t) | (x, t) <- readRational r]

readRational :: ReadS Rational -- NB: doesn't handle leading "-"

readRational r
  = [ ( (n%1)*10^^(k-d), t ) | (n,d,s) <- readFix r,
			       (k,t)   <- readExp s]
              where readFix r = [(read (ds++ds'), length ds', t)
					| (ds,'.':s) <- lexDigits r,
					  (ds',t)    <- lexDigits s ]

		    readExp (e:s) | e `elem` "eE" = readExp' s
                    readExp s			  = [(0,s)]

                    readExp' ('-':s) = [(-k,t) | (k,t) <- readDec s]
                    readExp' ('+':s) = readDec s
                    readExp' s	     = readDec s

readRational__ :: String -> Rational -- we export this one (non-std)
				    -- NB: *does* handle a leading "-"
readRational__ top_s
  = case top_s of
      '-' : xs -> - (read_me xs)
      xs       -> read_me xs
  where
    read_me s
      = case [x | (x,t) <- readRational s, ("","") <- lex t] of
	  [x] -> x
	  []  -> error ("readRational__: no parse:"        ++ top_s)
	  _   -> error ("readRational__: ambiguous parse:" ++ top_s)

-- The number of decimal digits m below is chosen to guarantee 
-- read (show x) == x.  See
--	Matula, D. W.  A formalization of floating-point numeric base
--	conversion.  IEEE Transactions on Computers C-19, 8 (1970 August),
--	681-692.
\end{code}


%*********************************************************
%*							*
\subsection{Lexical analysis}
%*							*
%*********************************************************

This lexer is not completely faithful to the Haskell lexical syntax.
Current limitations:
   Qualified names are not handled properly
   A `--' does not terminate a symbol
   Octal and hexidecimal numerics are not recognized as a single token

\begin{code}
lex                   :: ReadS String

lex ""                = [("","")]
lex (c:s) | isSpace c = lex (dropWhile isSpace s)
lex ('\'':s)          = [('\'':ch++"'", t) | (ch,'\'':t)  <- lexLitChar s,
                                              ch /= "'"                ]
lex ('"':s)           = [('"':str, t)      | (str,t) <- lexString s]
                        where
                        lexString ('"':s) = [("\"",s)]
                        lexString s = [(ch++str, u)
                                              | (ch,t)  <- lexStrItem s,
                                                (str,u) <- lexString t  ]

                        lexStrItem ('\\':'&':s) = [("\\&",s)]
                        lexStrItem ('\\':c:s) | isSpace c
                            = [("\\&",t) | '\\':t <- [dropWhile isSpace s]]
                        lexStrItem s            = lexLitChar s

lex (c:s) | isSingle c = [([c],s)]
          | isSym c    = [(c:sym,t)       | (sym,t) <- [span isSym s]]
          | isAlpha c  = [(c:nam,t)       | (nam,t) <- [span isIdChar s]]
          | isDigit c  = [(c:ds++fe,t)    | (ds,s)  <- [span isDigit s],
                                            (fe,t)  <- lexFracExp s     ]
          | otherwise  = []    -- bad character
             where
              isSingle c =  c `elem` ",;()[]{}_`"
              isSym c    =  c `elem` "!@#$%&*+./<=>?\\^|:-~"
              isIdChar c =  isAlphanum c || c `elem` "_'"

              lexFracExp ('.':s) = [('.':ds++e,u) | (ds,t) <- lexDigits s,
                                                    (e,u)  <- lexExp t]
              lexFracExp s       = [("",s)]

              lexExp (e:s) | e `elem` "eE"
                       = [(e:c:ds,u) | (c:t)  <- [s], c `elem` "+-",
                                                 (ds,u) <- lexDigits t] ++
                         [(e:ds,t)   | (ds,t) <- lexDigits s]
              lexExp s = [("",s)]

lexDigits               :: ReadS String 
lexDigits               =  nonnull isDigit

nonnull                 :: (Char -> Bool) -> ReadS String
nonnull p s             =  [(cs,t) | (cs@(_:_),t) <- [span p s]]

lexLitChar              :: ReadS String
lexLitChar ('\\':s)     =  [('\\':esc, t) | (esc,t) <- lexEsc s]
        where
        lexEsc (c:s)     | c `elem` "abfnrtv\\\"'" = [([c],s)]
        lexEsc s@(d:_)   | isDigit d               = lexDigits s
        lexEsc _                                   = []
lexLitChar (c:s)        =  [([c],s)]
lexLitChar ""           =  []
\end{code}


