%
% (c) The AQUA Project, Glasgow University, 1994-1998
%

\section[PrelRead]{Module @PrelRead@}

Instances of the Read class.

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module PrelRead where

import {-# SOURCE #-} PrelErr ( error )
import PrelNum
import PrelNumExtra
import PrelList
import PrelTup
import PrelMaybe
import PrelEither
import PrelBase
import Monad

-- needed for readIO.
import PrelIOBase ( IO, userError )
import PrelException ( fail )
\end{code}

%*********************************************************
%*							*
\subsection{The @Read@ class}
%*							*
%*********************************************************

Note: if you compile this with -DNEW_READS_REP, you'll get
a (simpler) ReadS representation that only allow one valid
parse of a string of characters, instead of a list of
possible ones.

[changing the ReadS rep has implications for the deriving
machinery for Read, a change that hasn't been made, so you
probably won't want to compile in this new rep. except
when in an experimental mood.]

\begin{code}

#ifndef NEW_READS_REP
type  ReadS a   = String -> [(a,String)]
#else
type  ReadS a   = String -> Maybe (a,String)
#endif

class  Read a  where
    readsPrec :: Int -> ReadS a

    readList  :: ReadS [a]
    readList   = readList__ reads
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
read s          =  
   case read_s s of
#ifndef NEW_READS_REP
      [x]     -> x
      []      -> error "PreludeText.read: no parse"
      _	      -> error "PreludeText.read: ambiguous parse"
#else
      Just x  -> x
      Nothing -> error "PreludeText.read: no parse"
#endif
 where
  read_s s = do
    (x,t)   <- reads s
    ("","") <- lex t
    return x

  -- raises an exception instead of an error
readIO          :: Read a => String -> IO a
readIO s        =  case (do { (x,t) <- reads s ; ("","") <- lex t ; return x }) of
#ifndef NEW_READS_REP
			[x]    -> return x
			[]     -> fail (userError "PreludeIO.readIO: no parse")
			_      -> fail (userError "PreludeIO.readIO: ambiguous parse")
#else
                        Just x -> return x
                        Nothing  -> fail (userError "PreludeIO.readIO: no parse")
#endif

\end{code}

\begin{code}
readParen       :: Bool -> ReadS a -> ReadS a
readParen b g   =  if b then mandatory else optional
                   where optional r  = g r ++ mandatory r
                         mandatory r = do
				("(",s) <- lex r
				(x,t)   <- optional s
				(")",u) <- lex t
				return (x,u)


readList__ :: ReadS a -> ReadS [a]

readList__ readx
  = readParen False (\r -> do
		       ("[",s) <- lex r
		       readl s)
  where readl  s = 
           (do { ("]",t) <- lex s ; return ([],t) }) ++
	   (do { (x,t) <- readx s ; (xs,u) <- readl2 t ; return (x:xs,u) })

	readl2 s = 
	   (do { ("]",t) <- lex s ; return ([],t) }) ++
	   (do { (",",t) <- lex s ; (x,u) <- readx t ; (xs,v) <- readl2 u ; return (x:xs,v) })

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

lex ""                = return ("","")
lex (c:s) | isSpace c = lex (dropWhile isSpace s)
lex ('\'':s)          = do
	    (ch, '\'':t) <- lexLitChar s
	    guard (ch /= "'")
	    return ('\'':ch++"'", t)
lex ('"':s)           = do
	    (str,t) <- lexString s
	    return ('"':str, t)

          where
	    lexString ('"':s) = return ("\"",s)
            lexString s = do
		    (ch,t)  <- lexStrItem s
		    (str,u) <- lexString t
		    return (ch++str, u)

	    
            lexStrItem ('\\':'&':s) = return ("\\&",s)
            lexStrItem ('\\':c:s) | isSpace c = do
			('\\':t) <- return (dropWhile isSpace s)
			return ("\\&",t)
	    lexStrItem s            = lexLitChar s
     
lex (c:s) | isSingle c = return ([c],s)
          | isSym c    = do
		(sym,t) <- return (span isSym s)
		return (c:sym,t)
          | isAlpha c  = do
		(nam,t) <- return (span isIdChar s)
		return (c:nam, t)
          | isDigit c  = do
		 (ds,s)  <- return (span isDigit s)
		 (fe,t)  <- lexFracExp s
		 return (c:ds++fe,t)
          | otherwise  = zero    -- bad character
             where
              isSingle c =  c `elem` ",;()[]{}_`"
              isSym c    =  c `elem` "!@#$%&*+./<=>?\\^|:-~"
              isIdChar c =  isAlphanum c || c `elem` "_'"

              lexFracExp ('.':cs)   = do
			(ds,t) <- lex0Digits cs
			(e,u)  <- lexExp t
			return ('.':ds++e,u)
              lexFracExp s          = return ("",s)

              lexExp (e:s) | e `elem` "eE" = 
	          (do
		    (c:t) <- return s
		    guard (c `elem` "+-")
		    (ds,u) <- lexDigits t
		    return (e:c:ds,u))	    ++
		  (do
		    (ds,t) <- lexDigits s
		    return (e:ds,t))

              lexExp s = return ("",s)

lexDigits               :: ReadS String 
lexDigits               =  nonnull isDigit

-- 0 or more digits
lex0Digits               :: ReadS String 
lex0Digits  s            =  return (span isDigit s)

nonnull                 :: (Char -> Bool) -> ReadS String
nonnull p s             = do
	    (cs@(_:_),t) <- return (span p s)
	    return (cs,t)

lexLitChar              :: ReadS String
lexLitChar ('\\':s)     =  do
	    (esc,t) <- lexEsc s
	    return ('\\':esc, t)
        where
        lexEsc (c:s)     | c `elem` "abfnrtv\\\"'" = return ([c],s)
        lexEsc s@(d:_)   | isDigit d               = lexDigits s
        lexEsc _                                   = zero

lexLitChar (c:s)        =  return ([c],s)
lexLitChar ""           =  zero
\end{code}

%*********************************************************
%*							*
\subsection{Instances of @Read@}
%*							*
%*********************************************************

\begin{code}
instance  Read Char  where
    readsPrec p      = readParen False
    	    	    	    (\r -> do
				('\'':s,t) <- lex r
				(c,_)	   <- readLitChar s
				return (c,t))

    readList = readParen False (\r -> do
				('"':s,t) <- lex r
				(l,_)	  <- readl s
				return (l,t))
	       where readl ('"':s)	= return ("",s)
		     readl ('\\':'&':s)	= readl s
		     readl s		= do
			    (c,t)  <- readLitChar s 
			    (cs,u) <- readl t
			    return (c:cs,u)

instance Read Bool where
    readsPrec p = readParen False
			(\r ->
			   lex r >>= \ lr ->
			   (do { ("True", rest)  <- return lr ; return (True,  rest) }) ++
			   (do { ("False", rest) <- return lr ; return (False, rest) }))
		

instance Read Ordering where
    readsPrec p = readParen False
			(\r -> 
			   lex r >>= \ lr ->
			   (do { ("LT", rest) <- return lr ; return (LT,  rest) }) ++
			   (do { ("EQ", rest) <- return lr ; return (EQ, rest) })  ++
			   (do { ("GT", rest) <- return lr ; return (GT, rest) }))

instance Read a => Read (Maybe a) where
    readsPrec p = readParen False
			(\r -> 
			    lex r >>= \ lr ->
			    (do { ("Nothing", rest) <- return lr ; return (Nothing, rest)}) ++
			    (do 
				("Just", rest1) <- return lr
				(x, rest2)	<- reads rest1
				return (Just x, rest2)))

instance (Read a, Read b) => Read (Either a b) where
    readsPrec p = readParen False
			(\r ->
			    lex r >>= \ lr ->
			    (do 
				("Left", rest1) <- return lr
				(x, rest2)	<- reads rest1
				return (Left x, rest2)) ++
			    (do 
				("Right", rest1) <- return lr
				(x, rest2)	<- reads rest1
				return (Right x, rest2)))

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
			      (\r -> do
				(x,s)   <- reads r
				("%",t) <- lex s
				(y,u)   <- reads t
				return (x%y,u))

instance  (Read a) => Read [a]  where
    readsPrec p         = readList

instance Read () where
    readsPrec p    = readParen False
                            (\r -> do
				("(",s) <- lex r
				(")",t) <- lex s
				return ((),t))

instance  (Read a, Read b) => Read (a,b)  where
    readsPrec p = readParen False
                            (\r -> do
			        ("(",s) <- lex r
			        (x,t)   <- readsPrec 0 s
			        (",",u) <- lex t
			        (y,v)   <- readsPrec 0 u
			        (")",w) <- lex v
				return ((x,y), w))

instance (Read a, Read b, Read c) => Read (a, b, c) where
    readsPrec p = readParen False
                            (\a -> do
			        ("(",b) <- lex a
			        (x,c)   <- readsPrec 0 b
			        (",",d) <- lex c
			        (y,e)   <- readsPrec 0 d
			        (",",f) <- lex e
			        (z,g)   <- readsPrec 0 f
			        (")",h) <- lex g
				return ((x,y,z), h))

instance (Read a, Read b, Read c, Read d) => Read (a, b, c, d) where
    readsPrec p = readParen False
                            (\a -> do
			        ("(",b) <- lex a
			        (w,c)   <- readsPrec 0 b
			        (",",d) <- lex c
			        (x,e)   <- readsPrec 0 d
			        (",",f) <- lex e
			        (y,g)   <- readsPrec 0 f
			        (",",h) <- lex g
			        (z,h)   <- readsPrec 0 h
			        (")",i) <- lex h
				return ((w,x,y,z), i))

instance (Read a, Read b, Read c, Read d, Read e) => Read (a, b, c, d, e) where
    readsPrec p = readParen False
                            (\a -> do
			        ("(",b) <- lex a
			        (v,c)   <- readsPrec 0 b
			        (",",d) <- lex c
			        (w,e)   <- readsPrec 0 d
			        (",",f) <- lex e
			        (x,g)   <- readsPrec 0 f
			        (",",h) <- lex g
			        (y,i)   <- readsPrec 0 h
			        (",",j) <- lex i
			        (z,k)   <- readsPrec 0 j
			        (")",l) <- lex k
				return ((v,w,x,y,z), l))
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
	readEsc ('a':s)	 = return ('\a',s)
	readEsc ('b':s)	 = return ('\b',s)
	readEsc ('f':s)	 = return ('\f',s)
	readEsc ('n':s)	 = return ('\n',s)
	readEsc ('r':s)	 = return ('\r',s)
	readEsc ('t':s)	 = return ('\t',s)
	readEsc ('v':s)	 = return ('\v',s)
	readEsc ('\\':s) = return ('\\',s)
	readEsc ('"':s)	 = return ('"',s)
	readEsc ('\'':s) = return ('\'',s)
	readEsc ('^':c:s) | c >= '@' && c <= '_'
			 = return (chr (ord c - ord '@'), s)
	readEsc s@(d:_) | isDigit d
			 = do
			  (n,t) <- readDec s
			  return (chr n,t)
	readEsc ('o':s)  = do
			  (n,t) <- readOct s
			  return (chr n,t)
	readEsc ('x':s)	 = do
			  (n,t) <- readHex s
			  return (chr n,t)

	readEsc s@(c:_) | isUpper c
			 = let table = ('\DEL', "DEL") : zip ['\NUL'..] asciiTab
			   in case [(c,s') | (c, mne) <- table,
					     ([],s') <- [match mne s]]
			      of (pr:_) -> return pr
				 []	-> zero
	readEsc _	 = zero

readLitChar (c:s)	=  return (c,s)

match			:: (Eq a) => [a] -> [a] -> ([a],[a])
match (x:xs) (y:ys) | x == y  =  match xs ys
match xs     ys		      =  (xs,ys)

\end{code}


%*********************************************************
%*							*
\subsection{Reading numbers}
%*							*
%*********************************************************

Note: reading numbers at bases different than 10, does not
include lexing common prefixes such as '0x' or '0o' etc.

\begin{code}
{-# SPECIALISE readDec :: 
		ReadS Int,
		ReadS Integer #-}
readDec :: (Integral a) => ReadS a
readDec = readInt 10 isDigit (\d -> ord d - ord_0)

{-# SPECIALISE readOct :: 
		ReadS Int,
		ReadS Integer #-}
readOct :: (Integral a) => ReadS a
readOct = readInt 8 isOctDigit (\d -> ord d - ord_0)

{-# SPECIALISE readHex :: 
		ReadS Int,
		ReadS Integer #-}
readHex :: (Integral a) => ReadS a
readHex = readInt 16 isHexDigit hex
	    where hex d = ord d - (if isDigit d then ord_0
				   else ord (if isUpper d then 'A' else 'a') - 10)

readInt :: (Integral a) => a -> (Char -> Bool) -> (Char -> Int) -> ReadS a
readInt radix isDig digToInt s = do
    (ds,r) <- nonnull isDig s
    return (foldl1 (\n d -> n * radix + d) (map (fromInt . digToInt) ds), r)

{-# SPECIALISE readSigned ::
		ReadS Int     -> ReadS Int,
		ReadS Integer -> ReadS Integer,
		ReadS Double  -> ReadS Double	    #-}
readSigned :: (Real a) => ReadS a -> ReadS a
readSigned readPos = readParen False read'
		     where read' r  = read'' r ++
				      (do
				        ("-",s) <- lex r
					(x,t)   <- read'' s
					return (-x,t))
			   read'' r = do
			       (str,s) <- lex r
		      	       (n,"")  <- readPos str
			       return (n,s)
\end{code}

The functions readFloat below uses rational arithmetic
to ensure correct conversion between the floating-point radix and
decimal.  It is often possible to use a higher-precision floating-
point type to obtain the same results.

\begin{code}
{-# SPECIALISE readFloat ::
		    ReadS Double,
		    ReadS Float	    #-} 
readFloat :: (RealFloat a) => ReadS a
readFloat r = do
    (x,t) <- readRational r
    return (fromRational x,t)

readRational :: ReadS Rational -- NB: doesn't handle leading "-"

readRational r =
   (do 
      (n,d,s) <- readFix r
      (k,t)   <- readExp s
      return ((n%1)*10^^(k-d), t )) ++
   (do
      ("NaN",t) <- lex r
      return (0/0,t) ) ++
   (do
      ("Infinity",t) <- lex r
      return (1/0,t) )
 where
     readFix r = do
	(ds,s)  <- lexDigits r
	(ds',t) <- lexDotDigits s
	return (read (ds++ds'), length ds', t)

     readExp (e:s) | e `elem` "eE" = readExp' s
     readExp s			   = return (0,s)

     readExp' ('+':s) = readDec s
     readExp' ('-':s) = do
			(k,t) <- readDec s
			return (-k,t)
     readExp' s	      = readDec s

     lexDotDigits ('.':s) = lex0Digits s
     lexDotDigits s       = return ("",s)

readRational__ :: String -> Rational -- we export this one (non-std)
				    -- NB: *does* handle a leading "-"
readRational__ top_s
  = case top_s of
      '-' : xs -> - (read_me xs)
      xs       -> read_me xs
  where
    read_me s
      = case (do { (x,t) <- readRational s ; ("","") <- lex t ; return x }) of
#ifndef NEW_READS_REP
	  [x] -> x
	  []  -> error ("readRational__: no parse:"        ++ top_s)
	  _   -> error ("readRational__: ambiguous parse:" ++ top_s)
#else
	  Just x  -> x
	  Nothing -> error ("readRational__: no parse:"        ++ top_s)
#endif

\end{code}
