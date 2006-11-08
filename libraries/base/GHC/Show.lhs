\begin{code}
{-# OPTIONS_GHC -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Show
-- Copyright   :  (c) The University of Glasgow, 1992-2002
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- The 'Show' class, and related operations.
--
-----------------------------------------------------------------------------

-- #hide
module GHC.Show
	(
	Show(..), ShowS,

	-- Instances for Show: (), [], Bool, Ordering, Int, Char

	-- Show support code
	shows, showChar, showString, showParen, showList__, showSpace,
	showLitChar, protectEsc, 
	intToDigit, showSignedInt,
	appPrec, appPrec1,

	-- Character operations
	asciiTab,
  ) 
	where

import GHC.Base
import Data.Maybe
import Data.Either
import GHC.List	( (!!), foldr1
#ifdef USE_REPORT_PRELUDE
                , concatMap
#endif
                )
\end{code}



%*********************************************************
%*							*
\subsection{The @Show@ class}
%*							*
%*********************************************************

\begin{code}
-- | The @shows@ functions return a function that prepends the
-- output 'String' to an existing 'String'.  This allows constant-time
-- concatenation of results using function composition.
type ShowS = String -> String

-- | Conversion of values to readable 'String's.
--
-- Minimal complete definition: 'showsPrec' or 'show'.
--
-- Derived instances of 'Show' have the following properties, which
-- are compatible with derived instances of 'Text.Read.Read':
--
-- * The result of 'show' is a syntactically correct Haskell
--   expression containing only constants, given the fixity
--   declarations in force at the point where the type is declared.
--   It contains only the constructor names defined in the data type,
--   parentheses, and spaces.  When labelled constructor fields are
--   used, braces, commas, field names, and equal signs are also used.
--
-- * If the constructor is defined to be an infix operator, then
--   'showsPrec' will produce infix applications of the constructor.
--
-- * the representation will be enclosed in parentheses if the
--   precedence of the top-level constructor in @x@ is less than @d@
--   (associativity is ignored).  Thus, if @d@ is @0@ then the result
--   is never surrounded in parentheses; if @d@ is @11@ it is always
--   surrounded in parentheses, unless it is an atomic expression.
--
-- * If the constructor is defined using record syntax, then 'show'
--   will produce the record-syntax form, with the fields given in the
--   same order as the original declaration.
--
-- For example, given the declarations
--
-- > infixr 5 :^:
-- > data Tree a =  Leaf a  |  Tree a :^: Tree a
--
-- the derived instance of 'Show' is equivalent to
--
-- > instance (Show a) => Show (Tree a) where
-- >
-- >        showsPrec d (Leaf m) = showParen (d > app_prec) $
-- >             showString "Leaf " . showsPrec (app_prec+1) m
-- >          where app_prec = 10
-- >
-- >        showsPrec d (u :^: v) = showParen (d > up_prec) $
-- >             showsPrec (up_prec+1) u . 
-- >             showString " :^: "      .
-- >             showsPrec (up_prec+1) v
-- >          where up_prec = 5
--
-- Note that right-associativity of @:^:@ is ignored.  For example,
--
-- * @'show' (Leaf 1 :^: Leaf 2 :^: Leaf 3)@ produces the string
--   @\"Leaf 1 :^: (Leaf 2 :^: Leaf 3)\"@.

class  Show a  where
    -- | Convert a value to a readable 'String'.
    --
    -- 'showsPrec' should satisfy the law
    --
    -- > showsPrec d x r ++ s  ==  showsPrec d x (r ++ s)
    --
    -- Derived instances of 'Text.Read.Read' and 'Show' satisfy the following:
    --
    -- * @(x,\"\")@ is an element of
    --   @('Text.Read.readsPrec' d ('showsPrec' d x \"\"))@.
    --
    -- That is, 'Text.Read.readsPrec' parses the string produced by
    -- 'showsPrec', and delivers the value that 'showsPrec' started with.

    showsPrec :: Int	-- ^ the operator precedence of the enclosing
			-- context (a number from @0@ to @11@).
			-- Function application has precedence @10@.
	      -> a	-- ^ the value to be converted to a 'String'
	      -> ShowS

    -- | A specialised variant of 'showsPrec', using precedence context
    -- zero, and returning an ordinary 'String'.
    show      :: a   -> String

    -- | The method 'showList' is provided to allow the programmer to
    -- give a specialised way of showing lists of values.
    -- For example, this is used by the predefined 'Show' instance of
    -- the 'Char' type, where values of type 'String' should be shown
    -- in double quotes, rather than between square brackets.
    showList  :: [a] -> ShowS

    showsPrec _ x s = show x ++ s
    show x          = shows x ""
    showList ls   s = showList__ shows ls s

showList__ :: (a -> ShowS) ->  [a] -> ShowS
showList__ _     []     s = "[]" ++ s
showList__ showx (x:xs) s = '[' : showx x (showl xs)
  where
    showl []     = ']' : s
    showl (y:ys) = ',' : showx y (showl ys)

appPrec, appPrec1 :: Int
	-- Use unboxed stuff because we don't have overloaded numerics yet
appPrec = I# 10#	-- Precedence of application:
			--   one more than the maximum operator precedence of 9
appPrec1 = I# 11#	-- appPrec + 1
\end{code}

%*********************************************************
%*							*
\subsection{Simple Instances}
%*							*
%*********************************************************

\begin{code}
 
instance  Show ()  where
    showsPrec _ () = showString "()"

instance Show a => Show [a]  where
    showsPrec _         = showList

instance Show Bool where
  showsPrec _ True  = showString "True"
  showsPrec _ False = showString "False"

instance Show Ordering where
  showsPrec _ LT = showString "LT"
  showsPrec _ EQ = showString "EQ"
  showsPrec _ GT = showString "GT"

instance  Show Char  where
    showsPrec _ '\'' = showString "'\\''"
    showsPrec _ c    = showChar '\'' . showLitChar c . showChar '\''

    showList cs = showChar '"' . showl cs
		 where showl ""       s = showChar '"' s
		       showl ('"':xs) s = showString "\\\"" (showl xs s)
		       showl (x:xs)   s = showLitChar x (showl xs s)
		-- Making 's' an explicit parameter makes it clear to GHC
		-- that showl has arity 2, which avoids it allocating an extra lambda
		-- The sticking point is the recursive call to (showl xs), which
		-- it can't figure out would be ok with arity 2.

instance Show Int where
    showsPrec = showSignedInt

instance Show a => Show (Maybe a) where
    showsPrec _p Nothing s = showString "Nothing" s
    showsPrec p (Just x) s
                          = (showParen (p > appPrec) $ 
    			     showString "Just " . 
			     showsPrec appPrec1 x) s

instance (Show a, Show b) => Show (Either a b) where
    showsPrec p e s =
       (showParen (p > appPrec) $
        case e of
         Left  a -> showString "Left "  . showsPrec appPrec1 a
	 Right b -> showString "Right " . showsPrec appPrec1 b)
       s
\end{code}


%*********************************************************
%*							*
\subsection{Show instances for the first few tuples
%*							*
%*********************************************************

\begin{code}
-- The explicit 's' parameters are important
-- Otherwise GHC thinks that "shows x" might take a lot of work to compute
-- and generates defns like
--	showsPrec _ (x,y) = let sx = shows x; sy = shows y in
--			    \s -> showChar '(' (sx (showChar ',' (sy (showChar ')' s))))

instance  (Show a, Show b) => Show (a,b)  where
  showsPrec _ (a,b) s = show_tuple [shows a, shows b] s

instance (Show a, Show b, Show c) => Show (a, b, c) where
  showsPrec _ (a,b,c) s = show_tuple [shows a, shows b, shows c] s

instance (Show a, Show b, Show c, Show d) => Show (a, b, c, d) where
  showsPrec _ (a,b,c,d) s = show_tuple [shows a, shows b, shows c, shows d] s

instance (Show a, Show b, Show c, Show d, Show e) => Show (a, b, c, d, e) where
  showsPrec _ (a,b,c,d,e) s = show_tuple [shows a, shows b, shows c, shows d, shows e] s

instance (Show a, Show b, Show c, Show d, Show e, Show f) => Show (a,b,c,d,e,f) where
  showsPrec _ (a,b,c,d,e,f) s = show_tuple [shows a, shows b, shows c, shows d, shows e, shows f] s

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g)
	=> Show (a,b,c,d,e,f,g) where
  showsPrec _ (a,b,c,d,e,f,g) s 
	= show_tuple [shows a, shows b, shows c, shows d, shows e, shows f, shows g] s

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h)
	 => Show (a,b,c,d,e,f,g,h) where
  showsPrec _ (a,b,c,d,e,f,g,h) s 
	= show_tuple [shows a, shows b, shows c, shows d, shows e, shows f, shows g, shows h] s

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i)
	 => Show (a,b,c,d,e,f,g,h,i) where
  showsPrec _ (a,b,c,d,e,f,g,h,i) s 
	= show_tuple [shows a, shows b, shows c, shows d, shows e, shows f, shows g, shows h, 
		      shows i] s

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j)
	 => Show (a,b,c,d,e,f,g,h,i,j) where
  showsPrec _ (a,b,c,d,e,f,g,h,i,j) s 
	= show_tuple [shows a, shows b, shows c, shows d, shows e, shows f, shows g, shows h, 
		      shows i, shows j] s

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k)
	 => Show (a,b,c,d,e,f,g,h,i,j,k) where
  showsPrec _ (a,b,c,d,e,f,g,h,i,j,k) s 
	= show_tuple [shows a, shows b, shows c, shows d, shows e, shows f, shows g, shows h, 
		      shows i, shows j, shows k] s

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k,
	  Show l)
	 => Show (a,b,c,d,e,f,g,h,i,j,k,l) where
  showsPrec _ (a,b,c,d,e,f,g,h,i,j,k,l) s 
	= show_tuple [shows a, shows b, shows c, shows d, shows e, shows f, shows g, shows h, 
		      shows i, shows j, shows k, shows l] s

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k,
	  Show l, Show m)
	 => Show (a,b,c,d,e,f,g,h,i,j,k,l,m) where
  showsPrec _ (a,b,c,d,e,f,g,h,i,j,k,l,m) s 
	= show_tuple [shows a, shows b, shows c, shows d, shows e, shows f, shows g, shows h, 
		      shows i, shows j, shows k, shows l, shows m] s

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k,
	  Show l, Show m, Show n)
	 => Show (a,b,c,d,e,f,g,h,i,j,k,l,m,n) where
  showsPrec _ (a,b,c,d,e,f,g,h,i,j,k,l,m,n) s 
	= show_tuple [shows a, shows b, shows c, shows d, shows e, shows f, shows g, shows h, 
		      shows i, shows j, shows k, shows l, shows m, shows n] s

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k,
	  Show l, Show m, Show n, Show o)
	 => Show (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) where
  showsPrec _ (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) s 
	= show_tuple [shows a, shows b, shows c, shows d, shows e, shows f, shows g, shows h, 
		      shows i, shows j, shows k, shows l, shows m, shows n, shows o] s

show_tuple :: [ShowS] -> ShowS
show_tuple ss = showChar '('
	      .	foldr1 (\s r -> s . showChar ',' . r) ss
	      .	showChar ')'
\end{code}


%*********************************************************
%*							*
\subsection{Support code for @Show@}
%*							*
%*********************************************************

\begin{code}
-- | equivalent to 'showsPrec' with a precedence of 0.
shows           :: (Show a) => a -> ShowS
shows           =  showsPrec zeroInt

-- | utility function converting a 'Char' to a show function that
-- simply prepends the character unchanged.
showChar        :: Char -> ShowS
showChar        =  (:)

-- | utility function converting a 'String' to a show function that
-- simply prepends the string unchanged.
showString      :: String -> ShowS
showString      =  (++)

-- | utility function that surrounds the inner show function with
-- parentheses when the 'Bool' parameter is 'True'.
showParen       :: Bool -> ShowS -> ShowS
showParen b p   =  if b then showChar '(' . p . showChar ')' else p

showSpace :: ShowS
showSpace = {-showChar ' '-} \ xs -> ' ' : xs
\end{code}

Code specific for characters

\begin{code}
-- | Convert a character to a string using only printable characters,
-- using Haskell source-language escape conventions.  For example:
--
-- > showLitChar '\n' s  =  "\\n" ++ s
--
showLitChar 		   :: Char -> ShowS
showLitChar c s | c > '\DEL' =  showChar '\\' (protectEsc isDec (shows (ord c)) s)
showLitChar '\DEL'	   s =  showString "\\DEL" s
showLitChar '\\'	   s =  showString "\\\\" s
showLitChar c s | c >= ' '   =  showChar c s
showLitChar '\a'	   s =  showString "\\a" s
showLitChar '\b'	   s =  showString "\\b" s
showLitChar '\f'	   s =  showString "\\f" s
showLitChar '\n'	   s =  showString "\\n" s
showLitChar '\r'	   s =  showString "\\r" s
showLitChar '\t'	   s =  showString "\\t" s
showLitChar '\v'	   s =  showString "\\v" s
showLitChar '\SO'	   s =  protectEsc (== 'H') (showString "\\SO") s
showLitChar c		   s =  showString ('\\' : asciiTab!!ord c) s
	-- I've done manual eta-expansion here, becuase otherwise it's
	-- impossible to stop (asciiTab!!ord) getting floated out as an MFE

isDec c = c >= '0' && c <= '9'

protectEsc :: (Char -> Bool) -> ShowS -> ShowS
protectEsc p f		   = f . cont
			     where cont s@(c:_) | p c = "\\&" ++ s
				   cont s	      = s


asciiTab :: [String]
asciiTab = -- Using an array drags in the array module.  listArray ('\NUL', ' ')
	   ["NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL",
	    "BS",  "HT",  "LF",  "VT",  "FF",  "CR",  "SO",  "SI", 
	    "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB",
	    "CAN", "EM",  "SUB", "ESC", "FS",  "GS",  "RS",  "US", 
	    "SP"] 
\end{code}

Code specific for Ints.

\begin{code}
-- | Convert an 'Int' in the range @0@..@15@ to the corresponding single
-- digit 'Char'.  This function fails on other inputs, and generates
-- lower-case hexadecimal digits.
intToDigit :: Int -> Char
intToDigit (I# i)
    | i >=# 0#  && i <=#  9# =  unsafeChr (ord '0' `plusInt` I# i)
    | i >=# 10# && i <=# 15# =  unsafeChr (ord 'a' `minusInt` ten `plusInt` I# i)
    | otherwise		  =  error ("Char.intToDigit: not a digit " ++ show (I# i))

ten = I# 10#

showSignedInt :: Int -> Int -> ShowS
showSignedInt (I# p) (I# n) r
    | n <# 0# && p ># 6# = '(' : itos n (')' : r)
    | otherwise          = itos n r

itos :: Int# -> String -> String
itos n# cs
    | n# <# 0# =
	let I# minInt# = minInt in
	if n# ==# minInt#
		-- negateInt# minInt overflows, so we can't do that:
	   then '-' : itos' (negateInt# (n# `quotInt#` 10#))
                             (itos' (negateInt# (n# `remInt#` 10#)) cs)
	   else '-' : itos' (negateInt# n#) cs
    | otherwise = itos' n# cs
    where
    itos' :: Int# -> String -> String
    itos' n# cs
        | n# <# 10#  = C# (chr# (ord# '0'# +# n#)) : cs
        | otherwise = case chr# (ord# '0'# +# (n# `remInt#` 10#)) of { c# ->
		      itos' (n# `quotInt#` 10#) (C# c# : cs) }
\end{code}

%*********************************************************
%*							*
\subsection{Other instances}
%*							*
%*********************************************************

\begin{code}
instance Show Unknown where
 show _ = "This is an untyped, unevaluated computation. You can use seq to force its evaluation and then :print to recover its type"
 showList _ = ("This is an untyped, unevaluated computation. You can use seq to force its evaluation and then :print to recover its type" ++)

instance Show (Unknown1 a) where
 show _ = "This is an untyped, unevaluated computation. You can use seq to force its evaluation and then :print to recover its type"
 showList _ = ("This is an untyped, unevaluated computation. You can use seq to force its evaluation and then :print to recover its type" ++)

instance Show (Unknown2 a b) where
 show _ = "This is an untyped, unevaluated computation. You can use seq to force its evaluation and then :print to recover its type"
 showList _ = ("This is an untyped, unevaluated computation. You can use seq to force its evaluation and then :print to recover its type" ++)

instance Show (Unknown3 a b c) where
 show _ = "This is an untyped, unevaluated computation. You can use seq to force its evaluation and then :print to recover its type"
 showList _ = ("This is an untyped, unevaluated computation. You can use seq to force its evaluation and then :print to recover its type" ++)

instance Show (Unknown4 a b c d) where
 show _ = "This is an untyped, unevaluated computation. You can use seq to force its evaluation and then :print to recover its type"
 showList _ = ("This is an untyped, unevaluated computation. You can use seq to force its evaluation and then :print to recover its type" ++)

\end{code}