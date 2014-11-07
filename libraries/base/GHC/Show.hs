{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, NoImplicitPrelude, BangPatterns, StandaloneDeriving,
             MagicHash, UnboxedTuples #-}
{-# OPTIONS_HADDOCK hide #-}

#include "MachDeps.h"
#if SIZEOF_HSWORD == 4
#define DIGITS       9
#define BASE         1000000000
#elif SIZEOF_HSWORD == 8
#define DIGITS       18
#define BASE         1000000000000000000
#else
#error Please define DIGITS and BASE
-- DIGITS should be the largest integer such that
--     10^DIGITS < 2^(SIZEOF_HSWORD * 8 - 1)
-- BASE should be 10^DIGITS. Note that ^ is not available yet.
#endif

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

module GHC.Show
        (
        Show(..), ShowS,

        -- Instances for Show: (), [], Bool, Ordering, Int, Char

        -- Show support code
        shows, showChar, showString, showMultiLineString,
        showParen, showList__, showSpace,
        showLitChar, showLitString, protectEsc,
        intToDigit, showSignedInt,
        appPrec, appPrec1,

        -- Character operations
        asciiTab,
  )
        where

import GHC.Base
import GHC.Num
import GHC.List ((!!), foldr1, break)

-- | The @shows@ functions return a function that prepends the
-- output 'String' to an existing 'String'.  This allows constant-time
-- concatenation of results using function composition.
type ShowS = String -> String

-- | Conversion of values to readable 'String's.
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
    {-# MINIMAL showsPrec | show #-}

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

    showsPrec :: Int    -- ^ the operator precedence of the enclosing
                        -- context (a number from @0@ to @11@).
                        -- Function application has precedence @10@.
              -> a      -- ^ the value to be converted to a 'String'
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
appPrec = I# 10#        -- Precedence of application:
                        --   one more than the maximum operator precedence of 9
appPrec1 = I# 11#       -- appPrec + 1

--------------------------------------------------------------
-- Simple Instances
--------------------------------------------------------------

deriving instance Show ()

instance Show a => Show [a]  where
  {-# SPECIALISE instance Show [String] #-}
  {-# SPECIALISE instance Show [Char] #-}
  {-# SPECIALISE instance Show [Int] #-}
  showsPrec _         = showList

deriving instance Show Bool
deriving instance Show Ordering

instance  Show Char  where
    showsPrec _ '\'' = showString "'\\''"
    showsPrec _ c    = showChar '\'' . showLitChar c . showChar '\''

    showList cs = showChar '"' . showLitString cs . showChar '"'

instance Show Int where
    showsPrec = showSignedInt

instance Show Word where
    showsPrec _ (W# w) = showWord w

showWord :: Word# -> ShowS
showWord w# cs
 | isTrue# (w# `ltWord#` 10##) = C# (chr# (ord# '0'# +# word2Int# w#)) : cs
 | otherwise = case chr# (ord# '0'# +# word2Int# (w# `remWord#` 10##)) of
               c# ->
                   showWord (w# `quotWord#` 10##) (C# c# : cs)

deriving instance Show a => Show (Maybe a)

--------------------------------------------------------------
-- Show instances for the first few tuple
--------------------------------------------------------------

-- The explicit 's' parameters are important
-- Otherwise GHC thinks that "shows x" might take a lot of work to compute
-- and generates defns like
--      showsPrec _ (x,y) = let sx = shows x; sy = shows y in
--                          \s -> showChar '(' (sx (showChar ',' (sy (showChar ')' s))))

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
              . foldr1 (\s r -> s . showChar ',' . r) ss
              . showChar ')'

--------------------------------------------------------------
-- Support code for Show
--------------------------------------------------------------

-- | equivalent to 'showsPrec' with a precedence of 0.
shows           :: (Show a) => a -> ShowS
shows           =  showsPrec 0

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

-- Code specific for characters

-- | Convert a character to a string using only printable characters,
-- using Haskell source-language escape conventions.  For example:
--
-- > showLitChar '\n' s  =  "\\n" ++ s
--
showLitChar                :: Char -> ShowS
showLitChar c s | c > '\DEL' =  showChar '\\' (protectEsc isDec (shows (ord c)) s)
showLitChar '\DEL'         s =  showString "\\DEL" s
showLitChar '\\'           s =  showString "\\\\" s
showLitChar c s | c >= ' '   =  showChar c s
showLitChar '\a'           s =  showString "\\a" s
showLitChar '\b'           s =  showString "\\b" s
showLitChar '\f'           s =  showString "\\f" s
showLitChar '\n'           s =  showString "\\n" s
showLitChar '\r'           s =  showString "\\r" s
showLitChar '\t'           s =  showString "\\t" s
showLitChar '\v'           s =  showString "\\v" s
showLitChar '\SO'          s =  protectEsc (== 'H') (showString "\\SO") s
showLitChar c              s =  showString ('\\' : asciiTab!!ord c) s
        -- I've done manual eta-expansion here, because otherwise it's
        -- impossible to stop (asciiTab!!ord) getting floated out as an MFE

showLitString :: String -> ShowS
-- | Same as 'showLitChar', but for strings
-- It converts the string to a string using Haskell escape conventions
-- for non-printable characters. Does not add double-quotes around the
-- whole thing; the caller should do that.
-- The main difference from showLitChar (apart from the fact that the
-- argument is a string not a list) is that we must escape double-quotes
showLitString []         s = s
showLitString ('"' : cs) s = showString "\\\"" (showLitString cs s)
showLitString (c   : cs) s = showLitChar c (showLitString cs s)
   -- Making 's' an explicit parameter makes it clear to GHC that
   -- showLitString has arity 2, which avoids it allocating an extra lambda
   -- The sticking point is the recursive call to (showLitString cs), which
   -- it can't figure out would be ok with arity 2.

showMultiLineString :: String -> [String]
-- | Like 'showLitString' (expand escape characters using Haskell
-- escape conventions), but
--   * break the string into multiple lines
--   * wrap the entire thing in double quotes
-- Example:  @showMultiLineString "hello\ngoodbye\nblah"@
-- returns   @["\"hello\\n\\", "\\goodbye\n\\", "\\blah\""]@
showMultiLineString str
  = go '\"' str
  where
    go ch s = case break (== '\n') s of
                (l, _:s'@(_:_)) -> (ch : showLitString l "\\n\\") : go '\\' s'
                (l, "\n")       -> [ch : showLitString l "\\n\""]
                (l, _)          -> [ch : showLitString l "\""]

isDec :: Char -> Bool
isDec c = c >= '0' && c <= '9'

protectEsc :: (Char -> Bool) -> ShowS -> ShowS
protectEsc p f             = f . cont
                             where cont s@(c:_) | p c = "\\&" ++ s
                                   cont s             = s


asciiTab :: [String]
asciiTab = -- Using an array drags in the array module.  listArray ('\NUL', ' ')
           ["NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL",
            "BS",  "HT",  "LF",  "VT",  "FF",  "CR",  "SO",  "SI",
            "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB",
            "CAN", "EM",  "SUB", "ESC", "FS",  "GS",  "RS",  "US",
            "SP"]

-- Code specific for Ints.

-- | Convert an 'Int' in the range @0@..@15@ to the corresponding single
-- digit 'Char'.  This function fails on other inputs, and generates
-- lower-case hexadecimal digits.
intToDigit :: Int -> Char
intToDigit (I# i)
    | isTrue# (i >=# 0#)  && isTrue# (i <=#  9#) = unsafeChr (ord '0' + I# i)
    | isTrue# (i >=# 10#) && isTrue# (i <=# 15#) = unsafeChr (ord 'a' + I# i - 10)
    | otherwise =  error ("Char.intToDigit: not a digit " ++ show (I# i))

showSignedInt :: Int -> Int -> ShowS
showSignedInt (I# p) (I# n) r
    | isTrue# (n <# 0#) && isTrue# (p ># 6#) = '(' : itos n (')' : r)
    | otherwise                              = itos n r

itos :: Int# -> String -> String
itos n# cs
    | isTrue# (n# <# 0#) =
        let !(I# minInt#) = minInt in
        if isTrue# (n# ==# minInt#)
                -- negateInt# minInt overflows, so we can't do that:
           then '-' : (case n# `quotRemInt#` 10# of
                       (# q, r #) ->
                           itos' (negateInt# q) (itos' (negateInt# r) cs))
           else '-' : itos' (negateInt# n#) cs
    | otherwise = itos' n# cs
    where
    itos' :: Int# -> String -> String
    itos' x# cs'
        | isTrue# (x# <# 10#) = C# (chr# (ord# '0'# +# x#)) : cs'
        | otherwise = case x# `quotRemInt#` 10# of
                      (# q, r #) ->
                          case chr# (ord# '0'# +# r) of
                          c# ->
                              itos' q (C# c# : cs')

--------------------------------------------------------------
-- The Integer instances for Show
--------------------------------------------------------------

instance Show Integer where
    showsPrec p n r
        | p > 6 && n < 0 = '(' : integerToString n (')' : r)
        -- Minor point: testing p first gives better code
        -- in the not-uncommon case where the p argument
        -- is a constant
        | otherwise = integerToString n r
    showList = showList__ (showsPrec 0)

-- Divide an conquer implementation of string conversion
integerToString :: Integer -> String -> String
integerToString n0 cs0
    | n0 < 0    = '-' : integerToString' (- n0) cs0
    | otherwise = integerToString' n0 cs0
    where
    integerToString' :: Integer -> String -> String
    integerToString' n cs
        | n < BASE  = jhead (fromInteger n) cs
        | otherwise = jprinth (jsplitf (BASE*BASE) n) cs

    -- Split n into digits in base p. We first split n into digits
    -- in base p*p and then split each of these digits into two.
    -- Note that the first 'digit' modulo p*p may have a leading zero
    -- in base p that we need to drop - this is what jsplith takes care of.
    -- jsplitb the handles the remaining digits.
    jsplitf :: Integer -> Integer -> [Integer]
    jsplitf p n
        | p > n     = [n]
        | otherwise = jsplith p (jsplitf (p*p) n)

    jsplith :: Integer -> [Integer] -> [Integer]
    jsplith p (n:ns) =
        case n `quotRemInteger` p of
        (# q, r #) ->
            if q > 0 then q : r : jsplitb p ns
                     else     r : jsplitb p ns
    jsplith _ [] = error "jsplith: []"

    jsplitb :: Integer -> [Integer] -> [Integer]
    jsplitb _ []     = []
    jsplitb p (n:ns) = case n `quotRemInteger` p of
                       (# q, r #) ->
                           q : r : jsplitb p ns

    -- Convert a number that has been split into digits in base BASE^2
    -- this includes a last splitting step and then conversion of digits
    -- that all fit into a machine word.
    jprinth :: [Integer] -> String -> String
    jprinth (n:ns) cs =
        case n `quotRemInteger` BASE of
        (# q', r' #) ->
            let q = fromInteger q'
                r = fromInteger r'
            in if q > 0 then jhead q $ jblock r $ jprintb ns cs
                        else jhead r $ jprintb ns cs
    jprinth [] _ = error "jprinth []"

    jprintb :: [Integer] -> String -> String
    jprintb []     cs = cs
    jprintb (n:ns) cs = case n `quotRemInteger` BASE of
                        (# q', r' #) ->
                            let q = fromInteger q'
                                r = fromInteger r'
                            in jblock q $ jblock r $ jprintb ns cs

    -- Convert an integer that fits into a machine word. Again, we have two
    -- functions, one that drops leading zeros (jhead) and one that doesn't
    -- (jblock)
    jhead :: Int -> String -> String
    jhead n cs
        | n < 10    = case unsafeChr (ord '0' + n) of
            c@(C# _) -> c : cs
        | otherwise = case unsafeChr (ord '0' + r) of
            c@(C# _) -> jhead q (c : cs)
        where
        (q, r) = n `quotRemInt` 10

    jblock = jblock' {- ' -} DIGITS

    jblock' :: Int -> Int -> String -> String
    jblock' d n cs
        | d == 1    = case unsafeChr (ord '0' + n) of
             c@(C# _) -> c : cs
        | otherwise = case unsafeChr (ord '0' + r) of
             c@(C# _) -> jblock' (d - 1) q (c : cs)
        where
        (q, r) = n `quotRemInt` 10
