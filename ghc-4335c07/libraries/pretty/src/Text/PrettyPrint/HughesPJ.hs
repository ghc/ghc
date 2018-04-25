{-# OPTIONS_HADDOCK not-home #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveGeneric #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.PrettyPrint.HughesPJ
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  David Terei <code@davidterei.com>
-- Stability   :  stable
-- Portability :  portable
--
-- Provides a collection of pretty printer combinators, a set of API's that
-- provides a way to easily print out text in a consistent format of your
-- choosing.
--
-- Originally designed by John Hughes's and Simon Peyton Jones's.
--
-- For more information you can refer to the
-- <http://belle.sourceforge.net/doc/hughes95design.pdf original paper> that
-- serves as the basis for this libraries design: /The Design of a
-- Pretty-printing Library/ by John Hughes, in Advanced Functional Programming,
-- 1995.
--
-----------------------------------------------------------------------------

#ifndef TESTING
module Text.PrettyPrint.HughesPJ (

        -- * The document type
        Doc, TextDetails(..),

        -- * Constructing documents

        -- ** Converting values into documents
        char, text, ptext, sizedText, zeroWidthText,
        int, integer, float, double, rational,

        -- ** Simple derived documents
        semi, comma, colon, space, equals,
        lparen, rparen, lbrack, rbrack, lbrace, rbrace,

        -- ** Wrapping documents in delimiters
        parens, brackets, braces, quotes, doubleQuotes,
        maybeParens, maybeBrackets, maybeBraces, maybeQuotes, maybeDoubleQuotes,

        -- ** Combining documents
        empty,
        (<>), (<+>), hcat, hsep,
        ($$), ($+$), vcat,
        sep, cat,
        fsep, fcat,
        nest,
        hang, punctuate,

        -- * Predicates on documents
        isEmpty,

        -- * Utility functions for documents
        first, reduceDoc,

        -- * Rendering documents

        -- ** Default rendering
        render,

        -- ** Rendering with a particular style
        Style(..),
        style,
        renderStyle,
        Mode(..),

        -- ** General rendering
        fullRender

    ) where
#endif

import           Text.PrettyPrint.Annotated.HughesPJ
                     ( TextDetails(..), Mode(..), Style(..), style )
import qualified Text.PrettyPrint.Annotated.HughesPJ as Ann

import Control.DeepSeq ( NFData(rnf) )
import Data.Function   ( on )
#if __GLASGOW_HASKELL__ >= 803
import Prelude         hiding ( (<>) )
#endif
#if __GLASGOW_HASKELL__ >= 800
import qualified Data.Semigroup as Semi ( Semigroup((<>)) )
#elif __GLASGOW_HASKELL__ < 709
import Data.Monoid     ( Monoid(mempty, mappend)  )
#endif
import Data.String     ( IsString(fromString) )

import GHC.Generics


-- ---------------------------------------------------------------------------
-- Operator fixity

infixl 6 <>
infixl 6 <+>
infixl 5 $$, $+$

-- ---------------------------------------------------------------------------
-- The Doc data type

-- | The abstract type of documents. A Doc represents a /set/ of layouts. A
-- Doc with no occurrences of Union or NoDoc represents just one layout.
newtype Doc = Doc (Ann.Doc ())
#if __GLASGOW_HASKELL__ >= 701
                    deriving (Generic)
#endif

liftList :: ([Ann.Doc ()] -> Ann.Doc ()) -> ([Doc] -> Doc)
liftList f ds = Doc (f [ d | Doc d <- ds ])
{-# INLINE liftList #-}

liftBinary :: (Ann.Doc () -> Ann.Doc () -> Ann.Doc ())
           -> (    Doc    ->     Doc    ->     Doc   )
liftBinary f (Doc a) (Doc b) = Doc (f a b)
{-# INLINE liftBinary #-}

-- | RDoc is a "reduced GDoc", guaranteed not to have a top-level Above or
-- Beside.
type RDoc = Doc

-- Combining @Doc@ values
#if __GLASGOW_HASKELL__ >= 800
instance Semi.Semigroup Doc where
    (<>) = (Text.PrettyPrint.HughesPJ.<>)

instance Monoid Doc where
    mempty  = empty
    mappend = (Semi.<>)
#else
instance Monoid Doc where
    mempty  = empty
    mappend = (<>)
#endif

instance IsString Doc where
    fromString = text

instance Show Doc where
  showsPrec _ doc cont = fullRender (mode style) (lineLength style)
                                    (ribbonsPerLine style)
                                    txtPrinter cont doc

instance Eq Doc where
  (==) = (==) `on` render

instance NFData Doc where
  rnf (Doc a) = rnf a

-- ---------------------------------------------------------------------------
-- Values and Predicates on GDocs and TextDetails

-- | A document of height and width 1, containing a literal character.
char :: Char -> Doc
char c = Doc (Ann.char c)
{-# INLINE char #-}

-- | A document of height 1 containing a literal string.
-- 'text' satisfies the following laws:
--
-- * @'text' s '<>' 'text' t = 'text' (s'++'t)@
--
-- * @'text' \"\" '<>' x = x@, if @x@ non-empty
--
-- The side condition on the last law is necessary because @'text' \"\"@
-- has height 1, while 'empty' has no height.
text :: String -> Doc
text s = Doc (Ann.text s)
{-# INLINE text #-}

-- | Same as @text@. Used to be used for Bytestrings.
ptext :: String -> Doc
ptext s = Doc (Ann.ptext s)
{-# INLINE ptext #-}

-- | Some text with any width. (@text s = sizedText (length s) s@)
sizedText :: Int -> String -> Doc
sizedText l s = Doc (Ann.sizedText l s)

-- | Some text, but without any width. Use for non-printing text
-- such as a HTML or Latex tags
zeroWidthText :: String -> Doc
zeroWidthText = sizedText 0

-- | The empty document, with no height and no width.
-- 'empty' is the identity for '<>', '<+>', '$$' and '$+$', and anywhere
-- in the argument list for 'sep', 'hcat', 'hsep', 'vcat', 'fcat' etc.
empty :: Doc
empty = Doc Ann.empty

-- | Returns 'True' if the document is empty
isEmpty :: Doc -> Bool
isEmpty (Doc d) = Ann.isEmpty d

semi   :: Doc -- ^ A ';' character
comma  :: Doc -- ^ A ',' character
colon  :: Doc -- ^ A ':' character
space  :: Doc -- ^ A space character
equals :: Doc -- ^ A '=' character
lparen :: Doc -- ^ A '(' character
rparen :: Doc -- ^ A ')' character
lbrack :: Doc -- ^ A '[' character
rbrack :: Doc -- ^ A ']' character
lbrace :: Doc -- ^ A '{' character
rbrace :: Doc -- ^ A '}' character
semi   = char ';'
comma  = char ','
colon  = char ':'
space  = char ' '
equals = char '='
lparen = char '('
rparen = char ')'
lbrack = char '['
rbrack = char ']'
lbrace = char '{'
rbrace = char '}'

int      :: Int      -> Doc -- ^ @int n = text (show n)@
integer  :: Integer  -> Doc -- ^ @integer n = text (show n)@
float    :: Float    -> Doc -- ^ @float n = text (show n)@
double   :: Double   -> Doc -- ^ @double n = text (show n)@
rational :: Rational -> Doc -- ^ @rational n = text (show n)@
int      n = text (show n)
integer  n = text (show n)
float    n = text (show n)
double   n = text (show n)
rational n = text (show n)

parens       :: Doc -> Doc -- ^ Wrap document in @(...)@
brackets     :: Doc -> Doc -- ^ Wrap document in @[...]@
braces       :: Doc -> Doc -- ^ Wrap document in @{...}@
quotes       :: Doc -> Doc -- ^ Wrap document in @\'...\'@
doubleQuotes :: Doc -> Doc -- ^ Wrap document in @\"...\"@
quotes p       = char '\'' <> p <> char '\''
doubleQuotes p = char '"' <> p <> char '"'
parens p       = char '(' <> p <> char ')'
brackets p     = char '[' <> p <> char ']'
braces p       = char '{' <> p <> char '}'

-- | Apply 'parens' to 'Doc' if boolean is true.
maybeParens :: Bool -> Doc -> Doc
maybeParens False = id
maybeParens True = parens

-- | Apply 'brackets' to 'Doc' if boolean is true.
maybeBrackets :: Bool -> Doc -> Doc
maybeBrackets False = id
maybeBrackets True = brackets

-- | Apply 'braces' to 'Doc' if boolean is true.
maybeBraces :: Bool -> Doc -> Doc
maybeBraces False = id
maybeBraces True = braces

-- | Apply 'quotes' to 'Doc' if boolean is true.
maybeQuotes :: Bool -> Doc -> Doc
maybeQuotes False = id
maybeQuotes True = quotes

-- | Apply 'doubleQuotes' to 'Doc' if boolean is true.
maybeDoubleQuotes :: Bool -> Doc -> Doc
maybeDoubleQuotes False = id
maybeDoubleQuotes True = doubleQuotes

-- ---------------------------------------------------------------------------
-- Structural operations on GDocs

-- | Perform some simplification of a built up @GDoc@.
reduceDoc :: Doc -> RDoc
reduceDoc (Doc d) = Doc (Ann.reduceDoc d)
{-# INLINE reduceDoc #-}

-- | List version of '<>'.
hcat :: [Doc] -> Doc
hcat = liftList Ann.hcat
{-# INLINE hcat #-}

-- | List version of '<+>'.
hsep :: [Doc] -> Doc
hsep = liftList Ann.hsep
{-# INLINE hsep #-}

-- | List version of '$$'.
vcat :: [Doc] -> Doc
vcat = liftList Ann.vcat
{-# INLINE vcat #-}

-- | Nest (or indent) a document by a given number of positions
-- (which may also be negative).  'nest' satisfies the laws:
--
-- * @'nest' 0 x = x@
--
-- * @'nest' k ('nest' k' x) = 'nest' (k+k') x@
--
-- * @'nest' k (x '<>' y) = 'nest' k x '<>' 'nest' k y@
--
-- * @'nest' k (x '$$' y) = 'nest' k x '$$' 'nest' k y@
--
-- * @'nest' k 'empty' = 'empty'@
--
-- * @x '<>' 'nest' k y = x '<>' y@, if @x@ non-empty
--
-- The side condition on the last law is needed because
-- 'empty' is a left identity for '<>'.
nest :: Int -> Doc -> Doc
nest k (Doc p) = Doc (Ann.nest k p)
{-# INLINE nest #-}

-- | @hang d1 n d2 = sep [d1, nest n d2]@
hang :: Doc -> Int -> Doc -> Doc
hang (Doc d1) n (Doc d2) = Doc (Ann.hang d1 n d2)
{-# INLINE hang #-}

-- | @punctuate p [d1, ... dn] = [d1 \<> p, d2 \<> p, ... dn-1 \<> p, dn]@
punctuate :: Doc -> [Doc] -> [Doc]
punctuate (Doc p) ds = [ Doc d | d <- Ann.punctuate p [ d | Doc d <- ds ] ]
{-# INLINE punctuate #-}


-- ---------------------------------------------------------------------------
-- Vertical composition @$$@

-- | Above, except that if the last line of the first argument stops
-- at least one position before the first line of the second begins,
-- these two lines are overlapped.  For example:
--
-- >    text "hi" $$ nest 5 (text "there")
--
-- lays out as
--
-- >    hi   there
--
-- rather than
--
-- >    hi
-- >         there
--
-- '$$' is associative, with identity 'empty', and also satisfies
--
-- * @(x '$$' y) '<>' z = x '$$' (y '<>' z)@, if @y@ non-empty.
--
($$) :: Doc -> Doc -> Doc
($$) = liftBinary (Ann.$$)
{-# INLINE ($$) #-}

-- | Above, with no overlapping.
-- '$+$' is associative, with identity 'empty'.
($+$) :: Doc -> Doc -> Doc
($+$) = liftBinary (Ann.$+$)
{-# INLINE ($+$) #-}


-- ---------------------------------------------------------------------------
-- Horizontal composition @<>@

-- We intentionally avoid Data.Monoid.(<>) here due to interactions of
-- Data.Monoid.(<>) and (<+>).  See
-- http://www.haskell.org/pipermail/libraries/2011-November/017066.html

-- | Beside.
-- '<>' is associative, with identity 'empty'.
(<>) :: Doc -> Doc -> Doc
(<>) = liftBinary (Ann.<>)
{-# INLINE (<>) #-}

-- | Beside, separated by space, unless one of the arguments is 'empty'.
-- '<+>' is associative, with identity 'empty'.
(<+>) :: Doc -> Doc -> Doc
(<+>) = liftBinary (Ann.<+>)
{-# INLINE (<+>) #-}


-- ---------------------------------------------------------------------------
-- Separate, @sep@

-- Specification: sep ps  = oneLiner (hsep ps)
--                         `union`
--                          vcat ps

-- | Either 'hsep' or 'vcat'.
sep  :: [Doc] -> Doc
sep  = liftList Ann.sep
{-# INLINE sep #-}

-- | Either 'hcat' or 'vcat'.
cat :: [Doc] -> Doc
cat = liftList Ann.cat
{-# INLINE cat #-}


-- ---------------------------------------------------------------------------
-- @fill@

-- | \"Paragraph fill\" version of 'cat'.
fcat :: [Doc] -> Doc
fcat = liftList Ann.fcat
{-# INLINE fcat #-}

-- | \"Paragraph fill\" version of 'sep'.
fsep :: [Doc] -> Doc
fsep = liftList Ann.fsep
{-# INLINE fsep #-}


-- ---------------------------------------------------------------------------
-- Selecting the best layout

-- | @first@ returns its first argument if it is non-empty, otherwise its second.
first :: Doc -> Doc -> Doc
first  = liftBinary Ann.first
{-# INLINE first #-}


-- ---------------------------------------------------------------------------
-- Rendering

-- | Render the @Doc@ to a String using the default @Style@ (see 'style').
render :: Doc -> String
render = fullRender (mode style) (lineLength style) (ribbonsPerLine style)
                    txtPrinter ""
{-# INLINE render #-}

-- | Render the @Doc@ to a String using the given @Style@.
renderStyle :: Style -> Doc -> String
renderStyle s = fullRender (mode s) (lineLength s) (ribbonsPerLine s)
                txtPrinter ""
{-# INLINE renderStyle #-}

-- | Default TextDetails printer.
txtPrinter :: TextDetails -> String -> String
txtPrinter (Chr c)   s  = c:s
txtPrinter (Str s1)  s2 = s1 ++ s2
txtPrinter (PStr s1) s2 = s1 ++ s2

-- | The general rendering interface. Please refer to the @Style@ and @Mode@
-- types for a description of rendering mode, line length and ribbons.
fullRender :: Mode                     -- ^ Rendering mode.
           -> Int                      -- ^ Line length.
           -> Float                    -- ^ Ribbons per line.
           -> (TextDetails -> a -> a)  -- ^ What to do with text.
           -> a                        -- ^ What to do at the end.
           -> Doc                      -- ^ The document.
           -> a                        -- ^ Result.
fullRender m lineLen ribbons txt rest (Doc doc)
  = Ann.fullRender m lineLen ribbons txt rest doc
{-# INLINE fullRender #-}

