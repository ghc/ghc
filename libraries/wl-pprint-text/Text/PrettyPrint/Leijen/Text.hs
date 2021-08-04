{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.PrettyPrint.Leijen.Text
-- Copyright   :  Ivan Lazar Miljenovic (c) 2010,
--                Daan Leijen (c) 2000, http://www.cs.uu.nl/~daan
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Ivan.Miljenovic@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This library is a port of the /wl-pprint/ package to use 'Text' values rather than 'String's.
--
-- Pretty print module based on Philip Wadler's \"prettier printer\"
--
-- @
--      \"A prettier printer\"
--      Draft paper, April 1997, revised March 1998.
--      <http://cm.bell-labs.com/cm/cs/who/wadler/papers/prettier/prettier.ps>
-- @
--
-- PPrint is an implementation of the pretty printing combinators
-- described by Philip Wadler (1997). In their bare essence, the
-- combinators of Wadler are not expressive enough to describe some
-- commonly occurring layouts. The PPrint library adds new primitives
-- to describe these layouts and works well in practice.
--
-- The library is based on a single way to concatenate documents,
-- which is associative and has both a left and right unit.  This
-- simple design leads to an efficient and short implementation. The
-- simplicity is reflected in the predictable behaviour of the
-- combinators which make them easy to use in practice.
--
-- A thorough description of the primitive combinators and their
-- implementation can be found in Philip Wadler's paper
-- (1997). Additions and the main differences with his original paper
-- are:
--
-- * The nil document is called empty.
--
-- * The above combinator is called '<$>'. The operator '</>' is used
--   for soft line breaks.
--
-- * There are three new primitives: 'align', 'fill' and
--   'fillBreak'. These are very useful in practice.
--
-- * Lots of other useful combinators, like 'fillSep' and 'list'.
--
-- * There are two renderers, 'renderPretty' for pretty printing and
--   'renderCompact' for compact output. The pretty printing algorithm
--   also uses a ribbon-width now for even prettier output.
--
-- * There are two displayers, 'displayT' for 'Text' values and 'displayIO'
--   for file based output.
--
-- * There is a 'Pretty' class.
--
-- * The implementation uses optimised representations and strictness
--   annotations.
--
-- Ways that this library differs from /wl-pprint/ (apart from using
-- 'Text' rather than 'String'):
--
-- * Smarter treatment of 'empty' sub-documents (partially copied over
--   from the /pretty/ library).
-----------------------------------------------------------
module Text.PrettyPrint.Leijen.Text (
   -- * Documents
   Doc,

   -- * Basic combinators
   empty, isEmpty, char, text, textStrict, beside, nest, line, linebreak, group,
   softline, softbreak, spacebreak,

   -- * Alignment
   --
   -- | The combinators in this section can not be described by Wadler's
   --   original combinators. They align their output relative to the
   --   current output position - in contrast to @nest@ which always
   --   aligns to the current nesting level. This deprives these
   --   combinators from being \`optimal\'. In practice however they
   --   prove to be very useful. The combinators in this section should
   --   be used with care, since they are more expensive than the other
   --   combinators. For example, @align@ shouldn't be used to pretty
   --   print all top-level declarations of a language, but using @hang@
   --   for let expressions is fine.
   align, hang, indent, encloseSep, list, tupled, semiBraces,

   -- * Operators
   (<+>), (<++>), (<$>), (</>), (<$$>), (<//>),

   -- * List combinators
   hsep, vsep, fillSep, sep, hcat, vcat, fillCat, cat, punctuate,

   -- * Fillers
   fill, fillBreak,

   -- * Bracketing combinators
   enclose, squotes, dquotes, parens, angles, braces, brackets,

   -- * Character documents
   lparen, rparen, langle, rangle, lbrace, rbrace, lbracket, rbracket,
   squote, dquote, semi, colon, comma, space, dot, backslash, equals,

   -- * Primitive type documents
   string, stringStrict, int, integer, float, double, rational, bool,

   -- * Position-based combinators
   column, nesting, width,

   -- * Pretty class
   Pretty(..),

   -- * Rendering
   SimpleDoc(..), renderPretty, renderCompact, renderOneLine,
   displayB, displayT, displayTStrict, displayIO, putDoc, hPutDoc

   ) where

import Prelude        ()
import Prelude.Compat hiding ((<$>))

import Data.String (IsString(..))
import System.IO   (Handle, hPutChar, stdout)

import           Data.Int               (Int64)
import           Data.List              (intersperse)
import qualified Data.Text              as TS
import           Data.Text.Lazy         (Text)
import qualified Data.Text.Lazy         as T
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.IO      as T

#if MIN_VERSION_base (4,9,0)
import Data.Semigroup (Semigroup(..))
#else
import Data.Monoid ((<>))
#endif

infixr 5 </>,<//>,<$>,<$$>
infixr 6 <+>,<++>,`beside`


-----------------------------------------------------------
-- list, tupled and semiBraces pretty print a list of
-- documents either horizontally or vertically aligned.
-----------------------------------------------------------


-- | The document @(list xs)@ comma separates the documents @xs@ and
--   encloses them in square brackets. The documents are rendered
--   horizontally if that fits the page. Otherwise they are aligned
--   vertically. All comma separators are put in front of the
--   elements.
list :: [Doc] -> Doc
list = encloseSep lbracket rbracket comma

-- | The document @(tupled xs)@ comma separates the documents @xs@ and
--   encloses them in parenthesis. The documents are rendered
--   horizontally if that fits the page. Otherwise they are aligned
--   vertically. All comma separators are put in front of the
--   elements.
tupled :: [Doc] -> Doc
tupled = encloseSep lparen rparen comma

-- | The document @(semiBraces xs)@ separates the documents @xs@ with
--   semi colons and encloses them in braces. The documents are
--   rendered horizontally if that fits the page. Otherwise they are
--   aligned vertically. All semi colons are put in front of the
--   elements.
semiBraces :: [Doc] -> Doc
semiBraces = encloseSep lbrace rbrace semi

-- | The document @(encloseSep l r sep xs)@ concatenates the documents
--   @xs@ separated by @sep@ and encloses the resulting document by
--   @l@ and @r@. The documents are rendered horizontally if that fits
--   the page. Otherwise they are aligned vertically. All separators
--   are put in front of the elements. For example, the combinator
--   'list' can be defined with @encloseSep@:
--
--   > list xs = encloseSep lbracket rbracket comma xs
--   > test = text "list" <+> (list (map int [10,200,3000]))
--
--   Which is laid out with a page width of 20 as:
--
--   @
--   list [10,200,3000]
--   @
--
--   But when the page width is 15, it is laid out as:
--
--   @
--   list [10
--        ,200
--        ,3000]
--   @
encloseSep :: Doc -> Doc -> Doc -> [Doc] -> Doc
encloseSep left right sp ds
  = case ds of
      []  -> left <> right
      [d] -> left <> d <> right
      _   -> align (cat (zipWith (<>) (left : repeat sp) ds) <> right)

-----------------------------------------------------------
-- punctuate p [d1,d2,...,dn] => [d1 <> p,d2 <> p, ... ,dn]
-----------------------------------------------------------


-- | @(punctuate p xs)@ concatenates all documents in @xs@ with
--   document @p@ except for the last document.
--
--   > someText = map text ["words","in","a","tuple"]
--   > test = parens (align (cat (punctuate comma someText)))
--
--   This is laid out on a page width of 20 as:
--
--   @
--   (words,in,a,tuple)
--   @
--
--   But when the page width is 15, it is laid out as:
--
--   @
--   (words,
--    in,
--    a,
--    tuple)
--   @
--
--   (If you want put the commas in front of their elements instead of
--   at the end, you should use 'tupled' or, in general, 'encloseSep'.)
punctuate :: Doc -> [Doc] -> [Doc]
punctuate _ []     = []
punctuate _ [d]    = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds


-----------------------------------------------------------
-- high-level combinators
-----------------------------------------------------------


-- | The document @(sep xs)@ concatenates all documents @xs@ either
--   horizontally with @(\<+\>)@, if it fits the page, or vertically
--   with @(\<$\>)@.
--
--   > sep xs = group (vsep xs)
sep :: [Doc] -> Doc
sep = group . vsep

-- | The document @(fillSep xs)@ concatenates documents @xs@
--   horizontally with @(\<+\>)@ as long as its fits the page, then
--   inserts a @line@ and continues doing that for all documents in
--   @xs@.
--
--   > fillSep xs = foldr (</>) empty xs
fillSep :: [Doc] -> Doc
fillSep = fold (</>)

-- | The document @(hsep xs)@ concatenates all documents @xs@
--   horizontally with @(\<+\>)@.
hsep :: [Doc] -> Doc
hsep = fold (<+>)

-- | The document @(vsep xs)@ concatenates all documents @xs@
--   vertically with @(\<$\>)@. If a 'group' undoes the line breaks
--   inserted by @vsep@, all documents are separated with a space.
--
--   > someText = map text (words ("text to lay out"))
--   >
--   > test = text "some" <+> vsep someText
--
--   This is laid out as:
--
--   @
--   some text
--   to
--   lay
--   out
--   @
--
--   The 'align' combinator can be used to align the documents under
--   their first element
--
--   > test = text "some" <+> align (vsep someText)
--
--   Which is printed as:
--
--   @
--   some text
--        to
--        lay
--        out
--   @
vsep :: [Doc] -> Doc
vsep = fold (<$>)

-- | The document @(cat xs)@ concatenates all documents @xs@ either
--   horizontally with @(\<\>)@, if it fits the page, or vertically
--   with @(\<$$\>)@.
--
--   > cat xs = group (vcat xs)
cat :: [Doc] -> Doc
cat = group . vcat

-- | The document @(fillCat xs)@ concatenates documents @xs@
--   horizontally with @(\<\>)@ as long as its fits the page, then
--   inserts a @linebreak@ and continues doing that for all documents
--   in @xs@.
--
--   > fillCat xs = foldr (<//>) empty xs
fillCat :: [Doc] -> Doc
fillCat = fold (<//>)

-- | The document @(hcat xs)@ concatenates all documents @xs@
--   horizontally with @(\<\>)@.
hcat :: [Doc] -> Doc
hcat = fold (<>)

-- | The document @(vcat xs)@ concatenates all documents @xs@
--   vertically with @(\<$$\>)@. If a 'group' undoes the line breaks
--   inserted by @vcat@, all documents are directly concatenated.
vcat :: [Doc] -> Doc
vcat = fold (<$$>)

fold      :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold _ [] = empty
fold f ds = foldr1 f ds

-- | The document @(x \<+\> y)@ concatenates document @x@ and @y@ with
--   a 'space' in between.  (infixr 6)
(<+>) :: Doc -> Doc -> Doc
Empty <+> y     = y
x     <+> Empty = x
x     <+> y     = x <> space <> y

-- | The document @(x \<++\> y)@ concatenates document @x@ and @y@ with
--   a 'spacebreak' in between.  (infixr 6)
(<++>) :: Doc -> Doc -> Doc
Empty <++> y     = y
x     <++> Empty = x
x     <++> y     = x <> spacebreak <> y


-- | The document @(x \<\/\> y)@ concatenates document @x@ and @y@
--   with a 'softline' in between. This effectively puts @x@ and @y@
--   either next to each other (with a @space@ in between) or
--   underneath each other. (infixr 5)
(</>) :: Doc -> Doc -> Doc
(</>) = splitWithBreak False

-- | The document @(x \<\/\/\> y)@ concatenates document @x@ and @y@
--   with a 'softbreak' in between. This effectively puts @x@ and @y@
--   either right next to each other or underneath each other. (infixr
--   5)
(<//>) :: Doc -> Doc -> Doc
(<//>) = splitWithBreak True

splitWithBreak               :: Bool -> Doc -> Doc -> Doc
splitWithBreak _ Empty b     = b
splitWithBreak _ a     Empty = a
splitWithBreak f a     b     = a <> group (Line f) <> b

-- | The document @(x \<$\> y)@ concatenates document @x@ and @y@ with
--   a 'line' in between. (infixr 5)
(<$>) :: Doc -> Doc -> Doc
(<$>) = splitWithLine False

-- | The document @(x \<$$\> y)@ concatenates document @x@ and @y@
--   with a 'linebreak' in between. (infixr 5)
(<$$>) :: Doc -> Doc -> Doc
(<$$>) = splitWithLine True

splitWithLine               :: Bool -> Doc -> Doc -> Doc
splitWithLine _ Empty b     = b
splitWithLine _ a     Empty = a
splitWithLine f a     b     = a <> Line f <> b

-- | The document @softline@ behaves like 'space' if the resulting
--   output fits the page, otherwise it behaves like 'line'.
--
--   > softline = group line
softline :: Doc
softline = group line

-- | The document @softbreak@ behaves like 'empty' if the resulting
--   output fits the page, otherwise it behaves like 'line'.
--
--   > softbreak = group linebreak
softbreak :: Doc
softbreak = group linebreak

-- | The document @spacebreak@ behaves like 'space' when rendered normally
-- but like 'empty' when using 'renderCompact' or 'renderOneLine'.
spacebreak :: Doc
spacebreak = Spaces 1

-- | Document @(squotes x)@ encloses document @x@ with single quotes
--   \"'\".
squotes :: Doc -> Doc
squotes = enclose squote squote

-- | Document @(dquotes x)@ encloses document @x@ with double quotes
--   '\"'.
dquotes :: Doc -> Doc
dquotes = enclose dquote dquote

-- | Document @(braces x)@ encloses document @x@ in braces, \"{\" and
--   \"}\".
braces :: Doc -> Doc
braces = enclose lbrace rbrace

-- | Document @(parens x)@ encloses document @x@ in parenthesis, \"(\"
--   and \")\".
parens :: Doc -> Doc
parens = enclose lparen rparen

-- | Document @(angles x)@ encloses document @x@ in angles, \"\<\" and
--   \"\>\".
angles :: Doc -> Doc
angles = enclose langle rangle

-- | Document @(brackets x)@ encloses document @x@ in square brackets,
--   \"[\" and \"]\".
brackets :: Doc -> Doc
brackets = enclose lbracket rbracket

-- | The document @(enclose l r x)@ encloses document @x@ between
--   documents @l@ and @r@ using @(\<\>)@.
--
--   > enclose l r x = l <> x <> r
enclose       :: Doc -> Doc -> Doc -> Doc
enclose l r x = l <> x <> r

-- | The document @lparen@ contains a left parenthesis, \"(\".
lparen :: Doc
lparen = char '('

-- | The document @rparen@ contains a right parenthesis, \")\".
rparen :: Doc
rparen = char ')'

-- | The document @langle@ contains a left angle, \"\<\".
langle :: Doc
langle = char '<'

-- | The document @rangle@ contains a right angle, \">\".
rangle :: Doc
rangle = char '>'

-- | The document @lbrace@ contains a left brace, \"{\".
lbrace :: Doc
lbrace = char '{'

-- | The document @rbrace@ contains a right brace, \"}\".
rbrace :: Doc
rbrace = char '}'

-- | The document @lbracket@ contains a left square bracket, \"[\".
lbracket :: Doc
lbracket = char '['

-- | The document @rbracket@ contains a right square bracket, \"]\".
rbracket :: Doc
rbracket = char ']'

-- | The document @squote@ contains a single quote, \"'\".
squote :: Doc
squote = char '\''

-- | The document @dquote@ contains a double quote, '\"'.
dquote :: Doc
dquote = char '"'

-- | The document @semi@ contains a semi colon, \";\".
semi :: Doc
semi = char ';'

-- | The document @colon@ contains a colon, \":\".
colon :: Doc
colon = char ':'

-- | The document @comma@ contains a comma, \",\".
comma :: Doc
comma = char ','

-- | The document @space@ contains a single space, \" \".
--
--   > x <+> y = x <> space <> y
space :: Doc
space = char ' '

-- | The document @dot@ contains a single dot, \".\".
dot :: Doc
dot = char '.'

-- | The document @backslash@ contains a back slash, \"\\\".
backslash :: Doc
backslash = char '\\'

-- | The document @equals@ contains an equal sign, \"=\".
equals :: Doc
equals = char '='

-----------------------------------------------------------
-- Combinators for prelude types
-----------------------------------------------------------

-- string is like "text" but replaces '\n' by "line"

-- | The document @(string s)@ concatenates all characters in @s@
--   using @line@ for newline characters and @char@ for all other
--   characters. It is used instead of 'text' whenever the text
--   contains newline characters.
string :: Text -> Doc
string = mconcat . intersperse line . map text . T.lines

stringStrict :: TS.Text -> Doc
stringStrict = mconcat . intersperse line . map textStrict . TS.lines

-- | The document @(bool b)@ shows the literal boolean @b@ using
--   'text'.
bool   :: Bool -> Doc
bool = text'

-- | The document @(int i)@ shows the literal integer @i@ using
--   'text'.
int   :: Int -> Doc
int = text'

-- | The document @(integer i)@ shows the literal integer @i@ using
--   'text'.
integer   :: Integer -> Doc
integer = text'

-- | The document @(float f)@ shows the literal float @f@ using
--   'text'.
float   :: Float -> Doc
float = text'

-- | The document @(double d)@ shows the literal double @d@ using
--   'text'.
double   :: Double -> Doc
double = text'

-- | The document @(rational r)@ shows the literal rational @r@ using
--   'text'.
rational   :: Rational -> Doc
rational = text'

text' :: (Show a) => a -> Doc
text' = text . T.pack . show

-----------------------------------------------------------
-- overloading "pretty"
-----------------------------------------------------------

-- | The member @prettyList@ is only used to define the @instance
--   Pretty a => Pretty [a]@. In normal circumstances only the
--   @pretty@ function is used.
class Pretty a where
  pretty :: a -> Doc

  prettyList :: [a] -> Doc
  prettyList = list . map pretty

instance Pretty a => Pretty [a] where
  pretty = prettyList

instance Pretty Doc where
  pretty = id

instance Pretty Text where
  pretty = string

instance Pretty TS.Text where
  pretty = stringStrict

instance Pretty () where
  pretty () = text' ()

instance Pretty Bool where
  pretty = bool

instance Pretty Char where
  pretty = char

  prettyList = string . T.pack

instance Pretty Int where
  pretty = int

instance Pretty Integer where
  pretty = integer

instance Pretty Float where
  pretty = float

instance Pretty Double where
  pretty = double

--instance Pretty Rational where
--  pretty r = rational r

instance (Pretty a, Pretty b) => Pretty (a,b) where
  pretty (x,y) = tupled [pretty x, pretty y]

instance (Pretty a, Pretty b, Pretty c) => Pretty (a,b,c) where
  pretty (x,y,z)= tupled [pretty x, pretty y, pretty z]

instance Pretty a => Pretty (Maybe a) where
  pretty Nothing  = empty

  pretty (Just x) = pretty x

-----------------------------------------------------------
-- semi primitive: fill and fillBreak
-----------------------------------------------------------

-- | The document @(fillBreak i x)@ first renders document @x@. It
--   then appends @space@s until the width is equal to @i@. If the
--   width of @x@ is already larger than @i@, the nesting level is
--   increased by @i@ and a @line@ is appended. When we redefine
--   @ptype@ in the previous example to use @fillBreak@, we get a
--   useful variation of the previous output:
--
--   > ptype (name,tp)
--   > = fillBreak 6 (text name) <+> text "::" <+> text tp
--
--   The output will now be:
--
--   @
--   let empty  :: Doc
--       nest   :: Int -> Doc -> Doc
--       linebreak
--              :: Doc
--   @
fillBreak     :: Int -> Doc -> Doc
fillBreak f x = width x (\w ->
                          if w > f
                            then nest f linebreak
                            else spaced (f - w)
                        )


-- | The document @(fill i x)@ renders document @x@. It then appends
--   @space@s until the width is equal to @i@. If the width of @x@ is
--   already larger, nothing is appended. This combinator is quite
--   useful in practice to output a list of bindings. The following
--   example demonstrates this.
--
--   > types = [("empty","Doc")
--   >          ,("nest","Int -> Doc -> Doc")
--   >          ,("linebreak","Doc")]
--   >
--   > ptype (name,tp)
--   > = fill 6 (text name) <+> text "::" <+> text tp
--   >
--   > test = text "let" <+> align (vcat (map ptype types))
--
--   Which is laid out as:
--
--   @
--   let empty  :: Doc
--       nest   :: Int -> Doc -> Doc
--       linebreak :: Doc
--   @
fill     :: Int -> Doc -> Doc
fill f d = width d (\w ->
                     if w >= f
                       then empty
                       else spaced (f - w)
                   )


width     :: Doc -> (Int -> Doc) -> Doc
width d f = column (\k1 -> d <> column (\k2 -> f (k2 - k1)))

-----------------------------------------------------------
-- semi primitive: Alignment and indentation
-----------------------------------------------------------

-- | The document @(indent i x)@ indents document @x@ with @i@ spaces.
--
--   > test = indent 4 (fillSep (map text
--   >         (words "the indent combinator indents these words !")))
--
--   Which lays out with a page width of 20 as:
--
--   @
--       the indent
--       combinator
--       indents these
--       words !
--   @
indent         :: Int -> Doc -> Doc
indent _ Empty = Empty
indent i d     = hang i (spaced i <> d)

-- | The hang combinator implements hanging indentation. The document
--   @(hang i x)@ renders document @x@ with a nesting level set to the
--   current column plus @i@. The following example uses hanging
--   indentation for some text:
--
--   > test = hang 4 (fillSep (map text
--   >         (words "the hang combinator indents these words !")))
--
--   Which lays out on a page with a width of 20 characters as:
--
--   @
--   the hang combinator
--       indents these
--       words !
--   @
--
--   The @hang@ combinator is implemented as:
--
--   > hang i x = align (nest i x)
hang     :: Int -> Doc -> Doc
hang i d = align (nest i d)

-- | The document @(align x)@ renders document @x@ with the nesting
--   level set to the current column. It is used for example to
--   implement 'hang'.
--
--   As an example, we will put a document right above another one,
--   regardless of the current nesting level:
--
--   > x $$ y = align (x <$> y)
--
--   > test = text "hi" <+> (text "nice" $$ text "world")
--
--   which will be laid out as:
--
--   @
--   hi nice
--      world
--   @
align   :: Doc -> Doc
align d = column (\k ->
                   nesting (\i -> nest (k - i) d))   --nesting might be negative :-)

-----------------------------------------------------------
-- Primitives
-----------------------------------------------------------

-- | The abstract data type @Doc@ represents pretty documents.
--
--   @Doc@ is an instance of the 'Show' class. @(show doc)@ pretty
--   prints document @doc@ with a page width of 100 characters and a
--   ribbon width of 40 characters.
--
--   > show (text "hello" <$> text "world")
--
--   Which would return the string \"hello\\nworld\", i.e.
--
--   @
--   hello
--   world
--   @
data Doc = Empty
         | Char Char             -- invariant: char is not '\n'
         | Text !Int64 Builder   -- invariant: text doesn't contain '\n'
         | Line !Bool            -- True <=> when undone by group, do not insert a space
         | Cat Doc Doc
         | Nest !Int64 Doc
         | Union Doc Doc         -- invariant: first lines of first doc longer than the first lines of the second doc
         | Column  (Int64 -> Doc)
         | Nesting (Int64 -> Doc)
         | Spaces !Int64

instance IsString Doc where
  fromString = string . T.pack

-- | In particular, note that the document @(x '<>' y)@ concatenates
--   document @x@ and document @y@. It is an associative operation
--   having 'empty' as a left and right unit.  (infixr 6)
#if MIN_VERSION_base (4,9,0)
instance Semigroup Doc where
    (<>) = beside

#endif
instance Monoid Doc where
    mempty  = empty
    mappend = beside

-- | The data type @SimpleDoc@ represents rendered documents and is
--   used by the display functions.
--
--   The @Int@ in @SText@ contains the length of the string. The @Int@
--   in @SLine@ contains the indentation for that line. The library
--   provides two default display functions 'displayS' and
--   'displayIO'. You can provide your own display function by writing
--   a function from a @SimpleDoc@ to your own output format.
data SimpleDoc = SEmpty
                | SChar Char SimpleDoc
                | SText !Int64 Builder SimpleDoc
                | SLine !Int64 SimpleDoc

-- | The empty document is, indeed, empty. Although @empty@ has no
--   content, it does have a \'height\' of 1 and behaves exactly like
--   @(text \"\")@ (and is therefore not a unit of @\<$\>@).
empty :: Doc
empty = Empty

-- | Determine if the document is empty or not.
isEmpty :: Doc -> Bool
isEmpty Empty = True
isEmpty _     = False

-- | The document @(char c)@ contains the literal character @c@. The
--   character shouldn't be a newline (@'\n'@), the function 'line'
--   should be used for line breaks.
char      :: Char -> Doc
char '\n' = line
char  c   = Char c

-- | The document @(text s)@ contains the literal string @s@. The
--   string shouldn't contain any newline (@'\n'@) characters. If the
--   string contains newline characters, the function 'string' should
--   be used.
text :: Text -> Doc
text s
  | T.null s  = Empty
  | otherwise = Text (T.length s) (B.fromLazyText s)

textStrict :: TS.Text -> Doc
textStrict s
  | TS.null s  = Empty
  | otherwise  = Text (fromIntegral $ TS.length s) (B.fromText s)

-- | The @line@ document advances to the next line and indents to the
--   current nesting level. Document @line@ behaves like @(text \"
--   \")@ if the line break is undone by 'group' or if rendered with
--   'renderOneLine'.
line :: Doc
line = Line False

-- | The @linebreak@ document advances to the next line and indents to
--   the current nesting level. Document @linebreak@ behaves like
--   'empty' if the line break is undone by 'group'.
linebreak :: Doc
linebreak = Line True

-- | The document @(x `beside` y)@ concatenates document @x@ and @y@.
--   It is an associative operation having 'empty' as a left and right
--   unit.  (infixr 6)
--
--   It is equivalent to `<>`.
beside             :: Doc -> Doc -> Doc
beside Empty r     = r
beside l     Empty = l
beside l     r     = Cat l r

-- | The document @(nest i x)@ renders document @x@ with the current
--   indentation level increased by @i@ (See also 'hang', 'align' and
--   'indent').
--
--   > nest 2 (text "hello" <$> text "world") <$> text "!"
--
--   outputs as:
--
--   @
--   hello
--     world
--   !
--   @
nest         :: Int -> Doc -> Doc
nest _ Empty = Empty
nest i x     = Nest (fromIntegral i) x

-- | Specifies how to create the document based upon which column it is in.
column   :: (Int -> Doc) -> Doc
column f = Column (f . fromIntegral)

-- | Specifies how to nest the document based upon which column it is
--   being nested in.
nesting   :: (Int -> Doc) -> Doc
nesting f = Nesting (f . fromIntegral)

-- | The @group@ combinator is used to specify alternative
--   layouts. The document @(group x)@ undoes all line breaks in
--   document @x@. The resulting line is added to the current line if
--   that fits the page. Otherwise, the document @x@ is rendered
--   without any changes.
group   :: Doc -> Doc
group x = Union (flatten x) x

flatten              :: Doc -> Doc
flatten (Cat x y)   = Cat (flatten x) (flatten y)
flatten (Nest i x)  = Nest i (flatten x)
flatten (Line brk)  = if brk then Empty else Text 1 (B.singleton ' ')
flatten (Union x _) = flatten x
flatten (Column f)  = Column (flatten . f)
flatten (Nesting f) = Nesting (flatten . f)
flatten other       = other                     --Empty,Char,Text

-----------------------------------------------------------
-- Renderers
-----------------------------------------------------------

-----------------------------------------------------------
-- renderPretty: the default pretty printing algorithm
-----------------------------------------------------------

-- list of indentation/document pairs; saves an indirection over [(Int,Doc)]
data Docs = Nil
          | Cons !Int64 Doc Docs

-- | This is the default pretty printer which is used by 'show',
--   'putDoc' and 'hPutDoc'. @(renderPretty ribbonfrac width x)@
--   renders document @x@ with a page width of @width@ and a ribbon
--   width of @(ribbonfrac * width)@ characters. The ribbon width is
--   the maximal amount of non-indentation characters on a line. The
--   parameter @ribbonfrac@ should be between @0.0@ and @1.0@. If it
--   is lower or higher, the ribbon width will be 0 or @width@
--   respectively.
renderPretty :: Float -> Int -> Doc -> SimpleDoc
renderPretty rfrac w doc
 = best 0 0 (Cons 0 doc Nil)
    where
      -- r :: the ribbon width in characters
      r = max 0 (min w64 (round (fromIntegral w * rfrac)))

      w64 = fromIntegral w

      -- best :: n = indentation of current line
      --         k = current column
      --        (ie. (k >= n) && (k - n == count of inserted characters)
      best _ _ Nil = SEmpty
      best n k (Cons i d ds)
        = case d of
            Empty     -> best n k ds
            Char c    -> let k' = k+1 in seq k' $ SChar c (best n k' ds)
            Text l s  -> let k' = k+l in seq k' $ SText l s (best n k' ds)
            Line _    -> SLine i (best i i ds)
            Cat x y   -> best n k (Cons i x (Cons i y ds))
            Nest j x  -> let i' = i+j in seq i' (best n k (Cons i' x ds))
            Union x y -> nicest n k (best n k $ Cons i x ds)
                                    (best n k $ Cons i y ds)
            Column f  -> best n k (Cons i (f k) ds)
            Nesting f -> best n k (Cons i (f i) ds)
            Spaces l  -> let k' = k+l in seq k' $ SText l (spaces l) (best n k' ds)

      --nicest :: r = ribbon width, w = page width,
      --          n = indentation of current line, k = current column
      --          x and y, the (simple) documents to chose from.
      --          precondition: first lines of x are longer than the first lines of y.
      nicest n k x y
        | fits wth x = x
        | otherwise    = y
          where
            wth = min (w64 - k) (r - k + n)

fits                 :: Int64 -> SimpleDoc -> Bool
fits w _             | w < 0     = False
fits _ SEmpty        = True
fits w (SChar _ x)   = fits (w - 1) x
fits w (SText l _ x) = fits (w - l) x
fits _ SLine{}       = True

-----------------------------------------------------------
-- renderCompact: renders documents without indentation
--  fast and fewer characters output, good for machines
-----------------------------------------------------------

-- | @(renderCompact x)@ renders document @x@ without adding any
--   indentation. Since no \'pretty\' printing is involved, this
--   renderer is very fast. The resulting output contains fewer
--   characters than a pretty printed version and can be used for
--   output that is read by other programs.
renderCompact :: Doc -> SimpleDoc
renderCompact dc
  = scan 0 [dc]
    where
      scan _ [] = SEmpty
      scan k (d:ds)
        = case d of
            Empty     -> scan k ds
            Char c    -> let k' = k+1 in seq k' (SChar c (scan k' ds))
            Text l s  -> let k' = k+l in seq k' (SText l s (scan k' ds))
            Line _    -> SLine 0 (scan 0 ds)
            Cat x y   -> scan k (x:y:ds)
            Nest _ x  -> scan k (x:ds)
            Union _ y -> scan k (y:ds)
            Column f  -> scan k (f k:ds)
            Nesting f -> scan k (f 0:ds)
            Spaces _  -> scan k ds

-- | @(renderOneLine x)@ renders document @x@ without adding any
--   indentation or newlines.
renderOneLine :: Doc -> SimpleDoc
renderOneLine dc
  = scan 0 [dc]
    where
      scan _ [] = SEmpty
      scan k (d:ds)
        = case d of
            Empty      -> scan k ds
            Char c     -> let k' = k+1 in seq k' (SChar c (scan k' ds))
            Text l s   -> let k' = k+l in seq k' (SText l s (scan k' ds))
            Line False -> let k' = k+1 in seq k' (SChar ' ' (scan k' ds))
            Line _     -> scan k ds
            Cat x y    -> scan k (x:y:ds)
            Nest _ x   -> scan k (x:ds)
            Union _ y  -> scan k (y:ds)
            Column f   -> scan k (f k:ds)
            Nesting f  -> scan k (f 0:ds)
            Spaces _   -> scan k ds

-----------------------------------------------------------
-- Displayers:  displayS and displayIO
-----------------------------------------------------------


-- | @(displayB simpleDoc)@ takes the output @simpleDoc@ from a
--   rendering function and transforms it to a 'Builder' type (for
--   further manipulation before converting to a lazy 'Text').
displayB               :: SimpleDoc -> Builder
displayB SEmpty        = mempty
displayB (SChar c x)   = c `consB` displayB x
displayB (SText _ s x) = s `mappend` displayB x
displayB (SLine i x)   = '\n' `consB` (indentation i `mappend` displayB x)

consB       :: Char -> Builder -> Builder
c `consB` b = B.singleton c `mappend` b

-- | @(displayT simpleDoc)@ takes the output @simpleDoc@ from a
--   rendering function and transforms it to a lazy 'Text' value.
--
--   > showWidth :: Int -> Doc -> Text
--   > showWidth w x = displayT (renderPretty 0.4 w x)
displayT :: SimpleDoc -> Text
displayT = B.toLazyText . displayB

displayTStrict :: SimpleDoc -> TS.Text
displayTStrict = T.toStrict . displayT

-- | @(displayIO handle simpleDoc)@ writes @simpleDoc@ to the
--   file handle @handle@. This function is used for example by
--   'hPutDoc':
--
--   > hPutDoc handle doc = displayIO handle (renderPretty 0.4 100 doc)
displayIO :: Handle -> SimpleDoc -> IO ()
displayIO handle = display
    where
      display SEmpty        = return ()
      display (SChar c x)   = hPutChar handle c >> display x
      display (SText _ s x) = T.hPutStr handle (B.toLazyText s) >> display x
      display (SLine i x)   = T.hPutStr handle newLine >> display x
        where
          newLine = B.toLazyText $ '\n' `consB` indentation i

-----------------------------------------------------------
-- default pretty printers: show, putDoc and hPutDoc
-----------------------------------------------------------

instance Show Doc where
  showsPrec d doc = showsPrec d (displayT $ renderPretty 0.4 80 doc)
  show doc = T.unpack (displayT $ renderPretty 0.4 80 doc)

instance Show SimpleDoc where
  show simpleDoc = T.unpack (displayT simpleDoc)

-- | The action @(putDoc doc)@ pretty prints document @doc@ to the
-- standard output, with a page width of 100 characters and a ribbon
-- width of 40 characters.
--
-- > main :: IO ()
-- > main = do{ putDoc (text "hello" <+> text "world") }
--
-- Which would output
--
-- @
-- hello world
-- @
putDoc :: Doc -> IO ()
putDoc = hPutDoc stdout

-- | @(hPutDoc handle doc)@ pretty prints document @doc@ to the file
--   handle @handle@ with a page width of 100 characters and a ribbon
--   width of 40 characters.
--
--   > main = do handle <- 'openFile' "MyFile" 'WriteMode'
--   >           'hPutDoc' handle ('vcat' ('map' 'text'
--   >                           ['T.pack' "vertical", 'T.pack' "text"]))
--   >           'hClose' handle
hPutDoc :: Handle -> Doc -> IO ()
hPutDoc handle doc = displayIO handle (renderPretty 0.4 80 doc)

-----------------------------------------------------------
-- insert spaces
-- "indentation" used to insert tabs but tabs seem to cause
-- more trouble than they solve :-)
-----------------------------------------------------------
spaces :: Int64 -> Builder
spaces n
  | n <= 0 = mempty
  | otherwise = B.fromLazyText $ T.replicate n (T.singleton ' ')

spaced   :: Int -> Doc
spaced l = Spaces l'
  where
    l' = fromIntegral l

-- An alias for readability purposes
indentation :: Int64 -> Builder
indentation = spaces

--  LocalWords:  PPrint combinators Wadler Wadler's encloseSep
