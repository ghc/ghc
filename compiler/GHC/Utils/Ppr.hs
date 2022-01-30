{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Utils.Ppr
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  David Terei <code@davidterei.com>
-- Stability   :  stable
-- Portability :  portable
--
-- John Hughes's and Simon Peyton Jones's Pretty Printer Combinators
--
-- Based on /The Design of a Pretty-printing Library/
-- in Advanced Functional Programming,
-- Johan Jeuring and Erik Meijer (eds), LNCS 925
-- <http://www.cse.chalmers.se/~rjmh/Papers/pretty.ps>
--
-----------------------------------------------------------------------------

{-
Note [Differences between libraries/pretty and compiler/GHC/Utils/Ppr.hs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For historical reasons, there are two different copies of `Pretty` in the GHC
source tree:
 * `libraries/pretty` is a submodule containing
   https://github.com/haskell/pretty. This is the `pretty` library as released
   on hackage. It is used by several other libraries in the GHC source tree
   (e.g. template-haskell and Cabal).
 * `compiler/GHC/Utils/Ppr.hs` (this module). It is used by GHC only.

There is an ongoing effort in https://github.com/haskell/pretty/issues/1 and
https://gitlab.haskell.org/ghc/ghc/issues/10735 to try to get rid of GHC's copy
of Pretty.

Currently, GHC's copy of Pretty resembles pretty-1.1.2.0, with the following
major differences:
 * GHC's copy uses `Faststring` for performance reasons.
 * GHC's copy has received a backported bugfix for #12227, which was
   released as pretty-1.1.3.4 ("Remove harmful $! forcing in beside",
   https://github.com/haskell/pretty/pull/35).

Other differences are minor. Both copies define some extra functions and
instances not defined in the other copy. To see all differences, do this in a
ghc git tree:

    $ cd libraries/pretty
    $ git checkout v1.1.2.0
    $ cd -
    $ vimdiff compiler/GHC/Utils/Ppr.hs \
              libraries/pretty/src/Text/PrettyPrint/HughesPJ.hs

For parity with `pretty-1.1.2.1`, the following two `pretty` commits would
have to be backported:
  * "Resolve foldr-strictness stack overflow bug"
    (307b8173f41cd776eae8f547267df6d72bff2d68)
  * "Special-case reduce for horiz/vert"
    (c57c7a9dfc49617ba8d6e4fcdb019a3f29f1044c)
This has not been done sofar, because these commits seem to cause more
allocation in the compiler (see thomie's comments in
https://github.com/haskell/pretty/pull/9).
-}

module GHC.Utils.Ppr (

        -- * The document type
        Doc, TextDetails(..),

        -- * Constructing documents

        -- ** Converting values into documents
        char, text, ftext, ptext, ztext, sizedText, zeroWidthText, emptyText,
        int, integer, float, double, rational, hex,

        -- ** Simple derived documents
        semi, comma, colon, space, equals,
        lparen, rparen, lbrack, rbrack, lbrace, rbrace,

        -- ** Wrapping documents in delimiters
        parens, brackets, braces, quotes, quote, doubleQuotes,
        maybeParens,

        -- ** Combining documents
        empty,
        (<>), (<+>), hcat, hsep,
        ($$), ($+$), vcat,
        sep, cat,
        fsep, fcat,
        nest,
        hang, hangNotEmpty, punctuate,

        -- * Predicates on documents
        isEmpty,

        -- * Rendering documents

        -- ** Rendering with a particular style
        Style(..),
        style,
        renderStyle,
        Mode(..),

        -- ** General rendering
        fullRender, txtPrinter,

        -- ** GHC-specific rendering
        printDoc, printDoc_,
        bufLeftRender -- performance hack

  ) where

import GHC.Prelude hiding (error)

import GHC.Utils.BufHandle
import GHC.Data.FastString
import GHC.Utils.Panic.Plain
import System.IO
import Numeric (showHex)

--for a RULES
import GHC.Base ( unpackCString#, unpackNBytes#, Int(..) )
import GHC.Ptr  ( Ptr(..) )

-- ---------------------------------------------------------------------------
-- The Doc calculus

{-
Laws for $$
~~~~~~~~~~~
<a1>    (x $$ y) $$ z   = x $$ (y $$ z)
<a2>    empty $$ x      = x
<a3>    x $$ empty      = x

        ...ditto $+$...

Laws for <>
~~~~~~~~~~~
<b1>    (x <> y) <> z   = x <> (y <> z)
<b2>    empty <> x      = empty
<b3>    x <> empty      = x

        ...ditto <+>...

Laws for text
~~~~~~~~~~~~~
<t1>    text s <> text t        = text (s++t)
<t2>    text "" <> x            = x, if x non-empty

** because of law n6, t2 only holds if x doesn't
** start with `nest'.


Laws for nest
~~~~~~~~~~~~~
<n1>    nest 0 x                = x
<n2>    nest k (nest k' x)      = nest (k+k') x
<n3>    nest k (x <> y)         = nest k x <> nest k y
<n4>    nest k (x $$ y)         = nest k x $$ nest k y
<n5>    nest k empty            = empty
<n6>    x <> nest k y           = x <> y, if x non-empty

** Note the side condition on <n6>!  It is this that
** makes it OK for empty to be a left unit for <>.

Miscellaneous
~~~~~~~~~~~~~
<m1>    (text s <> x) $$ y = text s <> ((text "" <> x) $$
                                         nest (-length s) y)

<m2>    (x $$ y) <> z = x $$ (y <> z)
        if y non-empty


Laws for list versions
~~~~~~~~~~~~~~~~~~~~~~
<l1>    sep (ps++[empty]++qs)   = sep (ps ++ qs)
        ...ditto hsep, hcat, vcat, fill...

<l2>    nest k (sep ps) = sep (map (nest k) ps)
        ...ditto hsep, hcat, vcat, fill...

Laws for oneLiner
~~~~~~~~~~~~~~~~~
<o1>    oneLiner (nest k p) = nest k (oneLiner p)
<o2>    oneLiner (x <> y)   = oneLiner x <> oneLiner y

You might think that the following version of <m1> would
be neater:

<3 NO>  (text s <> x) $$ y = text s <> ((empty <> x)) $$
                                         nest (-length s) y)

But it doesn't work, for if x=empty, we would have

        text s $$ y = text s <> (empty $$ nest (-length s) y)
                    = text s <> nest (-length s) y
-}

-- ---------------------------------------------------------------------------
-- Operator fixity

infixl 6 <>
infixl 6 <+>
infixl 5 $$, $+$


-- ---------------------------------------------------------------------------
-- The Doc data type

-- | The abstract type of documents.
-- A Doc represents a *set* of layouts. A Doc with
-- no occurrences of Union or NoDoc represents just one layout.
data Doc
  = Empty                                            -- empty
  | NilAbove Doc                                     -- text "" $$ x
  | TextBeside !TextDetails {-# UNPACK #-} !Int Doc  -- text s <> x
  | Nest {-# UNPACK #-} !Int Doc                     -- nest k x
  | Union Doc Doc                                    -- ul `union` ur
  | NoDoc                                            -- The empty set of documents
  | Beside Doc Bool Doc                              -- True <=> space between
  | Above Doc Bool Doc                               -- True <=> never overlap

{-
Here are the invariants:

1) The argument of NilAbove is never Empty. Therefore
   a NilAbove occupies at least two lines.

2) The argument of @TextBeside@ is never @Nest@.

3) The layouts of the two arguments of @Union@ both flatten to the same
   string.

4) The arguments of @Union@ are either @TextBeside@, or @NilAbove@.

5) A @NoDoc@ may only appear on the first line of the left argument of an
   union. Therefore, the right argument of an union can never be equivalent
   to the empty set (@NoDoc@).

6) An empty document is always represented by @Empty@.  It can't be
   hidden inside a @Nest@, or a @Union@ of two @Empty@s.

7) The first line of every layout in the left argument of @Union@ is
   longer than the first line of any layout in the right argument.
   (1) ensures that the left argument has a first line.  In view of
   (3), this invariant means that the right argument must have at
   least two lines.

Notice the difference between
   * NoDoc (no documents)
   * Empty (one empty document; no height and no width)
   * text "" (a document containing the empty string;
              one line high, but has no width)
-}


-- | RDoc is a "reduced GDoc", guaranteed not to have a top-level Above or Beside.
type RDoc = Doc

-- | The TextDetails data type
--
-- A TextDetails represents a fragment of text that will be
-- output at some point.
data TextDetails = Chr  {-# UNPACK #-} !Char -- ^ A single Char fragment
                 | Str  String -- ^ A whole String fragment
                 | PStr FastString                      -- a hashed string
                 | ZStr FastZString                     -- a z-encoded string
                 | LStr {-# UNPACK #-} !PtrString
                   -- a '\0'-terminated array of bytes
                 | RStr {-# UNPACK #-} !Int {-# UNPACK #-} !Char
                   -- a repeated character (e.g., ' ')

instance Show Doc where
  showsPrec _ doc cont = fullRender (mode style) (lineLength style)
                                    (ribbonsPerLine style)
                                    txtPrinter cont doc


-- ---------------------------------------------------------------------------
-- Values and Predicates on GDocs and TextDetails

-- | A document of height and width 1, containing a literal character.
char :: Char -> Doc
char c = textBeside_ (Chr c) 1 Empty

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
text s = textBeside_ (Str s) (length s) Empty
{-# NOINLINE [0] text #-}   -- Give the RULE a chance to fire
                            -- It must wait till after phase 1 when
                            -- the unpackCString first is manifested

-- RULE that turns (text "abc") into (ptext (A# "abc"#)) to avoid the
-- intermediate packing/unpacking of the string.
{-# RULES "text/str"
    forall a. text (unpackCString# a)  = ptext (mkPtrString# a)
  #-}
{-# RULES "text/unpackNBytes#"
    forall p n. text (unpackNBytes# p n) = ptext (PtrString (Ptr p) (I# n))
  #-}

-- Empty strings are desugared into [] (not "unpackCString#..."), hence they are
-- not matched by the text/str rule above.
{-# RULES "text/[]"
    text [] = emptyText
  #-}

ftext :: FastString -> Doc
ftext s = textBeside_ (PStr s) (lengthFS s) Empty

ptext :: PtrString -> Doc
ptext s = textBeside_ (LStr s) (lengthPS s) Empty

ztext :: FastZString -> Doc
ztext s = textBeside_ (ZStr s) (lengthFZS s) Empty

-- | Some text with any width. (@text s = sizedText (length s) s@)
sizedText :: Int -> String -> Doc
sizedText l s = textBeside_ (Str s) l Empty

-- | Some text, but without any width. Use for non-printing text
-- such as a HTML or Latex tags
zeroWidthText :: String -> Doc
zeroWidthText = sizedText 0

-- | Empty text (one line high but no width). (@emptyText = text ""@)
emptyText :: Doc
emptyText = sizedText 0 []
  -- defined as a CAF. Sharing occurs especially via the text/[] rule above.
  -- Every use of `text ""` in user code should be replaced with this.

-- | The empty document, with no height and no width.
-- 'empty' is the identity for '<>', '<+>', '$$' and '$+$', and anywhere
-- in the argument list for 'sep', 'hcat', 'hsep', 'vcat', 'fcat' etc.
empty :: Doc
empty = Empty

-- | Returns 'True' if the document is empty
isEmpty :: Doc -> Bool
isEmpty Empty = True
isEmpty _     = False

{-
Q: What is the reason for negative indentation (i.e. argument to indent
   is < 0) ?

A:
This indicates an error in the library client's code.
If we compose a <> b, and the first line of b is more indented than some
other lines of b, the law <n6> (<> eats nests) may cause the pretty
printer to produce an invalid layout:

doc       |0123345
------------------
d1        |a...|
d2        |...b|
          |c...|

d1<>d2    |ab..|
         c|....|

Consider a <> b, let `s' be the length of the last line of `a', `k' the
indentation of the first line of b, and `k0' the indentation of the
left-most line b_i of b.

The produced layout will have negative indentation if `k - k0 > s', as
the first line of b will be put on the (s+1)th column, effectively
translating b horizontally by (k-s). Now if the i^th line of b has an
indentation k0 < (k-s), it is translated out-of-page, causing
`negative indentation'.
-}


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

spaceText, nlText :: TextDetails
spaceText = Chr ' '
nlText    = Chr '\n'

int      :: Int      -> Doc -- ^ @int n = text (show n)@
integer  :: Integer  -> Doc -- ^ @integer n = text (show n)@
float    :: Float    -> Doc -- ^ @float n = text (show n)@
double   :: Double   -> Doc -- ^ @double n = text (show n)@
rational :: Rational -> Doc -- ^ @rational n = text (show n)@
hex      :: Integer  -> Doc -- ^ See Note [Print Hexadecimal Literals]
int      n = text (show n)
integer  n = text (show n)
float    n = text (show n)
double   n = text (show n)
rational n = text (show n)
hex      n = text ('0' : 'x' : padded)
    where
    str = showHex n ""
    strLen = max 1 (length str)
    len = 2 ^ (ceiling (logBase 2 (fromIntegral strLen :: Double)) :: Int)
    padded = replicate (len - strLen) '0' ++ str

parens       :: Doc -> Doc -- ^ Wrap document in @(...)@
brackets     :: Doc -> Doc -- ^ Wrap document in @[...]@
braces       :: Doc -> Doc -- ^ Wrap document in @{...}@
quotes       :: Doc -> Doc -- ^ Wrap document in @\'...\'@
quote        :: Doc -> Doc
doubleQuotes :: Doc -> Doc -- ^ Wrap document in @\"...\"@
quotes p       = char '`' <> p <> char '\''
quote p        = char '\'' <> p
doubleQuotes p = char '"' <> p <> char '"'
parens p       = char '(' <> p <> char ')'
brackets p     = char '[' <> p <> char ']'
braces p       = char '{' <> p <> char '}'

{-
Note [Print Hexadecimal Literals]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Relevant discussions:
 * Phabricator: https://phabricator.haskell.org/D4465
 * GHC Trac: https://gitlab.haskell.org/ghc/ghc/issues/14872

There is a flag `-dhex-word-literals` that causes literals of
type `Word#` or `Word64#` to be displayed in hexadecimal instead
of decimal when dumping GHC core. It also affects the presentation
of these in GHC's error messages. Additionally, the hexadecimal
encoding of these numbers is zero-padded so that its length is
a power of two. As an example of what this does,
consider the following haskell file `Literals.hs`:

    module Literals where

    alpha :: Int
    alpha = 100 + 200

    beta :: Word -> Word
    beta x = x + div maxBound 255 + div 0xFFFFFFFF 255 + 0x0202

We get the following dumped core when we compile on a 64-bit
machine with ghc -O2 -fforce-recomp -ddump-simpl -dsuppress-all
-dhex-word-literals literals.hs:

    ==================== Tidy Core ====================

    ... omitted for brevity ...

    -- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
    alpha
    alpha = I# 300#

    -- RHS size: {terms: 12, types: 3, coercions: 0, joins: 0/0}
    beta
    beta
      = \ x_aYE ->
          case x_aYE of { W# x#_a1v0 ->
          W#
            (plusWord#
               (plusWord# (plusWord# x#_a1v0 0x0101010101010101##) 0x01010101##)
               0x0202##)
          }

Notice that the word literals are in hexadecimals and that they have
been padded with zeroes so that their lengths are 16, 8, and 4, respectively.

-}

-- | Apply 'parens' to 'Doc' if boolean is true.
maybeParens :: Bool -> Doc -> Doc
maybeParens False = id
maybeParens True = parens

-- ---------------------------------------------------------------------------
-- Structural operations on GDocs

-- | Perform some simplification of a built up @GDoc@.
reduceDoc :: Doc -> RDoc
reduceDoc (Beside p g q) = p `seq` g `seq` (beside p g $! reduceDoc q)
reduceDoc (Above  p g q) = p `seq` g `seq` (above  p g $! reduceDoc q)
reduceDoc p              = p

-- | List version of '<>'.
hcat :: [Doc] -> Doc
hcat = reduceAB . foldr (beside_' False) empty

-- | List version of '<+>'.
hsep :: [Doc] -> Doc
hsep = reduceAB . foldr (beside_' True)  empty

-- | List version of '$$'.
vcat :: [Doc] -> Doc
vcat = reduceAB . foldr (above_' False) empty

-- | Nest (or indent) a document by a given number of positions
-- (which may also be negative).  'nest' satisfies the laws:
--
-- * @'nest' 0 x = x@
--
-- * @'nest' k ('nest' k' x) = 'nest' (k+k') x@
--
-- * @'nest' k (x '<>' y) = 'nest' k z '<>' 'nest' k y@
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
nest k p = mkNest k (reduceDoc p)

-- | @hang d1 n d2 = sep [d1, nest n d2]@
hang :: Doc -> Int -> Doc -> Doc
hang d1 n d2 = sep [d1, nest n d2]

-- | Apply 'hang' to the arguments if the first 'Doc' is not empty.
hangNotEmpty :: Doc -> Int -> Doc -> Doc
hangNotEmpty d1 n d2 = if isEmpty d1
                       then d2
                       else hang d1 n d2

-- | @punctuate p [d1, ... dn] = [d1 \<> p, d2 \<> p, ... dn-1 \<> p, dn]@
punctuate :: Doc -> [Doc] -> [Doc]
punctuate _ []     = []
punctuate p (x:xs) = go x xs
                   where go y []     = [y]
                         go y (z:zs) = (y <> p) : go z zs

-- mkNest checks for Nest's invariant that it doesn't have an Empty inside it
mkNest :: Int -> Doc -> Doc
mkNest k _ | k `seq` False = undefined
mkNest k (Nest k1 p)       = mkNest (k + k1) p
mkNest _ NoDoc             = NoDoc
mkNest _ Empty             = Empty
mkNest 0 p                 = p
mkNest k p                 = nest_ k p

-- mkUnion checks for an empty document
mkUnion :: Doc -> Doc -> Doc
mkUnion Empty _ = Empty
mkUnion p q     = p `union_` q

beside_' :: Bool -> Doc -> Doc -> Doc
beside_' _ p Empty = p
beside_' g p q     = Beside p g q

above_' :: Bool -> Doc -> Doc -> Doc
above_' _ p Empty = p
above_' g p q     = Above p g q

reduceAB :: Doc -> Doc
reduceAB (Above  Empty _ q) = q
reduceAB (Beside Empty _ q) = q
reduceAB doc                = doc

nilAbove_ :: RDoc -> RDoc
nilAbove_ = NilAbove

-- Arg of a TextBeside is always an RDoc
textBeside_ :: TextDetails -> Int -> RDoc -> RDoc
textBeside_ = TextBeside

nest_ :: Int -> RDoc -> RDoc
nest_ = Nest

union_ :: RDoc -> RDoc -> RDoc
union_ = Union


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
p $$  q = above_ p False q

-- | Above, with no overlapping.
-- '$+$' is associative, with identity 'empty'.
($+$) :: Doc -> Doc -> Doc
p $+$ q = above_ p True q

above_ :: Doc -> Bool -> Doc -> Doc
above_ p _ Empty = p
above_ Empty _ q = q
above_ p g q     = Above p g q

above :: Doc -> Bool -> RDoc -> RDoc
above (Above p g1 q1)  g2 q2 = above p g1 (above q1 g2 q2)
above p@(Beside{})     g  q  = aboveNest (reduceDoc p) g 0 (reduceDoc q)
above p g q                  = aboveNest p             g 0 (reduceDoc q)

-- Specification: aboveNest p g k q = p $g$ (nest k q)
aboveNest :: RDoc -> Bool -> Int -> RDoc -> RDoc
aboveNest _                   _ k _ | k `seq` False = undefined
aboveNest NoDoc               _ _ _ = NoDoc
aboveNest (p1 `Union` p2)     g k q = aboveNest p1 g k q `union_`
                                      aboveNest p2 g k q

aboveNest Empty               _ k q = mkNest k q
aboveNest (Nest k1 p)         g k q = nest_ k1 (aboveNest p g (k - k1) q)
                                  -- p can't be Empty, so no need for mkNest

aboveNest (NilAbove p)        g k q = nilAbove_ (aboveNest p g k q)
aboveNest (TextBeside s sl p) g k q = textBeside_ s sl rest
                                    where
                                      !k1  = k - sl
                                      rest = case p of
                                                Empty -> nilAboveNest g k1 q
                                                _     -> aboveNest  p g k1 q
aboveNest (Above {})          _ _ _ = error "aboveNest Above"
aboveNest (Beside {})         _ _ _ = error "aboveNest Beside"

-- Specification: text s <> nilaboveNest g k q
--              = text s <> (text "" $g$ nest k q)
nilAboveNest :: Bool -> Int -> RDoc -> RDoc
nilAboveNest _ k _           | k `seq` False = undefined
nilAboveNest _ _ Empty       = Empty
                               -- Here's why the "text s <>" is in the spec!
nilAboveNest g k (Nest k1 q) = nilAboveNest g (k + k1) q
nilAboveNest g k q           | not g && k > 0      -- No newline if no overlap
                             = textBeside_ (RStr k ' ') k q
                             | otherwise           -- Put them really above
                             = nilAbove_ (mkNest k q)


-- ---------------------------------------------------------------------------
-- Horizontal composition @<>@

-- We intentionally avoid Data.Monoid.(<>) here due to interactions of
-- Data.Monoid.(<>) and (<+>).  See
-- http://www.haskell.org/pipermail/libraries/2011-November/017066.html

-- | Beside.
-- '<>' is associative, with identity 'empty'.
(<>) :: Doc -> Doc -> Doc
p <>  q = beside_ p False q

-- | Beside, separated by space, unless one of the arguments is 'empty'.
-- '<+>' is associative, with identity 'empty'.
(<+>) :: Doc -> Doc -> Doc
p <+> q = beside_ p True  q

beside_ :: Doc -> Bool -> Doc -> Doc
beside_ p _ Empty = p
beside_ Empty _ q = q
beside_ p g q     = Beside p g q

-- Specification: beside g p q = p <g> q
beside :: Doc -> Bool -> RDoc -> RDoc
beside NoDoc               _ _   = NoDoc
beside (p1 `Union` p2)     g q   = beside p1 g q `union_` beside p2 g q
beside Empty               _ q   = q
beside (Nest k p)          g q   = nest_ k $! beside p g q
beside p@(Beside p1 g1 q1) g2 q2
         | g1 == g2              = beside p1 g1 $! beside q1 g2 q2
         | otherwise             = beside (reduceDoc p) g2 q2
beside p@(Above{})         g q   = let !d = reduceDoc p in beside d g q
beside (NilAbove p)        g q   = nilAbove_ $! beside p g q
beside (TextBeside s sl p) g q   = textBeside_ s sl rest
                               where
                                  rest = case p of
                                           Empty -> nilBeside g q
                                           _     -> beside p g q

-- Specification: text "" <> nilBeside g p
--              = text "" <g> p
nilBeside :: Bool -> RDoc -> RDoc
nilBeside _ Empty         = Empty -- Hence the text "" in the spec
nilBeside g (Nest _ p)    = nilBeside g p
nilBeside g p | g         = textBeside_ spaceText 1 p
              | otherwise = p


-- ---------------------------------------------------------------------------
-- Separate, @sep@

-- Specification: sep ps  = oneLiner (hsep ps)
--                         `union`
--                          vcat ps

-- | Either 'hsep' or 'vcat'.
sep  :: [Doc] -> Doc
sep = sepX True   -- Separate with spaces

-- | Either 'hcat' or 'vcat'.
cat :: [Doc] -> Doc
cat = sepX False  -- Don't

sepX :: Bool -> [Doc] -> Doc
sepX _ []     = empty
sepX x (p:ps) = sep1 x (reduceDoc p) 0 ps


-- Specification: sep1 g k ys = sep (x : map (nest k) ys)
--                            = oneLiner (x <g> nest k (hsep ys))
--                              `union` x $$ nest k (vcat ys)
sep1 :: Bool -> RDoc -> Int -> [Doc] -> RDoc
sep1 _ _                   k _  | k `seq` False = undefined
sep1 _ NoDoc               _ _  = NoDoc
sep1 g (p `Union` q)       k ys = sep1 g p k ys `union_`
                                  aboveNest q False k (reduceDoc (vcat ys))

sep1 g Empty               k ys = mkNest k (sepX g ys)
sep1 g (Nest n p)          k ys = nest_ n (sep1 g p (k - n) ys)

sep1 _ (NilAbove p)        k ys = nilAbove_
                                  (aboveNest p False k (reduceDoc (vcat ys)))
sep1 g (TextBeside s sl p) k ys = textBeside_ s sl (sepNB g p (k - sl) ys)
sep1 _ (Above {})          _ _  = error "sep1 Above"
sep1 _ (Beside {})         _ _  = error "sep1 Beside"

-- Specification: sepNB p k ys = sep1 (text "" <> p) k ys
-- Called when we have already found some text in the first item
-- We have to eat up nests
sepNB :: Bool -> Doc -> Int -> [Doc] -> Doc
sepNB g (Nest _ p) k ys
  = sepNB g p k ys -- Never triggered, because of invariant (2)
sepNB g Empty k ys
  = oneLiner (nilBeside g (reduceDoc rest)) `mkUnion`
    -- XXX: TODO: PRETTY: Used to use True here (but GHC used False...)
    nilAboveNest False k (reduceDoc (vcat ys))
  where
    rest | g         = hsep ys
         | otherwise = hcat ys
sepNB g p k ys
  = sep1 g p k ys


-- ---------------------------------------------------------------------------
-- @fill@

-- | \"Paragraph fill\" version of 'cat'.
fcat :: [Doc] -> Doc
fcat = fill False

-- | \"Paragraph fill\" version of 'sep'.
fsep :: [Doc] -> Doc
fsep = fill True

-- Specification:
--
-- fill g docs = fillIndent 0 docs
--
-- fillIndent k [] = []
-- fillIndent k [p] = p
-- fillIndent k (p1:p2:ps) =
--    oneLiner p1 <g> fillIndent (k + length p1 + g ? 1 : 0)
--                               (remove_nests (oneLiner p2) : ps)
--     `Union`
--    (p1 $*$ nest (-k) (fillIndent 0 ps))
--
-- $*$ is defined for layouts (not Docs) as
-- layout1 $*$ layout2 | hasMoreThanOneLine layout1 = layout1 $$ layout2
--                     | otherwise                  = layout1 $+$ layout2

fill :: Bool -> [Doc] -> RDoc
fill _ []     = empty
fill g (p:ps) = fill1 g (reduceDoc p) 0 ps

fill1 :: Bool -> RDoc -> Int -> [Doc] -> Doc
fill1 _ _                   k _  | k `seq` False = undefined
fill1 _ NoDoc               _ _  = NoDoc
fill1 g (p `Union` q)       k ys = fill1 g p k ys `union_`
                                   aboveNest q False k (fill g ys)
fill1 g Empty               k ys = mkNest k (fill g ys)
fill1 g (Nest n p)          k ys = nest_ n (fill1 g p (k - n) ys)
fill1 g (NilAbove p)        k ys = nilAbove_ (aboveNest p False k (fill g ys))
fill1 g (TextBeside s sl p) k ys = textBeside_ s sl (fillNB g p (k - sl) ys)
fill1 _ (Above {})          _ _  = error "fill1 Above"
fill1 _ (Beside {})         _ _  = error "fill1 Beside"

fillNB :: Bool -> Doc -> Int -> [Doc] -> Doc
fillNB _ _           k _  | k `seq` False = undefined
fillNB g (Nest _ p)  k ys   = fillNB g p k ys
                              -- Never triggered, because of invariant (2)
fillNB _ Empty _ []         = Empty
fillNB g Empty k (Empty:ys) = fillNB g Empty k ys
fillNB g Empty k (y:ys)     = fillNBE g k y ys
fillNB g p k ys             = fill1 g p k ys


fillNBE :: Bool -> Int -> Doc -> [Doc] -> Doc
fillNBE g k y ys
  = nilBeside g (fill1 g ((elideNest . oneLiner . reduceDoc) y) k' ys)
    -- XXX: TODO: PRETTY: Used to use True here (but GHC used False...)
    `mkUnion` nilAboveNest False k (fill g (y:ys))
  where k' = if g then k - 1 else k

elideNest :: Doc -> Doc
elideNest (Nest _ d) = d
elideNest d          = d

-- ---------------------------------------------------------------------------
-- Selecting the best layout

best :: Int   -- Line length
     -> Int   -- Ribbon length
     -> RDoc
     -> RDoc  -- No unions in here!
best w0 r = get w0
  where
    get :: Int          -- (Remaining) width of line
        -> Doc -> Doc
    get w _ | w == 0 && False = undefined
    get _ Empty               = Empty
    get _ NoDoc               = NoDoc
    get w (NilAbove p)        = nilAbove_ (get w p)
    get w (TextBeside s sl p) = textBeside_ s sl (get1 w sl p)
    get w (Nest k p)          = nest_ k (get (w - k) p)
    get w (p `Union` q)       = nicest w r (get w p) (get w q)
    get _ (Above {})          = error "best get Above"
    get _ (Beside {})         = error "best get Beside"

    get1 :: Int         -- (Remaining) width of line
         -> Int         -- Amount of first line already eaten up
         -> Doc         -- This is an argument to TextBeside => eat Nests
         -> Doc         -- No unions in here!

    get1 w _ _ | w == 0 && False  = undefined
    get1 _ _  Empty               = Empty
    get1 _ _  NoDoc               = NoDoc
    get1 w sl (NilAbove p)        = nilAbove_ (get (w - sl) p)
    get1 w sl (TextBeside t tl p) = textBeside_ t tl (get1 w (sl + tl) p)
    get1 w sl (Nest _ p)          = get1 w sl p
    get1 w sl (p `Union` q)       = nicest1 w r sl (get1 w sl p)
                                                   (get1 w sl q)
    get1 _ _  (Above {})          = error "best get1 Above"
    get1 _ _  (Beside {})         = error "best get1 Beside"

nicest :: Int -> Int -> Doc -> Doc -> Doc
nicest !w !r = nicest1 w r 0

nicest1 :: Int -> Int -> Int -> Doc -> Doc -> Doc
nicest1 !w !r !sl p q | fits ((w `min` r) - sl) p = p
                      | otherwise                 = q

fits :: Int  -- Space available
     -> Doc
     -> Bool -- True if *first line* of Doc fits in space available
fits n _ | n < 0           = False
fits _ NoDoc               = False
fits _ Empty               = True
fits _ (NilAbove _)        = True
fits n (TextBeside _ sl p) = fits (n - sl) p
fits _ (Above {})          = error "fits Above"
fits _ (Beside {})         = error "fits Beside"
fits _ (Union {})          = error "fits Union"
fits _ (Nest {})           = error "fits Nest"

-- | @first@ returns its first argument if it is non-empty, otherwise its second.
first :: Doc -> Doc -> Doc
first p q | nonEmptySet p = p -- unused, because (get OneLineMode) is unused
          | otherwise     = q

nonEmptySet :: Doc -> Bool
nonEmptySet NoDoc              = False
nonEmptySet (_ `Union` _)      = True
nonEmptySet Empty              = True
nonEmptySet (NilAbove _)       = True
nonEmptySet (TextBeside _ _ p) = nonEmptySet p
nonEmptySet (Nest _ p)         = nonEmptySet p
nonEmptySet (Above {})         = error "nonEmptySet Above"
nonEmptySet (Beside {})        = error "nonEmptySet Beside"

-- @oneLiner@ returns the one-line members of the given set of @GDoc@s.
oneLiner :: Doc -> Doc
oneLiner NoDoc               = NoDoc
oneLiner Empty               = Empty
oneLiner (NilAbove _)        = NoDoc
oneLiner (TextBeside s sl p) = textBeside_ s sl (oneLiner p)
oneLiner (Nest k p)          = nest_ k (oneLiner p)
oneLiner (p `Union` _)       = oneLiner p
oneLiner (Above {})          = error "oneLiner Above"
oneLiner (Beside {})         = error "oneLiner Beside"


-- ---------------------------------------------------------------------------
-- Rendering

-- | A rendering style.
data Style
  = Style { mode           :: Mode  -- ^ The rendering mode
          , lineLength     :: Int   -- ^ Length of line, in chars
          , ribbonsPerLine :: Float -- ^ Ratio of line length to ribbon length
          }

-- | The default style (@mode=PageMode False, lineLength=100, ribbonsPerLine=1.5@).
style :: Style
style = Style { lineLength = 100, ribbonsPerLine = 1.5, mode = PageMode False }

-- | Rendering mode.
data Mode = PageMode { asciiSpace :: Bool }    -- ^ Normal
          | ZigZagMode   -- ^ With zig-zag cuts
          | LeftMode     -- ^ No indentation, infinitely long lines
          | OneLineMode  -- ^ All on one line

-- | Can we output an ascii space character for spaces?
--   Mostly true, but not for e.g. UTF16
--   See Note [putSpaces optimizations] for why we bother
--   to track this.
hasAsciiSpace :: Mode -> Bool
hasAsciiSpace mode =
  case mode of
    PageMode asciiSpace -> asciiSpace
    _ -> False

-- | Render the @Doc@ to a String using the given @Style@.
renderStyle :: Style -> Doc -> String
renderStyle s = fullRender (mode s) (lineLength s) (ribbonsPerLine s)
                txtPrinter ""

-- | Default TextDetails printer
txtPrinter :: TextDetails -> String -> String
txtPrinter (Chr c)    s  = c:s
txtPrinter (Str s1)   s2 = s1 ++ s2
txtPrinter (PStr s1)  s2 = unpackFS s1 ++ s2
txtPrinter (ZStr s1)  s2 = zString s1 ++ s2
txtPrinter (LStr s1)  s2 = unpackPtrString s1 ++ s2
txtPrinter (RStr n c) s2 = replicate n c ++ s2

-- | The general rendering interface.
fullRender :: Mode                     -- ^ Rendering mode
           -> Int                      -- ^ Line length
           -> Float                    -- ^ Ribbons per line
           -> (TextDetails -> a -> a)  -- ^ What to do with text
           -> a                        -- ^ What to do at the end
           -> Doc                      -- ^ The document
           -> a                        -- ^ Result
fullRender OneLineMode _ _ txt end doc
  = easyDisplay spaceText (\_ y -> y) txt end (reduceDoc doc)
fullRender LeftMode    _ _ txt end doc
  = easyDisplay nlText first txt end (reduceDoc doc)

fullRender m lineLen ribbons txt rest doc
  = display m lineLen ribbonLen txt rest doc'
  where
    doc' = best bestLineLen ribbonLen (reduceDoc doc)

    bestLineLen, ribbonLen :: Int
    ribbonLen   = round (fromIntegral lineLen / ribbons)
    bestLineLen = case m of
                      ZigZagMode -> maxBound
                      _          -> lineLen

easyDisplay :: TextDetails
             -> (Doc -> Doc -> Doc)
             -> (TextDetails -> a -> a)
             -> a
             -> Doc
             -> a
easyDisplay nlSpaceText choose txt end
  = lay
  where
    lay NoDoc              = error "easyDisplay: NoDoc"
    lay (Union p q)        = lay (choose p q)
    lay (Nest _ p)         = lay p
    lay Empty              = end
    lay (NilAbove p)       = nlSpaceText `txt` lay p
    lay (TextBeside s _ p) = s `txt` lay p
    lay (Above {})         = error "easyDisplay Above"
    lay (Beside {})        = error "easyDisplay Beside"

display :: Mode -> Int -> Int -> (TextDetails -> a -> a) -> a -> Doc -> a
display m !page_width !ribbon_width txt end doc
  = case page_width - ribbon_width of { gap_width ->
    case gap_width `quot` 2 of { shift ->
    let
        lay k _            | k `seq` False = undefined
        lay k (Nest k1 p)  = lay (k + k1) p
        lay _ Empty        = end
        lay k (NilAbove p) = nlText `txt` lay k p
        lay k (TextBeside s sl p)
            = case m of
                    ZigZagMode |  k >= gap_width
                               -> nlText `txt` (
                                  Str (replicate shift '/') `txt` (
                                  nlText `txt`
                                  lay1 (k - shift) s sl p ))

                               |  k < 0
                               -> nlText `txt` (
                                  Str (replicate shift '\\') `txt` (
                                  nlText `txt`
                                  lay1 (k + shift) s sl p ))

                    _ -> lay1 k s sl p
        lay _ (Above {})   = error "display lay Above"
        lay _ (Beside {})  = error "display lay Beside"
        lay _ NoDoc        = error "display lay NoDoc"
        lay _ (Union {})   = error "display lay Union"

        lay1 !k s !sl p    = let !r = k + sl
                             in indent k (s `txt` lay2 r p)

        lay2 k _ | k `seq` False   = undefined
        lay2 k (NilAbove p)        = nlText `txt` lay k p
        lay2 k (TextBeside s sl p) = s `txt` lay2 (k + sl) p
        lay2 k (Nest _ p)          = lay2 k p
        lay2 _ Empty               = end
        lay2 _ (Above {})          = error "display lay2 Above"
        lay2 _ (Beside {})         = error "display lay2 Beside"
        lay2 _ NoDoc               = error "display lay2 NoDoc"
        lay2 _ (Union {})          = error "display lay2 Union"

        indent !n r                = RStr n ' ' `txt` r
    in
    lay 0 doc
    }}

printDoc :: Mode -> Int -> Handle -> Doc -> IO ()
-- printDoc adds a newline to the end
printDoc mode cols hdl doc = printDoc_ mode cols hdl (doc $$ text "")

{- Note [putSpaces optimizations]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When using dump flags a lot of what we are dumping ends up being whitespace.
This is especially true for Core/Stg dumps. Enough so that it's worth optimizing.

Especially in the common case of writing to an UTF8 or similarly encoded file
where space is equal to ascii space we use hPutBuf to write a preallocated
buffer to the file. This avoids a fair bit of allocation.

For other cases we fall back to the old and slow path for simplicity.

-}

printDoc_ :: Mode -> Int -> Handle -> Doc -> IO ()
-- printDoc_ does not add a newline at the end, so that
-- successive calls can output stuff on the same line
-- Rather like putStr vs putStrLn
printDoc_ LeftMode _ hdl doc
  = do { printLeftRender hdl doc; hFlush hdl }
printDoc_ mode pprCols hdl doc
  = do { fullRender mode pprCols 1.5 put done doc ;
         hFlush hdl }
  where
    put (Chr c)    next = hPutChar hdl c >> next
    put (Str s)    next = hPutStr  hdl s >> next
    put (PStr s)   next = hPutStr  hdl (unpackFS s) >> next
                          -- NB. not hPutFS, we want this to go through
                          -- the I/O library's encoding layer. (#3398)
    put (ZStr s)   next = hPutFZS  hdl s >> next
    put (LStr s)   next = hPutPtrString hdl s >> next
    put (RStr n c) next
      | c == ' '
      = putSpaces n >> next
      | otherwise
      = hPutStr hdl (replicate n c) >> next
    putSpaces n
      -- If we use ascii spaces we are allowed to use hPutBuf
      -- See Note [putSpaces optimizations]
      | hasAsciiSpace mode
      , n <= 100
      = hPutBuf hdl (Ptr spaces') n
      | hasAsciiSpace mode
      , n > 100
      = hPutBuf hdl (Ptr spaces') 100 >> putSpaces (n-100)

      | otherwise = hPutStr hdl (replicate n ' ')

    done = return () -- hPutChar hdl '\n'
    -- 100 spaces, so we avoid the allocation of replicate n ' '
    spaces' = "                                                                                                    "#


  -- some versions of hPutBuf will barf if the length is zero
hPutPtrString :: Handle -> PtrString -> IO ()
hPutPtrString _handle (PtrString _ 0) = return ()
hPutPtrString handle  (PtrString a l) = hPutBuf handle a l

-- Printing output in LeftMode is performance critical: it's used when
-- dumping C and assembly output, so we allow ourselves a few dirty
-- hacks:
--
-- (1) we specialise fullRender for LeftMode with IO output.
--
-- (2) we add a layer of buffering on top of Handles.  Handles
--     don't perform well with lots of hPutChars, which is mostly
--     what we're doing here, because Handles have to be thread-safe
--     and async exception-safe.  We only have a single thread and don't
--     care about exceptions, so we add a layer of fast buffering
--     over the Handle interface.

printLeftRender :: Handle -> Doc -> IO ()
printLeftRender hdl doc = do
  b <- newBufHandle hdl
  bufLeftRender b doc
  bFlush b

bufLeftRender :: BufHandle -> Doc -> IO ()
bufLeftRender b doc = layLeft b (reduceDoc doc)

layLeft :: BufHandle -> Doc -> IO ()
layLeft b _ | b `seq` False  = undefined -- make it strict in b
layLeft _ NoDoc              = error "layLeft: NoDoc"
layLeft b (Union p q)        = layLeft b $! first p q
layLeft b (Nest _ p)         = layLeft b $! p
layLeft b Empty              = bPutChar b '\n'
layLeft b (NilAbove p)       = p `seq` (bPutChar b '\n' >> layLeft b p)
layLeft b (TextBeside s _ p) = s `seq` (put b s >> layLeft b p)
 where
    put b _ | b `seq` False = undefined
    put b (Chr c)    = bPutChar b c
    put b (Str s)    = bPutStr  b s
    put b (PStr s)   = bPutFS   b s
    put b (ZStr s)   = bPutFZS  b s
    put b (LStr s)   = bPutPtrString b s
    put b (RStr n c) = bPutReplicate b n c
layLeft _ _                  = panic "layLeft: Unhandled case"

-- Define error=panic, for easier comparison with libraries/pretty.
error :: String -> a
error = panic
