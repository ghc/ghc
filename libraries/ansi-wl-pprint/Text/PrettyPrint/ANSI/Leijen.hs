{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.PrettyPrint.ANSI.Leijen
-- Copyright   :  Daan Leijen (c) 2000, http://www.cs.uu.nl/~daan
--                Max Bolingbroke (c) 2008, http://blog.omega-prime.co.uk
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
--
-- This module is an extended implementation of the functional pretty printer
-- given by Philip Wadler (1997):
--
-- @
--      \"A prettier printer\"
--      Draft paper, April 1997, revised March 1998.
--      <https://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf>
-- @
--
-- In their bare essence, the combinators given by Wadler are
-- not expressive enough to describe some commonly occurring layouts.
-- This library adds new primitives to describe these layouts and
-- works well in practice.
--
-- The library is based on a single way to concatenate documents,
-- which is associative and has both a left and right unit.  This
-- simple design leads to an efficient and short implementation. The
-- simplicity is reflected in the predictable behaviour of the
-- combinators which make them easy to use in practice.
--
-- A thorough description of the primitive combinators and their
-- implementation can be found in Philip Wadler's paper.
-- The main differences with his original paper are:
--
-- * The nil document is called 'empty'.
--
-- * The above combinator is called '<$>'. The operator '</>' is used
-- for soft line breaks.
--
-- * There are three new primitives: 'align', 'fill' and
-- 'fillBreak'. These are very useful in practice.
--
-- * There are many additional useful combinators, like 'fillSep' and 'list'.
--
-- * There are two renderers: 'renderPretty' for pretty printing, and
-- 'renderCompact' for quickly rendered, compact output more suitable
-- for generating input to other programs.
--
-- * The pretty printing algorithm used by 'renderPretty' extends the algorithm
-- given by Wadler to take into account a \"ribbon width\", i.e., a desired
-- maximum number of non-indentation characters to output on any one line.
--
-- * There are two displayers, 'displayS' for strings and 'displayIO' for
-- file-based output.
--
-- * There is a 'Pretty' class.
--
-- * The implementation uses optimised representations and strictness
-- annotations.
--
-- * The library has been extended to allow formatting text for output
-- to ANSI style consoles. New combinators allow control of foreground and
-- background color and the ability to make parts of the text bold or
-- underlined.
-----------------------------------------------------------
module Text.PrettyPrint.ANSI.Leijen (
   -- * The algebra of pretty-printing
   -- $DocumentAlgebra

   -- * Documents
   Doc,

   -- * Basic combinators
   empty, char, text, string, int, integer, float, double, rational, bool,
   (<>), nest, line, linebreak,
   group, softline, softbreak, hardline, flatAlt,

   -- * Alignment combinators
   --
   -- | The combinators in this section cannot be described by Wadler's
   -- original combinators. They align their output relative to the
   -- current output position — in contrast to @nest@ which always
   -- aligns to the current nesting level. This deprives these
   -- combinators from being \`optimal\'. In practice however they
   -- prove to be very useful. The combinators in this section should
   -- be used with care, since they are more expensive than the other
   -- combinators. For example, @align@ shouldn't be used to pretty
   -- print all top-level declarations of a language, but using @hang@
   -- for let expressions is fine.
   align, hang, indent, encloseSep, list, tupled, semiBraces,

   -- * Operators
   (<+>), (Text.PrettyPrint.ANSI.Leijen.Internal.<$>), (</>), (<$$>), (<//>),

   -- * List combinators
   hsep, vsep, fillSep, sep, hcat, vcat, fillCat, cat, punctuate,

   -- * Filler combinators
   fill, fillBreak,

   -- * Bracketing combinators
   enclose, squotes, dquotes, parens, angles, braces, brackets,

   -- * Named character combinators
   lparen, rparen, langle, rangle, lbrace, rbrace, lbracket, rbracket,
   squote, dquote, semi, colon, comma, space, dot, backslash, equals,


   -- * ANSI formatting combinators
   --
   -- | This terminal formatting functionality is, as far as possible,
   -- portable across platforms with their varying terminals. However,
   -- note that to display ANSI colors and formatting will only be displayed
   -- on Windows consoles if the 'Doc' value is output using the 'putDoc'
   -- function or one of its friends.  Rendering the 'Doc' to a 'String'
   -- and then outputing /that/ will only work on Unix-style operating systems.

   -- ** Forecolor combinators
   black, red, green, yellow, blue, magenta, cyan, white,
   dullblack, dullred, dullgreen, dullyellow, dullblue, dullmagenta,
   dullcyan, dullwhite,

   -- ** Backcolor combinators
   onblack, onred, ongreen, onyellow, onblue, onmagenta, oncyan, onwhite,
   ondullblack, ondullred, ondullgreen, ondullyellow, ondullblue, ondullmagenta,
   ondullcyan, ondullwhite,

   -- ** Emboldening combinators
   bold, debold,

   -- ** Underlining combinators
   underline, deunderline,

   -- ** Formatting elimination combinators
   plain,


   -- * Pretty class
   Pretty(..),


   -- * Rendering and displaying documents

   -- ** Simple (i.e., rendered) documents
   SimpleDoc(..),
   renderPretty, renderCompact, renderSmart,
   displayS,
   displayIO,

   -- ** Simultaneous rendering and displaying of documents
   putDoc, hPutDoc,


   -- * Undocumented
   column, columns, nesting, width
   ) where

import Text.PrettyPrint.ANSI.Leijen.Internal

#if __GLASGOW_HASKELL__ >= 710
import Data.Monoid ((<>))
#elif __GLASGOW_HASKELL__ >= 704
import Data.Monoid (Monoid, mappend, mconcat, mempty, (<>))
#else
import Data.Monoid (Monoid, mappend, mconcat, mempty)
#endif

-- $DocumentAlgebra
-- The combinators in this library satisfy many algebraic laws.
--
-- The concatenation operator '<>' is associative and has 'empty' as a left
-- and right unit:
--
--     > x <> (y <> z)           = (x <> y) <> z
--     > x <> empty              = x
--     > empty <> x              = x
--
-- The 'text' combinator is a homomorphism from string concatenation to
-- document concatenation:
--
--     > text (s ++ t)           = text s <> text t
--     > text ""                 = empty
--
-- The 'char' combinator behaves like one-element text:
--
--     > char c                  = text [c]
--
-- The 'nest' combinator is a homomorphism from addition to document
-- composition.  'nest' also distributes through document concatenation and is
-- absorbed by 'text' and 'align':
--
--     > nest (i + j) x          = nest i (nest j x)
--     > nest 0 x                = x
--     > nest i (x <> y)         = nest i x <> nest i y
--     > nest i empty            = empty
--     > nest i (text s)         = text s
--     > nest i (align x)        = align x
--
-- The 'group' combinator is absorbed by 'empty'.  'group' is commutative with
-- 'nest' and 'align':
--
--     > group empty             = empty
--     > group (text s <> x)     = text s <> group x
--     > group (nest i x)        = nest i (group x)
--     > group (align x)         = align (group x)
--
-- The 'align' combinator is absorbed by 'empty' and 'text'.
-- 'align' is idempotent:
--
--     > align empty             = empty
--     > align (text s)          = text s
--     > align (align x)         = align x
--
-- From the laws of the primitive combinators, we can derive many other laws
-- for the derived combinators.  For example, the /above/ operator '<$>' is
-- defined as:
--
--     > x <$> y                 = x <> line <> y
--
-- It follows that '<$>' is associative and that '<$>' and '<>' associate
-- with each other:
--
--     > x <$> (y <$> z)         = (x <$> y) <$> z
--     > x <> (y <$> z)          = (x <> y) <$> z
--     > x <$> (y <> z)          = (x <$> y) <> z
--
-- Similar laws also hold for the other line break operators '</>', '<$$>',
-- and '<//>'.
