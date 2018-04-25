#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.PrettyPrint.Annotated
-- Copyright   :  (c) Trevor Elliott <revor@galois.com> 2015
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  David Terei <code@davidterei.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This module provides a version of pretty that allows for annotations to be
-- attached to documents. Annotations are arbitrary pieces of metadata that can
-- be attached to sub-documents.
--
-- This module should be used as opposed to the
-- 'Text.PrettyPrint.Annotated.HughesPJ' module. Both are equivalent though as
-- this module simply re-exports the other.
--
-----------------------------------------------------------------------------

module Text.PrettyPrint.Annotated ( 

        -- * The document type
        Doc,

        -- * Constructing documents

        -- ** Converting values into documents
        char, text, ptext, sizedText, zeroWidthText,
        int, integer, float, double, rational,

        -- ** Simple derived documents
        semi, comma, colon, space, equals,
        lparen, rparen, lbrack, rbrack, lbrace, rbrace,

        -- ** Wrapping documents in delimiters
        parens, brackets, braces, quotes, doubleQuotes,

        -- ** Combining documents
        empty,
        (X.<>), (<+>), hcat, hsep,
        ($$), ($+$), vcat,
        sep, cat,
        fsep, fcat,
        nest,
        hang, punctuate,

        -- ** Annotating documents
        annotate,

        -- * Predicates on documents
        isEmpty,

        -- * Rendering documents

        -- ** Default rendering
        render,

        -- ** Annotation rendering
        renderSpans, Span(..),

        -- ** Rendering with a particular style
        Style(..),
        style,
        renderStyle,

        -- ** General rendering
        fullRender,
        fullRenderAnn,
        Mode(..), TextDetails(..)

    ) where

import Text.PrettyPrint.Annotated.HughesPJ as X

