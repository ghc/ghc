{-|
Module      :  Text.Parsec
Copyright   :  (c) Daan Leijen 1999-2001, (c) Paolo Martini 2007
License     :  BSD-style (see the LICENSE file)

Maintainer  :  aslatter@gmail.com
Stability   :  provisional
Portability :  portable

This module includes everything you need to get started writing a
parser.

By default this module is set up to parse character data. If you'd like
to parse the result of your own tokenizer you should start with the following
imports:

@
 import Text.Parsec.Prim
 import Text.Parsec.Combinator
@

Then you can implement your own version of 'satisfy' on top of the 'tokenPrim'
primitive.

-}

module Text.Parsec
    ( -- * Parsers
      ParsecT
    , Parsec
    , token
    , tokens
    , runParserT
    , runParser
    , parse
    , parseTest
    , getPosition
    , getInput
    , getState
    , putState
    , modifyState
     -- * Combinators
    , (<|>)
    , (<?>)
    , label
    , labels
    , try
    , unexpected
    , choice
    , many
    , many1
    , skipMany
    , skipMany1
    , count
    , between
    , option
    , optionMaybe
    , optional
    , sepBy
    , sepBy1
    , endBy
    , endBy1
    , sepEndBy
    , sepEndBy1
    , chainl
    , chainl1
    , chainr
    , chainr1
    , eof
    , notFollowedBy
    , manyTill
    , lookAhead
    , anyToken
     -- * Character Parsing
    , module Text.Parsec.Char
     -- * Error messages
    , ParseError
    , errorPos
     -- * Position
    , SourcePos
    , SourceName, Line, Column
    , sourceName, sourceLine, sourceColumn
    , incSourceLine, incSourceColumn
    , setSourceLine, setSourceColumn, setSourceName
     -- * Low-level operations
    , manyAccum
    , tokenPrim
    , tokenPrimEx
    , runPT
    , unknownError
    , sysUnExpectError
    , mergeErrorReply
    , getParserState
    , setParserState
    , updateParserState
    , Stream (..)
    , runParsecT
    , mkPT
    , runP
    , Consumed (..)
    , Reply (..)
    , State (..)
    , setPosition
    , setInput
     -- * Other stuff
    , setState
    , updateState
    , parsecMap
    , parserReturn
    , parserBind
    , parserFail
    , parserZero
    , parserPlus
    ) where

import Text.Parsec.Pos
import Text.Parsec.Error
import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Combinator
