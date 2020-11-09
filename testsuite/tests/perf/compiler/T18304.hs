{-# LANGUAGE RecordWildCards, PatternGuards #-}
{-# OPTIONS_GHC -Wunused-binds #-}

module Text.HTML.TagSoup.Specification
    (dat, Out(..) )
where

-- Code taken from the tagsoup library, which is BSD-3-licensed.

import Data.Char (isAlpha, isAlphaNum, isDigit, toLower)

data TypeTag = TypeNormal -- <foo
             | TypeXml    -- <?foo
             | TypeDecl   -- <!foo
             | TypeScript -- <script
               deriving Eq


type Parser = S -> [Out]

-- 8.2.4.1 Data state
dat :: S -> [Out]
dat S{..} = tagName TypeXml tl

-- 8.2.4.5 Tag name state
tagName :: TypeTag -> S -> [Out]
tagName typ S{..} = case hd of
    'a' -> beforeAttName typ tl

-- 8.2.4.6 Before attribute name state
beforeAttName :: TypeTag -> S -> [Out]
beforeAttName typ S{..} = case hd of
    _ | hd `elem` "=" -> beforeAttValue typ s -- NEIL

-- 8.2.4.9 Before attribute value state
beforeAttValue :: TypeTag -> S -> [Out]
beforeAttValue typ S{..} = case hd of
    'a' -> beforeAttValue typ tl
    '&' -> attValueUnquoted typ s

-- 8.2.4.12 Attribute value (unquoted) state
attValueUnquoted :: TypeTag -> Parser
attValueUnquoted typ S{..} = case hd of
    '?' -> neilXmlTagClose tl
    'a' -> beforeAttName typ tl
    'b' -> attValueUnquoted typ tl

-- seen "?", expecting ">"
neilXmlTagClose :: S -> [Out]
neilXmlTagClose S{..} = case hd of
    '>' -> dat tl
    _ -> beforeAttName TypeXml s

-----
-- Text.HTML.TagSoup.Implementation
-----

data Out = SomeOut


data S = S
    { s :: S
    , tl :: S
    ,hd :: Char
    ,eof :: Bool
    }

