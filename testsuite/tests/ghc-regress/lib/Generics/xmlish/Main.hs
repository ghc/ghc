{-# OPTIONS -fglasgow-exts #-}

{-

This example illustrates XMLish services
to treealise (say, "serialise") heterogenous
Haskell data as homogeneous tree structures
(say as XMLish elements) and vice versa.

-}

module Main where
import Data.Maybe
import Data.Generics
import CompanyDatatypes


-- HaXml-like types for XML elements
data Element   = Elem Name [Attribute] [Content]
                 deriving (Show, Typeable, Data)

data Content   = CElem Element
               | CString Bool CharData
                        -- ^ bool is whether whitespace is significant
               | CRef Reference
               | CMisc Misc
                 deriving (Show, Typeable, Data)

type CharData = String


-- In this simple example we disable some parts of XML
type Attribute = ()
type Reference = ()
type Misc      = ()


-- Treealisation
data2content :: forall a. Data a => a -> [Content]
data2content = gdefault `extQ` string `extQ` float

 where

  -- Generic default
  gdefault x = maybe (element x) concat (list x)

  -- Handle an element
  element x = [CElem (Elem (elemtype x)
                           noAttrs 
                           (concat (gmapQ data2content x)))]
  
  -- A special case for lists
  list x = lgmapQ data2content x

  -- A special case for strings
  string :: String -> [Content]
  string x = [CString True x]

  -- A special case for floats
  float :: Float -> [Content]
  float x = [CString True (show x)]

  -- Determine element type
  elemtype = tyconString
           . typerepTyCon 
           . typeOf

  -- No attributes
  noAttrs = []


-- De-treealisation
content2data :: forall a. Data a => Content -> a
content2data = undefined


-- Main function for testing
main = print $ (   genCom
               , ( data2content genCom
               ))
