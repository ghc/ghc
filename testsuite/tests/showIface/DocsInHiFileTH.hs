{-# LANGUAGE TemplateHaskell, FlexibleInstances, TypeFamilies #-}

-- |This is the module header
module DocInHiFilesTH where

import Language.Haskell.TH
import DocsInHiFileTHExternal

f :: Int
f = 42

$(putDoc (DeclDoc 'f) "The meaning of life" >> pure [])

-- |A data type
data Foo =
    -- |A constructor
    Foo

do
  Just "A data type" <- getDoc (DeclDoc ''Foo)
  Just "A constructor" <- getDoc (DeclDoc 'Foo)
  putDoc (DeclDoc ''Foo) "A new data type"
  putDoc (DeclDoc 'Foo) "A new constructor"
  Just "A new data type" <- getDoc (DeclDoc ''Foo)
  Just "A new constructor" <- getDoc (DeclDoc 'Foo)
  pure []

-- |Some documentation
g :: String
g = "Hello world"

do
  Just "Some documentation" <- getDoc (DeclDoc 'g)
  pure []

-- Testing module headers

do
  Just "This is the module header" <- getDoc ModuleDoc
  putDoc ModuleDoc "This is the new module header"
  Just "This is the new module header" <- getDoc ModuleDoc
  pure []

-- Testing argument documentation

h :: Int -- ^Your favourite number
  -> Bool -- ^Your favourite element in the Boolean algebra
  -> String -- ^A return value
h _ _ = "Hello world"

do
  Just "Your favourite number" <- getDoc (ArgDoc 'h 0)
  Just "Your favourite element in the Boolean algebra" <- getDoc (ArgDoc 'h 1)
  Just "A return value" <- getDoc (ArgDoc 'h 2)
  Nothing <- getDoc (ArgDoc 'h 3)
  putDoc (ArgDoc 'h 1) "Your least favourite Boolean"
  Just "Your least favourite Boolean" <- getDoc (ArgDoc 'h 1)
  pure []


-- Testing classes and instances

-- |A fancy class
class C a where

-- |A fancy instance
instance C Int where
instance C String where

class D a where
-- |Another fancy instance
instance D a where

-- |A type family
type family E a

-- |A type family instance
type instance E Bool = Int

i :: E Bool
i = 42

do
  Just "A fancy class" <- getDoc (DeclDoc ''C)
  Just "A fancy instance" <- getDoc (InstDoc ''C (ConT ''Int))
  Just "Another fancy instance" <- getDoc (InstDoc ''D (VarT (mkName "a")))
  Just "Another fancy instance" <- getDoc (InstDoc ''D (VarT (mkName "b")))
  Nothing <- getDoc (InstDoc ''C (ConT ''String))

  putDoc (DeclDoc ''C) "A new class"
  putDoc (InstDoc ''C (ConT ''Int)) "A new instance"
  putDoc (InstDoc ''D (VarT (mkName "a"))) "Another new instance"
  Just "A new class" <- getDoc (DeclDoc ''C)
  Just "A new instance" <- getDoc (InstDoc ''C (ConT ''Int))
  Just "Another new instance" <- getDoc (InstDoc ''D (VarT (mkName "a")))

  Just "A type family" <- getDoc (DeclDoc ''E)
  -- Doesn't work just yet. See T18241
  -- https://gitlab.haskell.org/ghc/ghc/issues/18241
  -- Just "A type family instance" <- getDoc (InstDoc ''E (VarT (mkName "a")))

  pure []

-- Testing documentation from external modules
do
  Just "This is an external function" <- getDoc (DeclDoc 'externalFunc)
  Just "Some integer" <- getDoc (ArgDoc 'externalFunc 0)

  Just "This is an external class" <- getDoc (DeclDoc ''ExternalClass)
  Just "This is an external instance" <-
    getDoc (InstDoc ''ExternalClass (ConT ''Int))

  pure []
