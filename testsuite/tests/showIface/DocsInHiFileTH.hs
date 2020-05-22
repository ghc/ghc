{-# LANGUAGE TemplateHaskell, FlexibleInstances, TypeFamilies, DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses, StandaloneKindSignatures, PolyKinds #-}

-- |This is the module header
module DocInHiFilesTH where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
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
  Just "A fancy instance" <- getDoc . InstDoc =<< [t| C Int |]
  Just "Another fancy instance" <- getDoc (InstDoc (AppT (ConT ''D) (VarT (mkName "a"))))
  Just "Another fancy instance" <- getDoc (InstDoc (AppT (ConT ''D) (VarT (mkName "b"))))
  Nothing <- getDoc . InstDoc =<< [t| C String |]

  putDoc (DeclDoc ''C) "A new class"
  putDoc (InstDoc (AppT (ConT ''C) (ConT ''Int))) "A new instance"
  putDoc (InstDoc (AppT (ConT ''C) (ConT ''String))) "Another new instance"
  putDoc (InstDoc (AppT (ConT ''D) (VarT (mkName "a")))) "Another new instance"
  Just "A new class" <- getDoc (DeclDoc ''C)
  Just "A new instance" <- getDoc . InstDoc =<< [t| C Int |]
  Just "Another new instance" <- getDoc . InstDoc =<< [t| C String |]
  Just "Another new instance" <- getDoc (InstDoc (AppT (ConT ''D) (VarT (mkName "a"))))

  Just "A type family" <- getDoc (DeclDoc ''E)
  -- Doesn't work just yet. See T18241
  -- https://gitlab.haskell.org/ghc/ghc/issues/18241
  Just "A type family instance" <- getDoc . InstDoc =<< [t| E Bool |]

  pure []

-- Testing documentation from external modules
do
  Just "This is an external function" <- getDoc (DeclDoc 'externalFunc)
  Just "Some integer" <- getDoc (ArgDoc 'externalFunc 0)

  Just "This is an external class" <- getDoc (DeclDoc ''ExternalClass)
  Just "This is an external instance" <-
    getDoc . InstDoc =<< [t| ExternalClass Int |]

  pure []

data family WD11 a
type family WD13 a

wd8 = ()

class F

-- Testing combinators

withDoc "1" [d| wd1 x = () |]
withDoc "2" [d| wd2 = () |]
withDoc "3" [d| data WD3 = WD3 |]
withDoc "4" [d| newtype WD4 = WD4 () |]
withDoc "5" [d| type WD5 = () |]
withDoc "6" [d| class WD6 a where |]
withDoc "7" [d| instance C Foo where |]
do
  d <- sigD 'wd8 [t| () |]
  withDoc "8" (pure [d])
--  this gives 'Illegal variable name: ‘WD9’' when splicing
--  withDoc "9"  [sigD ''WD9 [t| Type -> Type |]]
withDoc "10" [d| data family WD10 a|]
withDoc "11" [d| data instance WD11 Foo = Int |]
withDoc "12" [d| type family WD12 a |]
withDoc "13" [d| type instance WD13 Foo = Int |]

--  testing nullary classes here
withDoc "14" [d| instance F |]

do
  sequenceA
    [ funD_doc (mkName "qux") [clause [ [p| a |], [p| b |] ] (normalB [e| () |]) []]
        "This is qux" ["Arg uno", "Arg dos"]

    , dataD_doc (cxt []) (mkName "Quz") [] Nothing
        [ ( normalC (mkName "Quz1") [bangType (bang noSourceUnpackedness noSourceStrictness) (reifyType ''Int)]
          , "This is Quz1",  ["I am an integer"])
        , ( normalC (mkName "Quz2")
              [ bangType (bang noSourceUnpackedness noSourceStrictness) (reifyType ''String)
              , bangType (bang noSourceUnpackedness noSourceStrictness) (reifyType ''Bool)
              ]
          , "This is Quz2",  ["I am a string", "I am a bool"])
        ] [] "This is Quz"

    , dataD_doc (cxt []) (mkName "Qax") [] Nothing
        [ ( recC (mkName "Qax1") [varBangType (mkName "qax1_a") (bangType (bang noSourceUnpackedness noSourceStrictness) (reifyType ''String))]
          , "This is a record constructor", ["This is the record constructor's argument"])
        ] [] "This is a record type"

    , withDoc' "My cool class" $ do
        tyVar <- newName "a"
        classD (cxt []) (mkName "Pretty") [plainTV tyVar] []
          [ withDoc' "Prettily prints the object" $
              sigD (mkName "prettyPrint") [t| $(varT tyVar) -> String |]
          ]
    ]
