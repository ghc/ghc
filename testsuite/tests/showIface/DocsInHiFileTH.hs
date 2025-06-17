{-# LANGUAGE TemplateHaskell, FlexibleInstances, TypeFamilies, DataKinds, GHC2021 #-}
{-# LANGUAGE MultiParamTypeClasses, StandaloneKindSignatures, PolyKinds #-}
{-# LANGUAGE PatternSynonyms #-}

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
  Just "A new constructor" <- getDoc (DeclDoc 'Foo)
  Just "A new data type" <- getDoc (DeclDoc ''Foo)
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

withDecsDoc "1" [d| wd1 x = () |]
withDecsDoc "2" [d| wd2 = () |]
withDecsDoc "3" [d| data WD3 = WD3 |]
withDecsDoc "4" [d| newtype WD4 = WD4 () |]
withDecsDoc "5" [d| type WD5 = () |]
withDecsDoc "6" [d| class WD6 a where |]
withDecsDoc "7" [d| instance C Foo where |]
do
  d <- withDecDoc "8" $ sigD 'wd8 [t| () |]
  pure [d]
--  this gives 'Illegal variable name: ‘WD9’' when splicing
--  withDoc "9"  [sigD ''WD9 [t| Type -> Type |]]
withDecsDoc "10" [d| data family WD10 a|]
withDecsDoc "11" [d| data instance WD11 Foo = WD11Foo |]
withDecsDoc "12" [d| type family WD12 a |]
withDecsDoc "13" [d| type instance WD13 Foo = Int |]

--  testing nullary classes here
withDecsDoc "14" [d| instance F |]

withDecsDoc "15" [d| foreign import ccall "math.h sin" sin :: Double -> Double |]
-- this gives 'Foreign export not (yet) handled by Template Haskell'
-- withDecsDoc "16" [d| foreign export ccall "addInt" (+) :: Int -> Int -> Int |]

wd17 = 42

do
  d <- withDecDoc "17" (sigD 'wd17 [t| Int |])
  pure [d]

do
  let nm = mkName "wd18"
  d' <- withDecDoc "18" $ sigD nm [t| Int |]
  d <-  withDecDoc "19" $ valD (varP nm) (normalB [| 42 |]) []
  pure [d, d']

-- Doing this to test that wd20 is documented as "20" and not "2020"
withDecsDoc "20" [d|
  wd20 :: Int
  wd20 = 42
  |]

do
  let defBang = bang noSourceUnpackedness noSourceStrictness
  patSynVarName <- newName "a"
  sequenceA
    [ funD_doc (mkName "qux") [clause [[p| a |], [p| b |] ] (normalB [e| () |]) []]
        (Just "This is qux") [Just "Arg uno", Just "Arg dos"]

    , dataD_doc (cxt []) (mkName "Quux") [] Nothing
        [ ( normalC (mkName "Quux1") [bangType defBang (reifyType ''Int)]
          , Just "This is Quux1",  [Just "I am an integer"])
        , ( normalC (mkName "Quux2")
              [ bangType defBang (reifyType ''String)
              , bangType defBang (reifyType ''Bool)
              ]
          , Just "This is Quux2", map Just ["I am a string", "I am a bool"])
        ] [] (Just "This is Quux")

    , dataD_doc (cxt []) (mkName "Quuz") [] Nothing
        [ ( recC (mkName "Quuz") [varBangType (mkName "quuz1_a") (bangType defBang (reifyType ''String))]
        , Just "This is a record constructor", [Just "This is the record constructor's argument"])
        ] [] (Just "This is a record type")

    , newtypeD_doc (cxt []) (mkName "Corge") [] Nothing
        ( recC (mkName ("Corge")) [varBangType (mkName "runCorge") (bangType defBang [t| Int |])]
        , Just "This is a newtype record constructor", [Just "This is the newtype record constructor's argument"]
        ) [] (Just "This is a record newtype")

    , dataInstD_doc (cxt []) Nothing [t| WD11 Int |] Nothing
        [ ( normalC (mkName "WD11Int") [bangType defBang [t| Int |]]
        , Just "This is a data instance constructor", [Just "This is a data instance constructor argument"])
        ] [] (Just "This is a data instance")

    , newtypeInstD_doc (cxt []) Nothing [t| WD11 Bool |] Nothing
        (normalC (mkName "WD11Bool") [bangType defBang [t| Bool |]]
        , Just "This is a newtype instance constructor", [Just "This is a newtype instance constructor argument"])
        [] (Just "This is a newtype instance")

    , patSynD_doc (mkName "Tup2") (prefixPatSyn [patSynVarName]) unidir
        [p| ($(varP patSynVarName), $(varP patSynVarName)) |]
        (Just "Matches a tuple of (a, a)") [Just "The thing to match twice"]

    , withDecDoc "My cool class" $ do
        tyVar <- newName "a"
        classD (cxt []) (mkName "Pretty") [plainTV tyVar] []
          [ withDecDoc "Prettily prints the object" $
              sigD (mkName "prettyPrint") [t| $(varT tyVar) -> String |]
          ]
    ]
