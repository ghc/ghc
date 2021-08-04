{-# OPTIONS -fglasgow-exts #-}

module XML (tests) where

{-

This example illustrates XMLish services
to trealise (say, "serialise") heterogenous
Haskell data as homogeneous tree structures
(say, XMLish elements) and vice versa.

-}

import Test.Tasty.HUnit

import Control.Applicative (Alternative(..), Applicative(..))
import Control.Monad
import Data.Maybe
import Data.Generics
import CompanyDatatypes


-- HaXml-like types for XML elements
data Element   = Elem Name [Attribute] [Content]
                 deriving (Show, Eq, Typeable, Data)

data Content   = CElem Element
               | CString Bool CharData
                        -- ^ bool is whether whitespace is significant
               | CRef Reference
               | CMisc Misc
                 deriving (Show, Eq, Typeable, Data)

type CharData = String


-- In this simple example we disable some parts of XML
type Attribute = ()
type Reference = ()
type Misc      = ()


-- Trealisation
data2content :: Data a => a -> [Content]
data2content =         element
               `ext1Q` list
               `extQ`  string
               `extQ`  float

 where

  -- Handle an element
  element x = [CElem (Elem (tyconUQname (dataTypeName (dataTypeOf x)))
                           [] -- no attributes
                           (concat (gmapQ data2content x)))]

  -- A special case for lists
  list :: Data a => [a] -> [Content]
  list = concat . map data2content

  -- A special case for strings
  string :: String -> [Content]
  string x = [CString True x]

  -- A special case for floats
  float :: Double -> [Content]
  float x = [CString True (show x)]


-- De-trealisation
content2data :: forall a. Data a => ReadX a
content2data = result

 where

  -- Case-discriminating worker
  result =         element
           `ext1R` list
           `extR`  string
           `extR`  float


  -- Determine type of data to be constructed
  myType = myTypeOf result
    where
      myTypeOf :: forall a. ReadX a -> a
      myTypeOf =  undefined

  -- Handle an element
  element = do c <- readX
               case c of
                 (CElem (Elem x as cs))
                    |    as == [] -- no attributes
                      && x  == (tyconUQname (dataTypeName (dataTypeOf myType)))
                   -> alts cs
                 _ -> mzero


  -- A special case for lists
  list :: forall a. Data a => ReadX [a]
  list =          ( do h <- content2data
                       t <- list
                       return (h:t) )
         `mplus`  return []

  -- Fold over all alternatives, say constructors
  alts cs = foldr (mplus . recurse cs) mzero shapes

  -- Possible top-level shapes
  shapes = map fromConstr consOf

  -- Retrieve all constructors of the requested type
  consOf = dataTypeConstrs
         $ dataTypeOf
         $ myType

  -- Recurse into subterms
  recurse cs x = maybe mzero
                       return
                       (runReadX (gmapM (const content2data) x) cs)

  -- A special case for strings
  string :: ReadX String
  string =  do c <- readX
               case c of
                 (CString _ x) -> return x
                 _             -> mzero

  -- A special case for floats
  float :: ReadX Double
  float =  do c <- readX
              case c of
                (CString _ x) -> return (read x)
                _             -> mzero



-----------------------------------------------------------------------------
--
-- An XML-hungry parser-like monad
--
-----------------------------------------------------------------------------

-- Type constructor
newtype ReadX a =
        ReadX { unReadX :: [Content]
                        -> Maybe ([Content], a) }

-- Run a computation
runReadX x y = case unReadX x y of
                 Just ([],y) -> Just y
                 _           -> Nothing

-- Read one content particle
readX :: ReadX Content
readX =  ReadX (\x -> if null x
                        then Nothing
                        else Just (tail x, head x)
               )

instance Functor ReadX where
  fmap  = liftM

instance Applicative ReadX where
  pure  = return
  (<*>) = ap

instance Alternative ReadX where
  (<|>) = mplus
  empty = mzero

-- ReadX is a monad!
instance Monad ReadX where
  return x = ReadX (\y -> Just (y,x))
  c >>= f  = ReadX (\x -> case unReadX c x of
                            Nothing -> Nothing
                            Just (x', a) -> unReadX (f a) x'
                   )

-- ReadX also accommodates mzero and mplus!
instance MonadPlus ReadX where
  mzero = ReadX (const Nothing)
  f `mplus` g = ReadX (\x -> case unReadX f x of
                               Nothing -> unReadX g x
                               y -> y
                      )



-----------------------------------------------------------------------------
--
--	Main function for testing
--
-----------------------------------------------------------------------------

tests = (   genCom
        , ( data2content genCom
        , ( zigzag person1 :: Maybe Person
        , ( zigzag genCom  :: Maybe Company
        , ( zigzag genCom == Just genCom
        ))))) @=? output
 where
  -- Trealise back and forth
  zigzag :: Data a => a -> Maybe a
  zigzag = runReadX content2data . data2content

output = (C [D "Research" (E (P "Laemmel" "Amsterdam") (S 8000.0)) [PU (E (P "Joost" "Amsterdam") (S 1000.0)),PU (E (P "Marlow" "Cambridge") (S 2000.0))],D "Strategy" (E (P "Blair" "London") (S 100000.0)) []],([CElem (Elem "Company" [] [CElem (Elem "Dept" [] [CString True "Research",CElem (Elem "Employee" [] [CElem (Elem "Person" [] [CString True "Laemmel",CString True "Amsterdam"]),CElem (Elem "Salary" [] [CString True "8000.0"])]),CElem (Elem "Unit" [] [CElem (Elem "Employee" [] [CElem (Elem "Person" [] [CString True "Joost",CString True "Amsterdam"]),CElem (Elem "Salary" [] [CString True "1000.0"])])]),CElem (Elem "Unit" [] [CElem (Elem "Employee" [] [CElem (Elem "Person" [] [CString True "Marlow",CString True "Cambridge"]),CElem (Elem "Salary" [] [CString True "2000.0"])])])]),CElem (Elem "Dept" [] [CString True "Strategy",CElem (Elem "Employee" [] [CElem (Elem "Person" [] [CString True "Blair",CString True "London"]),CElem (Elem "Salary" [] [CString True "100000.0"])])])])],(Just (P "Lazy" "Home"),(Just (C [D "Research" (E (P "Laemmel" "Amsterdam") (S 8000.0)) [PU (E (P "Joost" "Amsterdam") (S 1000.0)),PU (E (P "Marlow" "Cambridge") (S 2000.0))],D "Strategy" (E (P "Blair" "London") (S 100000.0)) []]),True))))
