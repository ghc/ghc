{-# OPTIONS -fglasgow-exts #-}

{-

This example illustrates XMLish services
to trealise (say, "serialise") heterogenous
Haskell data as homogeneous tree structures
(say, XMLish elements) and vice versa.

-}

module Main where
import Control.Monad
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


-- No attributes
noAttrs = []


-- Trealisation
data2content :: Data a => a -> [Content]
data2content =        gdefault
               `extQ` string 
               `extQ` float

 where

  -- Generic default
  gdefault x = if isList x 
                 then concat (list x)
                 else element x

  -- Handle an element
  element x = [CElem (Elem (elemtype x)
                           noAttrs 
                           (concat (gmapQ data2content x)))]
  
  -- A special case for lists
  list x = gmapListQ data2content x

  -- A special case for strings
  string :: String -> [Content]
  string x = [CString True x]

  -- A special case for floats
  float :: Float -> [Content]
  float x = [CString True (show x)]


-- Determine element type
elemtype :: Data a => a -> String
elemtype = unqualify
         . tyconString
         . typerepTyCon 
         . typeOf


-- Remove *.*.*... before name
unqualify :: String -> String
unqualify x = let x' = dropWhile (not . (==) '.') x
               in if x' == [] then x else unqualify (tail x')


-- De-trealisation
content2data :: forall a. Data a => ReadX a
content2data = result

 where
 
  -- Case-discriminating worker
  result =        gdefault
           `extR` string
           `extR` float


  -- Determine type of data to be constructed
  myType = myTypeOf result
    where
      myTypeOf :: ReadX a -> a
      myTypeOf =  undefined


  -- Generic default
  gdefault = if isList myType
               then list
               else element


  -- Handle an element
  element = do c <- readX
               case c of
                 (CElem (Elem x as cs))
                    |    as == noAttrs
                      && x  == elemtype myType
                   -> alts cs
                 _ -> mzero


  -- Fold over all alternatives, say constructors
  alts cs = foldr (mplus . recurse cs) mzero shapes


  -- Possible top-level shapes
  shapes = map fromConstr consOf


  -- Retrieve all constructors of the requested type
  consOf = dataTypeCons
         $ dataTypeOf 
         $ myType


  -- Recurse into subterms
  recurse cs x = maybe mzero
                       return
                       (runReadX (gmapM (const content2data) x) cs)


  -- A special case for lists
  list =          gmapM (const content2data) mkCons
         `mplus`  return mkNil


  -- A special case for strings
  string :: ReadX String
  string =  do c <- readX
               case c of
                 (CString _ x) -> return x
                 _             -> mzero


  -- A special case for floats
  float :: ReadX Float
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
newtype ReadX a = ReadX ([Content] -> Maybe ([Content], a))

-- Unwrap constructor
unReadX (ReadX x) = x

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



-------------------------------------------------------------
--
--	Processing polymorphic lists
--
-------------------------------------------------------------


-- | Test for list datatype
isList :: Data a => a -> Bool
isList x = typerepTyCon (typeOf x) ==
           typerepTyCon (typeOf (undefined::[()]))


-- | Test for nil
isNil :: Data a => a -> Bool
isNil x = toConstr x == toConstr ([]::[()])


-- | Test for cons
isCons :: Data a => a -> Bool
isCons x = toConstr x == toConstr (():[])


-- | gmapQ for polymorphic lists
gmapListQ :: forall a q. Data a => (forall a. Data a => a -> q) -> a -> [q]
gmapListQ f x =
  if not $ isList x 
    then error "gmapListQ"
    else if isNil x
           then []
           else if isCons x
                  then ( gmapQi 0 f x : gmapQi 1 (gmapListQ f) x )
                  else error "gmapListQ"


-- | Build nil
mkNil :: Data a => a
mkNil = fromConstr $ toConstr ([]::[()])


-- | Build cons
mkCons :: Data a => a
mkCons = fromConstr $ toConstr ((undefined:undefined)::[()])



-----------------------------------------------------------------------------
--
--	Main function for testing
--
-----------------------------------------------------------------------------

main = print $ (   genCom
               , ( data2content genCom
               , ( zigzag person1 :: Maybe Person
               , ( zigzag genCom  :: Maybe Company
               , ( zigzag genCom == Just genCom
               )))))
 where 
  -- Trealise back and forth
  zigzag :: Data a => a -> Maybe a
  zigzag = runReadX content2data . data2content
