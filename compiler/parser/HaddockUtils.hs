module HaddockUtils where

import HsSyn
import HsDoc
import {-# SOURCE #-} HaddockLex
import HaddockParse
import SrcLoc
import RdrName

import Control.Monad
import Data.Maybe
import Data.Char
import Data.Either

-- -----------------------------------------------------------------------------
-- Parsing module headers

-- NB.  The headers must be given in the order Module, Description,
-- Copyright, License, Maintainer, Stability, Portability, except that
-- any or all may be omitted.
parseModuleHeader :: String -> Either String (String, HaddockModInfo RdrName)                                
parseModuleHeader str0 =                                                                        
   let                                                                                          
      getKey :: String -> String -> (Maybe String,String)                                       
      getKey key str = case parseKey key str of                                                 
         Nothing -> (Nothing,str)                                                               
         Just (value,rest) -> (Just value,rest)                                                 
                                                                                                
      (moduleOpt,str1) = getKey "Module" str0                                                   
      (descriptionOpt,str2) = getKey "Description" str1                                         
      (copyrightOpt,str3) = getKey "Copyright" str2                                             
      (licenseOpt,str4) = getKey "License" str3                                                 
      (licenceOpt,str5) = getKey "Licence" str4                                                 
      (maintainerOpt,str6) = getKey "Maintainer" str5                                           
      (stabilityOpt,str7) = getKey "Stability" str6                                             
      (portabilityOpt,str8) = getKey "Portability" str7                                         
                                                                                                
      description1 :: Either String (Maybe (HsDoc RdrName))                                                 
      description1 = case descriptionOpt of                                                     
         Nothing -> Right Nothing                                                               
         Just description -> case parseHaddockString . tokenise $ description of                       

            Left mess -> Left ("Cannot parse Description: " ++ mess)                            
            Right doc -> Right (Just doc)                                                       
   in                                                                                           
      case description1 of                                                                      
         Left mess -> Left mess                                                                 
         Right docOpt -> Right (str8,HaddockModInfo {                                               
            hmi_description = docOpt,                                                               
            hmi_portability = portabilityOpt,                                                       
            hmi_stability = stabilityOpt,                                                           
            hmi_maintainer = maintainerOpt                                                          
            })

-- | This function is how we read keys.
--
-- all fields in the header are optional and have the form
--
-- [spaces1][field name][spaces] ":" 
--    [text]"\n" ([spaces2][space][text]"\n" | [spaces]"\n")*
-- where each [spaces2] should have [spaces1] as a prefix.
--
-- Thus for the key "Description",
--
-- > Description : this is a
-- >    rather long
-- >
-- >    description
-- >
-- > The module comment starts here
-- 
-- the value will be "this is a .. description" and the rest will begin
-- at "The module comment".
parseKey :: String -> String -> Maybe (String,String)
parseKey key toParse0 =
   do
      let
         (spaces0,toParse1) = extractLeadingSpaces toParse0

         indentation = spaces0
      afterKey0 <- extractPrefix key toParse1
      let
         afterKey1 = extractLeadingSpaces afterKey0
      afterColon0 <- case snd afterKey1 of
         ':':afterColon -> return afterColon
         _ -> Nothing
      let
         (_,afterColon1) = extractLeadingSpaces afterColon0

      return (scanKey True indentation afterColon1)
   where
      scanKey :: Bool -> String -> String -> (String,String)
      scanKey isFirst indentation [] = ([],[])
      scanKey isFirst indentation str =
         let
            (nextLine,rest1) = extractNextLine str

            accept = isFirst || sufficientIndentation || allSpaces

            sufficientIndentation = case extractPrefix indentation nextLine of
               Just (c:_) | isSpace c -> True
               _ -> False

            allSpaces = case extractLeadingSpaces nextLine of
               (_,[]) -> True
               _ -> False
         in
            if accept 
               then
                  let
                     (scanned1,rest2) = scanKey False indentation rest1

                     scanned2 = case scanned1 of 
                        "" -> if allSpaces then "" else nextLine
                        _ -> nextLine ++ "\n" ++ scanned1
                  in
                     (scanned2,rest2)
               else
                  ([],str)

      extractLeadingSpaces :: String -> (String,String)
      extractLeadingSpaces [] = ([],[])
      extractLeadingSpaces (s@(c:cs)) 
         | isSpace c = 
            let
               (spaces1,cs1) = extractLeadingSpaces cs
            in
               (c:spaces1,cs1)
         | True = ([],s)

      extractNextLine :: String -> (String,String)
      extractNextLine [] = ([],[])
      extractNextLine (c:cs) 
         | c == '\n' =
            ([],cs)
         | True =
            let
               (line,rest) = extractNextLine cs
            in
               (c:line,rest)
         

      -- indentation returns characters after last newline.
      indentation :: String -> String
      indentation s = fromMaybe s (indentation0 s)
         where
            indentation0 :: String -> Maybe String
            indentation0 [] = Nothing
            indentation0 (c:cs) =
               case indentation0 cs of
                  Nothing -> if c == '\n' then Just cs else Nothing
                  in0 -> in0
               
      -- comparison is case-insensitive.
      extractPrefix :: String -> String -> Maybe String
      extractPrefix [] s = Just s
      extractPrefix s [] = Nothing
      extractPrefix (c1:cs1) (c2:cs2)
         | toUpper c1 == toUpper c2 = extractPrefix cs1 cs2
         | True = Nothing

-- -----------------------------------------------------------------------------
-- Adding documentation to record fields (used in parsing).

type Field a = ([Located a], LBangType a, Maybe (LHsDoc a))

addFieldDoc :: Field a -> Maybe (LHsDoc a) -> Field a
addFieldDoc (a, b, c) doc = (a, b, c `mplus` doc)

addFieldDocs :: [Field a] -> Maybe (LHsDoc a) -> [Field a]
addFieldDocs [] _ = []
addFieldDocs (x:xs) doc = addFieldDoc x doc : xs

addConDoc :: LConDecl a -> Maybe (LHsDoc a) -> LConDecl a
addConDoc (L p c) doc = L p ( c { con_doc = con_doc c `mplus` doc } )

addConDocs :: [LConDecl a] -> Maybe (LHsDoc a) -> [LConDecl a]
addConDocs [] _ = []
addConDocs [x] doc = [addConDoc x doc]
addConDocs (x:xs) doc = x : addConDocs xs doc

addConDocFirst :: [LConDecl a] -> Maybe (LHsDoc a) -> [LConDecl a]
addConDocFirst [] _ = []
addConDocFirst (x:xs) doc = addConDoc x doc : xs
