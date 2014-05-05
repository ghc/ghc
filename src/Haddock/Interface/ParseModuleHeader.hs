{-# OPTIONS_GHC -Wwarn #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.Interface.ParseModuleHeader
-- Copyright   :  (c) Simon Marlow 2006, Isaac Dupree 2009
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
module Haddock.Interface.ParseModuleHeader (parseModuleHeader) where

import Haddock.Types
import Haddock.Parser.Util

import RdrName
import DynFlags

import Data.Char
import Control.Monad (mplus)

-- -----------------------------------------------------------------------------
-- Parsing module headers

-- NB.  The headers must be given in the order Module, Description,
-- Copyright, License, Maintainer, Stability, Portability, except that
-- any or all may be omitted.
parseModuleHeader :: DynFlags -> String -> Either String (HaddockModInfo RdrName, Doc RdrName)
parseModuleHeader dflags str0 =
   let
      getKey :: String -> String -> (Maybe String,String)
      getKey key str = case parseKey key str of
         Nothing -> (Nothing,str)
         Just (value,rest) -> (Just value,rest)

      (_moduleOpt,str1) = getKey "Module" str0
      (descriptionOpt,str2) = getKey "Description" str1
      (copyrightOpt,str3) = getKey "Copyright" str2
      (licenseOpt,str4) = getKey "License" str3
      (licenceOpt,str5) = getKey "Licence" str4
      (maintainerOpt,str6) = getKey "Maintainer" str5
      (stabilityOpt,str7) = getKey "Stability" str6
      (portabilityOpt,str8) = getKey "Portability" str7

      description1 :: Either String (Maybe (Doc RdrName))
      description1 = case descriptionOpt of
         Nothing -> Right Nothing
         Just description -> case parseStringMaybe dflags description of
            Nothing -> Left ("Cannot parse Description: " ++ description)
            Just doc -> Right (Just doc)
   in
      case description1 of
         Left mess -> Left mess
         Right docOpt -> case parseParasMaybe dflags str8 of
           Nothing -> Left "Cannot parse header documentation paragraphs"
           Just doc -> Right (HaddockModInfo {
            hmi_description = docOpt,
            hmi_copyright = copyrightOpt,
            hmi_license = licenseOpt `mplus` licenceOpt,
            hmi_maintainer = maintainerOpt,
            hmi_stability = stabilityOpt,
            hmi_portability = portabilityOpt,
            hmi_safety = Nothing,
            hmi_language = Nothing, -- set in LexParseRn
            hmi_extensions = [] -- also set in LexParseRn
            }, doc)

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
      scanKey _       _           [] = ([],[])
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
         | otherwise = ([],s)

      extractNextLine :: String -> (String,String)
      extractNextLine [] = ([],[])
      extractNextLine (c:cs)
         | c == '\n' =
            ([],cs)
         | otherwise =
            let
               (line,rest) = extractNextLine cs
            in
               (c:line,rest)

      -- comparison is case-insensitive.
      extractPrefix :: String -> String -> Maybe String
      extractPrefix [] s = Just s
      extractPrefix _ [] = Nothing
      extractPrefix (c1:cs1) (c2:cs2)
         | toUpper c1 == toUpper c2 = extractPrefix cs1 cs2
         | otherwise = Nothing
