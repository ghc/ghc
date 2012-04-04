  -----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.Interface.LexParseRn
-- Copyright   :  (c) Isaac Dupree 2009,
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
module Haddock.Interface.LexParseRn
  ( processDocString
  , processDocStringParas
  , processDocStrings
  , processModuleHeader
  ) where


import Haddock.Types
import Haddock.Lex
import Haddock.Parse
import Haddock.Interface.ParseModuleHeader
import Haddock.Doc

import Control.Applicative
import Data.List
import Data.Maybe
import FastString
import GHC
import Name
import Outputable
import RdrName
import RnEnv


processDocStrings :: DynFlags -> GlobalRdrEnv -> [HsDocString] -> ErrMsgM (Maybe (Doc Name))
processDocStrings dflags gre strs = do
  docs <- catMaybes <$> mapM (processDocStringParas dflags gre) strs
  let doc = foldl' docAppend DocEmpty docs
  case doc of
    DocEmpty -> return Nothing
    _ -> return (Just doc)


processDocStringParas :: DynFlags -> GlobalRdrEnv -> HsDocString -> ErrMsgM (Maybe (Doc Name))
processDocStringParas = process parseParas


processDocString :: DynFlags -> GlobalRdrEnv -> HsDocString -> ErrMsgM (Maybe (Doc Name))
processDocString = process parseString

process :: ([LToken] -> Maybe (Doc RdrName))
        -> DynFlags
        -> GlobalRdrEnv
        -> HsDocString
        -> ErrMsgM (Maybe (Doc Name))
process parse dflags gre (HsDocString fs) = do
   let str = unpackFS fs
   let toks = tokenise dflags str (0,0)  -- TODO: real position
   case parse toks of
     Nothing -> do
       tell [ "doc comment parse failed: " ++ str ]
       return Nothing
     Just doc -> return (Just (rename gre doc))


processModuleHeader :: DynFlags -> GlobalRdrEnv -> SafeHaskellMode -> Maybe LHsDocString
                    -> ErrMsgM (HaddockModInfo Name, Maybe (Doc Name))
processModuleHeader dflags gre safety mayStr = do
  (hmi, doc) <-
    case mayStr of
      Nothing -> return failure
      Just (L _ (HsDocString fs)) -> do
        let str = unpackFS fs
        case parseModuleHeader dflags str of
          Left msg -> do
            tell ["haddock module header parse failed: " ++ msg]
            return failure
          Right (hmi, doc) -> do
            let hmi' = hmi { hmi_description = rename gre <$> hmi_description hmi }
                doc' = rename gre doc
            return (hmi', Just doc')
  return (hmi { hmi_safety = Just $ showPpr safety }, doc)
  where
    failure = (emptyHaddockModInfo, Nothing)


rename :: GlobalRdrEnv -> Doc RdrName -> Doc Name
rename gre = rn
  where
    rn d = case d of
      DocAppend a b -> DocAppend (rn a) (rn b)
      DocParagraph doc -> DocParagraph (rn doc)
      DocIdentifier x -> do
        let choices = dataTcOccs x
        let names = concatMap (\c -> map gre_name (lookupGRE_RdrName c gre)) choices
        case names of
          [] ->
            case choices of
              [] -> DocMonospaced (DocString (showSDoc $ ppr x))
              [a] -> outOfScope a
              a:b:_ | isRdrTc a -> outOfScope a | otherwise -> outOfScope b
          [a] -> DocIdentifier a
          a:b:_ | isTyConName a -> DocIdentifier a | otherwise -> DocIdentifier b
              -- If an id can refer to multiple things, we give precedence to type
              -- constructors.
      DocWarning doc -> DocWarning (rn doc)
      DocEmphasis doc -> DocEmphasis (rn doc)
      DocMonospaced doc -> DocMonospaced (rn doc)
      DocUnorderedList docs -> DocUnorderedList (map rn docs)
      DocOrderedList docs -> DocOrderedList (map rn docs)
      DocDefList list -> DocDefList [ (rn a, rn b) | (a, b) <- list ]
      DocCodeBlock doc -> DocCodeBlock (rn doc)
      DocIdentifierUnchecked x -> DocIdentifierUnchecked x
      DocModule str -> DocModule str
      DocURL str -> DocURL str
      DocPic str -> DocPic str
      DocAName str -> DocAName str
      DocExamples e -> DocExamples e
      DocEmpty -> DocEmpty
      DocString str -> DocString str


outOfScope :: RdrName -> Doc a
outOfScope x =
  case x of
    Unqual occ -> monospaced occ
    Qual mdl occ -> DocIdentifierUnchecked (mdl, occ)
    Orig _ occ -> monospaced occ
    Exact name -> monospaced name  -- Shouldn't happen since x is out of scope
  where
    monospaced a = DocMonospaced (DocString (showSDoc $ ppr a))
