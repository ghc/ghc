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
  ( HaddockCommentType(..)
  , lexParseRnHaddockComment
  , lexParseRnHaddockCommentList
  , lexParseRnMbHaddockComment
  , lexParseRnHaddockModHeader
  ) where


import Haddock.Types
import Haddock.Lex
import Haddock.Parse
import Haddock.Interface.ParseModuleHeader
import Haddock.Doc

import Control.Applicative
import Data.Maybe
import FastString
import GHC
import Name
import Outputable
import RdrName
import RnEnv


data HaddockCommentType = NormalHaddockComment | DocSectionComment


lexParseRnHaddockCommentList :: DynFlags -> HaddockCommentType -> GlobalRdrEnv -> [HsDocString] -> ErrMsgM (Maybe (Doc Name))
lexParseRnHaddockCommentList dflags hty gre docStrs = do
  docMbs <- mapM (lexParseRnHaddockComment dflags hty gre) docStrs
  let docs = catMaybes docMbs
  let doc = foldl docAppend DocEmpty docs
  case doc of
    DocEmpty -> return Nothing
    _ -> return (Just doc)


lexParseRnHaddockComment :: DynFlags -> HaddockCommentType ->
    GlobalRdrEnv -> HsDocString -> ErrMsgM (Maybe (Doc Name))
lexParseRnHaddockComment dflags hty gre (HsDocString fs) = do
   let str = unpackFS fs
   let toks = tokenise dflags str (0,0) -- TODO: real position
   let parse = case hty of
         NormalHaddockComment -> parseParas
         DocSectionComment -> parseString
   case parse toks of
     Nothing -> do
       tell ["doc comment parse failed: "++str]
       return Nothing
     Just doc -> return (Just (rename gre doc))


lexParseRnMbHaddockComment :: DynFlags -> HaddockCommentType -> GlobalRdrEnv -> Maybe HsDocString -> ErrMsgM (Maybe (Doc Name))
lexParseRnMbHaddockComment _ _ _ Nothing = return Nothing
lexParseRnMbHaddockComment dflags hty gre (Just d) = lexParseRnHaddockComment dflags hty gre d


-- yes, you always get a HaddockModInfo though it might be empty
lexParseRnHaddockModHeader :: DynFlags -> GlobalRdrEnv -> GhcDocHdr -> ErrMsgM (HaddockModInfo Name, Maybe (Doc Name))
lexParseRnHaddockModHeader dflags gre mbStr = do
  (hmi, docn) <-
    case mbStr of
      Nothing -> return failure
      Just (L _ (HsDocString fs)) -> do
        let str = unpackFS fs
        case parseModuleHeader dflags str of
          Left mess -> do
            tell ["haddock module header parse failed: " ++ mess]
            return failure
          Right (info, doc) -> return (renameHmi gre info, Just (rename gre doc))
  return (hmi { hmi_safety = safety }, docn)
  where
    safety  = Just $ showPpr $ safeHaskell dflags
    failure = (emptyHaddockModInfo, Nothing)


renameHmi :: GlobalRdrEnv -> HaddockModInfo RdrName -> HaddockModInfo Name
renameHmi gre hmi = hmi { hmi_description = rename gre <$> hmi_description hmi }


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
