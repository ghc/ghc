
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

module Haddock.Interface.LexParseRn (
  HaddockCommentType(..),
  lexParseRnHaddockComment,
  lexParseRnHaddockCommentList,
  lexParseRnMbHaddockComment,
  lexParseRnHaddockModHeader
  ) where

import Haddock.Types
import Haddock.Lex
import Haddock.Parse
import Haddock.Interface.Rn
import Haddock.Interface.ParseModuleHeader
import Haddock.Doc
import Data.Maybe
import FastString
import GHC
import RdrName

data HaddockCommentType = NormalHaddockComment | DocSectionComment

lexParseRnHaddockCommentList :: HaddockCommentType -> GlobalRdrEnv -> [HsDocString] -> ErrMsgM (Maybe (Doc Name))
lexParseRnHaddockCommentList hty gre docStrs = do
  docMbs <- mapM (lexParseRnHaddockComment hty gre) docStrs
  let docs = catMaybes docMbs
  let doc = foldl docAppend DocEmpty docs
  case doc of
    DocEmpty -> return Nothing
    _ -> return (Just doc)

lexParseRnHaddockComment :: HaddockCommentType ->
    GlobalRdrEnv -> HsDocString -> ErrMsgM (Maybe (Doc Name))
lexParseRnHaddockComment hty gre (HsDocString fs) = do
   let str = unpackFS fs
   let toks = tokenise str (0,0) -- TODO: real position
   let parse = case hty of
         NormalHaddockComment -> parseParas
         DocSectionComment -> parseString
   case parse toks of
     Nothing -> do
       tell ["doc comment parse failed: "++str]
       return Nothing
     Just doc -> return (Just (rnDoc gre doc))

lexParseRnMbHaddockComment :: HaddockCommentType -> GlobalRdrEnv -> Maybe HsDocString -> ErrMsgM (Maybe (Doc Name))
lexParseRnMbHaddockComment _ _ Nothing = return Nothing
lexParseRnMbHaddockComment hty gre (Just d) = lexParseRnHaddockComment hty gre d

-- yes, you always get a HaddockModInfo though it might be empty
lexParseRnHaddockModHeader :: GlobalRdrEnv -> GhcDocHdr -> ErrMsgM (HaddockModInfo Name, Maybe (Doc Name))
lexParseRnHaddockModHeader gre mbStr = do
  let failure = (emptyHaddockModInfo, Nothing)
  case mbStr of
    Nothing -> return failure
    Just (L _ (HsDocString fs)) -> do
      let str = unpackFS fs
      case parseModuleHeader str of
        Left mess -> do
          tell ["haddock module header parse failed: " ++ mess]
          return failure
        Right (info, doc) ->
          return (rnHaddockModInfo gre info, Just (rnDoc gre doc))
