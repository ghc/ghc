
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
     Just doc -> return (Just (rnDoc gre doc))

lexParseRnMbHaddockComment :: DynFlags -> HaddockCommentType -> GlobalRdrEnv -> Maybe HsDocString -> ErrMsgM (Maybe (Doc Name))
lexParseRnMbHaddockComment _ _ _ Nothing = return Nothing
lexParseRnMbHaddockComment dflags hty gre (Just d) = lexParseRnHaddockComment dflags hty gre d

-- yes, you always get a HaddockModInfo though it might be empty
lexParseRnHaddockModHeader :: DynFlags -> GlobalRdrEnv -> GhcDocHdr -> ErrMsgM (HaddockModInfo Name, Maybe (Doc Name))
lexParseRnHaddockModHeader dflags gre mbStr = do
  let failure = (emptyHaddockModInfo, Nothing)
  case mbStr of
    Nothing -> return failure
    Just (L _ (HsDocString fs)) -> do
      let str = unpackFS fs
      case parseModuleHeader dflags str of
        Left mess -> do
          tell ["haddock module header parse failed: " ++ mess]
          return failure
        Right (info, doc) ->
          return (rnHaddockModInfo gre info, Just (rnDoc gre doc))
