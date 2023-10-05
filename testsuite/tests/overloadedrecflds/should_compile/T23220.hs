{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module T23220 where

import Language.Haskell.TH

import T23220_aux ( makeExtendingDatatype )

type Uri = String

data TextDocumentIdentifier =
  TextDocumentIdentifier
    { _uri :: Uri
    }

type TextDocumentVersion = Maybe Int

makeExtendingDatatype "VersionedTextDocumentIdentifier" [''TextDocumentIdentifier]
  [ ("_version", [t| TextDocumentVersion |])]
