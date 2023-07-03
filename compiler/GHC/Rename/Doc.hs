module GHC.Rename.Doc ( rnHsDoc, rnLHsDoc, rnLDocDecl, rnDocDecl ) where

import GHC.Prelude

import GHC.Tc.Types
import GHC.Hs
import GHC.Types.Name.Reader
import GHC.Types.Name
import GHC.Types.SrcLoc
import GHC.Tc.Utils.Monad (getGblEnv)
import GHC.Rename.Env

rnLHsDoc :: LHsDoc GhcPs -> RnM (LHsDoc GhcRn)
rnLHsDoc = traverse rnHsDoc

rnLDocDecl :: LDocDecl GhcPs -> RnM (LDocDecl GhcRn)
rnLDocDecl = traverse rnDocDecl

rnDocDecl :: DocDecl GhcPs -> RnM (DocDecl GhcRn)
rnDocDecl (DocCommentNext doc) = do
  doc' <- rnLHsDoc doc
  pure $ (DocCommentNext doc')
rnDocDecl (DocCommentPrev doc) = do
  doc' <- rnLHsDoc doc
  pure $ (DocCommentPrev doc')
rnDocDecl (DocCommentNamed n doc) = do
  doc' <- rnLHsDoc doc
  pure $ (DocCommentNamed n doc')
rnDocDecl (DocGroup i doc) = do
  doc' <- rnLHsDoc doc
  pure $ (DocGroup i doc')

rnHsDoc :: WithHsDocIdentifiers a GhcPs -> RnM (WithHsDocIdentifiers a GhcRn)
rnHsDoc (WithHsDocIdentifiers s ids) = do
  gre <- tcg_rdr_env <$> getGblEnv
  pure (WithHsDocIdentifiers s (rnHsDocIdentifiers gre ids))

rnHsDocIdentifiers :: GlobalRdrEnv
                   -> [Located RdrName]
                   -> [Located Name]
rnHsDocIdentifiers gre_env ns =
  [ L l nm
  | L l rdr_name <- ns
  , gre <- lookupGRE_RdrName AllNameSpaces gre_env rdr_name
  , let nm = greName gre
  , rdrRelevantNameSpace rdr_name (nameNameSpace nm)
  ]
