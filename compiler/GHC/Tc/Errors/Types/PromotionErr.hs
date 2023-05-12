module GHC.Tc.Errors.Types.PromotionErr ( PromotionErr(..)
                                        , pprPECategory
                                        , peCategory
                                        ) where

import GHC.Prelude
import GHC.Core.Type (ThetaType)
import GHC.Utils.Outputable
import GHC.Utils.Misc

data PromotionErr
  = TyConPE          -- TyCon used in a kind before we are ready
                     --     data T :: T -> * where ...
  | ClassPE          -- Ditto Class

  | FamDataConPE     -- Data constructor for a data family
                     -- See Note [AFamDataCon: not promoting data family constructors]
                     -- in GHC.Tc.Utils.Env.
  | ConstrainedDataConPE ThetaType -- Data constructor with a context
                                   -- See Note [No constraints in kinds] in GHC.Tc.Validity
  | PatSynPE         -- Pattern synonyms
                     -- See Note [Don't promote pattern synonyms] in GHC.Tc.Utils.Env

  | RecDataConPE     -- Data constructor in a recursive loop
                     -- See Note [Recursion and promoting data constructors] in GHC.Tc.TyCl
  | TermVariablePE   -- See Note [Promoted variables in types]
  | NoDataKindsDC    -- -XDataKinds not enabled (for a datacon)

instance Outputable PromotionErr where
  ppr ClassPE              = text "ClassPE"
  ppr TyConPE              = text "TyConPE"
  ppr PatSynPE             = text "PatSynPE"
  ppr FamDataConPE         = text "FamDataConPE"
  ppr (ConstrainedDataConPE theta) = text "ConstrainedDataConPE" <+> parens (ppr theta)
  ppr RecDataConPE         = text "RecDataConPE"
  ppr NoDataKindsDC        = text "NoDataKindsDC"
  ppr TermVariablePE       = text "TermVariablePE"

pprPECategory :: PromotionErr -> SDoc
pprPECategory = text . capitalise . peCategory

peCategory :: PromotionErr -> String
peCategory ClassPE              = "class"
peCategory TyConPE              = "type constructor"
peCategory PatSynPE             = "pattern synonym"
peCategory FamDataConPE         = "data constructor"
peCategory ConstrainedDataConPE{} = "data constructor"
peCategory RecDataConPE         = "data constructor"
peCategory NoDataKindsDC        = "data constructor"
peCategory TermVariablePE       = "term variable"