{-# LANGUAGE DerivingStrategies #-}
module GHC.Types.ConInfo (
  ConFieldEnv, ConInfo(..), mkConInfo, conInfoFields,
  ) where

import GHC.Prelude
import GHC.Types.Name.Env (NameEnv)
import Data.List.NonEmpty (NonEmpty)
import GHC.Types.FieldLabel ( FieldLabel )
import qualified Data.List.NonEmpty as NonEmpty
import GHC.Types.Basic (Arity)
import GHC.Utils.Outputable (Outputable(..), text, (<+>), equals, braces, (<>))

{- Note [Local constructor info in the renamer]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
During renaming, we need certain information about constructors.

While we can use TypeEnv to get this info for constructors from imported modules,
the same cannot be done for constructors defined in the module
that we are currently renaming, as they have not been type checked yet.

Hence, we use ConFieldEnv to store the minimal information required to proceed
with renaming, getting it from the parse tree.

For example, consider
  data T = T1 { x, y :: Int }
         | T2 { x :: Int }
         | T3
         | T4 Int Bool

Specifically we need to know:
* The fields of the data constructor, so that
  - We can complain if you say `T1 { v = 3 }`, where `v` is not a field of `T1`
    See the following call stack
    * GHC.Rename.Expr.rnExpr (RecordCon case)
    * GHC.Rename.Pat.rnHsRecFields
    * GHC.Rename.Env.lookupRecFieldOcc
  - Ditto if you pattern match on `T1 { v = x }`.
    See the following call stack
    * GHC.Rename.Pat.rnHsRecPatsAndThen
    * GHC.Rename.Pat.rnHsRecFields
    * GHC.Rename.Env.lookupRecFieldOcc
  - We can fill in the dots if you say `T1 {..}` in construction or pattern matching
    See GHC.Rename.Pat.rnHsRecFields.rn_dotdot

* Whether the contructor is nullary.
  We need to know this to accept `T2 {..}`, and `T3 {..}`, but reject `T4 {..}`,
  in both construction and pattern matching.
  See GHC.Rename.Pat.rnHsRecFields.rn_dotdot
  and Note [Nullary constructors and empty record wildcards]

Note [Nullary constructors and empty record wildcards]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A nullary constructor is one with no arguments.
For example, both `data T = MkT` and `data T = MkT {}` are nullary.

For consistency and TH convenience, it was agreed that a `{..}`
match or usage on nullary constructors would be accepted.
This is done as as per https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0496-empty-record-wildcards.rst
-}
type ConFieldEnv = NameEnv ConInfo

-- | See Note [Local constructor info in the renamer]
data ConInfo
  = ConHasRecordFields (NonEmpty FieldLabel)
  | ConHasPositionalArgs
  | ConIsNullary
  deriving stock Eq

mkConInfo :: Arity -> [FieldLabel] -> ConInfo
mkConInfo 0 _ = ConIsNullary
mkConInfo _ fields = maybe ConHasPositionalArgs ConHasRecordFields $ NonEmpty.nonEmpty fields

conInfoFields :: ConInfo -> [FieldLabel]
conInfoFields (ConHasRecordFields fields) = NonEmpty.toList fields
conInfoFields ConHasPositionalArgs = []
conInfoFields ConIsNullary = []

instance Outputable ConInfo where
  ppr ConIsNullary = text "ConIsNullary"
  ppr ConHasPositionalArgs = text "ConHasPositionalArgs"
  ppr (ConHasRecordFields fieldLabels) = text "ConHasRecordFields" <> braces (text "fieldLabels" <+> equals <+> ppr fieldLabels)
