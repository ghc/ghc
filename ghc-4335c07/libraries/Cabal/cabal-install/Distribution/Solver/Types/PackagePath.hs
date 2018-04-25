module Distribution.Solver.Types.PackagePath
    ( PackagePath(..)
    , Namespace(..)
    , Qualifier(..)
    , dispQualifier
    , Qualified(..)
    , QPN
    , dispQPN
    , showQPN
    ) where

import Distribution.Package
import Distribution.Text
import qualified Text.PrettyPrint as Disp
import Distribution.Solver.Compat.Prelude ((<<>>))

-- | A package path consists of a namespace and a package path inside that
-- namespace.
data PackagePath = PackagePath Namespace Qualifier
  deriving (Eq, Ord, Show)

-- | Top-level namespace
--
-- Package choices in different namespaces are considered completely independent
-- by the solver.
data Namespace =
    -- | The default namespace
    DefaultNamespace

    -- | A namespace for a specific build target
  | Independent PackageName
  deriving (Eq, Ord, Show)

-- | Pretty-prints a namespace. The result is either empty or
-- ends in a period, so it can be prepended onto a qualifier.
dispNamespace :: Namespace -> Disp.Doc
dispNamespace DefaultNamespace = Disp.empty
dispNamespace (Independent i) = disp i <<>> Disp.text "."

-- | Qualifier of a package within a namespace (see 'PackagePath')
data Qualifier =
    -- | Top-level dependency in this namespace
    QualToplevel

    -- | Any dependency on base is considered independent
    --
    -- This makes it possible to have base shims.
  | QualBase PackageName

    -- | Setup dependency
    --
    -- By rights setup dependencies ought to be nestable; after all, the setup
    -- dependencies of a package might themselves have setup dependencies, which
    -- are independent from everything else. However, this very quickly leads to
    -- infinite search trees in the solver. Therefore we limit ourselves to
    -- a single qualifier (within a given namespace).
  | QualSetup PackageName

    -- | If we depend on an executable from a package (via
    -- @build-tools@), we should solve for the dependencies of that
    -- package separately (since we're not going to actually try to
    -- link it.)  We qualify for EACH package separately; e.g.,
    -- @'Exe' pn1 pn2@ qualifies the @build-tools@ dependency on
    -- @pn2@ from package @pn1@.  (If we tracked only @pn1@, that
    -- would require a consistent dependency resolution for all
    -- of the depended upon executables from a package; if we
    -- tracked only @pn2@, that would require us to pick only one
    -- version of an executable over the entire install plan.)
  | QualExe PackageName PackageName
  deriving (Eq, Ord, Show)

-- | Pretty-prints a qualifier. The result is either empty or
-- ends in a period, so it can be prepended onto a package name.
--
-- NOTE: the base qualifier is for a dependency _on_ base; the qualifier is
-- there to make sure different dependencies on base are all independent.
-- So we want to print something like @"A.base"@, where the @"A."@ part
-- is the qualifier and @"base"@ is the actual dependency (which, for the
-- 'Base' qualifier, will always be @base@).
dispQualifier :: Qualifier -> Disp.Doc
dispQualifier QualToplevel = Disp.empty
dispQualifier (QualSetup pn)  = disp pn <<>> Disp.text ":setup."
dispQualifier (QualExe pn pn2) = disp pn <<>> Disp.text ":" <<>>
                                 disp pn2 <<>> Disp.text ":exe."
dispQualifier (QualBase pn)  = disp pn <<>> Disp.text "."

-- | A qualified entity. Pairs a package path with the entity.
data Qualified a = Q PackagePath a
  deriving (Eq, Ord, Show)

-- | Qualified package name.
type QPN = Qualified PackageName

-- | Pretty-prints a qualified package name.
dispQPN :: QPN -> Disp.Doc
dispQPN (Q (PackagePath ns qual) pn) =
  dispNamespace ns <<>> dispQualifier qual <<>> disp pn

-- | String representation of a qualified package name.
showQPN :: QPN -> String
showQPN = Disp.renderStyle flatStyle . dispQPN
