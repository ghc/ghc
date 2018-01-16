module Distribution.Solver.Types.Variable where

import Distribution.Solver.Types.OptionalStanza

import Distribution.PackageDescription (FlagName)

-- | Variables used by the dependency solver. This type is similar to the
-- internal 'Var' type, except that flags and stanzas are associated with
-- package names instead of package instances.
data Variable qpn =
    PackageVar qpn
  | FlagVar qpn FlagName
  | StanzaVar qpn OptionalStanza
  deriving (Eq, Show)
