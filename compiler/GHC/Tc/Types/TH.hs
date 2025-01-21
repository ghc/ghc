module GHC.Tc.Types.TH (
    SpliceType(..)
  , SpliceOrBracket(..)
  , ThLevel(..)
  , PendingStuff(..)
  , ThLevelIndex
  , topLevel
  , topAnnLevel
  , topSpliceLevel
  , thLevelIndex
  , topLevelIndex
  , spliceLevelIndex
  , quoteLevelIndex
  , thLevelIndexFromImportLevel
  ) where

import GHCi.RemoteTypes
import qualified GHC.Boot.TH.Syntax as TH
import GHC.Tc.Types.Evidence
import GHC.Utils.Outputable
import GHC.Tc.Types.TcRef
import GHC.Tc.Types.Constraint
import GHC.Hs.Expr ( PendingTcSplice, PendingRnSplice )
import GHC.Types.ThLevelIndex

---------------------------
-- Template Haskell stages and levels
---------------------------

data SpliceType = Typed | Untyped
data SpliceOrBracket = IsSplice | IsBracket

data ThLevel    -- See Note [Template Haskell state diagram]
                -- and Note [Template Haskell levels] in GHC.Tc.Gen.Splice
    -- Start at:   Comp
    -- At bracket: wrap current stage in Brack
    -- At splice:  wrap current stage in Splice
  = Splice SpliceType ThLevel -- Inside a splice

  | RunSplice (TcRef [ForeignRef (TH.Q ())])
      -- Set when running a splice, i.e. NOT when renaming or typechecking the
      -- Haskell code for the splice. See Note [RunSplice ThLevel].
      --
      -- Contains a list of mod finalizers collected while executing the splice.
      --
      -- 'addModFinalizer' inserts finalizers here, and from here they are taken
      -- to construct an @HsSpliced@ annotation for untyped splices. See Note
      -- [Delaying modFinalizers in untyped splices] in GHC.Rename.Splice.
      --
      -- For typed splices, the typechecker takes finalizers from here and
      -- inserts them in the list of finalizers in the global environment.
      --
      -- See Note [Collecting modFinalizers in typed splices] in "GHC.Tc.Gen.Splice".

  | Comp        -- Ordinary Haskell code
                -- Binding level = 0

  | Brack                       -- Inside brackets
      ThLevel                   --   Enclosing level
      PendingStuff

data PendingStuff
  = RnPendingUntyped              -- Renaming the inside of an *untyped* bracket
      (TcRef [PendingRnSplice])   -- Pending splices in here

  | RnPendingTyped                -- Renaming the inside of a *typed* bracket

  | TcPending                     -- Typechecking the inside of a typed bracket
      (TcRef [PendingTcSplice])   --   Accumulate pending splices here
      (TcRef WantedConstraints)   --     and type constraints here
      QuoteWrapper                -- A type variable and evidence variable
                                  -- for the overall monad of
                                  -- the bracket. Splices are checked
                                  -- against this monad. The evidence
                                  -- variable is used for desugaring
                                  -- `lift`.


topLevel, topAnnLevel, topSpliceLevel :: ThLevel
topLevel       = Comp
topAnnLevel    = Splice Untyped Comp
topSpliceLevel = Splice Untyped Comp

instance Outputable ThLevel where
   ppr (Splice _ s)  = text "Splice" <> parens (ppr s)
   ppr (RunSplice _) = text "RunSplice"
   ppr Comp          = text "Comp"
   ppr (Brack s _)   = text "Brack" <> parens (ppr s)


thLevelIndex :: ThLevel -> ThLevelIndex
thLevelIndex (Splice _ s)  = decThLevelIndex (thLevelIndex s)
thLevelIndex Comp          = topLevelIndex
thLevelIndex (Brack s _)   = incThLevelIndex (thLevelIndex s)
thLevelIndex (RunSplice _) = thLevelIndex (Splice Untyped Comp) -- previously: panic "thLevel: called when running a splice"
                        -- See Note [RunSplice ThLevel].


{- Note [RunSplice ThLevel]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The 'RunSplice' level is set when executing a splice, and only when running a
splice. In particular it is not set when the splice is renamed or typechecked.

However, this is not true. `reifyInstances` for example does rename the given type,
and these types may contain variables (#9262 allow free variables in reifyInstances).
Therefore here we assume that thLevel (RunSplice _) = 0
Proper fix would probably require renaming argument `reifyInstances` separately prior
to evaluation of the overall splice.

'RunSplice' is needed to provide a reference where 'addModFinalizer' can insert
the finalizer (see Note [Delaying modFinalizers in untyped splices]), and
'addModFinalizer' runs when doing Q things. Therefore, It doesn't make sense to
set 'RunSplice' when renaming or typechecking the splice, where 'Splice',
'Brack' or 'Comp' are used instead.

-}

