module GHC.Unit.External.ModuleOrigin (
  ModuleOrigin(..),
  fromExposedModules,
  fromReexportedModules,
  fromFlag,
  originVisible,
  originEmpty,
) where

import Data.Semigroup qualified as Semigroup
import GHC.Prelude
import GHC.Unit.External.Validate
import GHC.Unit.Info
import GHC.Utils.Outputable
import GHC.Utils.Panic

-- | Given a module name, there may be multiple ways it came into scope,
-- possibly simultaneously.  This data type tracks all the possible ways
-- it could have come into scope.  Warning: don't use the record functions,
-- they're partial!
data ModuleOrigin =
    -- | Module is hidden, and thus never will be available for import.
    -- (But maybe the user didn't realize), so we'll still keep track
    -- of these modules.)
    ModHidden

    -- | Module is unavailable because the unit is unusable.
  | ModUnusable !UnusableUnit

    -- | Module is public, and could have come from some places.
  | ModOrigin {
        -- | @Just False@ means that this module is in
        -- someone's @exported-modules@ list, but that package is hidden;
        -- @Just True@ means that it is available; @Nothing@ means neither
        -- applies.
        fromOrigUnit :: Maybe Bool
        -- | Is the module available from a reexport of an exposed package?
        -- There could be multiple.
      , fromExposedReexport :: [UnitInfo]
        -- | Is the module available from a reexport of a hidden package?
      , fromHiddenReexport :: [UnitInfo]
        -- | Did the module export come from a package flag? (ToDo: track
        -- more information.
      , fromPackageFlag :: Bool
      }

instance Outputable ModuleOrigin where
    ppr ModHidden = text "hidden module"
    ppr (ModUnusable _) = text "unusable module"
    ppr (ModOrigin e res rhs f) = sep (punctuate comma (
        (case e of
            Nothing -> []
            Just False -> [text "hidden package"]
            Just True -> [text "exposed package"]) ++
        (if null res
            then []
            else [text "reexport by" <+>
                    sep (map (ppr . mkUnit) res)]) ++
        (if null rhs
            then []
            else [text "hidden reexport by" <+>
                    sep (map (ppr . mkUnit) rhs)]) ++
        (if f then [text "package flag"] else [])
        ))

-- | Smart constructor for a module which is in @exposed-modules@.  Takes
-- as an argument whether or not the defining package is exposed.
fromExposedModules :: Bool -> ModuleOrigin
fromExposedModules e = ModOrigin (Just e) [] [] False

-- | Smart constructor for a module which is in @reexported-modules@.  Takes
-- as an argument whether or not the reexporting package is exposed, and
-- also its 'UnitInfo'.
fromReexportedModules :: Bool -> UnitInfo -> ModuleOrigin
fromReexportedModules True pkg = ModOrigin Nothing [pkg] [] False
fromReexportedModules False pkg = ModOrigin Nothing [] [pkg] False

-- | Smart constructor for a module which was bound by a package flag.
fromFlag :: ModuleOrigin
fromFlag = ModOrigin Nothing [] [] True

instance Semigroup ModuleOrigin where
    x@(ModOrigin e res rhs f) <> y@(ModOrigin e' res' rhs' f') =
        ModOrigin (g e e') (res ++ res') (rhs ++ rhs') (f || f')
      where g (Just b) (Just b')
                | b == b'   = Just b
                | otherwise = pprPanic "ModOrigin: package both exposed/hidden" $
                    text "x: " <> ppr x $$ text "y: " <> ppr y
            g Nothing x = x
            g x Nothing = x

    x <> y = pprPanic "ModOrigin: module origin mismatch" $
                 text "x: " <> ppr x $$ text "y: " <> ppr y

instance Monoid ModuleOrigin where
    mempty = ModOrigin Nothing [] [] False
    mappend = (Semigroup.<>)

-- | Is the name from the import actually visible? (i.e. does it cause
-- ambiguity, or is it only relevant when we're making suggestions?)
originVisible :: ModuleOrigin -> Bool
originVisible ModHidden = False
originVisible (ModUnusable _) = False
originVisible (ModOrigin b res _ f) = b == Just True || not (null res) || f

-- | Are there actually no providers for this module?  This will never occur
-- except when we're filtering based on package imports.
originEmpty :: ModuleOrigin -> Bool
originEmpty (ModOrigin Nothing [] [] False) = True
originEmpty _ = False
