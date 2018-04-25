{-# LANGUAGE DeriveGeneric #-}
module Distribution.Types.ComponentRequestedSpec (
    -- $buildable_vs_enabled_components

    ComponentRequestedSpec(..),
    ComponentDisabledReason(..),

    defaultComponentRequestedSpec,
    componentNameRequested,

    componentEnabled,
    componentDisabledReason,
) where

import Prelude ()
import Distribution.Compat.Prelude
import Distribution.Text

import Distribution.Types.Component -- TODO: maybe remove me?
import Distribution.Types.ComponentName

-- $buildable_vs_enabled_components
-- #buildable_vs_enabled_components#
--
-- = Note: Buildable versus requested versus enabled components
-- What's the difference between a buildable component (ala
-- 'componentBuildable'), a requested component
-- (ala 'componentNameRequested'), and an enabled component (ala
-- 'componentEnabled')?
--
-- A component is __buildable__ if, after resolving flags and
-- conditionals, there is no @buildable: False@ property in it.
-- This is a /static/ property that arises from the
-- Cabal file and the package description flattening; once we have
-- a 'PackageDescription' buildability is known.
--
-- A component is __requested__ if a user specified, via a
-- the flags and arguments passed to configure, that it should be
-- built.  E.g., @--enable-tests@ or @--enable-benchmarks@ request
-- all tests and benchmarks, if they are provided.  What is requested
-- can be read off directly from 'ComponentRequestedSpec'.  A requested
-- component is not always buildable; e.g., a user may @--enable-tests@
-- but one of the test suites may have @buildable: False@.
--
-- A component is __enabled__ if it is BOTH buildable
-- and requested.  Once we have a 'LocalBuildInfo', whether or not a
-- component is enabled is known.
--
-- Generally speaking, most Cabal API code cares if a component
-- is enabled. (For example, if you want to run a preprocessor on each
-- component prior to building them, you want to run this on each
-- /enabled/ component.)
--
-- Note that post-configuration, you will generally not see a
-- non-buildable 'Component'.  This is because 'flattenPD' will drop
-- any such components from 'PackageDescription'.  See #3858 for
-- an example where this causes problems.

-- | Describes what components are enabled by user-interaction.
-- See also this note in
-- "Distribution.Types.ComponentRequestedSpec#buildable_vs_enabled_components".
--
-- @since 2.0.0.2
data ComponentRequestedSpec
    = ComponentRequestedSpec { testsRequested      :: Bool
                             , benchmarksRequested :: Bool }
    | OneComponentRequestedSpec ComponentName
  deriving (Generic, Read, Show, Eq)
instance Binary ComponentRequestedSpec

-- | The default set of enabled components.  Historically tests and
-- benchmarks are NOT enabled by default.
--
-- @since 2.0.0.2
defaultComponentRequestedSpec :: ComponentRequestedSpec
defaultComponentRequestedSpec = ComponentRequestedSpec False False

-- | Is this component enabled?  See also this note in
-- "Distribution.Types.ComponentRequestedSpec#buildable_vs_enabled_components".
--
-- @since 2.0.0.2
componentEnabled :: ComponentRequestedSpec -> Component -> Bool
componentEnabled enabled = isNothing . componentDisabledReason enabled

-- | Is this component name enabled?  See also this note in
-- "Distribution.Types.ComponentRequestedSpec#buildable_vs_enabled_components".
--
-- @since 2.0.0.2
componentNameRequested :: ComponentRequestedSpec -> ComponentName -> Bool
componentNameRequested enabled = isNothing . componentNameNotRequestedReason enabled

-- | Is this component disabled, and if so, why?
--
-- @since 2.0.0.2
componentDisabledReason :: ComponentRequestedSpec -> Component
                        -> Maybe ComponentDisabledReason
componentDisabledReason enabled comp
    | not (componentBuildable comp) = Just DisabledComponent
    | otherwise = componentNameNotRequestedReason enabled (componentName comp)

-- | Is this component name disabled, and if so, why?
--
-- @since 2.0.0.2
componentNameNotRequestedReason :: ComponentRequestedSpec -> ComponentName
                            -> Maybe ComponentDisabledReason
componentNameNotRequestedReason
    ComponentRequestedSpec{ testsRequested      = False } (CTestName _)
    = Just DisabledAllTests
componentNameNotRequestedReason
    ComponentRequestedSpec{ benchmarksRequested = False } (CBenchName _)
    = Just DisabledAllBenchmarks
componentNameNotRequestedReason ComponentRequestedSpec{} _ = Nothing
componentNameNotRequestedReason (OneComponentRequestedSpec cname) c
    | c == cname = Nothing
    | otherwise = Just (DisabledAllButOne (display cname))

-- | A reason explaining why a component is disabled.
--
-- @since 2.0.0.2
data ComponentDisabledReason = DisabledComponent
                             | DisabledAllTests
                             | DisabledAllBenchmarks
                             | DisabledAllButOne String
