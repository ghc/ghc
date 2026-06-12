module RexLib where

-- Home unit reexport-0 re-exports MyPlugin (from plugin-0) via the
-- -reexported-module flag; see the 'reexportunit' arguments. This module exists
-- only to make reexport-0 a non-empty home unit, mirroring a real library
-- package that reexports a module from one of its dependencies.
rexLib :: ()
rexLib = ()
