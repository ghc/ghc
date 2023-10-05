-- Test that we can re-export a module defining
-- duplicate record fields, without ourselves enabling
-- the DuplicateRecordFields extension.

module NoDRFModuleExport ( module NoDRFModuleExport_aux ) where
  import NoDRFModuleExport_aux
