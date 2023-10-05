{-# LANGUAGE DuplicateRecordFields #-}
module NoDRFModuleExport_aux where
  data A = MkA { foo :: A }
  data B = MkB { foo :: B }
