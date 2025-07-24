{-# LANGUAGE ExplicitLevelImports #-}
-- Module export only exports level 0 things (b)
module ModuleExportA (module ModuleExportB) where

-- Everything at level 1
import quote ModuleExportB
-- Only b at level 0
import ModuleExportB (b)
