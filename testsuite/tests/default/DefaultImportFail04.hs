-- | Import a module that re-exports a whole other module that exports a default.
--   The module re-export should *not* re-export the default.

import ReExportShowSumModule

main = print mempty
