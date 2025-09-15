-- | Import a module that implicitly exports a @default Monoid@

import ExportImplicitMonoidProduct ()
import ReExportShowSumModule ()

main = print mempty
