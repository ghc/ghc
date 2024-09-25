-- | Import a module with an unexported @default Monoid@ declaration

import NonExportMonoidSum ()

main = print mempty
