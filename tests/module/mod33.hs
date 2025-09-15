-- !!! Redefining an imported name
-- was: Imported var clashes with local var definition
module M where
import Prelude(id)
id x = x
