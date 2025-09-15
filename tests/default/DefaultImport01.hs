-- | Import a @default Monoid (Sum Integer)@ declaration

import ExportMonoidSum ()

main = do print mempty
          print 42     -- the built-in @default Num (Integer, Double)@ stays in effect
