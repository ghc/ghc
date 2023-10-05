module T21575b_aux ( Given(..), give, unsafeGive ) where

import GHC.Exts
  ( withDict )
import Unsafe.Coerce
  ( unsafeCoerce )

class Given a where
  given :: a

give, unsafeGive :: forall a r. a -> (Given a => r) -> r
give = withDict @(Given a) @a

unsafeGive a k = unsafeCoerce (Gift k :: Gift a r) a

newtype Gift a r = Gift (Given a => r)
