module ForwardWrongMode where

import Filter
import ForwardProducer
import ForwardSchemas

bad :: Hidden a -> HiddenBar a
bad p = choose True p (failAny "unselected")
