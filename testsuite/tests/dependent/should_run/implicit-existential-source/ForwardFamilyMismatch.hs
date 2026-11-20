module ForwardFamilyMismatch where

import ForwardProducer
import ForwardSchemas

bad :: SlotBox Bool -> SlotBox Int
bad x = choose True x (failAny "unused")
