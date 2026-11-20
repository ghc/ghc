module ForwardAmbiguous where

import NestedSignature
import ForwardProducer

bad :: PackedPair a
bad = ambiguous () (failAny "ordinary class argument")
