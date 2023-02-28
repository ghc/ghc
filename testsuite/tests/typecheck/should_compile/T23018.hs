module T23018 where

import qualified Control.DeepSeq as DeepSeq

class XX f where
   rnf :: DeepSeq.NFData a => f a -> ()

instance XX Maybe where
   rnf = DeepSeq.rnf
