-- test for escaping Timeout exceptions, see #7719

import System.Timeout
import Control.Monad
import Control.Concurrent

t d = timeout d $ timeout d $ timeout d $ timeout d $ timeout d $ timeout (10^9) $ threadDelay 100

main = forM_ [1..20] $ \_ -> forM_ [1..40] t

