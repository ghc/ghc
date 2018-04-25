import Control.Monad.ST
import Data.STRef

-- Make sure that the realWord# token used by runST (actually by runSTRep) is
-- not inlined, which would share the call to `newMutVar#`, and suddenly there
-- would be only one MutVar

main =
    let f () = runST $ do
        ref <- newSTRef 0
        modifySTRef ref (+1)
        readSTRef ref
    in print (f () + f ())
