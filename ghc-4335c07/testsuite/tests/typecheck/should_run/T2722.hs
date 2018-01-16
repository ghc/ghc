module Main (main, arid1, arid2) where

import Prelude hiding (id, (.))
import qualified Prelude
import Control.Category
import Control.Arrow
import System.IO
import Debug.Trace


main = do { xs <- runXIOState arid1        -- Works with arid2
          ; print xs }

runXIOState :: IOSLA () c -> IO [c]
runXIOState f = runIOSLA f ()

newtype IOSLA a b = IOSLA { runIOSLA :: a -> IO [b] }

instance Arrow IOSLA where
    arr f = IOSLA $ \ x -> return [f x]

instance Category IOSLA where
    id = arr id

-- arr :: Arrow m => (b->c) -> m b c
-- id  :: Category m => m b b
-- (arr id) :: Arrow m => m a a

arid1 :: Arrow m => m a a
arid1 = arr id

arid2 :: Arrow m => m a a
arid2 = arr Prelude.id

