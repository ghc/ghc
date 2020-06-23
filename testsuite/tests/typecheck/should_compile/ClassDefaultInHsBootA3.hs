{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
module ClassDefaultInHsBootA3 where

import ClassDefaultInHsBootA1
import ClassDefaultInHsBootA2

asdf :: String
asdf = show $ def @I
