
{-# LANGUAGE RecordWildCards #-}

-- trac #1578

module Foo where

import Rn065A

f T{..} = a + 1
