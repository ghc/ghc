
{-# LANGUAGE RecordWildCards #-}

-- trac #1491

module Foo where

import Rn065A

f T{..} = a + 1
