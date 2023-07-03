{-# LANGUAGE DuplicateRecordFields #-}

module T23279 where

import T23279_aux

bar = Bar { x = 3, y = 'x', z = False, w = 17.28 }
baz = Baz { z = 1.1 }

v = w
