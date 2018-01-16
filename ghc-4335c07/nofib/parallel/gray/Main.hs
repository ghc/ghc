-- Copyright (c) 2000 Galois Connections, Inc.
-- All rights reserved.  This software is distributed as
-- free software under the license in the file "LICENSE",
-- which is included in the distribution.

-- Modified to read sample input directly from a file.

module Main where

import System.IO

import Parse
import Eval

main = do { str <- readFile "galois.gml"
          ; hSetBinaryMode stdout True
          ; mainEval (rayParse str)
          }
