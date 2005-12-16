-- Copyright (c) 2000 Galois Connections, Inc.
-- All rights reserved.  This software is distributed as
-- free software under the license in the file "LICENSE",
-- which is included in the distribution.

module Misc where

import Debug.Trace

debug s v = trace (s ++" : "++ show v ++ "\n") v
-- debug s v = v
