module IFU where


import LazyST
import Ix

import Trans
import Hawk
import DLX

import qualified PreludeSig as Signaled

import Word

#include "hawk-macros.h"


ifu (a,b) c d = (unique',id) >< fetch (4,dlx2trans,a,b) c d

unique' ts = runST (
  do { v <- newSTRef 1
     ; step1(ts) { mapM (\(Trans x y z i)  -> do { v' <- readSTRef v
                                  ; writeSTRef v (v'+1)
                                  ; return $ Trans x y z (loc v':i)
                                  }) ts
                 }
     }
  )
