{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax #-}
module Dollar where
{-
Check $ interacting with multiplicity polymorphism.
This caused Core Lint error previously.
-}

import GHC.Base

data Q a = Q a

data QU = QU ()

test = QU $ ()

qux = Q $ ()
