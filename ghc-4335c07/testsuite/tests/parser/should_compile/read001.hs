-- !!! import qualified Prelude should leave (), [] etc in scope

module ShouldCompile where

import qualified Prelude

f :: Prelude.IO ()
f = Prelude.return ()
