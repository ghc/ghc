{-# LANGUAGE TemplateHaskell #-}

module T1476b where

import Language.Haskell.TH

baz = [| \ $( return $ VarP $ mkName "x" ) -> x |]

-- If this test starts passing, nested pattern splices scope correctly.
-- Good for you! Now, update the TH manual accordingly.
