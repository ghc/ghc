{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TemplateHaskell #-}
import Language.Haskell.TH

$(fmap (:[]) (implicitParamBindD "x" [e| 1 |]))
