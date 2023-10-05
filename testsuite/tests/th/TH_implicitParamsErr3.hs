{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TemplateHaskell #-}
import Language.Haskell.TH

main = print $(letE [implicitParamBindD "invalid name" [e| "hi" |]]
                    (implicitParamVarE "invalid name"))
