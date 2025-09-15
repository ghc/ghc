{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
module TypedSplice where

import Language.Haskell.TH

metaExp :: Code Q (Bool -> Bool)
metaExp = [|| not :: _ -> _b ||]
