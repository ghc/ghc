{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
module TypedSplice where

import Language.Haskell.TH

metaExp :: Q (TExp (Bool -> Bool))
metaExp = [|| not :: _ -> _b ||]
