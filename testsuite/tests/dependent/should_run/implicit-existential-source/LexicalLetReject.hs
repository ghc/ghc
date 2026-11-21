{-# LANGUAGE NoExperimentalDependentTypes #-}
module LexicalLetReject where

import LexicalLetProvider

escapeField :: HiddenPayload -> a
escapeField package =
  let (value, _) = package
  in value

joinFields :: HiddenPayload -> HiddenPayload -> Int
joinFields left right =
  let (value, _) = left
  in let (_, reader) = right
     in reader value
