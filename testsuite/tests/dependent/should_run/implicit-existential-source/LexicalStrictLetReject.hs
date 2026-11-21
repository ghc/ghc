{-# LANGUAGE BangPatterns, NoExperimentalDependentTypes, Strict #-}
module LexicalStrictLetReject where

import LexicalLetProvider

escapeTilde :: HiddenPayload -> a
escapeTilde package = let ~(value, _) = package in value

escapeBang :: HiddenPayload -> a
escapeBang package = let !(value, _) = package in value

escapeStrict :: HiddenPayload -> a
escapeStrict package = let (value, _) = package in value
