{-# LANGUAGE RequiredTypeArguments, LinearTypes #-}
module T24159_pat_parse_error_2 where

f (a %d -> b) = ()
