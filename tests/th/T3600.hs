{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}
module T3600 where
import T3600a
$(test)
