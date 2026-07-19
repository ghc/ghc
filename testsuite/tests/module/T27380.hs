{-# LANGUAGE NoImportQualifiedPost #-}
{-# OPTIONS_GHC -Wprepositive-qualified-module #-}
-- Negative control for #27380: with the extension explicitly disabled,
-- the ImportQualifiedPost suggestion must still appear.
import qualified System.IO
main :: IO ()
main = System.IO.print "hi"
