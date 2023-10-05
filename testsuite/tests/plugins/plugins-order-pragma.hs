{-# OPTIONS -fplugin PurePlugin -fplugin-opt ImpurePlugin:First_Option -fplugin-opt PurePlugin:Second_Option -fplugin-opt PurePlugin:Second_Option_2 -fplugin FingerprintPlugin -fplugin-opt FingerprintPlugin:1 #-}

-- Are plugins loaded in the correct order?
module Main where

main = pure ()
