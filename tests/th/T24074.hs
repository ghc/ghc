{-# LANGUAGE TemplateHaskell #-}
module T24074 where

class A a

$( [d| instance {-# OVERLAPS #-} A Int |] )
$( [d| instance {-# OVERLAPPABLE #-} A Bool |] )
$( [d| instance {-# OVERLAPPING #-} A () |] )
$( [d| instance {-# INCOHERENT #-} A Char |] )
