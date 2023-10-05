{-# LANGUAGE TemplateHaskell #-}

-- Test error message when the code in a splice
-- fails in a lazy fashion (e.g. a (head [])
-- thunk is embedded in the returned structure).

module TH where

$( do { ds <- [d| |] 
      ; return (tail ds) }
 )
