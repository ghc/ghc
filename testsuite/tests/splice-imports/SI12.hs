{-# LANGUAGE NoLiftCSP #-}
{-# LANGUAGE TemplateHaskell #-}
module SI11 where

-- Is lift-based CSP banned?
x (t :: String) = [| t |]



