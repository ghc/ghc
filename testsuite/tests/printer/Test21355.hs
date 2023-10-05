{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
module Test21355 where

emptyBK = [d| {} |]

data GitHubSignedReqBody''
  (proxy :: KProxy k)
  (key :: k)
  (list :: [Type])
  (result :: Type) where
