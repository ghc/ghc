module Test24753 where

type ErrorChoiceApi
     = "path0" :> Get '[JSON] Int                                     -- c0
  :<|> "path4" :> (ReqBody '[PlainText] Int :> Post '[PlainText] Int  -- c4
             :<|>  ReqBody '[PlainText] Int :> Post '[JSON] Int)      -- c5
  :<|> "path5" :> (ReqBody '[JSON] Int      :> Post '[PlainText] Int  -- c6
             :<|>  ReqBody '[PlainText] Int :> Post '[PlainText] Int) -- c7
