{-# LANGUAGE DuplicateRecordFields #-}

module T14487A where

data X = X {
    duplicateName :: Int
}
