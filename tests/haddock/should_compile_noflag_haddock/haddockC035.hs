{-# LANGUAGE GADTs #-}

module Hi where

data Hi where
    Hi :: () -- ^ This is a comment on the '()' field of 'Hi'
       -> Int
       -> String -- ^ This is a comment on the 'String' field of 'Hi'
       -> Hi -- ^ This is a comment on the return type of 'Hi'
