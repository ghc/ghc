{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module LinearTHFail2 where

$([d| data T where { MkT :: Int -> T } |])
