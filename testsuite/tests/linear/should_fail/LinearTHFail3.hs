{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module LinearTHFail3 where

$([d| data T where { MkT :: Int %m -> T } |])
