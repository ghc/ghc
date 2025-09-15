{-# LANGUAGE PartialTypeSignatures #-}

module T25950 where

fails :: _ => a
fails = id $ ()
