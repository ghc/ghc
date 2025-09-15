{-# LANGUAGE DisambiguateRecordFields #-}
module Main where
import T15149B
import T15149C
main = do print (AnDouble{an=1}, AnInt{an=1})
