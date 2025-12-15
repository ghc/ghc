{-# LANGUAGE GADTs, DataKinds #-}

module T7438 where
import T7438a

go Nil acc = acc
