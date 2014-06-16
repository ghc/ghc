{-# LANGUAGE PolyKinds, GADTs, KindSignatures, DataKinds, FlexibleInstances #-}

module T7438 where
import T7438a

go Nil acc = acc
