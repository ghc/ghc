{-# LANGUAGE ConstraintKinds, MultiParamTypeClasses, UndecidableInstances #-}
module TcFail where

class cls (A cls) => A cls c where
