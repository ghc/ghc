{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module T12866 where

type Test2 a = (Eq (TestM a))

class Test a where
    type TestM :: *
