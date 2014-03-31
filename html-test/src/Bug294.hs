{-# LANGUAGE TypeFamilies, FlexibleInstances, GADTs #-}
-- This tests that we are able to extract record selectors for
-- associated types when the type itself is not exported. Making this
-- bug exhibit is very simple: simply mention a record field defined
-- inside of the associated type anywhere in the export list.
--
-- Note: ProblemCtor only shows up when T or A are exported but PolyCtor
-- only shows up when the class is exported as well, since it's polymorphic.
module Bug294 ( A, problemField, problemField', gadtField
              , TP(ProblemCtor), DP(ProblemCtor'), TO'(PolyCtor)) where

data A

class T t where
  data TO t :: *
  data TP t :: *

  t :: t

instance T A where
  data TO A = TA { problemField :: A }
  data TP A = ProblemCtor A

data family DO t :: *
data family DP t :: *

data instance DO A = DA { problemField' :: A }
data instance DP A = ProblemCtor' A

data GADT :: * -> * where
  Ctor :: { gadtField :: A } -> GADT A

class T' t where
  data TO' t :: *

instance T' a where
  data TO' a = PolyCtor
