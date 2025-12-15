{-# LANGUAGE TypeFamilies #-}

module ClassEqContext  where 

class (Show a,a ~ b) => C a b
