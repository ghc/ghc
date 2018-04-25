{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ClassEqContext  where 

class (Show a,a ~ b) => C a b
