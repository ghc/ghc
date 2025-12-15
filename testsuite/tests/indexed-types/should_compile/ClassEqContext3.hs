{-# LANGUAGE TypeFamilies #-}

module ClassEqContext  where 

class a ~ b => C a b

instance C Char Char
