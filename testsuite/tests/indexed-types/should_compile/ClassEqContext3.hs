{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ClassEqContext  where 

class a ~ b => C a b

instance C Char Char
