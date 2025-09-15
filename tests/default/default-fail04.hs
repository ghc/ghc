{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedDefaults #-}

class C a b

instance C Int String

default C (Int)
