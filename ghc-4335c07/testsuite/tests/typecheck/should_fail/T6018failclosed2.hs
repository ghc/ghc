{-# LANGUAGE TypeFamilyDependencies  #-}

module T6018failclosed2 where

-- this one is a strange beast. Last equation is unreachable and thus it is
-- removed. It is then impossible to typecheck barapp and thus we generate an
-- error
type family Bar a = r | r -> a where
    Bar Int  = Bool
    Bar Bool = Int
    Bar Bool = Char

bar :: Bar a -> Bar a
bar x = x

barapp :: Char
barapp = bar 'c'
