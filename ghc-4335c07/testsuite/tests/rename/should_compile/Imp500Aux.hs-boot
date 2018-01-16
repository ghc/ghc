{-# LANGUAGE RoleAnnotations #-}
module Imp500Aux where
data T1 a
data T2 a b
data T3 a b c
data T4 a b c d
data T5 a b c d e 
data T6 a 
data T7 a b 
data T8 a b c 
data T9 a b c d 
data T10 a b c d e 

type role T1 phantom
type role T2 phantom phantom
type role T3 phantom phantom phantom
type role T4 phantom phantom phantom phantom
type role T5 phantom phantom phantom phantom phantom
type role T6 phantom
type role T7 phantom phantom
type role T8 phantom phantom phantom
type role T9 phantom phantom phantom phantom
type role T10 phantom phantom phantom phantom phantom
