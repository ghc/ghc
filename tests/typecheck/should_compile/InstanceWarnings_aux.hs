module InstanceWarnings_aux where

data T a = T1 a | T2

instance {-# WARNING "Don't use" #-} Show (T Int) where
    show _ = "T Int"

instance {-# DEPRECATED "Don't use either" #-} Show (T Bool) where
    show _ = "T Bool"

instance {-# WARNING ["Don't", "use", "multiline"] #-} Show (T Char) where
    show _ = "T Char"

deriving instance {-# WARNING "Deprecated deriving" #-} Eq a => Eq (T a)
