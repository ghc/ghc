{-# LANGUAGE PatternSynonyms #-}

data MyRec = MyRec { foo :: Int, qux :: String }

pattern HisRec{f1, f2} = MyRec{foo = f1, qux=f2}

updater,updater1, updater2 :: MyRec -> MyRec
updater a = a {f1 = 1 }

updater1 a = a {f1 = 1, qux = "two" }

updater2 a = a {f1 = 1, foo = 2 }
