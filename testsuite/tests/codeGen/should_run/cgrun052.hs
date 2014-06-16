-- !!! Caused a crash in GHC 5.04.2, fixed in CoreToStg.lhs rev. 1.98

data T1 = T1
data T2 = C1 !T1 | C2
data T3 = C3 !T2 Int

{-# NOINLINE f #-}
f 0 = C3 (C1 T1) 42
f n = C3 (C1 T1) n

main = case f 23 of
	  C3 y z -> case y of
			C1 T1 -> putStrLn "ok"
