module LogFun where

type TT1 = (Int,Int)
type TT2 = (Int,Int,Int,Int)

tt_con10, tt_id, tt_inv, tt_con11 :: TT1

tt_con10 = (0,0)
tt_id    = (0,1)
tt_inv   = (1,0)
tt_con11 = (1,1)

tt_con20, tt_and2, tt_nimp,  tt_id21   :: TT2
tt_nimp', tt_id22, tt_xor,   tt_or2    :: TT2
tt_nor2,  tt_equ2, tt_inv22, tt_imp'   :: TT2
tt_inv21, tt_imp,  tt_nand2, tt_con21  :: TT2

tt_con20 = (0,0,0,0)
tt_and2  = (0,0,0,1)
tt_nimp  = (0,0,1,0)
tt_id21  = (0,0,1,1)
tt_nimp' = (0,1,0,0)
tt_id22  = (0,1,0,1)
tt_xor   = (0,1,1,0)
tt_or2   = (0,1,1,1)
tt_nor2  = (1,0,0,0)
tt_equ2  = (1,0,0,1)
tt_inv22 = (1,0,1,0)
tt_imp'  = (1,0,1,1)
tt_inv21 = (1,1,0,0)
tt_imp   = (1,1,0,1)
tt_nand2 = (1,1,1,0)
tt_con21 = (1,1,1,1)

