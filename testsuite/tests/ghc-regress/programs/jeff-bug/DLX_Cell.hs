module DLX_Cell where

import Cell
import Register
import Words

data Value a = NotKnown 
             | Inv
             | Val a
	deriving (Eq,Show)

data DLX_Cell r w = Reg r (Value w) 
                  | Loc w
                  | Imm w
	deriving (Eq,Show)

instance Cell DLX_Cell where
  pcNothing = Reg pc NotKnown
  loc = Loc

  getReg (Reg r _)		= r

  getVal (Reg _ (Val val))		= val
  getVal (Imm val)			= val 
  getVal cell				= error ("No data for getData: " ++
						 show cell)

  putVal cell Nothing                 = invalidate     cell
  putVal reg@(Reg r x) (Just v) 
           | readOnly r = reg
           | otherwise = Reg r (Val v)
                            
  putVal valCell@(Imm _ ) _   
    = error ("Can't put data into a value cell: " ++ show valCell)


  invalidate     reg@(Reg r _ ) 
    | readOnly r = reg
    | otherwise     = Reg r Inv
  invalidate     imm@(Imm _ )           = imm


  isReg (Reg _ _ )	= True
  isReg _			= False

  isPC (Reg x _)	= ispc x
  isPC _			= False

  isSpecPC (Reg x  _)= isspecpc x
  isSpecPC _		= False

  isLoc (Loc _)	= True
  isLoc _			= False

  isVal (Imm _ )	= True
  isVal _			= False

  isInv (Reg _ Inv)	= True
  isInv _ 		        = False

  isAss (Reg _ (Val _ ))	= True
  isAss (Imm _ )		= True
  isAss _				= False

  isComputed (Reg _ NotKnown) = False
  isComputed _                     = True


-- Do the two cells name the same Loc (Reg or PC?)
  sameLoc (Reg reg1 _ ) (Reg reg2 _ ) = reg1 == reg2
  sameLoc _ _		= False


  cellHazard (Reg precReg pRegVal ) (Reg followReg fRegVal )
    | readOnly precReg		= False
    | precReg == followReg	= pRegVal /= Inv && fRegVal /= Inv
    | True			= False
  cellHazard _ _ 			= False








