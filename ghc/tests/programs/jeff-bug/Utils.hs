module Utils where

import Hawk
import Trans
import DLX
import Maybe
import LazyST

--(f,g) >< (x,y) = (f x, g y)



unique x = x
{-
unique (Sig xs) = runST (
  do { x <- newSTRef 0
     ; l <- mapM (anno x) xs
     ; return $ Sig l
     }
  )
  where anno x xs = mapM (anno' x) xs 
        anno' x y = do { v <- readSTRef x 
                       ; writeSTRef x (v+1)
                       ; return $ addInfo (Loc (Word v)) y
                       }
-}

convert (Loc y) = Loc y
convert (Imm y)    = Imm y
convert (Reg r x) = Reg (Real r) x

convertBack (Loc y) = Loc y
convertBack (Imm y)    = Imm y
convertBack (Reg (Real r) x) = Reg r x
convertBack (Reg (Virtual _ (Just r)) x) = Reg r x
convertBack t@(Reg (Virtual r Nothing) x) = error "ConvertBack"

removeVirtuals t = Trans dest op src info
      where src = map convertBack $ getSrc t
            dest = map convertBack $ getDst t
            info = map convertBack $ getInfo t
            op = getOp t

