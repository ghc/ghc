module TH_unresolvedInfix2 where

import Language.Haskell.TH

infixl 6 :+

data Tree = N
  | Tree :+ Tree 
  | Tree :* Tree 

$(return [])

-- Should fail
expr = $( let plus = conE '(:+)
              n = conE 'N
          in infixE Nothing plus (Just $ uInfixE n plus n) )
