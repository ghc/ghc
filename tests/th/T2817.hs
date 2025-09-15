{-# OPTIONS -XTemplateHaskell #-} 
module TH( x ) where 
import Language.Haskell.TH

data T f = MkT (f Int)

x = $(return (SigE (VarE 'x) (AppT (ConT ''T) (AppT ArrowT (ConT ''Int)))))
 



