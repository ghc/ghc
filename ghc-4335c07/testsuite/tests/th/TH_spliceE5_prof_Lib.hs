
module TH_spliceE5_prof_Lib where

import Language.Haskell.TH

expandVars :: [String] -> Q Exp
expandVars s = [| concat $(return (ListE (map f s))) |]
  where
    f x = VarE (mkName x)

