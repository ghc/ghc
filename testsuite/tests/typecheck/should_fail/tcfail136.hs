-- Kind error message test

module ShouldFail where

type IntMap a = [a]
 
data SymDict a = SymDict {idcounter:: Int, itot::IntMap a}

data SymTable = SymTable {  dict::SymDict   }
