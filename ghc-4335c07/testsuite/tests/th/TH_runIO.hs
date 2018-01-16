{-# LANGUAGE TemplateHaskell #-}

-- See Trac #1678

module TH where 

import Language.Haskell.TH 
 
 
-- foo = $(fail "hi")

foo = $(runIO (fail "hi"))
