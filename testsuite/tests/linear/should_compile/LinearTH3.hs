{-# LANGUAGE TemplateHaskell, LinearTypes #-}
module LinearTH3 where  -- #18736

import Language.Haskell.TH

idenq :: Quote m => Code m (a %1 -> a)
idenq = [|| \x -> x ||]
