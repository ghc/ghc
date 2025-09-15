{-# LANGUAGE TemplateHaskellQuotes, LinearTypes #-}

module LinearTHFail where -- #18465

import Language.Haskell.TH

f :: Q Exp %1 -> Q Exp
f x = [| Just $x |]

g :: Code Q a %1 -> Code Q (Maybe a)
g x = [|| Just $$x ||]
