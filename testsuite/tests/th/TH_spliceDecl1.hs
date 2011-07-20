-- test splicing of a generated data declarations

module TH_spliceDecl1
where

import Language.Haskell.TH


-- splice a simple data declaration
$(return [DataD [] (mkName "T") [] [NormalC (mkName "C") []] []])
