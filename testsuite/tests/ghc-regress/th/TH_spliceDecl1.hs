-- test splicing of a generated data declarations

module TH_spliceDecl1
where

import Language.Haskell.THSyntax


-- splice a simple data declaration
$(return [DataDec [] "T" [] [NormalCon "C" []] []])
