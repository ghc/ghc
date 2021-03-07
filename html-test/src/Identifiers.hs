{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TypeOperators #-}
module Identifiers where

import Data.List (elem, (++))

data Id = Id
data a :* b = a :* b

{-|

  * Unadorned:

        - Unqualified: '++', 'elem'
        - Qualified: 'Data.List.++', 'Data.Lis.elem'
        - Namespaced: v'++', t'++', v'elem', t'elem', v'Id', t'Id', v':*', t':*'

  * Parenthesized:

        - Unqualified: @'(++)' [1,2,3] [4,5,6]@
        - Qualified: @'(Data.List.++)' [1,2,3] [4,5,6]@
        - Namespaced: v'(++)', t'++', v'(:*)', t'(:*)'

  * Backticked:

        - Unqualified: @1 '`elem`' [-3..3]@
        - Qualified: @1 '`Data.List.elem`' [-3..3]@
        - Namespaced: v'`elem`', t'`elem`', v'`Id`', t'`Id`'

  * Edge cases:

        - Tuples: '()', '(,,,)'

-}
foo :: ()
foo = ()
