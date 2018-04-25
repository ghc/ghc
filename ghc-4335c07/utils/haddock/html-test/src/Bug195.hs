module Bug195 where

data T = A { someField :: () -- ^ Doc for someField of A
           , someOtherField :: () -- ^ Doc for someOtherField of A
           }
       | B { someField :: () -- ^ Doc for someField of B
           , someOtherField :: () -- ^ Doc for someOtherField of B
           }
       | C { someField :: () -- ^ Doc for someField of C
           , someOtherField :: () -- ^ Doc for someOtherField of C
           }
