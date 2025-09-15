module Test25467 where

fff = do
     let {
         ; (a, b) = foo
         }
     pure ()

foo = do
  let ;x =1

bar1 = do
    let {
        ; labels1     = getFieldLabels
        ; argexprA    = vhdlNameToVHDLExpr
        }

bar2 = do
    let {
        ; labels2      = getFieldLabels
        }
