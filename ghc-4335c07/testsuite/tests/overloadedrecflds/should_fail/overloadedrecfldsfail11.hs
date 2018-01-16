{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Werror #-}
import OverloadedRecFldsFail11_A

main = print (foo (MkS True :: S))
