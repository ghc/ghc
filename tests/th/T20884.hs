{-# LANGUAGE TemplateHaskell #-}

module T20884 where

import Language.Haskell.TH

list1 = $( conE '(:) `appE` litE (IntegerL 5) `appE` conE '[] )
  -- OK

list2 = $( conE ''(:) `appE` litE (IntegerL 5) `appE` conE '[] )
  -- should fail because we are trying to quote a type named (:),
  -- but (:) is not in the type namespace.
