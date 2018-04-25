-- test 'lookupTypeName' and 'lookupValueName'

import Language.Haskell.TH

import qualified TH_lookupName_Lib
import qualified TH_lookupName_Lib as TheLib

f :: String
f = "TH_lookupName.f"

data D = D

$(return [])

main = mapM_ print [
  -- looking up values
  $(do { Just n <- lookupValueName "f" ; varE n }),
  $(do { Nothing <- lookupTypeName "f";  [| "" |] }),
  -- looking up types
  $(do { Just n <- lookupTypeName "String"; sigE [| "" |] (conT n) }),
  $(do { Nothing <- lookupValueName "String"; [| "" |] }),
  -- namespacing
  $(do { Just n <- lookupValueName "D"; DataConI{} <- reify n; [| "" |] }),
  $(do { Just n <- lookupTypeName "D"; TyConI{} <- reify n; [| "" |] }),
  -- qualified lookup
  $(do { Just n <- lookupValueName "TH_lookupName_Lib.f"; varE n }),
  $(do { Just n <- lookupValueName "TheLib.f"; varE n }),
  -- shadowing
  $(TheLib.lookup_f),
  $( [| let f = "local f" in $(TheLib.lookup_f) |] ),
  $( [| let f = "local f" in $(do { Just n <- lookupValueName "f"; varE n }) |] ),
  $( [| let f = "local f" in $(varE 'f) |] ),
  let f = "local f" in $(TheLib.lookup_f),
  let f = "local f" in $(varE 'f)
 ]
