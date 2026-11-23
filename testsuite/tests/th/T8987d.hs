{-# LANGUAGE CPP, TemplateHaskell, TypeApplications #-}

module T8987d where
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.IO.Error
import Control.Exception
import Data.List
import Control.Monad

#define TEST_1  void $ qNewName                         ['1', throw (userError "IOError in qNewName") ]
#define TEST_2  void $ qLookupName            True      ['1', throw (userError "IOError in qLookupName") ]
#define TEST_3  void $ qAddDependentFile                ['1', throw (userError "IOError in qAddDependentFile") ]
#define TEST_4  void $ qAddDependentDirectory           ['1', throw (userError "IOError in qAddDependentDirectory") ]
#define TEST_5  void $ qAddTempFile                     ['1', throw (userError "IOError in qAddTempFile") ]
#define TEST_6  void $ qAddTopDecls                     [ throw (userError "IOError in qAddTopDecls") ]
#define TEST_7  void $ qAddForeignFilePath    LangC     ['1', throw (userError "IOError in qAddForeignFilePath") ]
#define TEST_8  void $ qAddForeignFilePath    undefined "foo"
#define TEST_9  void $ qAddCorePlugin                   ['1', throw (userError "IOError in qAddCorePlugin") ]
#define TEST_10 void $ qPutDoc                ModuleDoc ['1', throw (userError "IOError in qPutDoc") ]
#define TEST_11 void $ qPutDoc                undefined "foo"
#define TEST_12 void $ qReport                undefined "foo"
#define TEST_13 void $ qAddTopDecls                     [ FunD undefined []]
#define TEST_14 void $ qReify                 undefined

#define TEST_0  void $ newName                          ['1', throw (userError "IOError in newName") ]
#define TEST_15 void $ lookupName            True      ['1', throw (userError "IOError in qLookupName") ]
#define TEST_16 void $ addDependentFile                ['1', throw (userError "IOError in qAddDependentFile") ]
#define TEST_17 void $ addDependentDirectory           ['1', throw (userError "IOError in qAddDependentDirectory") ]
#define TEST_18 void $ addTempFile                     ['1', throw (userError "IOError in qAddTempFile") ]
#define TEST_19 void $ addTopDecls                     [ throw (userError "IOError in qAddTopDecls") ]
#define TEST_20 void $ addForeignFilePath    LangC     ['1', throw (userError "IOError in qAddForeignFilePath") ]
#define TEST_21 void $ addForeignFilePath    undefined "foo"
#define TEST_22 void $ addCorePlugin                   ['1', throw (userError "IOError in qAddCorePlugin") ]
#define TEST_23 void $ putDoc                ModuleDoc ['1', throw (userError "IOError in qPutDoc") ]
#define TEST_24 void $ putDoc                undefined "foo"
#define TEST_25 void $ reportWarning         ['1', throw (userError "IOError in reportWarning") ]
#define TEST_26 void $ reportError         ['1', throw (userError "IOError in reportWarning") ]
#define TEST_27 void $ addTopDecls                     [ FunD undefined []]
#define TEST_28 void $ reify                 undefined
#define TEST_29 void $ reifyFixity                 undefined
#define TEST_30 void $ reifyType                 undefined
#define TEST_31 void $ reifyInstances  undefined []
#define TEST_32 void $ reifyInstances  (mkName "x")  [undefined]
#define TEST_33 void $ reifyRoles undefined
#define TEST_34 void $ reifyAnnotations @String undefined
#define TEST_35 void $ reifyModule undefined
#define TEST_36 void $ reifyConStrictness undefined
#define TEST_37 void $ runIO undefined
#define TEST_38 void $ getPackageRoot >> error "exception so compile_fail passes"
#define TEST_39 void $ addModFinalizer undefined
#define TEST_40 void $ getQ @String >> error "exception so compile_fail passes"
#define TEST_41 void $ putQ @String undefined
#define TEST_42 void $ isExtEnabled undefined
#define TEST_43 void $ extsEnabled >> error "exception so compile_fail passes"



$(do
  TEST_INDEX
  pure []
  )
