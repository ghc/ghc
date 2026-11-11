{-# LANGUAGE CPP, TemplateHaskell #-}

module T8987d where
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.IO.Error
import Control.Exception
import Data.List
import Control.Monad

#define TEST_0  void $ newName                          ['1', throw (userError "IOError in newName") ]
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

$(do
  TEST_INDEX
  pure []
  )
