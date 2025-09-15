{-# LANGUAGE Haskell2010, DeriveAnyClass, MultiParamTypeClasses, QuantifiedConstraints, TemplateHaskell #-}

import Control.Monad.Reader (MonadReader)
import Language.Haskell.TH (runQ)
import Language.Haskell.TH.Ppr (pprint)

class C a b

main = do
  runQ [d|data Foo deriving (C a)|] >>= putStrLn . pprint
  runQ [d|newtype Foo m a = MkFoo (m a) deriving (forall r. MonadReader r)|] >>= putStrLn . pprint
  runQ [d|class (forall r. MonadReader r m) => MonadReaderPlus m|] >>= putStrLn . pprint
