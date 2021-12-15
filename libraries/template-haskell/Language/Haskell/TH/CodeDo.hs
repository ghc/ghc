-- | This module exists to work nicely with the QualifiedDo
-- extension.
--
-- @
-- import qualified Language.Haskell.TH.CodeDo as Code
--
-- myExample :: Monad m => Code m a -> Code m a -> Code m a
-- myExample opt1 opt2 =
--   Code.do
--    x <- someSideEffect               -- This one is of type `M Bool`
--    if x then opt1 else opt2
-- @
module Language.Haskell.TH.CodeDo((>>=), (>>)) where

import Language.Haskell.TH.Syntax
import Prelude(Monad)

-- | Module over monad operator for 'Code'
(>>=) :: Monad m => m a -> (a -> Code m b) -> Code m b
(>>=) = bindCode
(>>) :: Monad m => m a -> Code m b -> Code m b
(>>)  = bindCode_
