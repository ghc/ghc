module Expression.Type where

import Context.Type
import Way.Type

import Builder
import qualified Hadrian.Expression as H

-- | @Expr a@ is a computation that produces a value of type @Action a@ and can
-- read parameters of the current build 'Target'.
type Expr a = H.Expr Context Builder a

-- | The following expressions are used throughout the build system for
-- specifying conditions ('Predicate'), lists of arguments ('Args'), 'Ways'
-- and 'Packages'.
type Predicate = H.Predicate Context Builder
type Args      = H.Args      Context Builder
type Ways      = Expr [Way]
