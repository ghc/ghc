{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section{Haskell abstract syntax definition}

This module glues together the pieces of the Haskell abstract syntax,
which is declared in the various \tr{Hs*} modules.  This module,
therefore, is almost nothing but re-exporting.
-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-} -- For deriving instance Data
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-} -- Wrinkle in Note [Trees That Grow]
                                      -- in module Language.Haskell.Syntax.Extension
-- See Note [Language.Haskell.Syntax.* Hierarchy] for why not GHC.Hs.*
module Language.Haskell.Syntax (
        module Language.Haskell.Syntax.Binds,
        module Language.Haskell.Syntax.Decls,
        module Language.Haskell.Syntax.Expr,
        module Language.Haskell.Syntax.ImpExp,
        module Language.Haskell.Syntax.Lit,
        module Language.Haskell.Syntax.Module.Name,
        module Language.Haskell.Syntax.Pat,
        module Language.Haskell.Syntax.Type,
        module Language.Haskell.Syntax.Extension,
        ModuleName(..), HsModule(..)
) where

import Language.Haskell.Syntax.Decls
import Language.Haskell.Syntax.Binds
import Language.Haskell.Syntax.Expr
import Language.Haskell.Syntax.ImpExp
import Language.Haskell.Syntax.Module.Name
import Language.Haskell.Syntax.Lit
import Language.Haskell.Syntax.Extension
import Language.Haskell.Syntax.Pat
import Language.Haskell.Syntax.Type

import Data.Maybe (Maybe)

{-
Note [Language.Haskell.Syntax.* Hierarchy]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Why are these modules not 'GHC.Hs.*', or some other 'GHC.*'? The answer
is that they are to be separated from GHC and put into another package,
in accordance with the final goals of Trees That Grow. (See Note [Trees
That Grow] in 'Language.Haskell.Syntax.Extension'.) In short, the
'Language.Haskell.Syntax.*' tree should be entirely GHC-independent.
GHC-specific stuff related to source-language syntax should be in
'GHC.Hs.*'.

We cannot move them to the separate package yet, but by giving them
names like so, we hope to remind others that the goal is to factor them
out, and therefore dependencies on the rest of GHC should never be
added, only removed.

For more details, see
https://gitlab.haskell.org/ghc/ghc/-/wikis/implementing-trees-that-grow
-}

-- | Haskell Module
--
-- All we actually declare here is the top-level structure for a module.
data HsModule p
    = HsModule {
      hsmodExt :: XCModule p,
        -- ^ HsModule extension point
      hsmodName :: Maybe (XRec p ModuleName),
        -- ^ @Nothing@: \"module X where\" is omitted (in which case the next
        --     field is Nothing too)
      hsmodExports :: Maybe (XRec p [LIE p]),
        -- ^ Export list
        --
        --  - @Nothing@: export list omitted, so export everything
        --
        --  - @Just []@: export /nothing/
        --
        --  - @Just [...]@: as you would expect...
      hsmodImports :: [LImportDecl p],
      hsmodDecls :: [LHsDecl p]
        -- ^ Type, class, value, and interface signature decls
   }
  | XModule !(XXModule p)
