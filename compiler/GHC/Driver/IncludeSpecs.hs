module GHC.Driver.IncludeSpecs
  ( IncludeSpecs(..)
  , addGlobalInclude
  , addQuoteInclude
  , addImplicitQuoteInclude
  , flattenIncludes
  ) where

import GHC.Prelude

-- | Used to differentiate the scope an include needs to apply to.
-- We have to split the include paths to avoid accidentally forcing recursive
-- includes since -I overrides the system search paths. See #14312.
data IncludeSpecs
  = IncludeSpecs { includePathsQuote  :: [String]
                 , includePathsGlobal :: [String]
                 -- | See Note [Implicit include paths]
                 , includePathsQuoteImplicit :: [String]
                 }
  deriving Show

-- | Append to the list of includes a path that shall be included using `-I`
-- when the C compiler is called. These paths override system search paths.
addGlobalInclude :: IncludeSpecs -> [String] -> IncludeSpecs
addGlobalInclude spec paths  = let f = includePathsGlobal spec
                               in spec { includePathsGlobal = f ++ paths }

-- | Append to the list of includes a path that shall be included using
-- `-iquote` when the C compiler is called. These paths only apply when quoted
-- includes are used. e.g. #include "foo.h"
addQuoteInclude :: IncludeSpecs -> [String] -> IncludeSpecs
addQuoteInclude spec paths  = let f = includePathsQuote spec
                              in spec { includePathsQuote = f ++ paths }

-- | These includes are not considered while fingerprinting the flags for iface
-- | See Note [Implicit include paths]
addImplicitQuoteInclude :: IncludeSpecs -> [String] -> IncludeSpecs
addImplicitQuoteInclude spec paths  = let f = includePathsQuoteImplicit spec
                              in spec { includePathsQuoteImplicit = f ++ paths }


-- | Concatenate and flatten the list of global and quoted includes returning
-- just a flat list of paths.
flattenIncludes :: IncludeSpecs -> [String]
flattenIncludes specs =
    includePathsQuote specs ++
    includePathsQuoteImplicit specs ++
    includePathsGlobal specs
