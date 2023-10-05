{-# OPTIONS_GHC -dcore-lint #-}
{-# OPTIONS_GHC -fplugin EmitWantedPlugin #-}

module TcPlugin_EmitWanted where

import Definitions
  ( MyClass(methC) )

foo :: MyClass a => a
foo = methC

bar :: Bool
bar = foo
  -- We need to solve [W] MyClass a.
  -- The plugin emits [W] a ~# (), and solves [W] MyClass a
  -- using the coercion hole.
  -- We then report an error for the unsolved a ~# () constraint,
  -- where we get to see whether the source location of the newly
  -- emitted Wanted constraint is as expected.
  --
  -- The crucial thing is that the error message should have
  -- the correct SrcSpan, in this case line 13 column 7.
