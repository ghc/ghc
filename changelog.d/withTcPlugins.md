section: ghc-lib
issues: #26839
mrs: !15485
synopsis:
  Removal of ``withTcPlugins``, ``withHoleFitPlugins``, and ``withDefaultingPlugins``
  in favour of a single ``withTcMPlugins``.
description:
  The functions ``withTcPlugins``, ``withHoleFitPlugins``, and ``withDefaultingPlugins``
  have been removed in favour of a single ``withTcMPlugins`` which handles
  all plugins for the ``TcM`` monad (typechecker plugins, hole-fit plugins,
  and defaulting plugins). Note that GHC API users should not need to use these
  functions, as their existence was papering over the fact that ``initTcDsForSolver``
  did not properly initialise ``TcM`` plugins, which has been fixed.
