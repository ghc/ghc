section: ghc-lib
issues: #26839
mrs: !15485
synopsis:
  ``typecheckModule``, ``hscTypecheckRename``, ``hscTypecheckRenameWithDiagnostics``
  and ``hscTypecheckAndGetWarnings`` all take an additional argument that
  specifies how to start/stop ``TcM`` plugins.
description:
  The GHC API functions ``typecheckModule``, ``hscTypecheckRename``, ``hscTypecheckRenameWithDiagnostics`` and ``hscTypecheckAndGetWarnings``
  now take an additional argument that specifies how ``TcM`` plugins
  (typechecker plugins, defaulting plugins) should be started/stopped.

  If proceeding to desugaring, the plugins should be kept running for the
  benefit of the pattern match checker. Otherwise, they should be stopped at the
  end of typechecking. See the new ``TcMPluginHandling`` datatype for more
  details.

  Similarly, ``tcRnModule`` is changed to take a ``TcRnModuleOptions`` record
  as an argument, containing two pieces of information:

    - whether to keep the renamed syntax (the old ``Bool`` argument),
    - whether to start/stop ``TcM`` plugins (see ``TcMPluginHandling``)
