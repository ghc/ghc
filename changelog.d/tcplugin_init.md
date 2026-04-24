section: ghc-lib
issues: #26839
mrs: !15485
synopsis:
  Changes to typechecker plugin API surrounding initialisation/shutdown
description:
  The records for typechecker plugins and defaulting plugins have been updated to
  reflect the fact that these plugins may be invoked after the end of typechecking,
  due to the fact that the pattern-match checker (which runs in the desugarer) can
  invoke the constraint solver.

  The fields ``tcPluginStop :: s -> TcPluginM ()`` and ``dePluginStop :: s -> TcPluginM ()``
  are replaced by the two fields::

  tcPluginPostTc   :: s -> TcPluginM ()
  tcPluginShutdown :: s -> IO ()

  respectively::

  dePluginPostTc   :: s -> TcPluginM ()
  dePluginShutdown :: s -> IO ()

  The "post-tc" actions are run at the end of typechecking, and can be used to
  inspect the final ``TcGblEnv``/``TcLclEnv`` of the module being typechecker.
  They **should not** shut down the plugin.
  The "shutdown" actions are run at the end of the desugaring, and should
  clean up any resources acquired for the duration of the run of the plugin.

  For most typechecker plugins, the ``tcPluginStop`` action was a simple resource
  release shutdown action that could be run in ``IO``. To migrate such plugins,
  move that action to ``tcPluginShutdown``, and set
  ``tcPluginPostTc = const (return ())``.
