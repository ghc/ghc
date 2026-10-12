section: compiler
synopsis: Remove command-line flags deprecated in GHC 8.2 or earlier
issues: #27939
mrs: !16801
description: {
  The following flags, deprecated in GHC 8.2 or earlier, are no longer
  accepted: ``-n``, ``-Onot``, ``-recomp``, ``-syslib``, ``-package-conf``,
  ``-no-user-package-conf``, ``-auto``, ``-auto-all``, ``-caf-all``,
  ``-fglasgow-exts``, ``-frewrite-rules``, ``-fvia-C``, ``-fcontext-stack``,
  ``-ftype-function-depth``, ``-#include``, the pre-``-X`` extension
  spellings such as ``-fth`` and ``-fffi``, and their negations.

  The ``INCLUDE`` file-header pragma, which only expanded to ``-#include``,
  is no longer recognised either.
}
