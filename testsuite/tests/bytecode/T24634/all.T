# Skipping this on debug compilers because of a pipeline failure in `x86_64-linux-deb12-numa-slow-validate`:
#
# WARNING:
#   CorePrep: silly extra arguments:
#   say_hello
#   Call stack:
#       CallStack (from HasCallStack):
#         warnPprTrace, called at compiler/GHC/CoreToStg/Prep.hs:<line>:<column> in <package-id>:GHC.CoreToStg.Prep

test('T24634',
     [extra_files(['hello_c.h', 'hello_c.c', 'Hello.hs', 'Main.hs']),
      req_c,
      req_th,
      when(compiler_debugged(), skip),
      ],
     makefile_test,
     [])
