# A plugin (MyPlugin) lives in home unit plugin-0 and is *reexported* by a
# second home unit, reexport-0 (via -reexported-module). The consumer module
# App, in home unit app-0, enables it with -fplugin=MyPlugin but depends only on
# reexport-0, not on plugin-0 directly. Resolving -fplugin must therefore follow
# the reexport through the sibling home unit, exactly as an ordinary import
# does. A home-unit search that ignores reexported-modules fails with
# "Could not load module 'MyPlugin' ... hidden package".
#
# -fbyte-code-and-object-code -fprefer-byte-code makes the plugin load as
# byte-code from its (interpreted) home unit, mirroring how a multiple-home-unit
# session is driven by 'cabal repl --enable-multi-repl' and HLS.
test('multipleHomeUnits_plugin02',
     [req_plugins, extra_files(['p/', 'q/', 'r/', 'pluginunit', 'reexportunit', 'appunit'])],
     multiunit_compile,
     [['pluginunit', 'reexportunit', 'appunit'],
      '-v0 -fbyte-code-and-object-code -fprefer-byte-code'])
