# A plugin (MyPlugin, in home unit plugin-0) is enabled by a module (App, in
# home unit app-0) belonging to a *different* home unit in the same session.
# Resolving -fplugin must search sibling home units; previously the plugin
# finder only looked at the current home unit and registered external packages,
# so this failed with "Could not load module 'MyPlugin' ... hidden package".
#
# -fbyte-code-and-object-code -fprefer-byte-code makes the plugin load as
# byte-code from its (interpreted) home unit, mirroring how a multiple-home-unit
# session is driven by 'cabal repl --enable-multi-repl' and HLS.
test('multipleHomeUnits_plugin01',
     [req_plugins, extra_files(['p/', 'q/', 'pluginunit', 'appunit'])],
     multiunit_compile,
     [['pluginunit', 'appunit'],
      '-v0 -fbyte-code-and-object-code -fprefer-byte-code'])
