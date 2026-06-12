section: compiler
synopsis: Find plugins defined in sibling home units in multiple-home-unit sessions
issues: #27349
mrs: !16156
description: {
  In a multiple-home-unit session (e.g. ``cabal repl --enable-multi-repl`` or HLS), the plugin finder now also searches the home units that the current home unit depends on, following module reexports along the way, so a ``-fplugin`` defined in (or reexported by) a sibling home unit is found and loaded instead of failing as a hidden package.
}
