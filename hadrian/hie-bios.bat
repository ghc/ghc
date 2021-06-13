set TERM=dumb
set CABFLAGS=-v0
set TOOL_OUTPUT=%HIE_BIOS_OUTPUT%
%CD%\hadrian\build-cabal.bat tool:%1 --build-root=_hie-bios --flavour=ghc-in-ghci
