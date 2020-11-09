set TERM=dumb
set CABFLAGS=-v0
%CD%\hadrian\build-cabal.bat tool:%1 -q --build-root=_hie-bios --flavour=ghc-in-ghci > %HIE_BIOS_OUTPUT%
