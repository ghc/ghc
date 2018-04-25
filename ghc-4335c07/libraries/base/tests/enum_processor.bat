:;# Problem: GHC on Windows doesn't like '-pgmF ./enum_processor.py'.
:;#          See ticket:365#comment:7 for details.
:;#
:;# Workaround: this file, which functions both as a Windows .bat script and a
:;# Unix shell script. Hacky, but it seems to work.

:;# Starts with a ':', to skip on Windows.
:; "${PYTHON}" enum_processor.py $@; exit $?

:;# Windows only:
%PYTHON% enum_processor.py %*
