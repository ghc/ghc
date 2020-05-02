@echo off

if "%CABAL%"=="" (
    set CABAL=cabal
)

if "%CABFLAGS%"=="" (
    set CABFLAGS=--disable-documentation --disable-profiling --disable-library-profiling
)

rem It is currently more robust to pass Cabal an absolute path to the project file.
set PROJ="%CD%/hadrian/cabal.project"

if not exist %PROJ% (
    echo Current working directory must be GHC's top-level folder
    exit /B 2
)

"%CABAL%" 2> NUL
if not %ERRORLEVEL% equ 1 (
    echo Please make sure 'cabal' is in your PATH
    exit /B 2
)

for /F "tokens=*" %%a in ('%CABAL% --numeric-version') do set CABVERSTR=%%a
for /F "delims=. tokens=1,2,3,4" %%a in ("%CABVERSTR%") do (
    set CABMAJOR=%%a
    set CABMINOR=%%b
    set CABREV=%%c
    set CABPATCH=%%d
)

set "_cabal_ok=0"
if %CABMAJOR% gtr 2 set _cabal_ok=1
if %CABMAJOR% equ 2 (
    if %CABMINOR% geq 2 set _cabal_ok=1
)
if %_cabal_ok% equ 1 (
    "%CABAL%" --project-file=%PROJ% new-build %CABFLAGS% -j exe:hadrian
    rem use new-exec instead of new-run to make sure that the build-tools (alex & happy) are in PATH
    "%CABAL%" --project-file=%PROJ% new-exec  %CABFLAGS%    hadrian -- ^
        --directory "%CD%" ^
        %*
) else (
    echo Cabal version is too old; you need at least cabal-install 2.2
    exit /B 2
)
