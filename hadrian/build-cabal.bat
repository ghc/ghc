@echo off

if "%CABAL%"=="" (
    set CABAL=cabal
)

if "%GHC%"=="" (
    set GHC=ghc
)

if "%CABFLAGS%"=="" (
    set CABFLAGS=--with-compiler=%GHC% --disable-documentation --disable-profiling --disable-library-profiling
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

if %CABMAJOR% lss 3 (
    echo Cabal version is too old; you need at least cabal-install 3.0
    exit /B 2
)

for /F "tokens=*" %%a in ('"%CABAL%" --with-compiler=%GHC% path --output-format=key-value 2^>NUL ^| findstr /B "remote-repo-cache:"') do set REMOTE_REPO_CACHE=%%a
set REMOTE_REPO_CACHE=%REMOTE_REPO_CACHE:remote-repo-cache: =%
if not exist "%REMOTE_REPO_CACHE%\hackage.haskell.org" (
    echo Please run 'cabal update' first
    exit /B 2
)

"%CABAL%" --project-file=%PROJ% new-build %CABFLAGS% -j exe:hadrian
rem use new-exec instead of new-run to make sure that the build-tools (alex & happy) are in PATH
"%CABAL%" --project-file=%PROJ% new-exec  %CABFLAGS%    hadrian -- ^
    --directory "%CD%" ^
    %*
