@echo off
setlocal
if not exist tmp mkdir tmp
set LANG=C
set LC_ALL=C
set LC_CTYPE=C
set TMPDIR=tmp
set dir=.
set absdir=%~p0
set PREFIX=%absdir:\=/%
set GUILE_AUTO_COMPILE=0
rem set GUILE_LOAD_PATH=%GUILE_LOAD_PATH%;%dir%/share/guile/site/3.0;%dir%/share/guile/3.0
rem #set GUILE_LOAD_COMPILED_PATH=%GUILE_LOAD_COMPILED_PATH%;%dir%/lib/guile/3.0/site-ccache;%dir%/lib/guile/3.0/ccache
set GUILE_LOAD_PATH=%GUILE_LOAD_PATH%;%dir%/module
set GUILE_LOAD_COMPILED_PATH=%GUILE_LOAD_COMPILED_PATH%;%dir%/module
set HOME=%USERPROFILE%

set LIBRARY_PATH=%LIBRARY_PATH::=;%;
set LIBRARY_PATH_CROSS_LIB=%LIBRARY_PATH:/lib;=/x86_64-w64-mingw32/lib;%

set CROSS_LIBRARY_PATH=%CROSS_LIBRARY_PATH::=;%;
set CROSS_LIBRARY_PATH_BIN=%CROSS_LIBRARY_PATH:/lib;=/bin;%
set WINELOADERNOEXEC=0

set PATH=%~p0/libguile/.libs;%PATH%;%LIBRARY_PATH_CROSS_LIB%;%CROSS_LIBRARY_PATH%;%CROSS_LIBRARY_PATH_BIN%;
guile.exe %*
