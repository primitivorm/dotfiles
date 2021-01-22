@echo off

:: Windows SDK Include directory. No quotation marks.
set SDK_INCLUDE_DIR=C:\Program Files (x86)\Windows Kits\10\Include

:: Visual Studio directory. Quotation marks.
set VS_DIR="C:\Program Files (x86)\Microsoft Visual Studio\2019\Community"

:: Target architecture, AMD64 (64-bit) or I386 (32-bit)
set CPU=AMD64

:: Toolchain, x86_amd64 (cross-compile 64-bit) or x86 (32-bit) or amd64 (64-bit)
set TOOLCHAIN=x86_amd64

:: TINY, SMALL, NORMAL, BIG or HUGE. NORMAL or above recommended
set FEATURES=BIG

:: yes for gvim, no for vim
set GUI=yes

:: Whatever IDE integrations we don't need
set NETBEANS=no

:: UTF-8 encoding
set MBYTE=yes

:: Enable Python 2 scripting
:: set DYNAMIC_PYTHON=yes
:: set PYTHON="C:\Python27"
:: set PYTHON_VER=27

:: Enable Python 3 scripting
set DYNAMIC_PYTHON3=yes
set PYTHON3="C:\Python39"
set PYTHON3_VER=39

echo "Configuring Visual Studio..."
call %VS_DIR%\VC\Auxiliary\Build\vcvarsall.bat %TOOLCHAIN%