@echo off
REM Integration Test Runner for Plankalkül Compiler (Windows)
REM
REM Compiles .pk files to C, compiles C to executables, and verifies output.

setlocal enabledelayedexpansion

set SCRIPT_DIR=%~dp0
set SAMPLES_DIR=%SCRIPT_DIR%samples
set MANIFEST=%SCRIPT_DIR%test_manifest.txt
set BUILD_DIR=%SCRIPT_DIR%_build
set COMPILER=%SCRIPT_DIR%..\..\._build\default\src\bin\plankalkul.exe

REM Track results
set PASSED=0
set FAILED=0
set SKIPPED=0

echo === Plankalkül Integration Tests ===
echo.

REM Create build directory
if not exist "%BUILD_DIR%" mkdir "%BUILD_DIR%"

REM Check compiler exists
if not exist "%COMPILER%" (
    echo Error: Compiler not found at %COMPILER%
    echo Run 'dune build' first.
    exit /b 1
)

REM Read manifest and run tests
for /f "usebackq tokens=1,2,3 delims=|" %%a in ("%MANIFEST%") do (
    set "line=%%a"
    REM Skip comments
    if "!line:~0,1!" neq "#" (
        set "pk_file=%%a"
        set "args=%%b"
        set "expected=%%c"

        REM Trim whitespace (basic)
        for /f "tokens=* delims= " %%x in ("!pk_file!") do set "pk_file=%%x"
        for /f "tokens=* delims= " %%x in ("!args!") do set "args=%%x"
        for /f "tokens=* delims= " %%x in ("!expected!") do set "expected=%%x"

        if "!pk_file!" neq "" (
            call :run_test "!pk_file!" "!args!" "!expected!"
        )
    )
)

echo.
echo === Results ===
echo Passed:  %PASSED%
echo Failed:  %FAILED%
echo Skipped: %SKIPPED%

if %FAILED% gtr 0 exit /b 1
exit /b 0

:run_test
set "pk_file=%~1"
set "args=%~2"
set "expected=%~3"

REM Extract base name
for %%i in ("%pk_file%") do set "base_name=%%~ni"

set "c_file=%BUILD_DIR%\%base_name%.c"
set "exe_file=%BUILD_DIR%\%base_name%.exe"

REM Check file exists
if not exist "%SAMPLES_DIR%\%pk_file%" (
    echo SKIP %pk_file% (file not found)
    set /a SKIPPED+=1
    goto :eof
)

REM Compile .pk to .c
"%COMPILER%" --emit-c "%SAMPLES_DIR%\%pk_file%" > "%c_file%" 2>nul
if errorlevel 1 (
    echo FAIL %pk_file% (compilation to C failed)
    set /a FAILED+=1
    goto :eof
)

REM Compile .c to executable
gcc -o "%exe_file%" "%c_file%" -lm 2>nul
if errorlevel 1 (
    echo FAIL %pk_file% (gcc compilation failed)
    set /a FAILED+=1
    goto :eof
)

REM Run and capture output
for /f "tokens=2 delims=:" %%r in ('"%exe_file%" %args% 2^>^&1 ^| findstr "^Result:"') do (
    set "actual=%%r"
)
REM Trim leading space
for /f "tokens=* delims= " %%x in ("!actual!") do set "actual=%%x"

REM Compare
if "!actual!" == "!expected!" (
    echo PASS %pk_file% (%args%) =^> !actual!
    set /a PASSED+=1
) else (
    echo FAIL %pk_file% (%args%)
    echo        Expected: !expected!
    echo        Actual:   !actual!
    set /a FAILED+=1
)
goto :eof
