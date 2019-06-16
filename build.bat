@echo off
set filebase=%1
rgbasm -o%filebase%.obj %filebase%.asm
if %errorlevel% neq 0 call :exit 1
rgblink -m%filebase%.map -n%filebase%.sym -o%filebase%.gb %filebase%.obj
if %errorlevel% neq 0 call :exit 1
rgbfix -p0 -v %filebase%.gb
if %errorlevel% neq 0 call :exit 1
call :exit 0

:exit
rem pause
rem exit