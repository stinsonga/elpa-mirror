@echo off
set ERRORLEVEL=
call python fake-sendmail.py %*
exit /b %ERRORLEVEL%
