@echo off
echo Backing up KM project and Quicklisp setup...
echo.

REM Define variables
set "PROJECT_ROOT=E:\IdeaProjects\ez-knowledge-machine"
set "QUICKLISP_DIR=E:\Quicklisp"
set "SBCL_PATH=C:\Program Files\Steel Bank Common Lisp\sbcl.exe"
set "BACKUP_DIR=%PROJECT_ROOT%\backup"
for /f "tokens=2 delims==" %%i in ('wmic os get localdatetime /value') do set "DATETIME=%%i"
set "TIMESTAMP=%DATETIME:~0,4%%DATETIME:~4,2%%DATETIME:~6,2%_%DATETIME:~8,2%%DATETIME:~10,2%%DATETIME:~12,2%"
set "BACKUP_FILE=%BACKUP_DIR%\KM_Backup_%TIMESTAMP%.zip"

REM Step 1: Create backup directory if it doesn’t exist
echo [1/2] Preparing backup directory...
if not exist "%BACKUP_DIR%" (
  mkdir "%BACKUP_DIR%"
  if errorlevel 1 (
    echo Failed to create %BACKUP_DIR%
    pause
    exit /b 1
  )
) else (
  echo Directory %BACKUP_DIR% already exists, proceeding...
)

REM Step 2: Create ZIP backup
echo.
echo [2/2] Creating backup ZIP file: %BACKUP_FILE%...
if not exist "%PROJECT_ROOT%" (
  echo Project root %PROJECT_ROOT% not found, nothing to back up!
  pause
  exit /b 1
)
if not exist "%QUICKLISP_DIR%" (
  echo Quicklisp directory %QUICKLISP_DIR% not found, proceeding with project only...
  powershell -Command "Compress-Archive -Path '%PROJECT_ROOT%' -DestinationPath '%BACKUP_FILE%'"
) else (
  powershell -Command "Compress-Archive -Path '%PROJECT_ROOT%', '%QUICKLISP_DIR%' -DestinationPath '%BACKUP_FILE%'"
)
if errorlevel 1 (
  echo Failed to create backup ZIP file
  pause
  exit /b 1
)

echo.
echo Backup completed successfully!
echo Backup saved to: %BACKUP_FILE%
pause