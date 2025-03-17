@echo off
echo Restoring KM project and Quicklisp setup...
echo.

REM Define variables
set "PROJECT_ROOT=E:\IdeaProjects\ez-knowledge-machine"
set "QUICKLISP_DIR=E:\Quicklisp"
set "SBCL_PATH=C:\Program Files\Steel Bank Common Lisp\sbcl.exe"
set "BACKUP_DIR=%PROJECT_ROOT%\backup"
set "SCRIPTS_DIR=%PROJECT_ROOT%\scripts"

REM Step 1: Prompt for backup file
echo [1/4] Select a backup file to restore...
echo Available backups:
dir "%BACKUP_DIR%\KM_Backup_*.zip" /b
echo.
set "BACKUP_FILE="
set /p BACKUP_FILE="Enter the backup filename (e.g., KM_Backup_20250310_143022.zip): "
if "%BACKUP_FILE%"=="" (
  echo No backup file specified, aborting...
  pause
  exit /b 1
)
set "FULL_BACKUP_PATH=%BACKUP_DIR%\%BACKUP_FILE%"
if not exist "%FULL_BACKUP_PATH%" (
  echo Backup file %FULL_BACKUP_PATH% not found!
  pause
  exit /b 1
)

REM Step 2: Delete existing directories
echo.
echo [2/4] Deleting existing directories...
for %%d in ("%PROJECT_ROOT%" "%QUICKLISP_DIR%") do (
  if exist "%%d" (
    rmdir /s /q "%%d"
    if errorlevel 1 (
      echo Failed to delete %%d
      pause
      exit /b 1
    )
    echo Deleted %%d
  ) else (
    echo %%d does not exist, skipping deletion.
  )
)

REM Step 3: Restore from backup
echo.
echo [3/4] Restoring from %FULL_BACKUP_PATH%...
powershell -Command "Expand-Archive -Path '%FULL_BACKUP_PATH%' -DestinationPath 'E:\' -Force"
if errorlevel 1 (
  echo Failed to restore backup
  pause
  exit /b 1
)
echo Backup restored successfully.

REM Step 4: Run setup script
echo.
echo [4/4] Running setup script to finalize configuration...
if not exist "%SCRIPTS_DIR%\setup.bat" (
  echo setup.bat not found in %SCRIPTS_DIR%, please ensure it exists!
  pause
  exit /b 1
)
call "%SCRIPTS_DIR%\setup.bat"
if errorlevel 1 (
  echo Setup script failed, check output above.
  pause
  exit /b 1
)

echo.
echo Restore completed successfully!
echo Project and Quicklisp setup restored and configured.
pause