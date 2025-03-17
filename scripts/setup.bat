@echo off
echo Setting up KM project with embedded Quicklisp...
echo.

REM Define variables
set "PROJECT_ROOT=E:\IdeaProjects\ez-knowledge-machine"
set "QUICKLISP_DIR=E:\Quicklisp"
set "QUICKLISP_SETUP=%QUICKLISP_DIR%\setup.lisp"
set "SBCL_PATH=C:\Program Files\Steel Bank Common Lisp\sbcl.exe"

REM Step 1: Check and create necessary directories
echo [1/3] Checking and creating directories...
for %%d in (
  "%PROJECT_ROOT%\packages\km-common"
  "%PROJECT_ROOT%\packages\km-utils"
  "%PROJECT_ROOT%\packages\km-core"
  "%PROJECT_ROOT%\packages\km-ai"
  "%QUICKLISP_DIR%"
  "%PROJECT_ROOT%\scripts"
) do (
  if not exist "%%d" (
    mkdir "%%d"
    if errorlevel 1 (
      echo Failed to create %%d
      pause
      exit /b 1
    )
  ) else (
    echo Directory %%d already exists, skipping creation.
  )
)

REM Step 2: Install Quicklisp if setup.lisp doesn’t exist
echo.
echo [2/3] Setting up Quicklisp...
echo QUICKLISP_SETUP=%QUICKLISP_SETUP%
if exist "%QUICKLISP_SETUP%" (
  echo setup.lisp exists, skipping Quicklisp installation.
) else (
  echo setup.lisp not found, installing Quicklisp...
  cd /d "%QUICKLISP_DIR%" || (
    echo Failed to change directory to %QUICKLISP_DIR%
    pause
    exit /b 1
  )
  curl -O https://beta.quicklisp.org/quicklisp.lisp
  if errorlevel 1 (
    echo Failed to download quicklisp.lisp
    pause
    exit /b 1
  )
  "%SBCL_PATH%" --load quicklisp.lisp --eval "(quicklisp-quickstart:install :path \"E:/Quicklisp/\")" --quit
  if errorlevel 1 (
    echo Quicklisp installation failed
    pause
    exit /b 1
  )
  if exist "%PROJECT_ROOT%\scripts\install-quicklisp.lisp" (
    "%SBCL_PATH%" --noinform --script "%PROJECT_ROOT%\scripts\install-quicklisp.lisp"
    if errorlevel 1 (
      echo Running install-quicklisp.lisp failed
      pause
      exit /b 1
    )
  )
  del "quicklisp.lisp" 2>nul
  del "%PROJECT_ROOT%\scripts\install-quicklisp.lisp" 2>nul
)

REM Step 3: Create package junctions
echo.
echo [3/3] Creating package junctions...
if not exist "%QUICKLISP_DIR%\local-projects" (
  mkdir "%QUICKLISP_DIR%\local-projects"
  if errorlevel 1 (
    echo Failed to create %QUICKLISP_DIR%\local-projects
    pause
    exit /b 1
  )
) else (
  echo %QUICKLISP_DIR%\local-projects already exists, skipping creation.
)
for %%p in (km-common km-utils km-core km-ai) do (
  if exist "%QUICKLISP_DIR%\local-projects\%%p" (
    rmdir "%QUICKLISP_DIR%\local-projects\%%p"
    if errorlevel 1 (
      echo Failed to remove existing junction %%p
      pause
      exit /b 1
    )
  )
  mklink /J "%QUICKLISP_DIR%\local-projects\%%p" "%PROJECT_ROOT%\packages\%%p"
  if errorlevel 1 (
    echo Failed to create junction for %%p
    pause
    exit /b 1
  )
  echo Junction created: %%p
)

echo.
echo Setup completed successfully!
echo Open ez-knowledge-machine in IntelliJ and configure SLT with embedded Quicklisp.
pause