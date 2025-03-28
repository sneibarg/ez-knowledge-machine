@echo off
echo Setting up KM project with embedded Quicklisp...
echo.

REM Enable delayed expansion for proper variable handling
setlocal EnableDelayedExpansion

REM Define variables
set "PROJECT_ROOT=E:\VSCode\ez-knowledge-machine"
set "SRC_ROOT=%PROJECT_ROOT%\src"
set "QUICKLISP_DIR=E:\Quicklisp"
set "QUICKLISP_SETUP=%QUICKLISP_DIR%\setup.lisp"
set "SBCL_PATH=C:\Program Files\Steel Bank Common Lisp\sbcl.exe"
set "LOCAL_PROJECTS=%QUICKLISP_DIR%\local-projects"
set "PACKAGES_ROOT=%PROJECT_ROOT%\packages"

REM Step 1: Check and create necessary directories
echo [1/5] Checking and creating directories...
for %%d in (
  "%SRC_ROOT%"
  "%QUICKLISP_DIR%"
  "%LOCAL_PROJECTS%"
  "%PACKAGES_ROOT%"
) do (
  if not exist "%%d" (
    mkdir "%%d"
    if errorlevel 1 (
      echo Failed to create %%d
      pause
      exit /b 1
    )
    echo Created directory %%d
  ) else (
    echo Directory %%d already exists, skipping creation.
  )
)

REM Step 2: Install Quicklisp if setup.lisp doesn't exist
echo.
echo [2/5] Setting up Quicklisp...
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

REM Step 3: Generate .asd files based on Lisp files in SRC_ROOT and include package.lisp
echo.
echo [3/5] Generating .asd files...
echo Starting Step 3...
for %%p in (km km-threads km-rest) do (
  echo Processing %%p...
  set "LISP_FILE=%SRC_ROOT%\%%p.lisp"
  if exist "!LISP_FILE!" (
    echo Generating %%p.asd in %SRC_ROOT%...
    del "%SRC_ROOT%\%%p.asd" 2>nul
    if "%%p"=="km-rest" (
      set "content=(defsystem "km-rest" :description "Automatically generated system for km-rest" :author "Generated by build script" :version "0.1.0" :depends-on ("km" "hunchentoot" "km-threads" "jsown") :serial t :components ((:file "package") (:file "km-rest")))"
    )
    if "%%p"=="km-threads" (
      set "content=(defsystem "km-threads" :description "Automatically generated system for km-threads" :author "Generated by build script" :version "0.1.0" :depends-on ("jsown" "bordeaux-threads") :serial t :components ((:file "package") (:file "km-threads")))"
    ) 
    if "%%p"=="km" (
      set "content=(defsystem "km" :description "Automatically generated system for km" :author "Generated by build script" :version "0.1.0" :serial t :components ((:file "package") (:file "km")))"
    )
    echo !content! > "%SRC_ROOT%\%%p.asd"
    if errorlevel 1 (
      echo Failed to generate %%p.asd due to redirection error
      pause
      exit /b 1
    )
    if not exist "%SRC_ROOT%\%%p.asd" (
      echo File %%p.asd was not created
      pause
      exit /b 1
    )
    echo Generated %%p.asd successfully
  ) else (
    echo No .lisp file found for %%p at !LISP_FILE!, skipping .asd generation.
  )
)
echo Finished Step 3.

REM Step 4: Copy Lisp files, .asd files, and package.lisp to respective local-projects folders
echo.
echo [4/5] Copying Lisp files, .asd files, and package.lisp to local-projects...
for %%p in (km km-threads km-rest) do (
  REM Create or clear target directory
  if exist "%LOCAL_PROJECTS%\%%p" (
    rmdir /S /Q "%LOCAL_PROJECTS%\%%p"
    if errorlevel 1 (
      echo Failed to clear existing directory %LOCAL_PROJECTS%\%%p
      pause
      exit /b 1
    )
  )
  mkdir "%LOCAL_PROJECTS%\%%p"
  if errorlevel 1 (
    echo Failed to create %LOCAL_PROJECTS%\%%p
    pause
    exit /b 1
  )
  REM Copy the .asd file if it exists
  if exist "%SRC_ROOT%\%%p.asd" (
    xcopy "%SRC_ROOT%\%%p.asd" "%LOCAL_PROJECTS%\%%p" /Y >nul 2>&1
    if errorlevel 1 (
      echo Failed to copy %%p.asd to %LOCAL_PROJECTS%\%%p
      pause
      exit /b 1
    )
    echo Copied %%p.asd to %LOCAL_PROJECTS%\%%p
  ) else (
    echo No %%p.asd found in %SRC_ROOT%, skipping copy.
  )
  REM Copy the .lisp file if it exists
  if exist "%SRC_ROOT%\%%p.lisp" (
    xcopy "%SRC_ROOT%\%%p.lisp" "%LOCAL_PROJECTS%\%%p" /Y >nul 2>&1
    if errorlevel 1 (
      echo Failed to copy %%p.lisp to %LOCAL_PROJECTS%\%%p
      pause
      exit /b 1
    )
    echo Copied %%p.lisp to %LOCAL_PROJECTS%\%%p
  ) else (
    echo No %%p.lisp found in %SRC_ROOT%, skipping copy.
  )
  REM Copy package.lisp if it exists in PACKAGES_ROOT\%%p\
  set "PACKAGE_FILE=%PACKAGES_ROOT%\%%p\package.lisp"
  if exist "!PACKAGE_FILE!" (
    xcopy "!PACKAGE_FILE!" "%LOCAL_PROJECTS%\%%p" /Y >nul 2>&1
    if errorlevel 1 (
      echo Failed to copy package.lisp from !PACKAGE_FILE! to %LOCAL_PROJECTS%\%%p
      pause
      exit /b 1
    )
    echo Copied package.lisp from !PACKAGE_FILE! to %LOCAL_PROJECTS%\%%p
  ) else (
    echo Warning: No package.lisp found at !PACKAGE_FILE!. Build may fail for %%p.
  )
)

REM Step 5: Register and verify packages with Quicklisp
echo.
echo [5/5] Registering packages with Quicklisp...
"%SBCL_PATH%" --load "%QUICKLISP_SETUP%" --eval "(ql:register-local-projects)" --quit
if errorlevel 1 (
  echo Failed to register local projects with Quicklisp
  pause
  exit /b 1
)
for %%p in (km km-threads km-rest) do (
  "%SBCL_PATH%" --load "%QUICKLISP_SETUP%" --eval "(handler-case (ql:quickload :%%p) (error (c) (format t \"Error loading %%p: ~a~%%\" c) (sb-ext:exit :code 1)))" --quit
  if errorlevel 1 (
    echo Failed to load package %%p. Check error messages above.
    pause
    exit /b 1
  )
  echo Successfully loaded %%p
)

echo.
echo Setup and build completed successfully!
pause
endlocal