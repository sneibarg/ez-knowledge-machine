@echo off
set "PROJECT_ROOT=E:\IdeaProjects\ez-knowledge-machine"
set "QUICKLISP_DIR=E:\Quicklisp"
set "SBCL_PATH=C:\Program Files\Steel Bank Common Lisp\sbcl.exe"

echo Starting KM project build...
echo.

echo [1/3] Registering local projects...
"%SBCL_PATH%" --load "%QUICKLISP_DIR%\setup.lisp" --eval "(ql:register-local-projects)" --quit
if errorlevel 1 (
  echo Failed to register local projects.
  pause
  exit /b 1
)

echo [2/3] Building km-utils...
"%SBCL_PATH%" --load "%QUICKLISP_DIR%\setup.lisp" --eval "(handler-case (ql:quickload :km-utils) (error (c) (format t \"Error: ~a~%%\" c) (sb-ext:exit :code 1)))" --quit
if errorlevel 1 (
  echo Failed to build km-utils. Check error messages above.
  pause
  exit /b 1
)

echo [3/3] Building km-core...
"%SBCL_PATH%" --load "%QUICKLISP_DIR%\setup.lisp" --eval "(handler-case (ql:quickload :km-core) (error (c) (format t \"Error: ~a~%%\" c) (sb-ext:exit :code 1)))" --quit
if errorlevel 1 (
  echo Failed to build km-core. Check error messages above.
  pause
  exit /b 1
)

echo.
echo Build completed successfully!
pause