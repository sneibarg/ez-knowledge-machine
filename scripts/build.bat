@echo off
set "PROJECT_ROOT=E:\VSCode\ez-knowledge-machine\src"
set "QUICKLISP_DIR=E:\Quicklisp"
set "SBCL_PATH=C:\Program Files\Steel Bank Common Lisp\sbcl.exe"

echo Starting KM project build...
echo.

echo [1/4] Registering local projects...
"%SBCL_PATH%" --load "%QUICKLISP_DIR%\setup.lisp" --eval "(ql:register-local-projects)" --quit
if errorlevel 1 (
  echo Failed to register local projects.
  pause
  exit /b 1
)

echo [2/4] Building km-rest...
"%SBCL_PATH%" --load "%QUICKLISP_DIR%\setup.lisp" --eval "(handler-case (ql:quickload :km-rest) (error (c) (format t \"Error: ~a~%%\" c) (sb-ext:exit :code 1)))" --quit
if errorlevel 1 (
  echo Failed to build km-utils. Check error messages above.
  pause
  exit /b 1
)

echo [3/4] Building km-threads...
"%SBCL_PATH%" --load "%QUICKLISP_DIR%\setup.lisp" --eval "(handler-case (ql:quickload :km-threads) (error (c) (format t \"Error: ~a~%%\" c) (sb-ext:exit :code 1)))" --quit
if errorlevel 1 (
  echo Failed to build km-core. Check error messages above.
  pause
  exit /b 1
)

echo [4/4] Building km...
"%SBCL_PATH%" --load "%QUICKLISP_DIR%\setup.lisp" --eval "(handler-case (ql:quickload :km) (error (c) (format t \"Error: ~a~%%\" c) (sb-ext:exit :code 1)))" --quit
if errorlevel 1 (
  echo Failed to build km-core. Check error messages above.
  pause
  exit /b 1
)

echo.
echo Build completed successfully!
pause