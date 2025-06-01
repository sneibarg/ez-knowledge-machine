#!/bin/bash

PROJECT_ROOT="/mnt/e/VSCode/ez-knowledge-machine/src"
QUICKLISP_DIR="/mnt/e/Quicklisp"
SBCL_PATH="/mnt/c/Program Files/Steel Bank Common Lisp/sbcl"

echo "Starting KM project build..."
echo

echo "[1/4] Registering local projects..."
"$SBCL_PATH" --load "$QUICKLISP_DIR/setup.lisp" --eval "(ql:register-local-projects)" --quit
if [ $? -ne 0 ]; then
    echo "Failed to register local projects."
    read -p "Press Enter to continue..."
    exit 1
fi

echo "[2/4] Building km-rest..."
"$SBCL_PATH" --load "$QUICKLISP_DIR/setup.lisp" --eval "(handler-case (ql:quickload :km-rest) (error (c) (format t \"Error: ~a~%\" c) (sb-ext:exit :code 1)))" --quit
if [ $? -ne 0 ]; then
    echo "Failed to build km-utils. Check error messages above."
    read -p "Press Enter to continue..."
    exit 1
fi

echo "[3/4] Building km-threads..."
"$SBCL_PATH" --load "$QUICKLISP_DIR/setup.lisp" --eval "(handler-case (ql:quickload :km-threads) (error (c) (format t \"Error: ~a~%\" c) (sb-ext:exit :code 1)))" --quit
if [ $? -ne 0 ]; then
    echo "Failed to build km-core. Check error messages above."
    read -p "Press Enter to continue..."
    exit 1
fi

echo "[4/4] Building km..."
"$SBCL_PATH" --load "$QUICKLISP_DIR/setup.lisp" --eval "(handler-case (ql:quickload :km) (error (c) (format t \"Error: ~a~%\" c) (sb-ext:exit :code 1)))" --quit
if [ $? -ne 0 ]; then
    echo "Failed to build km-core. Check error messages above."
    read -p "Press Enter to continue..."
    exit 1
fi

echo
echo "Build completed successfully!"
read -p "Press Enter to continue..."