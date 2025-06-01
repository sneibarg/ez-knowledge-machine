#!/bin/bash

# Default Lisp runtime
DEFAULT_LISP="sbcl"
LISP_RUNTIME="$DEFAULT_LISP"
PROJECT_ROOT="/home/som/ez-knowledge-machine/src"
QUICKLISP_DIR="$HOME/.quicklisp"
SBCL_PATH="/usr/local/bin/sbcl"
GCL_PATH="/usr/local/bin/gcl"

# Parse command-line options
while [[ $# -gt 0 ]]; do
    case "$1" in
        --lisp)
            LISP_RUNTIME="$2"
            shift 2
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

# Determine Lisp executable
case "$LISP_RUNTIME" in
    sbcl)
        LISP_EXEC="$SBCL_PATH"
        ;;
    gcl)
        LISP_EXEC="$GCL_PATH"
        ;;
    *)
        echo "Unsupported Lisp runtime: $LISP_RUNTIME. Supported: sbcl, gcl"
        exit 1
        ;;
esac

# Verify Lisp executable exists
if ! command -v "$LISP_EXEC" >/dev/null 2>&1; then
    echo "Error: $LISP_RUNTIME not found at $LISP_EXEC. Please install it."
    exit 1
fi

echo "Starting KM project build using $LISP_RUNTIME..."
echo

echo "[1/4] Registering local projects..."
"$LISP_EXEC" --load "$QUICKLISP_DIR/setup.lisp" --eval "(ql:register-local-projects)" --quit
if [ $? -ne 0 ]; then
    echo "Failed to register local projects."
    read -p "Press Enter to continue..."
    exit 1
fi

echo "[2/4] Building km-rest..."
"$LISP_EXEC" --load "$QUICKLISP_DIR/setup.lisp" --eval "(handler-case (ql:quickload :km-rest) (error (c) (format t \"Error: ~a~%\" c) (uiop:quit 1)))" --quit
if [ $? -ne 0 ]; then
    echo "Failed to build km-rest. Check error messages above."
    read -p "Press Enter to continue..."
    exit 1
fi

echo "[3/4] Building km-threads..."
"$LISP_EXEC" --load "$QUICKLISP_DIR/setup.lisp" --eval "(handler-case (ql:quickload :km-threads) (error (c) (format t \"Error: ~a~%\" c) (uiop:quit 1)))" --quit
if [ $? -ne 0 ]; then
    echo "Failed to build km-threads. Check error messages above."
    read -p "Press Enter to continue..."
    exit 1
fi

echo "[4/4] Building km..."
"$LISP_EXEC" --load "$QUICKLISP_DIR/setup.lisp" --eval "(handler-case (ql:quickload :km) (error (c) (format t \"Error: ~a~%\" c) (uiop:quit 1)))" --quit
if [ $? -ne 0 ]; then
    echo "Failed to build km. Check error messages above."
    read -p "Press Enter to continue..."
    exit 1
fi

echo
echo "Build completed successfully using $LISP_RUNTIME!"
read -p "Press Enter to continue..."