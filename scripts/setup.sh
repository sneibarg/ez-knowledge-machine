#!/bin/bash
# Setup KM environment for IntelliJ SLT
echo "Initializing KM project structure..."

# Create directories
mkdir -p src/km-core src/km-utils src/km-ai lib scripts

# Install Quicklisp if not present
if [ ! -d "~/quicklisp" ]; then
  curl -O https://beta.quicklisp.org/quicklisp.lisp
  sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install)' --quit
  sbcl --load ~/quicklisp/setup.lisp --eval '(ql:add-to-init-file)' --quit
fi
