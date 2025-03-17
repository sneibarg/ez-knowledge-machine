# KM Project for IntelliJ SLT
## Setup
1. Run `scripts/setup.sh` to initialize.
2. Open `km-project/` in IntelliJ IDEA.
3. Configure SLT with SBCL and Quicklisp.
4. Load systems via REPL (e.g., `(asdf:load-system :km-core)`).
## Structure
- `packages/`: Individual KM packages.
- `lib/`: Quicklisp dependencies.
- `scripts/`: Automation tools.