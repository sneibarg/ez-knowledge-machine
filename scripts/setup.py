import re
import os
import subprocess

# Function to scan dependencies in a Lisp file (unchanged from original)
def scan_dependencies(lisp_file_path):
    """Scan a Lisp file for dependencies."""
    dependencies = []
    try:
        with open(lisp_file_path, 'r') as file:
            content = file.read()
            # Look for common dependency patterns like (ql:quickload "package")
            matches = re.findall(r'ql:quickload\s*[\'"]?([a-zA-Z0-9-_]+)[\'"]?', content)
            dependencies = list(set(matches))  # Remove duplicates
    except Exception as e:
        print(f"Error scanning dependencies: {e}")
    return dependencies

# Function to generate ASD content (unchanged from original)
def generate_asd(system_name, version, author, license, description, dependencies, component_path):
    """Generate the content for an ASD file."""
    dependencies_str = ', '.join(f':{dep}' for dep in dependencies) if dependencies else ''
    asd_template = f"""(asdf:defsystem "{system_name}"
  :description "{description}"
  :author "{author}"
  :license "{license}"
  :version "{version}"
  :depends-on ({dependencies_str})
  :components ((:file "{component_path}")))"""
    return asd_template

# Function to install Quicklisp
def install_quicklisp():
    """Download and install Quicklisp if not already present."""
    quicklisp_url = "https://beta.quicklisp.org/quicklisp.lisp"
    quicklisp_installer = "quicklisp.lisp"
    
    print("Installing Quicklisp...")
    try:
        # Download the Quicklisp installer
        subprocess.run(["curl", "-O", quicklisp_url], check=True)
        
        # Install Quicklisp using SBCL
        subprocess.run([
            "sbcl", 
            "--load", quicklisp_installer, 
            "--eval", "(quicklisp-quickstart:install)", 
            "--quit"
        ], check=True)
        
        # Clean up the installer file
        os.remove(quicklisp_installer)
        print("Quicklisp installed successfully.")
    except subprocess.CalledProcessError as e:
        print(f"Error installing Quicklisp: {e}")
    except Exception as e:
        print(f"Unexpected error: {e}")

# Function to load project-specific libraries
def load_libraries(libraries):
    """Load specified libraries using Quicklisp."""
    print("Loading project libraries...")
    for lib in libraries:
        try:
            subprocess.run([
                "sbcl", 
                "--eval", f"(ql:quickload :{lib})", 
                "--quit"
            ], check=True)
            print(f"Loaded library: {lib}")
        except subprocess.CalledProcessError as e:
            print(f"Error loading library {lib}: {e}")

# Function to set environment variables
def set_environment_variables():
    """Set environment variables for the Lisp project."""
    os.environ["LISP_PROJECT_PATH"] = os.path.abspath(".")
    print(f"Set LISP_PROJECT_PATH to {os.environ['LISP_PROJECT_PATH']}")
    # Add more environment variables as needed based on your setup.bat

# Main function
def main():
    print("Welcome to the Enhanced ASD Generator")
    
    # Step 1: Set up environment variables
    set_environment_variables()
    
    # Step 2: Check and install Quicklisp if not present
    quicklisp_dir = os.path.expanduser("~/quicklisp")
    if not os.path.exists(quicklisp_dir):
        print("Quicklisp not found. Installing...")
        install_quicklisp()
    else:
        print("Quicklisp already installed.")

    # Step 3: Load project-specific libraries (customize this list as needed)
    project_libraries = ["hunchentoot", "cl-json"]  # Example libraries
    load_libraries(project_libraries)
    
    # Step 4: Collect user inputs for ASD generation
    system_name = input("Enter the system name: ").strip()
    if not system_name:
        print("Error: System name cannot be empty.")
        return
    
    version = input("Enter the version: ").strip()
    author = input("Enter the author: ").strip()
    license = input("Enter the license: ").strip()
    description = input("Enter the description: ").strip()
    lisp_file_path = input("Enter the path to the Lisp file: ").strip()
    
    # Verify the Lisp file exists
    if not os.path.exists(lisp_file_path):
        print(f"Error: File '{lisp_file_path}' does not exist.")
        return
    
    # Step 5: Scan for dependencies in the Lisp file
    dependencies = scan_dependencies(lisp_file_path)
    if not dependencies:
        print("No dependencies found in the Lisp file.")
    
    # Step 6: Compute the component path for the ASD file
    relative_path = os.path.relpath(lisp_file_path, os.getcwd())
    component_path = os.path.splitext(relative_path)[0]  # Remove .lisp extension
    if os.name == 'nt':
        component_path = component_path.replace('\\', '/')  # Use forward slashes for ASDF
    
    # Step 7: Generate the ASD content
    asd_content = generate_asd(system_name, version, author, license, description, dependencies, component_path)
    
    # Step 8: Write the ASD file
    asd_file_name = f"{system_name}.asd"
    if os.path.exists(asd_file_name):
        overwrite = input(f"'{asd_file_name}' already exists. Overwrite? (y/n): ").lower()
        if overwrite != 'y':
            print("Aborted.")
            return
    
    try:
        with open(asd_file_name, 'w') as asd_file:
            asd_file.write(asd_content)
        print(f"ASD file '{asd_file_name}' generated successfully.")
    except Exception as e:
        print(f"Error writing ASD file: {e}")

if __name__ == "__main__":
    main()