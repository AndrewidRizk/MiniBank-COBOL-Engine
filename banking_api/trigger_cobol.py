import os
import subprocess

def run_cobol_engine():
    try:
        # Compute correct absolute path
        root_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
        run_bat_path = os.path.join(root_dir, "run.bat")
        input_data = "4\n8\n"

        result = subprocess.run(
            [run_bat_path], 
            input=input_data,
            capture_output=True,
            text=True,
            timeout=10,
            shell=True
        )
        return {
            "stdout": result.stdout,
            "stderr": result.stderr,
            "returncode": result.returncode
        }
    except subprocess.TimeoutExpired:
        return "COBOL execution timed out"
    except Exception as e:
        return f"Error: {str(e)}"
