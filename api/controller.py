import subprocess
import os

data_path = os.path.abspath(os.path.join(os.path.dirname(__file__), '../data'))
run_script = os.path.abspath(os.path.join(os.path.dirname(__file__), '../run.sh'))

def write_deposit_request(account_id, amount):
    deposit_file = os.path.join(data_path, 'deposit_requests.txt')
    with open(deposit_file, 'w') as f:
        f.write(f"{account_id}|{amount}\n")

def run_cobol():
    """Run the COBOL engine."""
    result = subprocess.run(run_script, shell=True, capture_output=True, text=True)
    return result.stdout