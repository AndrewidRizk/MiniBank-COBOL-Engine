from rest_framework.decorators import api_view
from rest_framework.response import Response
from rest_framework import status
import subprocess
import os
import re

# === Helper ===

def run_cobol_with_inputs(inputs):
    input_data = "\n".join(inputs) + "\n"
    root_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
    run_bat_path = os.path.join(root_dir, "run.bat")
    try:
        result = subprocess.run(
            [run_bat_path], 
            input=input_data,
            capture_output=True,
            text=True,
            timeout=10,
            shell=True
        )
        return result.stdout
    except subprocess.TimeoutExpired:
        return "COBOL execution timed out"
    except Exception as e:
        return f"Error: {str(e)}"


# === Endpoints ===

@api_view(["POST"])
def deposit_view(request):
    acc_id = request.data.get("account_id")
    amount = request.data.get("amount")
    print(f"Deposit request: acc_id={acc_id}, amount={amount}")
    if not acc_id or not amount:
        return Response({"error": "Missing fields"}, status=400)

    inputs = [
        "2", acc_id, str(amount), "8"
    ]

    cobol_output = run_cobol_with_inputs(inputs)

    return Response({
        "message": "Deposit processed.",
        "cobol_output": cobol_output
    }, status=201)


@api_view(["POST"])
def withdraw_view(request):
    acc_id = request.data.get("account_id")
    amount = request.data.get("amount")

    if not acc_id or not amount:
        return Response({"error": "Missing fields"}, status=400)

    inputs = [
        "3", acc_id, str(amount), "8"
    ]

    cobol_output = run_cobol_with_inputs(inputs)

    return Response({
        "message": "Withdraw processed.",
        "cobol_output": cobol_output
    }, status=201)


@api_view(["POST"])
def create_account_view(request):
    name = request.data.get("name")

    if not name:
        return Response({"error": "Missing name"}, status=400)

    inputs = [
        "1", name, "8"
    ]

    cobol_output = run_cobol_with_inputs(inputs)

    return Response({
        "message": "Account created.",
        "cobol_output": cobol_output
    }, status=201)


@api_view(["GET"])
def account_balance_view(request, account_id):
    inputs = [
        "5", account_id
    ]

    cobol_output = run_cobol_with_inputs(inputs)

    # Optional parse
    #match = re.search(r"Balance.*?:\s+([0-9.]+)", cobol_output)
    #balance = match.group(1) if match else "Unknown"

    return Response({
        "account_id": account_id,
        "balance": cobol_output,
        "raw_output": cobol_output
    })

 

@api_view(["GET"])
def accounts_view(request):
    output = run_cobol_with_inputs(["6"])

    # REGEX to match account lines
    account_pattern = re.compile(
        r"ID:\s*(\d+)\s+Name:\s*(.*?)\s+Balance:\s*([\d\.]+)"
    )

    accounts = []

    for match in account_pattern.finditer(output):
        account_id, name, balance = match.groups()
        accounts.append({
            "account_id": account_id,
            "name": name,
            "balance": float(balance)
        })

    return Response({"accounts": accounts})




@api_view(["GET"])
def transactions_view(request):
    output = run_cobol_with_inputs(["8"])

    # REGEX to match transaction blocks
    txn_pattern = re.compile(
        r"Transaction ID:\s*(\d+)\s*"
        r"Account ID:\s*(.*?)\s*"
        r"Amount:\s*([\d\.]+)\s*"
        r"Date/Time:\s*(.*?)\s*"
        r"Type:\s*(\w+)",
        re.DOTALL
    )

    transactions = []

    for match in txn_pattern.finditer(output):
        txn_id, account_id, amount, date_time, txn_type = match.groups()
        transactions.append({
            "transaction_id": txn_id,
            "account_id": account_id,
            "amount": float(amount),
            "date_time": date_time,
            "type": txn_type
        })
    print(f"Transactions found: {transactions}")

    return Response({"transactions": transactions})


@api_view(["GET"])
def account_transactions_view(request, account_id):
    try:
        tx_file = os.path.join(os.path.abspath(os.path.join(os.path.dirname(__file__), "..")), "data", "transaction.dat")
        
        if not os.path.exists(tx_file):
            return Response({"transactions": []})

        with open(tx_file, "rb") as f:
            data = f.read()

        # You can parse the transactions and filter by account_id
        # For now, just return full data — filtering can be added later
        return Response({
            "account_id": account_id,
            "transactions_raw": str(data)
        })

    except Exception as e:
        return Response({"error": str(e)}, status=500)
