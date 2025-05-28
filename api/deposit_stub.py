# deposit_stub.py
data = "0000000001|125.50\n"

with open("../data/deposit_request.txt", "w") as f:
    f.write(data)

print("Deposit request written.")
