def parse_accounts():
    accounts = []
    try:
        with open('../data/accounts.dat', 'rb') as f:
            while chunk := f.read(42):  # ID(10) + Name(20) + Balance(12)
                acc_id = chunk[0:10].decode('utf-8').strip()
                acc_name = chunk[10:30].decode('utf-8').strip()
                balance = chunk[30:42].decode('utf-8').strip()
                accounts.append({
                    "account_id": acc_id,
                    "name": acc_name,
                    "balance": balance
                })
    except FileNotFoundError:
        pass
    return accounts

def parse_transactions():
    txns = []
    try:
        with open('../data/transaction.dat', 'rb') as f:
            while chunk := f.read(57):  # ID(8) + DateTime(19) + Type(10) + Account(10) + Amount(10)
                txn_id = chunk[0:8].decode('utf-8').strip()
                dt = chunk[8:27].decode('utf-8').strip()
                typ = chunk[27:37].decode('utf-8').strip()
                acc = chunk[37:47].decode('utf-8').strip()
                amt = chunk[47:57].decode('utf-8').strip()
                txns.append({
                    "transaction_id": txn_id,
                    "datetime": dt,
                    "type": typ,
                    "account_id": acc,
                    "amount": amt
                })
    except FileNotFoundError:
        pass
    return txns