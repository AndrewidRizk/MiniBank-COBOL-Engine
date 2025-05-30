curl -X POST http://127.0.0.1:8000/api/deposit/ ^
     -H "Content-Type: application/json" ^
     -d "{\"account_id\": \"000000004\", \"amount\": 100500}"

curl -X GET http://127.0.0.1:8000/api/account/000000004/balance/ ^
     -H "Content-Type: application/json" ^ 