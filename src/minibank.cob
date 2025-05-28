       IDENTIFICATION DIVISION.
       PROGRAM-ID. MiniBank.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT AccountFile ASSIGN TO "data/accounts.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS Acc-ID
               FILE STATUS IS WS-FILE-STATUS.
           SELECT DepositFile ASSIGN TO "data/deposit_request.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT StaticFile ASSIGN TO "data/static.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS Static-Key
               FILE STATUS IS WS-FILE-STATUS.

           SELECT TransactionFile ASSIGN TO "data/transaction.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS Txn-ID
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD AccountFile.
       01 Account-Record.
           05 Acc-ID       PIC X(10).
           05 Acc-Name     PIC X(20).
           05 Balance      PIC 9(12)V99.

       FD DepositFile.
       01 Deposit-Line     PIC X(80).

       FD StaticFile.
       01 Static-Record.
           05 Static-Key    PIC X(20).
           05 Static-Value  PIC X(20).

       FD TransactionFile.
       01 Txn-Record.
           05 Txn-ID           PIC 9(8).             *> Unique transaction number
           05 Txn-DateTime     PIC X(19).            *> yyyy-mm-dd hh:mm:ss
           05 Txn-Type         PIC X(10).            *> "Deposit" or "Withdraw"
           05 Txn-Account      PIC X(10).
           05 Txn-Amount       PIC 9(12)V99.

       WORKING-STORAGE SECTION.
       01 WS-CHOICE         PIC 9.
       01 WS-ID             PIC X(10).
       01 WS-NAME           PIC X(20).
       01 WS-AMOUNT         PIC 9(12)V99.
       01 WS-MODE           PIC X.  *> "D" or "W"
       01 WS-FILE-STATUS    PIC XX.
       01 WS-FOUND          PIC X VALUE 'N'.
          88 FOUND          VALUE 'Y'.
          88 NOT-FOUND      VALUE 'N'.
       01 WS-DEPOSIT-ID     PIC X(10).
       01 WS-DEPOSIT-AMOUNT PIC X(10).
       01 WS-DEP-AMOUNT-NUM PIC 9(12)V99.
 
       01 WS-LAST-ID-NUMERIC   PIC 9(10).
       01 WS-LAST-ID-STRING    PIC X(10).
       01 WS-TEMP-KEY          PIC X(20).
       01 WS-TEMP-VALUE        PIC X(20).

       01 WS-NOW             PIC X(19).
       01 WS-TXN-TYPE        PIC X(10).
       01 WS-TXN-AMOUNT      PIC 9(12)V99.
       01 WS-AMOUNT-TEXT PIC Z(15).99.
       01 Txn-Counter         PIC 9(8) VALUE 0.
       01 WS-NEXT-TXN-ID      PIC 9(8).

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM MENU-LOOP
           STOP RUN.

       MENU-LOOP.
           DISPLAY "=========================="
           DISPLAY "     MiniBank System     "
           DISPLAY "=========================="
           DISPLAY "1. Create Account"
           DISPLAY "2. Deposit"
           DISPLAY "3. Withdraw"
           DISPLAY "4. Process Deposit File"
           DISPLAY "5. Balance Check"
           DISPLAY "6. Display Accounts"
           DISPLAY "7. Delete Account"
           DISPLAY "8. Transaction Log View"
           DISPLAY "9. Exit"
           DISPLAY "Enter Choice: "
           ACCEPT WS-CHOICE
           EVALUATE WS-CHOICE
             WHEN 1 PERFORM CREATE-ACCOUNT
             WHEN 2 PERFORM DEPOSIT
             WHEN 3 PERFORM WITHDRAW
             WHEN 4 PERFORM PROCESS-DEPOSIT-FILE
             WHEN 5 PERFORM BALANCE-CHECK
             WHEN 6 PERFORM DISPLAY-ACCOUNTS
             WHEN 7 PERFORM DELETE-ACCOUNT
             WHEN 8 PERFORM TRANSACTION-LOG-VIEW
             WHEN 9
               DISPLAY "Goodbye!"
               STOP RUN
             WHEN OTHER
               DISPLAY "Invalid Choice"
           END-EVALUATE
           PERFORM MENU-LOOP.

       CREATE-ACCOUNT.
           MOVE "LAST-ID" TO Static-Key
           OPEN I-O StaticFile

            READ StaticFile
               INVALID KEY
                 MOVE 1 TO WS-LAST-ID-NUMERIC
             NOT INVALID KEY
                MOVE FUNCTION NUMVAL(Static-Value) TO WS-LAST-ID-NUMERIC
                 ADD 1 TO WS-LAST-ID-NUMERIC
           END-READ

         MOVE FUNCTION NUMVAL-C(WS-LAST-ID-NUMERIC) TO WS-LAST-ID-STRING
           MOVE WS-LAST-ID-STRING TO Acc-ID

           *> Update static
           MOVE WS-LAST-ID-STRING TO Static-Value
           REWRITE Static-Record
             INVALID KEY
                 WRITE Static-Record
           END-REWRITE
           
           CLOSE StaticFile

           DISPLAY "Enter Account Name: " ACCEPT WS-NAME
            MOVE 0 TO Balance
           MOVE WS-NAME TO Acc-Name

           OPEN I-O AccountFile
           WRITE Account-Record
           DISPLAY "Account Created with ID: " Acc-ID
           CLOSE AccountFile.

       DEPOSIT.
           MOVE "D" TO WS-MODE
           DISPLAY "Enter Account ID: " ACCEPT WS-ID
           DISPLAY "Enter Deposit Amount: " ACCEPT WS-AMOUNT
           PERFORM UPDATE-BALANCE.

       WITHDRAW.
           MOVE "W" TO WS-MODE
           DISPLAY "Enter Account ID: " ACCEPT WS-ID
           DISPLAY "Enter Withdraw Amount: " ACCEPT WS-AMOUNT
           PERFORM UPDATE-BALANCE.


       UPDATE-BALANCE.
           OPEN I-O AccountFile
           MOVE WS-ID TO Acc-ID
           READ AccountFile KEY IS Acc-ID
               INVALID KEY
                   DISPLAY "Account not found."
               NOT INVALID KEY
                   IF WS-MODE = "D"
                       ADD WS-AMOUNT TO Balance
                       REWRITE Account-Record
                       DISPLAY "Deposit successful."
                   ELSE
                       IF Balance >= WS-AMOUNT
                           SUBTRACT WS-AMOUNT FROM Balance
                           REWRITE Account-Record
                           DISPLAY "Withdrawal successful."
                       ELSE
                           DISPLAY "Insufficient funds."
                       END-IF
                   END-IF
           END-READ
           PERFORM LOG-TRANSACTION
           CLOSE AccountFile.

       PROCESS-DEPOSIT-FILE.
           MOVE "D" TO WS-MODE
           DISPLAY "Processing deposit file..."
           OPEN INPUT DepositFile
           PERFORM UNTIL WS-FILE-STATUS = "10"
               READ DepositFile INTO Deposit-Line
                   AT END MOVE "10" TO WS-FILE-STATUS
                   NOT AT END
                       PERFORM PARSE-DEPOSIT-LINE
                       MOVE WS-DEPOSIT-ID TO WS-ID
                       MOVE WS-DEP-AMOUNT-NUM TO WS-AMOUNT
                       PERFORM UPDATE-BALANCE
           END-PERFORM
           CLOSE DepositFile.

       PARSE-DEPOSIT-LINE.
           UNSTRING Deposit-Line
             DELIMITED BY "|"
             INTO WS-DEPOSIT-ID WS-DEPOSIT-AMOUNT
           END-UNSTRING
           MOVE FUNCTION NUMVAL(WS-DEPOSIT-AMOUNT) TO WS-DEP-AMOUNT-NUM.

       DISPLAY-ACCOUNTS.
           OPEN I-O AccountFile
           DISPLAY "Current Accounts:"
           DISPLAY "============ Start of Account List. ============". 
           PERFORM UNTIL WS-FILE-STATUS = "10"
               READ AccountFile INTO Account-Record
                   AT END MOVE "10" TO WS-FILE-STATUS
                   NOT AT END
           DISPLAY "ID: " Acc-ID " Name: " Acc-Name " Balance: " Balance
               END-READ
           END-PERFORM
           CLOSE AccountFile.
           DISPLAY "============ End of Account List. ============".

       BALANCE-CHECK.
            DISPLAY "Enter Account ID for Balance Check: " ACCEPT WS-ID
            OPEN I-O AccountFile
            MOVE WS-ID TO Acc-ID
            READ AccountFile KEY IS Acc-ID
              INVALID KEY
                   DISPLAY "Account not found."
              NOT INVALID KEY
               DISPLAY "Balance for Account ID " Acc-ID ": " Balance
            END-READ
            CLOSE AccountFile.

       DELETE-ACCOUNT.
           DISPLAY "Enter Account ID to Delete: "
           ACCEPT WS-ID
    
           OPEN I-O AccountFile
           MOVE WS-ID TO Acc-ID
    
           READ AccountFile KEY IS Acc-ID
            INVALID KEY
                DISPLAY "Account not found."
            NOT INVALID KEY
                DELETE AccountFile
                DISPLAY "Account deleted successfully."
           END-READ
    
           CLOSE AccountFile.

       
       GET-DATE-TIME.
           ACCEPT WS-NOW FROM TIME.
           MOVE FUNCTION CURRENT-DATE(1:4) TO WS-NOW(1:4)       *> YYYY
           MOVE "-" TO WS-NOW(5:1)
           MOVE FUNCTION CURRENT-DATE(5:2) TO WS-NOW(6:2)       *> MM
           MOVE "-" TO WS-NOW(8:1)
           MOVE FUNCTION CURRENT-DATE(7:2) TO WS-NOW(9:2)       *> DD
           MOVE " " TO WS-NOW(11:1)
           MOVE FUNCTION CURRENT-DATE(9:2) TO WS-NOW(12:2)      *> HH
           MOVE ":" TO WS-NOW(14:1)
           MOVE FUNCTION CURRENT-DATE(11:2) TO WS-NOW(15:2)     *> MM
           MOVE ":" TO WS-NOW(17:1)
           MOVE FUNCTION CURRENT-DATE(13:2) TO WS-NOW(18:2)     *> SS
           DISPLAY " ".
       
       LOG-TRANSACTION.
           PERFORM GET-DATE-TIME
       
           MOVE "LAST-TXN-ID" TO Static-Key
           OPEN I-O StaticFile
           READ StaticFile
               INVALID KEY
                   MOVE 1 TO Txn-Counter
               NOT INVALID KEY
                   MOVE FUNCTION NUMVAL(Static-Value) TO Txn-Counter
                   ADD 1 TO Txn-Counter
           END-READ
       
           MOVE FUNCTION NUMVAL-C(Txn-Counter) TO Static-Value
           REWRITE Static-Record
               INVALID KEY
                   WRITE Static-Record
           END-REWRITE
           CLOSE StaticFile
       
           MOVE Txn-Counter TO Txn-ID
           MOVE WS-ID       TO Txn-Account
           MOVE WS-AMOUNT   TO Txn-Amount
           MOVE WS-NOW      TO Txn-DateTime
           IF WS-MODE = "D"
               MOVE "Deposit" TO Txn-Type
           ELSE
               MOVE "Withdraw" TO Txn-Type
           END-IF
       
           OPEN I-O TransactionFile
           WRITE Txn-Record
               INVALID KEY DISPLAY "Failed to log transaction."
               NOT INVALID KEY DISPLAY "Transaction ID: " Txn-ID
           END-WRITE
           CLOSE TransactionFile.

       TRANSACTION-LOG-VIEW.
           OPEN INPUT TransactionFile
           PERFORM UNTIL WS-FILE-STATUS = "10"
               READ TransactionFile INTO Txn-Record
                   AT END MOVE "10" TO WS-FILE-STATUS
                   NOT AT END
                       DISPLAY "-----------*******-----------" 
                       DISPLAY "Transaction ID: " Txn-ID
                       DISPLAY "Account ID: " Txn-Account
                       DISPLAY "Amount: " Txn-Amount
                       DISPLAY "Date/Time: " Txn-DateTime
                       DISPLAY "Type: " Txn-Type
                       DISPLAY "-----------*******-----------"
               END-READ
           END-PERFORM
           CLOSE TransactionFile.
