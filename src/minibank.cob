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

       DATA DIVISION.
       FILE SECTION.
       FD AccountFile.
       01 Account-Record.
           05 Acc-ID       PIC X(10).
           05 Acc-Name     PIC X(20).
           05 Balance      PIC 9(6)V99.

       FD DepositFile.
       01 Deposit-Line     PIC X(80).

       WORKING-STORAGE SECTION.
       01 WS-CHOICE         PIC 9.
       01 WS-ID             PIC X(10).
       01 WS-NAME           PIC X(20).
       01 WS-AMOUNT         PIC 9(6)V99.
       01 WS-MODE           PIC X.  *> "D" or "W"
       01 WS-FILE-STATUS    PIC XX.
       01 WS-FOUND          PIC X VALUE 'N'.
          88 FOUND          VALUE 'Y'.
          88 NOT-FOUND      VALUE 'N'.
       01 WS-DEPOSIT-ID     PIC X(10).
       01 WS-DEPOSIT-AMOUNT PIC X(10).
       01 WS-DEP-AMOUNT-NUM PIC 9(6)V99.

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
           DISPLAY "5. Process Deposit File"
           DISPLAY "6. Exit"
           DISPLAY "Enter Choice: "
           ACCEPT WS-CHOICE
           EVALUATE WS-CHOICE
             WHEN 1 PERFORM CREATE-ACCOUNT
             WHEN 2 PERFORM DEPOSIT
             WHEN 3 PERFORM WITHDRAW
             WHEN 4 PERFORM PROCESS-DEPOSIT-FILE
             WHEN 5 PERFORM DESPLAY-ACCOUNTS
             WHEN 6
               DISPLAY "Goodbye!"
               STOP RUN
             WHEN OTHER
               DISPLAY "Invalid Choice"
           END-EVALUATE
           PERFORM MENU-LOOP.

       CREATE-ACCOUNT.
           DISPLAY "Enter Account ID: " ACCEPT WS-ID
           DISPLAY "Enter Account Name: " ACCEPT WS-NAME
           MOVE 0 TO Balance
           MOVE WS-ID   TO Acc-ID
           MOVE WS-NAME TO Acc-Name
           OPEN I-O AccountFile
           READ AccountFile KEY IS Acc-ID
               INVALID KEY
                   WRITE Account-Record
                   DISPLAY "Account Created!"
               NOT INVALID KEY
                   DISPLAY "Account already exists."
           END-READ
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

       DESPLAY-ACCOUNTS.
           OPEN I-O AccountFile
           DISPLAY "Current Accounts:"
           PERFORM UNTIL WS-FILE-STATUS = "10"
               READ AccountFile INTO Account-Record
                   AT END MOVE "10" TO WS-FILE-STATUS
                   NOT AT END
           DISPLAY "ID: " Acc-ID " Name: " Acc-Name " Balance: " Balance
               END-READ
           END-PERFORM
           CLOSE AccountFile.
           