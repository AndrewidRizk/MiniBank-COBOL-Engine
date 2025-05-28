       IDENTIFICATION DIVISION.
       PROGRAM-ID. MiniBank.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT AccountFile ASSIGN TO "data/accounts.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT DepositFile ASSIGN TO "data/deposit_request.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD AccountFile.
       01 Account-Record.
           05 Acc-ID      PIC X(10).
           05 Acc-Name    PIC X(20).
           05 Balance     PIC 9(6)V99.

       FD DepositFile.
       01 Deposit-Line   PIC X(80).

       WORKING-STORAGE SECTION.
       01 WS-CHOICE       PIC 9.
       01 WS-ID           PIC X(10).
       01 WS-NAME         PIC X(20).
       01 WS-AMOUNT       PIC 9(6)V99.
       01 WS-FILE-END     PIC X VALUE 'N'.
          88 EOF          VALUE 'Y'.
          88 NOT-EOF      VALUE 'N'.
       01 WS-FOUND        PIC X VALUE 'N'.
          88 FOUND        VALUE 'Y'.
          88 NOT-FOUND    VALUE 'N'.
       01 WS-MODE         PIC X.            *> "D" or "W"
       01 WS-DEPOSIT-ID   PIC X(10).
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
           DISPLAY "5. Exit"
           DISPLAY "Enter Choice: "
           ACCEPT WS-CHOICE
           EVALUATE WS-CHOICE
             WHEN 1 PERFORM CREATE-ACCOUNT
             WHEN 2 PERFORM DEPOSIT
             WHEN 3 PERFORM WITHDRAW
             WHEN 4 PERFORM PROCESS-DEPOSIT-FILE
             WHEN 5
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
           OPEN EXTEND AccountFile
           WRITE Account-Record
           CLOSE AccountFile
           DISPLAY "Account Created!".

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
           MOVE NOT-FOUND TO WS-FOUND
           PERFORM UNTIL EOF
               READ AccountFile
                 AT END SET EOF TO TRUE
                 NOT AT END
                   IF Acc-ID = WS-ID THEN
                     SET FOUND TO TRUE
                     IF WS-MODE = "D" THEN
                       ADD WS-AMOUNT TO Balance
                     ELSE
                       SUBTRACT WS-AMOUNT FROM Balance
                     END-IF
                     REWRITE Account-Record
                   END-IF
           END-PERFORM
           CLOSE AccountFile
           IF FOUND
             DISPLAY "Transaction complete."
           ELSE
             DISPLAY "Account not found."
           END-IF.

       PROCESS-DEPOSIT-FILE.
           MOVE "D" TO WS-MODE
           DISPLAY "Processing deposit file..."
           OPEN INPUT DepositFile
           PERFORM UNTIL EOF
               READ DepositFile INTO Deposit-Line
                 AT END SET EOF TO TRUE
                 NOT AT END
                   PERFORM PARSE-DEPOSIT-LINE
                   MOVE WS-DEPOSIT-ID     TO WS-ID
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
