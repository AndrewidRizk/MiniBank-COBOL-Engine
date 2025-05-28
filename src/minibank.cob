       IDENTIFICATION DIVISION.
       PROGRAM-ID. MiniBank.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT AccountFile ASSIGN TO "accounts.dat"
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
       01 WS-CHOICE        PIC 9.
       01 WS-ID            PIC X(10).
       01 WS-NAME          PIC X(20).
       01 WS-AMOUNT        PIC 9(6)V99.
       01 WS-ACTION        PIC X.
       01 WS-END-OF-FILE   PIC X VALUE 'N'.
          88 EOF           VALUE 'Y'.
          88 NOT-EOF       VALUE 'N'.
       01 WS-FOUND         PIC X VALUE 'N'.
          88 FOUND         VALUE 'Y'.
          88 NOT-FOUND     VALUE 'N'.
       01 WS-DEP-ID        PIC X(10).
       01 WS-DEP-AMOUNT    PIC X(80).

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM MENU-LOOP
           STOP RUN.

       MENU-LOOP.
           DISPLAY "==== MiniBank Menu ===="
           DISPLAY "1. Create Account"
           DISPLAY "2. Deposit (manual)"
           DISPLAY "3. Withdraw"
           DISPLAY "4. Process Deposit File"
           DISPLAY "5. Exit"
           DISPLAY "Enter choice (1-5):" WITH NO ADVANCING
           ACCEPT WS-CHOICE

           EVALUATE WS-CHOICE
             WHEN 1 PERFORM CREATE-ACCOUNT
             WHEN 2 PERFORM DO-DEPOSIT
             WHEN 3 PERFORM DO-WITHDRAW
             WHEN 4 PERFORM PROCESS-DEPOSIT-FILE
             WHEN 5
               DISPLAY "Goodbye!"
               STOP RUN
             WHEN OTHER
               DISPLAY "Invalid choice"
           END-EVALUATE
           PERFORM MENU-LOOP.

       CREATE-ACCOUNT.
           DISPLAY "Enter Account ID: "    WITH NO ADVANCING
           ACCEPT WS-ID
           DISPLAY "Enter Account Name: "  WITH NO ADVANCING
           ACCEPT WS-NAME

           MOVE WS-ID    TO Acc-ID
           MOVE WS-NAME  TO Acc-Name
           MOVE 0        TO Balance

           OPEN OUTPUT AccountFile
           WRITE Account-Record
           CLOSE AccountFile

           DISPLAY "Account created.".

       DO-DEPOSIT.
           DISPLAY "Enter Account ID: "       WITH NO ADVANCING
           ACCEPT WS-ID
           DISPLAY "Enter Deposit Amount: "   WITH NO ADVANCING
           ACCEPT WS-AMOUNT
           MOVE 'D' TO WS-ACTION
           PERFORM UPDATE-BALANCE.

       DO-WITHDRAW.
           DISPLAY "Enter Account ID: "       WITH NO ADVANCING
           ACCEPT WS-ID
           DISPLAY "Enter Withdraw Amount: "  WITH NO ADVANCING
           ACCEPT WS-AMOUNT
           MOVE 'W' TO WS-ACTION
           PERFORM UPDATE-BALANCE.

       UPDATE-BALANCE.
           OPEN I-O AccountFile
           SET NOT-FOUND   TO WS-FOUND
           SET NOT-EOF     TO WS-END-OF-FILE

           PERFORM UNTIL WS-FOUND OR EOF
             READ AccountFile
               AT END
                 SET EOF TO TRUE
               NOT AT END
                 IF Acc-ID = WS-ID
                   SET FOUND TO TRUE
                   IF WS-ACTION = 'D'
                     ADD WS-AMOUNT TO Balance
                   ELSE
                     SUBTRACT WS-AMOUNT FROM Balance
                   END-IF
                   REWRITE Account-Record
                 END-IF
             END-READ
           END-PERFORM

           CLOSE AccountFile

           IF FOUND
             DISPLAY "Transaction complete."
           ELSE
             DISPLAY "Account not found."
           END-IF.

       PROCESS-DEPOSIT-FILE.
           DISPLAY "Processing deposit file..."
           OPEN INPUT DepositFile
           SET NOT-EOF TO WS-END-OF-FILE

           PERFORM UNTIL EOF
             READ DepositFile INTO Deposit-Line
               AT END
                 SET EOF TO TRUE
               NOT AT END
                 PERFORM PARSE-DEPOSIT
                 MOVE 'D' TO WS-ACTION
                 PERFORM UPDATE-BALANCE
           END-PERFORM

           CLOSE DepositFile.

       PARSE-DEPOSIT.
           UNSTRING Deposit-Line DELIMITED BY "|"
             INTO WS-DEP-ID WS-DEP-AMOUNT
           END-UNSTRING
           MOVE FUNCTION NUMVAL(WS-DEP-AMOUNT) TO WS-AMOUNT
           MOVE WS-DEP-ID                      TO WS-ID.
