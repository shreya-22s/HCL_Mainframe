
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INTTEST.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE
               ASSIGN TO "testdata.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-IN-STATUS.

           SELECT REPORT-FILE
               ASSIGN TO "report.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD INPUT-FILE.
       01 INPUT-RECORD             PIC X(16).

       FD REPORT-FILE.
       01 REPORT-RECORD            PIC X(100).

       WORKING-STORAGE SECTION.
       01 WS-IN-STATUS             PIC XX.
       01 WS-OUT-STATUS            PIC XX.

       01 WS-TEST-ID               PIC X(4).
       01 WS-TRANS-TYPE            PIC X(2).
       01 WS-TRANS-AMOUNT          PIC X(8).
       01 WS-ACCOUNT-STATUS        PIC X.
       01 WS-EXPECTED-RESULT       PIC X.

       01 WS-ACTUAL-RESULT         PIC X.
       01 WS-VERDICT               PIC X(4).
       01 WS-EOF                   PIC X VALUE 'N'.

       01 WS-TOTAL                 PIC 9(4) VALUE 0.
       01 WS-PASSED                PIC 9(4) VALUE 0.
       01 WS-FAILED                PIC 9(4) VALUE 0.

       01 WS-REPORT-LINE           PIC X(100).

       PROCEDURE DIVISION.
       MAIN-PARA.

           OPEN INPUT INPUT-FILE

           IF WS-IN-STATUS NOT = '00'
               DISPLAY 'ERROR OPENING INPUT. STATUS='
                   WS-IN-STATUS
               MOVE 12 TO RETURN-CODE
               STOP RUN
           END-IF

           OPEN OUTPUT REPORT-FILE

           IF WS-OUT-STATUS NOT = '00'
               DISPLAY 'ERROR OPENING REPORT. STATUS='
                   WS-OUT-STATUS
               CLOSE INPUT-FILE
               MOVE 12 TO RETURN-CODE
               STOP RUN
           END-IF

           MOVE SPACES TO REPORT-RECORD
           MOVE
             'ID   TYPE AMOUNT   STATUS EXPECTED ACTUAL VERDICT'
             TO REPORT-RECORD
           WRITE REPORT-RECORD

           PERFORM UNTIL WS-EOF = 'Y'
               READ INPUT-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       PERFORM PROCESS-TEST
               END-READ
           END-PERFORM

           IF WS-TOTAL = 0
               MOVE SPACES TO REPORT-RECORD
               MOVE 'FAIL: EMPTY INPUT DATASET'
                 TO REPORT-RECORD
               WRITE REPORT-RECORD
               MOVE 1 TO WS-FAILED
           END-IF

           MOVE SPACES TO REPORT-RECORD
           STRING
               'TOTAL=' DELIMITED BY SIZE
               WS-TOTAL DELIMITED BY SIZE
               ' PASSED=' DELIMITED BY SIZE
               WS-PASSED DELIMITED BY SIZE
               ' FAILED=' DELIMITED BY SIZE
               WS-FAILED DELIMITED BY SIZE
               INTO REPORT-RECORD
           END-STRING
           WRITE REPORT-RECORD

           CLOSE INPUT-FILE
           CLOSE REPORT-FILE

           IF WS-FAILED > 0
               MOVE 8 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF

           STOP RUN.

       PROCESS-TEST.

           MOVE INPUT-RECORD(1:4) TO WS-TEST-ID
           MOVE INPUT-RECORD(5:2) TO WS-TRANS-TYPE
           MOVE INPUT-RECORD(7:8) TO WS-TRANS-AMOUNT
           MOVE INPUT-RECORD(15:1) TO WS-ACCOUNT-STATUS
           MOVE INPUT-RECORD(16:1) TO WS-EXPECTED-RESULT

           ADD 1 TO WS-TOTAL
           MOVE 'Y' TO WS-ACTUAL-RESULT

           IF WS-TRANS-TYPE NOT = 'CR'
               AND WS-TRANS-TYPE NOT = 'DR'
               MOVE 'N' TO WS-ACTUAL-RESULT
           END-IF

           IF WS-TRANS-AMOUNT IS NOT NUMERIC
               MOVE 'N' TO WS-ACTUAL-RESULT
           ELSE
               IF WS-TRANS-AMOUNT = '00000000'
                   MOVE 'N' TO WS-ACTUAL-RESULT
               END-IF
           END-IF

           IF WS-ACCOUNT-STATUS NOT = 'A'
               MOVE 'N' TO WS-ACTUAL-RESULT
           END-IF

           IF WS-EXPECTED-RESULT NOT = WS-ACTUAL-RESULT
               MOVE 'FAIL' TO WS-VERDICT
               ADD 1 TO WS-FAILED
           ELSE
               MOVE 'PASS' TO WS-VERDICT
               ADD 1 TO WS-PASSED
           END-IF

           MOVE SPACES TO WS-REPORT-LINE

           STRING
               WS-TEST-ID DELIMITED BY SIZE
               ' ' DELIMITED BY SIZE
               WS-TRANS-TYPE DELIMITED BY SIZE
               ' ' DELIMITED BY SIZE
               WS-TRANS-AMOUNT DELIMITED BY SIZE
               ' ' DELIMITED BY SIZE
               WS-ACCOUNT-STATUS DELIMITED BY SIZE
               ' ' DELIMITED BY SIZE
               WS-EXPECTED-RESULT DELIMITED BY SIZE
               ' ' DELIMITED BY SIZE
               WS-ACTUAL-RESULT DELIMITED BY SIZE
               ' ' DELIMITED BY SIZE
               WS-VERDICT DELIMITED BY SIZE
               INTO WS-REPORT-LINE
           END-STRING

           MOVE WS-REPORT-LINE TO REPORT-RECORD
           WRITE REPORT-RECORD.

