       IDENTIFICATION DIVISION.
         PROGRAM-ID. RECORD_PARSER.

       DATA DIVISION.
         WORKING-STORAGE SECTION.
          01 WS-RECORD PIC X(20).
          01 WS-ACCOUNT PIC X(10).
          01 WS-AMOUNT PIC X(6).
          01 WS-TYPE PIC X(2).
          01 WS-STATUS PIC X(2).

       PROCEDURE DIVISION.
              DISPLAY "Enter record:"
              ACCEPT WS-RECORD
              MOVE WS-RECORD(1:10) TO WS-ACCOUNT
              MOVE WS-RECORD(11:6) TO WS-AMOUNT
              MOVE WS-RECORD(17:2) TO WS-TYPE
              MOVE WS-RECORD(19:2) TO WS-STATUS

              IF WS-ACCOUNT IS NUMERIC AND WS-AMOUNT IS NUMERIC 
              AND WS-STATUS IS NUMERIC
                 DISPLAY "Account:" WS-ACCOUNT
                 DISPLAY "Amount:" WS-AMOUNT
                 DISPLAY "Type:" WS-TYPE
                 DISPLAY "Status:" WS-STATUS
              ELSE
                 DISPLAY "INVALID NUMERIC FIELD"
              END-IF
    
            STOP RUN.