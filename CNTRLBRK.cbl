      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    CNTRLBRK.
       AUTHOR.        SAYLES.
      *--------------------
       ENVIRONMENT DIVISION.
      *--------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRINT-LINE ASSIGN TO PRTLINE.
           SELECT ACCT-REC   ASSIGN TO ACCTSORT.
      *-------------
       DATA DIVISION.
      *-------------
       FILE SECTION.
       FD  PRINT-LINE RECORDING MODE F.
       01  PRINT-REC.
           05 FILLER                    PIC X(03)      VALUE SPACE.
           05 USA-STATE-O               PIC X(18).
           05 FIRST-NAME-O              PIC X(15).
           05 LAST-NAME-O               PIC X(20).
           05 ELECTED-O                 PIC X(6).
           05 LAST-YEAR-O               PIC X(6).
           05 ACCT-LIMIT-O              PIC $$,$$$,$$9.99.
           05 FILLER                    PIC X(03)      VALUE SPACES.
           05 ACCT-BALANCE-O            PIC $$,$$$,$$9.99.
      *    05 FILLER                    PIC X(30)  VALUE SPACES.
           05 FILLER                    PIC X(03)      VALUE SPACE.
           05 SALARY-ACCUM-OUT          PIC $$$,$$$,$$$.99.
      *
       FD  ACCT-REC RECORDING MODE F.
       01  ACCT-FIELDS.
           05  ACCT-NO            PIC X(8).
           05  ACCT-LIMIT         PIC S9(7)V99 COMP-3.
           05  ACCT-BALANCE       PIC S9(7)V99 COMP-3.
           05  LAST-NAME          PIC X(20).
           05  FIRST-NAME         PIC X(15).
           05  CLIENT-ADDR.
               10  STREET-ADDR    PIC X(25).
               10  CITY-COUNTY    PIC X(20).
               10  USA-STATE      PIC X(15).  *> Input Sort Key
           05  RESERVED           PIC X(7).
           05  COMMENTS           PIC X(50).
      *
       WORKING-STORAGE SECTION.
       01 PROGRAM-INDICATOR-SWITCHES.
           05 WS-EOF-INPUT-SW           PIC X(1)       VALUE 'N'.
               88 EOF-INPUT                            VALUE 'Y'.

009800 01 WS-BREAK-CONTROLS.
009900     05 WS-CONTROL-KEY            PIC X(15). *> Hold/Control Key

      *************************************************************
      ****** Report headings begin here ******
      *************************************************************
       01 WS-BLANK-LINE                 PIC X(133)     VALUE SPACES.

017000 01 WS-HEADER-1.
017100     05 FILLER                    PIC X(1)       VALUE SPACES.
017200     05 FILLER                    PIC X(12)      VALUE
                                                         'Report: A124'.
           05 DATE-O                    PIC X(10)      VALUE SPACE.
017300     05 FILLER                    PIC X(13)      VALUE SPACES.
017400     05 FILLER                    PIC X(47)
017500                                                 VALUE
                              'Presidents Broken Out By State of Birth'.
017600     05 RPT-DATE                  PIC XXXX/XX/XX.
017700     05 FILLER                    PIC X(10)      VALUE SPACES.
017800     05 FILLER                    PIC X(5)       VALUE 'PAGE '.
017900     05 RPT-PAGE-NO               PIC ZZ.
018000     05 FILLER                    PIC X(12)      VALUE SPACES.
018100
018200 01 WS-HEADER-2.
018300     05 FILLER                    PIC X(3)       VALUE SPACES.
018400     05 FILLER                    PIC X(18)      VALUE 'STATE'.
018500     05 FILLER                    PIC X(9)       VALUE 'PRESIDENT'
                                                                      .
018600     05 FILLER                    PIC X(24)      VALUE SPACES.
019100     05 FILLER                    PIC X(7)       VALUE 'ELECTED'.
019200     05 FILLER                    PIC X(1)       VALUE SPACES.
019300     05 FILLER                    PIC X(8)       VALUE 'THRU'.
019500     05 FILLER                    PIC X(14)     VALUE 'SALARY'.
019700     05 FILLER                   PIC X(14)   VALUE '   NET WORTH'.
           05 FILLER                   PIC X(02)   VALUE SPACES.
           05 FILLER                  PIC X(14)   VALUE ' SALARY-ACCUM'.
018200 01  WS-HEADER-3.
018300     05 FILLER                    PIC X(3)       VALUE SPACES.
018400     05 FILLER                    PIC X(17)      VALUE ALL '='.
           05 FILLER                    PIC X(01)      VALUE SPACE.
018600     05 FILLER                    PIC X(32)      VALUE ALL '='.
           05 FILLER                    PIC X(01)      VALUE SPACE.
019100     05 FILLER                    PIC X(7)       VALUE '======='.
019200     05 FILLER                    PIC X(1)       VALUE SPACES.
019300     05 FILLER                    PIC X(7)        VALUE '====='.
019400     05 FILLER                    PIC X(01)      VALUE SPACES.
019500     05 FILLER                    PIC X(12)       VALUE ALL '='.
019600     05 FILLER                    PIC X(2)       VALUE SPACES.
019700     05 FILLER                    PIC X(14)      VALUE
                                                        '============='.
           05 FILLER                    PIC X(02)      VALUE SPACES.
           05 FILLER                    PIC X(14)      VALUE ALL '='.
      *************************************************************
      ****** Control Break Subtotal Line ******
      *************************************************************
018200 01  WS-TRLR-LINE-1.
018300     05 FILLER                    PIC X(03)       VALUE SPACES.
           05 FILLER                    PIC X(12) VALUE 'Sub Totals:'.
           05 STATE-TRLR-LINE           PIC X(15).
           05 FILLER                    PIC X(16) VALUE SPACE.
           05 FILLER                    PIC X(21)
                            VALUE 'Salary | Net Worth: ' JUST RIGHT.
           05 SALARY-SUB-TOT-OUT        PIC $$$,$$$,$$$.99.
           05 FILLER                    PIC X(02)       VALUE SPACES.
           05 NET-WORTH-SUB-TOT-OUT     PIC $$$,$$$,$$$.99.
           05 FILLER                    PIC X(03)      VALUE SPACE.
           05 SALARY-ACCUM-TOT-OUT      PIC $$$,$$$,$$$.99.
      *
       01 WS-TRLR-LINE-2.
          05 FILLER  PIC X(40)
                     VALUE 'GRAND TOTAL OF ALL PRESIDENT SALARIES:  '.
          05 WS-GRAND-TOTAL-SALARIES-O   PIC $$$$,$$$,$$$.99.
          05 FILLER                      PIC X(60).
      *
       01 WS-TRLR-LINE-3.
          05 FILLER   PIC X(40)
                      VALUE 'PRESIDENT WITH HIGHEST SALARY:  '.
          05 WS-HIGHEST-PRES-SALARY-O  PIC $$$$,$$$,$$$.99.
          05 FILLER                    PIC X(02) VALUE ', '.
          05 WS-HIGH-COST-PRESIDENT-NAME-O   PIC X(36).
          05 FILLER                    PIC X(22) VALUE SPACES.
      *
       01 WS-TRLR-LINE-4.
          05 FILLER    PIC X(40)
                       VALUE 'PRESIDENT WITH LOWEST SALARY:  '.
          05 WS-LOWEST-PRES-SALARY-O   PIC $$$$,$$$,$$$.99.
          05 FILLER                    PIC X(02) VALUE ', '.
          05 WS-LOW-COST-PRESIDENT-NAME-O PIC X(36).
          05 FILLER    PIC X(22) VALUE SPACES.
      *
       01  WS-TRLR-LINE-5.
           05 FILLER  PIC X(40)
                      VALUE "AVERAGE OF PRESIDENTS' SALARIES:  ".
           05 WS-AVERAGE-OF-SALARIES-O PIC $$$$,$$$,$$$.99.
           05 FILLER  PIC X(61) VALUE SPACES.
      *
       01  WS-TRLR-LINE-6.
           05 FILLER  PIC X(98)
                VALUE 'NOTE: LAST READ LOW/HIGH PRESIDENT SALARY DISPLAY
      -         'ED IF THERE ARE MULTIPLE LOW/HIGH SALARY MATCHES.'.
           05 FILLER PIC X(62) VALUE SPACES.

       01  WS-TRLR-LINE-7.
           05 FILLER  PIC X(107)
                VALUE 'NOTE: AVERAGE SALARY CALCUATED BY DIVIDING THE NU
      -         'MBER OF PRESIDENTS INTO THE GRAND TOTAL OF ALL SALARIES
      -         '.'.
           05 FILLER PIC X(37) VALUE SPACES.

      *
       01 WS-COUNTERS-AND-ACCUMULATORS.
          05 WS-CONTROL-BREAK-TOTAL    PIC S9(7)V99 COMP-3.
          05 WS-STATE-CTR              PIC  9(2) COMP.

       01 WS-FLAGS.
          05 WS-LASTREC                PIC X          VALUE SPACE.
          05 WS-LINE-KTR               PIC 9(4) COMP  VALUE 0.
          05 WS-SALARY-SUB-TOT         PIC 9(09)V99   VALUE 0.
          05 WS-NET-WORTH-SUB-TOT      PIC 9(09)V99   VALUE 0.
          05 WS-SALARY-ACCUM-SUB-TOT   PIC 9(09)V99   VALUE 0.
          05 WS-SALARY-ACCUM           PIC 9(09)V99   VALUE 0.
      *
       01 WS-TEMP-VARIABLES.
          05 WS-YEARS-IN-OFFICE          PIC 9(4)     VALUE 0.
          05 SALARY-ACCUM-TOT            PIC 9(11)V99 VALUE 0.
          05 ACCT-LIMIT-DISPLAY          PIC $$$,$$$,$$$.99.
          05 WS-HOLD-PRES-SALARY         PIC 9(9)V99  VALUE 0.
          05 WS-GRAND-TOTAL-SALARIES     PIC 9(9)V99  VALUE 0.
          05 WS-HIGHEST-PRES-SALARY      PIC 9(9)V99  VALUE 0.
          05 WS-LOWEST-PRES-SALARY       PIC 9(9)V99  VALUE 0.
          05 WS-AVERAGE-OF-SALARIES      PIC 9(9)V99  VALUE 0.
          05 WS-RECORDS-READ             PIC 9(03)    VALUE 0.
          05 WS-HIGH-COST-PRESIDENT-NAME PIC X(36)    VALUE SPACES.
          05 WS-LOW-COST-PRESIDENT-NAME  PIC X(36)    VALUE SPACES.
          05 WS-HOLD-PRESIDENT-NAME      PIC X(36)    VALUE SPACES.

      *------------------
       PROCEDURE DIVISION.
      *------------------
           PERFORM 100-INIT-RTN *> Housekeeping, Initial Report Headings
           PERFORM 300-PROCESS-RECORDS UNTIL EOF-INPUT
           PERFORM 500-CONTROL-BREAK *> Final Control Break paragraphs
           PERFORM 800-MOVE-TOTALS
           PERFORM 850-PRINT-TOTALS
           PERFORM 900-WRAP-UP
           GOBACK
           .
       100-INIT-RTN.
           MOVE FUNCTION CURRENT-DATE TO RPT-DATE.
           PERFORM 200-OPEN-FILES
           MOVE SPACES TO PRINT-REC
           PERFORM 700-READ-RECORD
           MOVE ACCT-LIMIT TO WS-HIGHEST-PRES-SALARY,
                              WS-LOWEST-PRES-SALARY
           PERFORM 350-CHECK-SALARY *>CHECK FOR HIGH/LOW PRES SALARIES
           PERFORM 500-CONTROL-BREAK *> Initial Control creates Rpt Headings
           .
       150-INIT-WS-FIELDS.
           INITIALIZE WS-COUNTERS-AND-ACCUMULATORS
           .
       200-OPEN-FILES.
           OPEN INPUT ACCT-REC
           OPEN OUTPUT PRINT-LINE
           .
       300-PROCESS-RECORDS.
           IF NOT EOF-INPUT   *> No duplicating last record
               PERFORM 350-CHECK-SALARY *>CHECK FOR HIGH/LOW SALARIES
               IF WS-CONTROL-KEY = USA-STATE *> Control Break Conditional
                   PERFORM 400-MOVE-DATA
                   PERFORM 600-WRITE-DATA
                   PERFORM 700-READ-RECORD
               ELSE
                   PERFORM 500-CONTROL-BREAK
               END-IF
           END-IF
           .
       350-CHECK-SALARY.
      *****************************************************************
      *  IF ACCT-LIMIT >= HIGHEST SALARY, MOVE ACCT-LIMIT TO HIGHEST
      *  SALARY TO REPLACE VALUE OF HIGHEST SALARY
      *****************************************************************
           IF ACCT-LIMIT >= WS-HIGHEST-PRES-SALARY
              MOVE ACCT-LIMIT TO WS-HIGHEST-PRES-SALARY
              MOVE SPACES TO WS-HIGH-COST-PRESIDENT-NAME,
                             WS-HOLD-PRESIDENT-NAME
      *****************************************************************
      *  CONCATENATE THE FIRST AND LAST NAME                          *
      *****************************************************************
              STRING FIRST-NAME DELIMITED BY SPACE
                     ' '        DELIMITED BY SIZE
                     LAST-NAME  DELIMITED BY SPACE
                       INTO WS-HOLD-PRESIDENT-NAME
      *****************************************************************
      *  CONVERT PRESIDENT NAME TO ALL UPPERCASE
      *****************************************************************
              MOVE FUNCTION UPPER-CASE(WS-HOLD-PRESIDENT-NAME) TO
                                       WS-HIGH-COST-PRESIDENT-NAME
           ELSE
      *****************************************************************
      *  IF ACCT-LIMIT <= LOWEST SALARY, MOVE ACCT-LIMIT TO LOWEST
      *  SALARY TO REPLACE VALUE OF LOWEST SALARY
      *****************************************************************
              IF ACCT-LIMIT <=  WS-LOWEST-PRES-SALARY
                 MOVE ACCT-LIMIT TO WS-LOWEST-PRES-SALARY
                 MOVE SPACES TO WS-LOW-COST-PRESIDENT-NAME,
                                WS-HOLD-PRESIDENT-NAME
      *****************************************************************
      *  CONCATENATE THE FIRST AND LAST NAME
      *****************************************************************
                 STRING FIRST-NAME DELIMITED BY SPACE
                        ' '        DELIMITED BY SIZE
                        LAST-NAME DELIMITED BY SPACE
                           INTO WS-HOLD-PRESIDENT-NAME
      *****************************************************************
      *  CONVERT PRESIDENT NAME TO ALL UPPERCASE
      *****************************************************************
                 MOVE FUNCTION UPPER-CASE(WS-HOLD-PRESIDENT-NAME) TO
                                       WS-LOW-COST-PRESIDENT-NAME
              END-IF
           END-IF.

       400-MOVE-DATA.
           MOVE SPACES TO PRINT-REC
           ADD +1 TO WS-STATE-CTR
           IF WS-STATE-CTR > 1 *> Logic to create outline view in State column
                MOVE SPACES TO USA-STATE-O
           ELSE
                MOVE USA-STATE TO USA-STATE-O,  *> MOVE IN-STATE -> HOLD-KEY
                                  STATE-TRLR-LINE
           END-IF
           ADD ACCT-LIMIT TO WS-SALARY-SUB-TOT.
           ADD ACCT-BALANCE TO WS-NET-WORTH-SUB-TOT
      *** The ACCT file is actually a repurposed file for the presidents
      *** The first four bytes is their inaugural yr => last year in office
           MOVE ACCT-NO(1:4) TO ELECTED-O
           MOVE ACCT-NO(5:4) TO LAST-YEAR-O
           MOVE ACCT-LIMIT TO ACCT-LIMIT-O
           MOVE ACCT-BALANCE TO ACCT-BALANCE-O
           MOVE LAST-NAME TO LAST-NAME-O
           MOVE FIRST-NAME TO FIRST-NAME-O
      *****************************************************************
      *  CALCULATE YEARS IN OFFICE BY CONVERTING START AND END DATES IN
      *  IN OFFICE TO NUMERIC AND SUBTRACTING START YEAR FROM END YEAR
      *****************************************************************
           COMPUTE WS-YEARS-IN-OFFICE =
                           FUNCTION NUMVAL (ACCT-NO (5:4)) -
                           FUNCTION NUMVAL (ACCT-NO (1:4))
      *****************************************************************
      *  IF START AND END YEAR IN OFFICE ARE THE SAME (YEARS IN OFFICE
      *  WILL EQUAL 0), ADD 1 TO YEARS IN OFFICE.  MULTIPLY THE NUMBER
      *  OF YEARS IN OFFICE BY THE SALARY (ACCT-LIMIT) TO OBTAIN THE
      *  ACCUMULATED SALARY FOR YEARS IN OFFICE
      *****************************************************************
           IF WS-YEARS-IN-OFFICE = 0
              ADD 1 TO WS-YEARS-IN-OFFICE
           END-IF

           COMPUTE WS-SALARY-ACCUM = WS-YEARS-IN-OFFICE *
                                           ACCT-LIMIT
           MOVE  WS-SALARY-ACCUM TO SALARY-ACCUM-OUT
           ADD WS-SALARY-ACCUM TO SALARY-ACCUM-TOT
           ADD ACCT-LIMIT TO WS-GRAND-TOTAL-SALARIES
           MOVE ACCT-LIMIT TO ACCT-LIMIT-DISPLAY
           .
       500-CONTROL-BREAK.
           IF WS-LINE-KTR > 0  *> Check for first time (beginning of program)
                MOVE WS-SALARY-SUB-TOT TO SALARY-SUB-TOT-OUT
                MOVE WS-NET-WORTH-SUB-TOT TO NET-WORTH-SUB-TOT-OUT
                MOVE SALARY-ACCUM-TOT TO SALARY-ACCUM-TOT-OUT
                WRITE PRINT-REC FROM WS-BLANK-LINE
                WRITE PRINT-REC FROM WS-TRLR-LINE-1
                WRITE PRINT-REC FROM WS-BLANK-LINE
                WRITE PRINT-REC FROM WS-BLANK-LINE
           END-IF
           IF NOT EOF-INPUT
                ADD +1 TO WS-LINE-KTR
                MOVE ZERO TO WS-SALARY-SUB-TOT,
                             WS-NET-WORTH-SUB-TOT,
                             SALARY-ACCUM-TOT,
                             SALARY-ACCUM-TOT-OUT
                MOVE  WS-LINE-KTR TO RPT-PAGE-NO
                MOVE  USA-STATE TO WS-CONTROL-KEY *> SET NEW CONTROL KEY
                WRITE PRINT-REC FROM WS-BLANK-LINE
                WRITE PRINT-REC FROM WS-HEADER-1
                WRITE PRINT-REC FROM WS-BLANK-LINE
                WRITE PRINT-REC FROM WS-HEADER-2
                WRITE PRINT-REC FROM WS-HEADER-3
                PERFORM 150-INIT-WS-FIELDS
           END-IF
           .
       600-WRITE-DATA.
           WRITE PRINT-REC
           .
       700-READ-RECORD.
           READ ACCT-REC
           AT END
              MOVE 'Y' TO WS-EOF-INPUT-SW
           END-READ.
           IF NOT EOF-INPUT
              ADD +1 TO WS-RECORDS-READ *>COUNT # OF INPUT RECORDS
           END-IF.
      *
       800-MOVE-TOTALS.
      *****************************************************************
      *  MOVE TOTAL COUNTS TO THE OUTPUT TRAILER LINES
      *****************************************************************
           MOVE WS-GRAND-TOTAL-SALARIES TO WS-GRAND-TOTAL-SALARIES-O.
           MOVE WS-HIGHEST-PRES-SALARY  TO WS-HIGHEST-PRES-SALARY-O.
           MOVE WS-LOWEST-PRES-SALARY   TO WS-LOWEST-PRES-SALARY-O.
           COMPUTE WS-AVERAGE-OF-SALARIES =  *>CALC AVERAGE SALARY
                   WS-GRAND-TOTAL-SALARIES / WS-RECORDS-READ.
           MOVE WS-AVERAGE-OF-SALARIES TO WS-AVERAGE-OF-SALARIES-O.
           MOVE WS-HIGH-COST-PRESIDENT-NAME
                                       TO WS-HIGH-COST-PRESIDENT-NAME-O.
            MOVE WS-LOW-COST-PRESIDENT-NAME
                                       TO WS-LOW-COST-PRESIDENT-NAME-O.
      *
       850-PRINT-TOTALS.
      *****************************************************************
      *  PRINT TOTAL LINES
      *****************************************************************
           WRITE PRINT-REC FROM WS-BLANK-LINE.
           WRITE PRINT-REC FROM WS-TRLR-LINE-2.
           WRITE PRINT-REC FROM WS-TRLR-LINE-3.
           WRITE PRINT-REC FROM WS-TRLR-LINE-4.
           WRITE PRINT-REC FROM WS-TRLR-LINE-5.
           WRITE PRINT-REC FROM WS-BLANK-LINE.
           WRITE PRINT-REC FROM WS-TRLR-LINE-6.
           WRITE PRINT-REC FROM WS-BLANK-LINE.
           WRITE PRINT-REC FROM WS-TRLR-LINE-7.

      *
       900-WRAP-UP.
           CLOSE ACCT-REC
           CLOSE PRINT-LINE
           .