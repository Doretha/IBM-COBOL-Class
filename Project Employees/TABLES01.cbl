       IDENTIFICATION DIVISION.
       PROGRAM-ID. TABLES01.
       AUTHOR. DORETHA RILEY.
       DATE-WRITTEN. 07/30/2020.
       DATE-COMPILED. CURRENT-DATE.
      *
      *****************************************************************
      *  DESCRIPTION
      *    THIS PROGRAM READS AN EMPLOYEE PROJECT FILE AND LOADS THE  *
      *    DATA INTO A TABLE (ARRAY).  PROCEDURES ARE THEN PERFORMED  *
      *    TO:                                                        *
      *    1. DISPLAY THE NAMES OF EMPLOYEES WORKING ON PROJECT A111. *
      *    2. DISPLAY THE NAMES OF PROGRAMMERS IN NORTH CAROLINA WHO  *
      *       ARE ALLOWED TO BILL OVERTIME HOURS.                     *
      *    3. CALCULATE THE TOTAL COSTS OF EMPLOYEES FOR PROJECT A111 *
      *    4. CALCULATE THE TOTAL COSTS FOR ALL OF THE PROJECTS BY    *
      *       ADDING THE 2 CALCULATIONS BELOW:                        *
      *       a. SUM OF ALL DAYS ALL EMPLOYEES WORKED ON PROJECTS     *
      *          MULTIPLIED BY THE SUM OF ALL DAILY BILLING RATES FOR *
      *          EMPLOYEES.                                           *
      *       b. SUM OF ALL OVERTIME HOURS WORKED BY ALL EMPLOYEES    *
      *          MULTIPLIED BY THE SUM OF ALL OVERTIME RATES FOR ALL  *
      *          EMPLOYEES.                                           *
      *                                                               *
      *    INPUT FILE:                                                *
      *      RTPOT44.LEARN.EMP1.PROJ                                  *
      *      INTERNAL FILE NAME:  INPUT-FILE                          *
      *      JCL DD NAME:         EMPROJ                              *
      *                                                               *
      *    OUTPUT FILES: N/A                                          *
      *                                                               *
      *    DD SYSOUT=* - DISPLAYS THE FOLLOWIMG                       *
      *    1. THE EMPLOYEE PROJECT ENTRIES IN THE TABLE (ARRAY)       *
      *    2. THE NAMES OF EMPLOYEES WORKING ON PROJECT A111          *
      *    3. THE NAMES OF EMPLOYEES IN NORTH CAROLINA WHO ARE        *
      *       PROGRAMMER/ANALYSTS AND WHO ARE ALLOWED TO BILL OVERTIME*
      *    4. TOTAL COST FOR ALL PROJECTS                             *
      *                                                               *
      *    JCL JOB: RTPOT44.LEARN.REUSABLE.JCL(TABLES01)              *
      *****************************************************************
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO EMPROJ.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS EMP-PROJECT-TABLE-I.
      *
       01  EMP-PROJECT-TABLE-I.    *>INPUT FILE FOR EMPLOYEE PROJECT
           05 EMP-PROJECT-I                 PIC X(4).
           05 EMP-NAME-I                    PIC X(15).
           05 EMP-STATE-OFFICE-I            PIC X(02).
           05 EMP-PROJECT-POSITION-I        PIC X(20).
           05 EMP-NBR-DAYS-ON-PROJ-I        PIC 9(03).
           05 EMP-NBR-OT-HOURS-I            PIC 9(03).
           05 EMP-PER-DAY-BILLING-RATE-I    PIC 9(03)V99.
           05 EMP-PER-HOUR-OT-RATE-I        PIC 9(03)V99.
           05 EMP-LANGUAGE-CERT-I           PIC X(20).
           05 EMP-ON-CALL-I                 PIC X(01).
           05 FILLER                        PIC X(02).
      *
       WORKING-STORAGE SECTION.
       77  PROJECT-INDEX     PIC S9(4) COMP. *> SUBSCRIPT FOR TABLE
       77  TABLE-MAX         PIC S9(4) COMP VALUE 20.
       77  SW-END-OF-FILE    PIC X(01) VALUE SPACES.
                88 END-OF-FILE         VALUE 'Y'.
      *
       01  EMP-PROJECT-TABLE.      *>EMPLOYEE PROJECT TABLE ARRAY
           05 EMP-PROJECT-ITEM OCCURS 20 TIMES
                ASCENDING KEY IS EMP-NAME
                INDEXED BY PROJ-IDX.
                10 EMP-PROJECT               PIC X(4).
                10 EMP-NAME                  PIC X(15).
                10 EMP-STATE-OFFICE          PIC X(02).
                10 EMP-PROJECT-POSITION      PIC X(20).
                10 EMP-NBR-DAYS-ON-PROJ      PIC 9(03).
                10 EMP-NBR-OT-HOURS          PIC 9(03).
                10 EMP-PER-DAY-BILLING-RATE  PIC 9(03)V99.
                10 EMP-PER-HOUR-OT-RATE      PIC 9(03)V99.
                10 EMP-LANGUAGE-CERT         PIC X(20).
                10 EMP-ON-CALL               PIC X(01).
                10 FILLER                    PIC X(02) VALUE SPACES.

       01 WS-TEMP-VARIABLES.       *>WORKING AREAS FOR TEMPORARY VARS.
          05 WS-EMP-REGULAR-COST             PIC 9(6)V99.
          05 WS-EMP-OT-COST                  PIC 9(6)V99.
          05 WS-A111-TOTAL-COST              PIC 9(6)V99.
          05 WS-CALC-ALL-EMP-REG-COSTS       PIC 9(8)V99.
          05 WS-CALC-ALL-EMP-OT-COSTS        PIC 9(8)V99.
          05 WS-CALC-ALL-PRJ-COSTS           PIC 9(9)V99.
          05 WS-TOTAL-EMP-COST               PIC 9(8)V99.
          05 WS-EMP-REGULAR-COST-EDIT        PIC $$$$,$$9.99.
          05 WS-EMP-OT-COST-EDIT             PIC $$$$,$$9.99.
          05 WS-A111-TOTAL-COST-EDIT         PIC $$$$,$$9.99.
          05 WS-CALC-ALL-EMP-REG-COSTS-EDIT  PIC $$$$,$$$,$$9.99.
          05 WS-CALC-ALL-EMP-OT-COSTS-EDIT   PIC $$$$,$$$,$$9.99.
          05 WS-CALC-ALL-PRJ-COSTS-EDIT      PIC $$$$,$$$,$$9.99.
          05 WS-TOTAL-EMP-COST-EDIT          PIC $$$$,$$9.99.
          05 WS-BILL-RATE-EDIT               PIC 999.99.
          05 WS-OT-RATE-EDIT                 PIC 999.99.
          05 WS-HOLD-OT-RATE                 PIC 999.99.
          05 WS-HOLD-OT-HOURS                PIC 999.

       77  SUM-1       PIC 9(18) VALUE 0.
       77  MAX-OUT     PIC 9(4).
       77  INDEX-COUNT PIC 9(5).
      *
      *****************************************************************
      *  DESCRIPTION:                                                 *
      *    THE MAIN PROCEDURE AREA PERFORMS ROUTINES TO CALL HOUSE-   *
      *    KEEPING, LOAD THE INPUT FILE INTO THE TABLE (ARRAY),       *
      *    PROCESS THE DATA IN THE TABLE, AND CLOSE THE INPUT FILE.   *
      *                                                               *
      *  CALLED BY:                                                   *
      *    - NONE                                                     *
      *                                                               *
      *  CALLS:                                                       *
      *    - 000-HOUSEKEEPING                                         *
      *    - 050-LOAD-DATA-INTO-TABLE                                 *
      *    - 100-PROCESS-TABLE-DATA                                   *
      *    - 900-WRAP-UP                                              *
      *****************************************************************
      *
       PROCEDURE DIVISION.
           PERFORM 000-HOUSEKEEPING.
           PERFORM 050-LOAD-DATA-INTO-TABLE.
           PERFORM 100-PROCESS-TABLE-DATA.
           PERFORM 900-WRAP-UP
           GOBACK.
      *
       000-HOUSEKEEPING.
      *
      *****************************************************************
      *  DESCRIPTION:                                                 *
      *    THIS PARAGRAPH INITIALIZES WORKING STORAGE VARIABLES, OPENS*
      *    THE INPUT FILE, READS THE FIRST INPUT RECORD AND SETS THE  *
      *    END OF FILE SWITCH WHEN THE END OF INPUT FILE IS DETECTED. *
      *                                                               *
      *  CALLED BY:                                                   *
      *    MAIN PROCEDURE AREA                                        *
      *                                                               *
      *  CALLS:                                                       *
      *    -  NONE                                                    *
      *****************************************************************
      *
           INITIALIZE EMP-PROJECT-TABLE, WS-TEMP-VARIABLES.
           OPEN INPUT INPUT-FILE.
           READ INPUT-FILE
              AT END MOVE 'Y' TO SW-END-OF-FILE.
      *
       050-LOAD-DATA-INTO-TABLE.
      *
      *****************************************************************
      *  DESCRIPTION:                                                 *
      *    THIS PARAGRAPH LOADS INPUT FILE RECORDS INTO THE TABLE     *
      *    UNTIL THE MAX NUMBER OF TABLE ENTRIES IS REACHED OR UNTIL  *
      *    THE END OF THE INPUT FILE IS READ. WHEN THE END OF THE     *
      *    INPUT FILE IS DETECTED, THE END OF FILE SWITCH IS SET.     *
      *                                                               *
      *  CALLED BY:                                                   *
      *    MAIN PROCEDURE AREA                                        *
      *                                                               *
      *  CALLS:                                                       *
      *    -  NONE                                                    *
      *****************************************************************
      *
      *
      *****************************************************************
      *  MOVE INPUT FILE FIELDS TO TABLE (ARRAY) FIELDS               *
      *****************************************************************
      *
           PERFORM VARYING PROJECT-INDEX FROM 1 BY 1
              UNTIL PROJECT-INDEX > TABLE-MAX OR END-OF-FILE
                 MOVE EMP-PROJECT-I             TO
                      EMP-PROJECT (PROJECT-INDEX)
                 MOVE EMP-NAME-I                TO
                      EMP-NAME (PROJECT-INDEX)
                 MOVE EMP-STATE-OFFICE-I        TO
                      EMP-STATE-OFFICE  (PROJECT-INDEX)
                 MOVE EMP-PROJECT-POSITION-I    TO
                      EMP-PROJECT-POSITION  (PROJECT-INDEX)
                 MOVE EMP-NBR-DAYS-ON-PROJ-I    TO
                      EMP-NBR-DAYS-ON-PROJ (PROJECT-INDEX)
                 MOVE EMP-NBR-OT-HOURS-I        TO
                      EMP-NBR-OT-HOURS (PROJECT-INDEX)
                 MOVE EMP-PER-DAY-BILLING-RATE-I TO
                       EMP-PER-DAY-BILLING-RATE (PROJECT-INDEX)
                 MOVE EMP-PER-HOUR-OT-RATE-I     TO
                      EMP-PER-HOUR-OT-RATE (PROJECT-INDEX)
                 MOVE EMP-LANGUAGE-CERT-I        TO
                      EMP-LANGUAGE-CERT (PROJECT-INDEX)
                 MOVE EMP-ON-CALL-I              TO
                      EMP-ON-CALL (PROJECT-INDEX)
      *
              DISPLAY EMP-PROJECT-ITEM(PROJECT-INDEX) *> DISPLAY RECORD

      *
      *****************************************************************
      *  READ THE NEXT INPUT RECORD AND SET SWITCH IF END OF FILE     *
      *****************************************************************
      *
              READ INPUT-FILE *>READ NEXT INPUT RECORD
                 AT END MOVE 'Y' TO  SW-END-OF-FILE
              END-READ
           END-PERFORM.
      *
           DISPLAY ' '. *> DISPLAY BLANK LINE IN SYSOUT
      *
       100-PROCESS-TABLE-DATA.
      *
      *****************************************************************
      *  DESCRIPTION:                                                 *
      *    THIS PARAGRAPH CALLS ROUTINES TO:                          *
      *    1. DISPLAY THE NAMES OF EMPLOYEES WORKING ON PROJECT A111. *
      *    2. DISPLAY THE NAMES OF PROGRAMMERS IN NORTH CAROLINA WHO  *
      *       ARE ALLOWED TO BILL OVERTIME HOURS.                     *
      *    3. CALCULATE THE TOTAL COSTS OF EMPLOYEES FOR PROJECT A111 *
      *    4. CALCULATE THE TOTAL COSTS FOR ALL OF THE PROJECTS BY    *
      *       ADDING THE 2 CALCULATIONS BELOW:                        *
      *       a. SUM OF ALL DAYS ALL EMPLOYEES WORKED ON PROJECTS     *
      *          MULTIPLIED BY THE SUM OF ALL DAILY BILLING RATES FOR *
      *          EMPLOYEES.                                           *
      *       b. SUM OF ALL OVERTIME HOURS WORKED BY ALL EMPLOYEES    *
      *          MULTIPLIED BY THE SUM OF ALL OVERTIME RATES FOR ALL  *
      *          EMPLOYEES.                                           *
      *                                                               *
      *  CALLED BY:                                                   *
      *    MAIN PROCEDURE AREA                                        *
      *                                                               *
      *  CALLS:                                                       *
      *    -  200-FIND-PROJECT                                        *
      *    -  300-FIND-NC-OT-SKILL                                    *
      *    -  400-TOTAL-PROJ-EXPENSE                                  *
      *    -  500-TOTAL-ALL-PROJECTS-EXPENSE                          *
      *****************************************************************
      *
           PERFORM 200-FIND-PROJECT.
           PERFORM 300-FIND-NC-OT-SKILL.
           PERFORM 400-TOTAL-PROJ-EXPENSE.
           PERFORM 500-TOTAL-ALL-PROJECTS-EXPENSE.

       200-FIND-PROJECT.
      *
      *****************************************************************
      *  DESCRIPTION:                                                 *
      *    THIS PARAGRAPH SEARCHES THE TABLE (ARRAY) AND DISPLAYS THE *
      *    NAMES OF EMPLOYEES WORKING ON PROJECT 'A111'.              *
      *                                                               *
      *  CALLED BY:                                                   *
      *    -  100-PROCESS-TABLE-DATA                                  *
      *                                                               *
      *  CALLS:                                                       *
      *    - NONE                                                     *
      *****************************************************************
      *
      *****************************************************************
      *  SEARCH TABLE FOR RECORDS WITH PROJECT CODE 'A111' USING INDEX*
      *  UNTIL INDEX IS GREATER THAN THE MAX TABLE ENTRIES. WHEN AN   *
      *  ENTRY WITH PROJECT CODE 'A111' IS FOUND, DISPLAY THE EMPLOYEE*
      *  NAME.                                                        *
      *****************************************************************
      *
           PERFORM VARYING PROJ-IDX FROM 1 BY 1
              UNTIL PROJ-IDX >  TABLE-MAX
                IF EMP-PROJECT (PROJ-IDX) = 'A111'
                   DISPLAY 'EMPLOYEE NAME WITH PROJECT = A111:  '
                            EMP-NAME (PROJ-IDX)
                END-IF
           END-PERFORM.
      *
           DISPLAY ' '. *> DISPLAY BLANK LINE IN SYSOUT

       300-FIND-NC-OT-SKILL.
      *
      *****************************************************************
      *  DESCRIPTION:                                                 *
      *    THIS PARAGRAPH SEARCHES THE TABLE (ARRAY) AND DISPLAYS THE *
      *    NAMES OF EMPLOYEES WHO MEET ALL OF THE FOLLOWING           *
      *    CRITERIA:                                                  *
      *    1. PROJECT POSITION = 'PROGRAMER/ANALYST'                  *
      *    2. EMPLOYEE STATE OFFICE = 'NC'                            *
      *    3. EMPLOYEE IS ALLOWED TO BILL FOR ON CALL WORK            *
      *       (EMP-ON-CALL FIELD  = 'Y')                              *
      *                                                               *
      *  CALLED BY:                                                   *
      *    -  100-PROCESS-TABLE-DATA                                  *
      *                                                               *
      *  CALLS:                                                       *
      *    -  NONE                                                    *
      *****************************************************************
      *
      *****************************************************************
      *  SEARCH TABLE FOR RECORDS THAT MEET SPECIFIED CRITERIA AND    *
      *  DISPLAY THE NAMES OF EMPLOYEES MEETING THE CRITERIA.         *
      *****************************************************************
      *
           PERFORM VARYING PROJ-IDX FROM 1 BY 1
              UNTIL PROJ-IDX > TABLE-MAX
               IF EMP-PROJECT-POSITION (PROJ-IDX)= 'PROGRAMMER/ANALYST'
                  IF EMP-STATE-OFFICE (PROJ-IDX) = 'NC'
                     IF EMP-ON-CALL (PROJ-IDX)   = 'Y'
                      DISPLAY
                        'NC BASED PROGRAMMER WHO CAN BILL FOR ON CALL: '
                         EMP-NAME (PROJ-IDX)
                     END-IF
                  END-IF
               END-IF
           END-PERFORM.
      *
           DISPLAY ' '. *> DISPLAY BLANK LINE IN SYSOUT
      *
       400-TOTAL-PROJ-EXPENSE.
      *
      *****************************************************************
      *  DESCRIPTION:                                                 *
      *    THIS PARAGRAPH SEARCHES THE TABLE (ARRAY) FOR ALL RECORDS  *
      *    WITH PROJECT CODE 'A111', CALLS A ROUTINE TO CALCULATE THE *
      *    TOTAL SALARIES (COSTS) FOR ALL EMPLOYEES WORKING ON THIS   *
      *    PROJECT AND DISPLAYS THE TOTAL RESULT.                     *
      *                                                               *
      *  CALLED BY:                                                   *
      *    -  100-PROCESS-TABLE-DATA                                  *
      *                                                               *
      *  CALLS:                                                       *
      *    - 425-CALCULATE-PROJECT-TOTALS                             *
      *****************************************************************
      *
      *****************************************************************
      *  SEARCH TABLE FOR RECORDS THAT MEET SPECIFIED CRITERIA AND    *
      *  CALL A ROUTINE TO CALCULATE THE SALARY FOR THAT EMPLOYEE.    *
      *  THE LOOP TO CALL THE SALARY CALCULATION ROUTINE IS PERFORMED *
      *  UNTIL THE END OF THE TABLE IS ENCOUNTERED.                   *
      *****************************************************************
      *
           PERFORM VARYING PROJ-IDX FROM 1 BY 1
              UNTIL PROJ-IDX > TABLE-MAX
                IF EMP-PROJECT (PROJ-IDX) = 'A111'
                   PERFORM 425-CALCULATE-PROJECT-TOTALS
      *
      *****************************************************************
      *  ADD THE CALCULATED SALARY TO THE TOTAL. MOVE THE CALCULATED  *
      *  SALARY TO A NUMERIC EDITED FIELD AND DISPLAY IT.             *
      *****************************************************************
      *
                   ADD WS-EMP-REGULAR-COST, WS-EMP-OT-COST TO
                       WS-A111-TOTAL-COST
                END-IF
           END-PERFORM.

           MOVE WS-A111-TOTAL-COST TO WS-A111-TOTAL-COST-EDIT.
           DISPLAY 'TOTAL A111 PROJECT COSTS:  '
                     WS-A111-TOTAL-COST-EDIT.
           DISPLAY ' '.   *> DISPLAY BLANK LINE
      *
       425-CALCULATE-PROJECT-TOTALS.
      *
      *****************************************************************
      *  DESCRIPTION:                                                 *
      *    THIS PARAGRAPH CALCULATES THE SALARY COST FOR AN EMPLOYEE  *
      *    WORKING ON PROJECT 'A111'.  THE SALARY IS CALCUATED BY     *
      *    MULTIPLYING THE DAYS WORKED ON THE PROJECT BY THE DAILY    *
      *    RATE AND ADDING THE RESULT TO THE OVERTIME HOURS WORKED    *
      *    MULTIPLIED BY THE OVERTIME HOURLY RATE.                    *
      *                                                               *
      *  CALLED BY:                                                   *
      *    -  400-TOTAL-PROJ-EXPENSE                                  *
      *                                                               *
      *  CALLS:                                                       *
      *    -  NONE                                                    *
      *****************************************************************
      *
           INITIALIZE WS-EMP-REGULAR-COST,  *>INITIALIZE VARIABLES
                      WS-EMP-OT-COST,
                      WS-TOTAL-EMP-COST.
      *
           COMPUTE WS-EMP-REGULAR-COST =  *> CALCULATE REGULAR SALARY
                   (EMP-NBR-DAYS-ON-PROJ (PROJ-IDX) *
                    EMP-PER-DAY-BILLING-RATE (PROJ-IDX))
           END-COMPUTE.
      *
           COMPUTE WS-EMP-OT-COST =       *> CALCUATE OVERTIME SALARY
                   (EMP-PER-HOUR-OT-RATE (PROJ-IDX) *
                    EMP-NBR-OT-HOURS (PROJ-IDX))
           END-COMPUTE.
      *
           COMPUTE WS-TOTAL-EMP-COST =  *> ADD REGULAR AND OT SALARY
                   WS-EMP-REGULAR-COST + WS-EMP-OT-COST
           END-COMPUTE.
      *
       500-TOTAL-ALL-PROJECTS-EXPENSE.
      *
      *****************************************************************
      *  DESCRIPTION:                                                 *
      *    CALCULATE THE TOTAL COST FOR ALL PROJECTS.  USE THE SUM    *
      *    FUNCTION TO SUM (ALL EMPLOYEE DAYS WORKED) MULTIPLIED BY   *
      *    THE SUM OF (ALL EMPLOYEE DAILY RATES).  THE RESULT OF THIS *
      *    CALCULATION IS ADDED TO THE SUM OF (ALL EMPLOYEE OVERTIME  *
      *    HOURS) * THE SUM OF (ALL OVERTIME RATES).  THE TOTAL COST  *
      *    OF ALL PROJECTS IS MOVED TO A NUMERIC EDITED FIELD AND     *
      *    DISPLAYED.                                                 *
      *                                                               *
      *  CALLED BY:                                                   *
      *    -  100-PROCESS-TABLE-DATA                                  *
      *                                                               *
      *  CALLS:                                                       *
      *    -  NONE                                                    *
      *****************************************************************
      *
      *
      *****************************************************************
      *  SUM ALL EMPLOYEE DAYS ON THE PROJECT AND MULTIPLY THE RESULT *
      *  BY THE SUM OF ALL DAILY BILLING RATES FOR EMPLOYEES.         *
      *****************************************************************
      *
           COMPUTE WS-CALC-ALL-EMP-REG-COSTS =
                 FUNCTION SUM(EMP-NBR-DAYS-ON-PROJ(ALL)) *
                 FUNCTION SUM(EMP-PER-DAY-BILLING-RATE(ALL))
           END-COMPUTE.
      *
      *****************************************************************
      *  SUM ALL EMPLOYEE OVERTIME HOURS ON THE PROJECT AND MULTIPLY  *
      *  THE RESULT BY THE SUM OF ALL OVERTIME RATES FOR EMPLOYEES.   *
      *****************************************************************
      *
           COMPUTE WS-CALC-ALL-EMP-OT-COSTS =
                 FUNCTION SUM(EMP-NBR-OT-HOURS(ALL)) *
                 FUNCTION SUM(EMP-PER-HOUR-OT-RATE(ALL))
           END-COMPUTE.

      *
      *****************************************************************
      *  ADD THE SUMMED AND MULTIPLIED REGULAR COSTS TO THE SUMMED AND*
      *  MULTIPLIED OVERTIME COSTS                                    *
      *****************************************************************
      *
           COMPUTE WS-CALC-ALL-PRJ-COSTS =
                 WS-CALC-ALL-EMP-REG-COSTS +
                 WS-CALC-ALL-EMP-OT-COSTS.
      *
      *****************************************************************
      *  MOVE THE TOTAL-CALCUATED COSTS TO A NUMERIC EDITED FIELD AND *
      *  DISPLAY THE RESULT                                           *
      *****************************************************************
      *
           MOVE WS-CALC-ALL-PRJ-COSTS         TO
                WS-CALC-ALL-PRJ-COSTS-EDIT.
           DISPLAY 'TOTAL REGULAR + OT EMPLOYEE COSTS (ALL PROJECT COSTS
      -            '):  ' WS-CALC-ALL-PRJ-COSTS-EDIT.
      *
       900-WRAP-UP.
      *
      *****************************************************************
      *  DESCRIPTION:                                                 *
      *    THIS PARAGRAPH CLOSES THE INPUT FILE.                      *
      *                                                               *
      *  CALLED BY:                                                   *
      *    -  MAIN AREA OF PROCEDURE DIVISION                         *
      *                                                               *
      *  CALLS:                                                       *
      *    -  NONE                                                    *
      *****************************************************************
           CLOSE INPUT-FILE.
