       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLAIMS.
       AUTHOR. DORETHA RILEY.
       DATE-WRITTEN. 07/25/2020.
       DATE-COMPILED. CURRENT-DATE.
      *
      *****************************************************************
      *  THIS PROGRAM READS A CLAIMS INPUT FILE AND CALCULATES THE    *
      *  THE DEDUCTIBLE, COINSURANCE AND AMOUNT THE INSURANCE COMPANY *
      *  WILL PAY FOR EACH CLAIM. FIELDS ON THE INPUT RECORDS ARE RUN *
      *  THROUGH A VALIDATION ROUTINE.  IF THE RECORD PASSES ALL      *
      *  VALIDATION EDITS, CLAIMS INFORMATION IS CALCULATED AND THE   *
      *  RECORD IS WRITTEN TO THE CLAIMS REPORT.  IF ANY FIELD ON A   *
      *  RECORD FAILS A VALIDATION  CHECK, NO CLAIM PAYMENT INFORMA-  *
      *  TION IS CALCULATED AND THE RECORD IS WRITTEN TO THE ERROR    *
      *  REPORT.                                                      *
      *                                                               *
      *  INPUT FILE: INCLUDES VALID AND INVALID RECORDS TO TEST EDITS *
      *      RTPOT44.LEARN.INCLAIMS.VALID.AND.ERRORS                  *
      *      INTERNAL FILE NAME:  CLAIMSIN                            *
      *      JCL DD NAME:         INCLAIMS                            *
      *                                                               *
      *  OUTPUT FILES: GO TO SYSOUT JCL JOB SPOOL                     *
      *      INTERNAL FILE NAMES:                                     *
      *       - CLAIMSRPT (CLAIMS REPORT - RECORDS THAT PASS EDIT     *
      *           CHECKS AND HAVE CALCULATED CLAIM PAYMENT DATA)      *
      *       - ERRORPT (ERROR REPORT - RECORDS THAT DID NOT PASS EDIT*
      *           CHECKS AND DO NOT HAVE CALCULATED CLAIM DATA)       *
      *                                                               *
      *      EXTERNAL FILE NAMES (JCL DD NAMES): FILES WRITTEN TO     *
      *       DD SYSOUT=*                                             *
      *       - CLAIMSRPT (CONTAINS VALID CLAIMS RECORDS)             *
      *       - ERRORPT (CONTAINS RECORDS WITH INVALID DATA IN FIELDS *
      *                                                               *
      *  JCL JOB: RTPOT44.LEARN.REUSABLE.JCL(CLAIMS)                  *
      *****************************************************************
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLAIMSIN
              ASSIGN TO INCLAIMS           *>CLAIMS INPUT FILE
              FILE STATUS IS CLAIM-STATUS.
      *
           SELECT CLAIMRPT
              ASSIGN TO CLAIMRPT           *>REPORT FOR PROCESSED CLAIMS
              FILE STATUS IS CLAIM-RPT-STATUS.
      *
           SELECT ERRORPT
              ASSIGN TO ERRORPT            *>REPORT FOR INVALID RECORDS
              FILE STATUS IS ERR-RPT-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  CLAIMSIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 90 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS CLAIM-RECORD-WS.
      *
           COPY CLAIMREC. *> COPYBOOK RECORD LAYOUT FOR CLAIMS FILE
      *
       FD  CLAIMRPT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 162 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS CLAIM-RPT-REC.
      *
       01 CLAIM-RPT-REC   PIC X(162).
      *
       FD  ERRORPT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 160 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS ERRORPT-REC.
      *
       01 ERRORPT-REC     PIC  X(160).

       WORKING-STORAGE SECTION.
      *
*      01 HEADER-REC-1.   *>CLAIMS REPORT HEADER RECORD
          05 REPORT-DATE  PIC 9999/99/99.
          05 FILLER       PIC X(45)  VALUE SPACES.
          05 FILLER       PIC X(25)  VALUE 'GROUP CLAIMS DAILY REPORT'.
          05 FILLER       PIC X(80)  VALUE SPACES.
      *
       01 HEADER-REC-2.   *>CLAIMS REPORT HEADER RECORD
          05 FILLER       PIC X(55)  VALUE ALL SPACES.
          05 FILLER       PIC X(25)  VALUE ALL '-'.
          05 FILLER       PIC X(80)  VALUE ALL SPACES.
      *
       01 HEADER-REC-3.   *>CLAIMS REPORT HEADER RECORD
          05 FILLER       PIC X(15)  VALUE 'POLICY'.
          05 FILLER       PIC X(05)  VALUE SPACES.
          05 FILLER       PIC X(08)  VALUE 'POLICY'.
          05 FILLER       PIC X(05)  VALUE SPACES.
          05 FILLER       PIC X(10)  VALUE 'FIRST'.
          05 FILLER       PIC X(05)  VALUE SPACES.
          05 FILLER       PIC X(15)  VALUE 'LAST'.
          05 FILLER       PIC X(05)  VALUE SPACES.
          05 FILLER       PIC X(10)  VALUE 'RENEW'.
          05 FILLER       PIC X(05)  VALUE SPACES.
          05 FILLER       PIC X(06)  VALUE 'DEDUCT'.
          05 FILLER       PIC X(05)  VALUE SPACES.
          05 FILLER       PIC X(10)  VALUE 'COPAY'.
          05 FILLER       PIC X(08)  VALUE SPACES.
          05 FILLER       PIC X(10)  VALUE 'DEDUCT'.
          05 FILLER       PIC X(05)  VALUE SPACES.
          05 FILLER       PIC X(10)  VALUE 'CLAIM'.
          05 FILLER       PIC X(05)  VALUE SPACES.
          05 FILLER       PIC X(10)  VALUE 'CLAIM'.
          05 FILLER       PIC X(08)  VALUE SPACES.
      *
       01 HEADER-REC-4.   *>CLAIMS REPORT HEADER RECORD
          05 FILLER       PIC X(15)  VALUE 'TYPE'.
          05 FILLER       PIC X(05)  VALUE SPACES.
          05 FILLER       PIC X(08)  VALUE 'NUMBER'.
          05 FILLER       PIC X(05)  VALUE SPACES.
          05 FILLER       PIC X(10)  VALUE 'NAME'.
          05 FILLER       PIC X(05)  VALUE SPACES.
          05 FILLER       PIC X(15)  VALUE 'NAME'.
          05 FILLER       PIC X(05)  VALUE SPACES.
          05 FILLER       PIC X(10)  VALUE 'DATE'.
          05 FILLER       PIC X(05)  VALUE SPACES.
          05 FILLER       PIC X(05)  VALUE 'MET'.
          05 FILLER       PIC X(06)  VALUE SPACES.
          05 FILLER       PIC X(10)  VALUE 'PERCENT'.
          05 FILLER       PIC X(08)  VALUE SPACES.
          05 FILLER       PIC X(10)  VALUE 'AMOUNT'.
          05 FILLER       PIC X(05)  VALUE SPACES.
          05 FILLER       PIC X(10)  VALUE 'AMOUNT'.
          05 FILLER       PIC X(05)  VALUE SPACES.
          05 FILLER       PIC X(10)  VALUE 'PAID'.
          05 FILLER       PIC X(08)  VALUE SPACES.
      *
       01 HEADER-REC-5.   *>CLAIMS REPORT HEADER RECORD
          05 FILLER       PIC X(15)  VALUE ALL '-'.
          05 FILLER       PIC X(05)  VALUE SPACES.
          05 FILLER       PIC X(08)  VALUE ALL '-'.
          05 FILLER       PIC X(05)  VALUE SPACES.
          05 FILLER       PIC X(10)  VALUE ALL '-'.
          05 FILLER       PIC X(05)  VALUE SPACES.
          05 FILLER       PIC X(15)  VALUE ALL '-'.
          05 FILLER       PIC X(05)  VALUE SPACES.
          05 FILLER       PIC X(10)  VALUE ALL '-'.
          05 FILLER       PIC X(05)  VALUE SPACES.
          05 FILLER       PIC X(05)  VALUE ALL '-'.
          05 FILLER       PIC X(05)  VALUE SPACES.
          05 FILLER       PIC X(10)  VALUE ALL '-'.
          05 FILLER       PIC X(07)  VALUE SPACES.
          05 FILLER       PIC X(10)  VALUE ALL '-'.
          05 FILLER       PIC X(05)  VALUE SPACES.
          05 FILLER       PIC X(10)  VALUE ALL '-'.
          05 FILLER       PIC X(05)  VALUE SPACES.
          05 FILLER       PIC X(10)  VALUE ALL '-'.
          05 FILLER       PIC X(10)  VALUE SPACES.
      *
       01 HEADER-REC-BLANK. *>BLANK REPORT LINE
          05 FILLER       PIC X(160) VALUE SPACES.
      *
       01 CLAIM-RPT-DETAIL. *>CLAIMS REPORT DETAIL RECORD
          05 INSURED-POLICY-TYPE-O        PIC X(15).
          05 FILLER                       PIC X(05)  VALUE SPACES.
          05 INSURED-POLICY-NUMBER-O      PIC X(09).
          05 FILLER                       PIC X(04)  VALUE SPACES.
          05 INSURED-FIRST-NAME-O         PIC X(10).
          05 FILLER                       PIC X(05)  VALUE SPACES.
          05 INSURED-LAST-NAME-O          PIC X(15).
          05 FILLER                       PIC X(05)  VALUE SPACES.
          05 POLICY-RENEW-DATE-O.
             10 POLICY-RENEW-DATE-YEAR-O  PIC 9(4).
             10 FILLER                    PIC X(01)  VALUE "/".
             10 POLICY-RENEW-DATE-MONTH-O PIC 9(02).
             10 FILLER                    PIC X(01)  VALUE "/".
             10 POLICY-RENEW-DATE-DAY-O   PIC 9(02).
          05 FILLER                       PIC X(07)  VALUE SPACES.
          05 POLICY-DEDUCT-MET-O          PIC X(05).
          05 FILLER                       PIC X(05)  VALUE SPACES.
          05 POLICY-COPY-PERCENT-GROUP.
             10 POLICY-COPAY-PERCENT-O    PIC 99.9.
             10 FILLER                    PIC X(01)  VALUE '%'.
          05 FILLER                       PIC X(10)  VALUE SPACES.
          05 POLICY-DEDUCT-AMOUNT-O       PIC $$$,$$9.99.
          05 FILLER                       PIC X(05)  VALUE SPACES.
          05 CLAIM-AMOUNT-O               PIC $$$,$$9.99.
          05 FILLER                       PIC X(05)  VALUE SPACES.
          05 CLAIM-AMOUNT-PAID-O          PIC $$$,$$9.99.
          05 FILLER                       PIC X(10).

       01 ERROR-RPT-HEADER-BLANK. *>BLANK REPORT LINE
          05 FILLER       PIC X(160) VALUE SPACES.

       01 ERROR-RPT-HEADER-1. *>ERROR REPORT HEADER RECORD
          05 FILLER       PIC X(50)  VALUE SPACES.
          05 FILLER       PIC X(46)  VALUE 'CLAIMS FILE DATA INPUT ERROR
      -    'S REPORT'.
          05  FILLER      PIC X(64)  VALUE SPACES.

       01 ERROR-RPT-HEADER-2. *>ERROR REPORT HEADER RECORD
          05 FILLER       PIC X(50)  VALUE SPACES.
          05 FILLER       PIC X(46)  VALUE ALL '-'.
          05 FILLER       PIC X(64)  VALUE SPACES.

       01 ERROR-RPT-HEADER-3. *>ERROR REPORT HEADER RECORD
          05 FILLER       PIC X(13)  VALUE 'POLICY NUMBER'.
          05 FILLER       PIC X(05)  VALUE SPACES.
          05 FILLER       PIC X(15)  VALUE 'LAST NAME'.
          05 FILLER       PIC X(05)  VALUE SPACES.
          05 FILLER       PIC X(10)  VALUE 'FIRST NAME'.
          05 FILLER       PIC X(05)  VALUE SPACES.
          05 FILLER       PIC X(25)  VALUE 'FIELD IN ERROR'.
          05 FILLER       PIC X(05)  VALUE SPACES.
          05 FILLER       PIC X(70)  VALUE 'ERROR DESCRIPTION'.
          05 FILLER       PIC X(07)  VALUE SPACES.

       01 ERROR-RPT-HEADER-4. *>ERROR REPORT HEADER RECORD
          05 FILLER       PIC X(13)  VALUE ALL '-'.
          05 FILLER       PIC X(05)  VALUE SPACES.
          05 FILLER       PIC X(15)  VALUE ALL '-'.
          05 FILLER       PIC X(05)  VALUE SPACES.
          05 FILLER       PIC X(10)  VALUE ALL '-'.
          05 FILLER       PIC X(05)  VALUE SPACES.
          05 FILLER       PIC X(25)  VALUE ALL '-'.
          05 FILLER       PIC X(05)  VALUE SPACES.
          05 FILLER       PIC X(70)  VALUE ALL '-'.
          05 FILLER       PIC X(07)  VALUE SPACES.

       01 ERROR-RPT-DETAIL.  *>ERROR REPORT DETAIL RECORD
          05 INSURED-POLICY-NO-ERR   PIC X(09).
          05 FILLER                  PIC X(09)  VALUE SPACES.
          05 INSURED-LAST-NAME-ERR   PIC X(15).
          05 FILLER                  PIC X(05)  VALUE SPACES.
          05 INSURED-FIRST-NAME-ERR  PIC X(10).
          05 FILLER                  PIC X(05)  VALUE SPACES.
          05 FIELD-IN-ERROR          PIC X(25).
          05 FILLER                  PIC X(05)  VALUE SPACES.
          05 FIELD-IN-ERROR-DESC     PIC X(70).
          05 FILLER                  PIC X(07)  VALUE SPACES.
      *
       01 WS-CLAIM-RECORD. *>CLAIMS INPUT FILE WORKING STORAGE HOLD AREA
          05 WS-INSURED-DETAILS.
             10 WS-INSURED-POLICY-NO        PIC 9(07).
             10 WS-INSURED-LAST-NAME        PIC X(15).
             10 WS-INSURED-FIRST-NAME       PIC X(10).
          05 WS-POLICY-DETAILS.
             10 WS-POLICY-TYPE              PIC 9.
                88 VALID-POLICY-TYPES       VALUE 1, 2, 3.
                88 WS-PRIVATE               VALUE 1.
                88 WS-MEDICARE              VALUE 2.
                88 WS-AFFORDABLE-CARE       VALUE 3.
             10 WS-POLICY-BENEFIT-DATE-NUM  PIC 9(08).
             10 WS-POLICY-BENEFIT-DATE-X REDEFINES
                   WS-POLICY-BENEFIT-DATE-NUM  PIC X(08).
             10 WS-POLICY-BENEFIT-PERIOD-RDF REDEFINES
                   WS-POLICY-BENEFIT-DATE-NUM.
                15 WS-POLICY-YEAR           PIC 9(04).
                15 WS-POLICY-MONTH          PIC 9(02).
                15 WS-POLICY-DAY            PIC 9(02).
             10 WS-POLICY-AMOUNT            PIC S9(7)V99.
             10 WS-POLICY-DEDUCTIBLE-PAID   PIC S9(4).
             10 WS-POLICY-COINSURANCE       PIC V99.
          05 WS-CLAIM-DETAILS.
             10 WS-CLAIM-AMOUNT             PIC S9(7)V99.
             10 WS-CLAIM-AMOUNT-PAID        PIC S9(7)V99.
          05 FILLER                         PIC X(6).
      *
       01 WS-PROGRAM-SWITCHES.
          05 WS-CLAIMSIN-EOF         PIC X(01)  VALUE SPACES.
             88 END-OF-CLAIMSIN                 VALUE 'Y'.
          05 INPUT-ERROR-SW          PIC X(01).
             88 DATA-INPUT-ERROR                VALUE 'Y'.
          05 WS-DEDUCTIBLE-MET-SW    PIC X(01).
             88 DEDUCTIBLE-MET                  VALUE 'Y'.
      *
       01 WS-FILE-STATUS-VARIABLES.
           05 CLAIM-STATUS           PIC X(02). *>CLAIMS INPUT FILE
           05 CLAIM-RPT-STATUS       PIC X(02). *>CLAIMS REPORT FILE
           05 ERR-RPT-STATUS         PIC X(02). *>ERROR REPORT FILE
      *
       01 WS-TEMP-VARIABLES.
          05 WS-GREG-DATE                 PIC 9(8).
          05 WS-CALC-RENEW-DATE.
             10 WS-CALC-RENEW-YEAR        PIC 9(4).
             10 WS-CALC-RENEW-MONTH       PIC 9(2).
             10 WS-CALC-RENEW-DAY         PIC 9(2).
          05 WS-DEDUCT-PERCENT            PIC V999.
          05 WS-CLAIM-BAL-AVAIL           PIC S9(7)V99.
          05 WS-RENEW-DATE-YEAR           PIC 9(4).
          05 WS-CALC-DEDUCT-AMOUNT        PIC S9(5)V99.
      *
        01 WS-DISPLAY-EDIT-VARIABLES.
           05 WS-POLICY-NUMBER-EDITED      PIC X(09).
           05 WS-POLICY-AMOUNT-EDIT        PIC $,$$$,$$9.99.
           05 WS-POLICY-DED-PAID-EDIT      PIC $$,$$$.99.
           05 WS-POLICY-DED-AMT-EDIT       PIC $,$$$.99.
           05 WS-POLICY-COINSURANCE-EDIT   PIC 99.99.
           05 WS-CLAIM-AMOUNT-EDIT         PIC $,$$$,$$9.99.
           05 WS-CLAIM-AMOUNT-PAID-EDIT    PIC $,$$$,$$9.99.
           05 WS-CLAIM-AMOUNT-TO-PAY       PIC S9(7)V99.
      *
       PROCEDURE DIVISION.
      *    DISPLAY 'ENTERING PROCEDURE DIVISION'.
      *
      *****************************************************************
      * DESCRIPTION:                                                  *
      *  CODE AT THE TOP OF THE PROCEDURE DIVISION PERFORMS THE       *
      *  FOLLOWING FUNCTIONS:                                         *
      *     -  HOUSEKEEPING (ONE-TIME AND INITIALIZATION) FUNCTIONS   *
      *     -  CALL TO THE MAIN CLAIM PROCESSING PARAGRAPH UNTIL THE  *
      *        END OF THE CLAIM INPUT FILE IS REACHED                 *
      *      - CALL TO THE PARAGRAPH TO CLOSE INPUT AND OUTPUT FILES  *
      * CALLED BY:                                                    *
      *     - NONE                                                    *
      * CALLS:                                                        *
      *    -  0000-HOUSEKEEPING                                       *
      *    -  0400-MAIN-PROCESS                                       *
      *    -  1500-CLOSE-FILES                                        *
      *****************************************************************
      *
           PERFORM 0000-HOUSEKEEPING.
           PERFORM 0400-MAIN-PROCESS
               UNTIL END-OF-CLAIMSIN.
           PERFORM 1500-CLOSE-FILES.
           GOBACK.

       0000-HOUSEKEEPING.
      *    DISPLAY 'ENTERING PARA 0000-HOUSEKEEPING'.
      *
      *****************************************************************
      * DESCRIPTION:                                                  *
      *  THIS PARAGRAPH PERFORMS/CALLS THE FOLLOWING FUNCTIONS:       *
      *     -  INITALIZE WORKING STORAGE VARIABLES                    *
      *     -  SET THE FIXED DEDUCTIBLE PERCENT                       *
      *     -  CALL PARAGRAPH TO OPEN FILES FOR INPUT/OUTPUT          *
      *     -  CALLL PARAGRAPH TO PERFORM THE FIRST READ OF THE       *
      *        CLAIMS INPUT FILE                                      *
      *     -  CALL PARAGRAPH TO WRITE HEADERS FOR THE CLAIMS REPORT  *
      *     -  CALL PARAGRAPH TO WRITE HEADERS FOR THE ERROR REPORT   *
      * CALLED BY:                                                    *
      *     - MAIN PROCEDURE STATEMENT                                *
      * CALLS:                                                        *
      *     -  0200-OPEN-FILES                                        *
      *     -  0300-READ-INPUT-FILE                                   *
      *     -  0900-WRITE-CLAIM-RPT-HEADERS                           *
      *     -  1000-WRITE-ERROR-RPT-HEADERS                           *
      *****************************************************************
      *
           INITIALIZE WS-CLAIM-RECORD,
                      ERROR-RPT-DETAIL,
                      WS-TEMP-VARIABLES,
                      WS-FILE-STATUS-VARIABLES,
                      WS-DISPLAY-EDIT-VARIABLES.
      *
           MOVE .002 TO WS-DEDUCT-PERCENT.  *> FIXED DEDUCTIBLE PERCENT
      *
           PERFORM 0200-OPEN-FILES.
           PERFORM 0300-READ-INPUT-FILE.
           PERFORM 0900-WRITE-CLAIM-RPT-HEADERS.
           PERFORM 1000-WRITE-ERROR-RPT-HEADERS.
      *
       0200-OPEN-FILES.
      *    DISPLAY 'ENTERING PARA 0200-OPEN-FILES'.
      *
      *****************************************************************
      * DESCRIPTION:                                                  *
      *  OPEN THE CLAIMS FILE FOR INPUT, THE CLAIMS REPORT FILE FOR   *
      *  OUTPUT AND THE ERROR REPORT FOR OUTPUT.  FOR EACH OPEN       *
      *  OPERATION, CHECK THE FILE STATUS FOR A SUCCESSFUL OPEN. IF   *
      *  THE OPEN OPERATION IS NOT SUCCESSFUL, DISPLAY AN ERROR       *
      *  MESSAGE.                                                     *
      * CALLED BY:                                                    *
      *     -  0000-HOUSEKEEPING                                      *
      * CALLS:                                                        *
      *     -  NONE                                                   *
      *****************************************************************
      *
           OPEN INPUT CLAIMSIN.  *>CLAIMS INPUT FILE
           IF CLAIM-STATUS EQUAL '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR ENCOUNTERED OPENING CLAIMS INPUT FILE'
           END-IF.
      *
           OPEN OUTPUT CLAIMRPT.  *>CLAIMS REPORT FILE
           IF CLAIM-RPT-STATUS EQUAL '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR ENCOUNTERED OPENING CLAIMS REPORT OUTPUT FI
      -               'LE'
           END-IF.
      *
           OPEN OUTPUT ERRORPT    *>ERROR REPORT FILE
           IF ERR-RPT-STATUS EQUAL '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR ENCOUNTERED OPENING ERROR REPORT OUTPUT FIL
      -                'E.'
            END-IF.

       0300-READ-INPUT-FILE.
      *    DISPLAY 'ENTERING PARA 0300-READ-INPUT-FILE'.
      *
      *****************************************************************
      * DESCRIPTION:                                                  *
      *  READ A CLAIMS INPUT FILE RECORD INTO THE WS-CLAIM-RECORD     *
      *  WORKING STORAGE AREA. SET THE END OF FILE SWITCH TO 'Y' WHEN *
      *  THE END OF THE CLAIMS FILE IS ENCOUNTERED.  CONFIRM THAT THE *
      *  READ OPERATION IS SUCCESFFUL (FILE STATUS = '00').  DISPLAY  *
      *  AN ERROR MESSAGE IF THE READ OPERATION IS NOT SUCCESSFUL.    *
      *  SET THE INPUT DATA ERROR SWITCH TO 'N' BEFORE PROCESSING     *
      *  THE RECORD.                                                  *
      * CALLED BY:                                                    *
      *     -  0000-HOUSEKEEPING                                      *
      *     -  0400-MAIN-PROCESS                                      *
      * CALLS:                                                        *
      *     -  NONE                                                   *
      *****************************************************************
      *
           READ CLAIMSIN INTO WS-CLAIM-RECORD
               AT END MOVE 'Y' TO WS-CLAIMSIN-EOF
           END-READ.
      *
           IF CLAIM-STATUS EQUAL '00' OR '10' *> '10' MEANS END OF FILE
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR ENCOUNTERED READING CLAIMS INPUT FILE'
           END-IF.
      *
           MOVE 'N' TO INPUT-ERROR-SW. *>SWITCH TRACKS DATA INPUT ERRORS
      *
       0400-MAIN-PROCESS.
      *    DISPLAY 'ENTERING PARA 0400-MAIN-PROCESS'.
      *
      *****************************************************************
      * DESCRIPTION:                                                  *
      *  THIS PARAGRAPH PROCESSES CLAIMS INPUT RECORDS UNTIL THE END  *
      *  OF THE CLAIMS FILE IS ENCOUNTERED. THE FOLLOWING ACTIVITES   *
      *  ARE PERFORMED/CALLED FROM THIS PARAGRAPH:                    *
      *     - CALL TO THE ERROR VALIDATION PARAGRAPH.  UPON RETURN    *
      *       FROM THE VALIDATION PARAGRAPH, CHECK TO SEE IF A        *
      *       VALIDATION ERROR OCCURRED WITH THE CURRENT RECORD. IF   *
      *       THE CURRENT RECORD CONTAINS VALIDATION ERRORS, DO NOT   *
      *       CALCULATE CLAIMS PAYMENT DATA AND SKIP TO THE CALL TO   *
      *       THE PARAGRAPH TO READ THE INPUT FILE.                   *
      *     - IF THE INPUT RECORD IS ERROR FREE, COMPARE THE CLAIM    *
      *       AMOUNT ON THE RECORD WITH THE MAXIMUM ALLOWABLE CLAIM   *
      *       AMOUNT ($999,999.98)                                    *
      *     - IF THE CLAIM AMOUNT IS > THE MAX ALLOWABLE CLAIM AMOUNT *
      *       CALL PARAGRAPHS TO MOVE FIELDS TO THE ERROR REPORT AND  *
      *       WRITE THE ERRORED RECORD.                               *
      *     - IF THE CLAIM AMOUNT IS < THE MAX ALLOWABLE CLAIM, CALL  *
      *       THE PARAGRAPH TO CALCULATE THE PAYABLE CLAIM AMOUNT.    *
      *       UPON RETURN TO THIS PARAGRAPH, READ THE NEXT CLAIM      *
      *       INPUT RECORD.                                           *
      * CALLED BY:                                                    *
      *     -  0000-HOUSEKEEPING                                      *
      * CALLS:                                                        *
      *     -  0500-VALIDATE-INPUT-DATA                               *
      *     -  1200-MOVE-FIELDS-TO-ERROR-RPT                          *
      *     -  1400-WRITE-ERROR-RPT-DETAIL                            *
      *     -  0600-PROCESS-CLAIM                                     *
      *     -  0300-READ-INPUT-FILE                                   *
      *****************************************************************
      *
           PERFORM 0500-VALIDATE-INPUT-DATA.
      *
           IF DATA-INPUT-ERROR *>DATA INPUT ERROR ECOUNTERED
              NEXT SENTENCE
           ELSE
              IF WS-CLAIM-AMOUNT > 9999999.98
                 MOVE 'CLAIM AMOUNT' TO FIELD-IN-ERROR
                 MOVE 'CLAIM AMOUNT IS GREATER THAN MAXIMUM ALLOWABLE CL
      -                'AIM FOR THIS POLICY' TO FIELD-IN-ERROR-DESC
                 MOVE 'Y' TO INPUT-ERROR-SW
                 PERFORM 1200-MOVE-FIELDS-TO-ERROR-RPT
                 PERFORM 1400-WRITE-ERROR-RPT-DETAIL
              ELSE
                 PERFORM 0600-PROCESS-CLAIM
              END-IF
           END-IF.
      *
           PERFORM 0300-READ-INPUT-FILE.
      *
       0500-VALIDATE-INPUT-DATA.
      *    DISPLAY 'ENTERING PARA 0500-VALIDATE-INPUT-DATA'.
      *
      *****************************************************************
      * DESCRIPTION:                                                  *
      *  THIS PARAGRAPH VALIDATES FIELDS ON CLAIMS INPUT FILE RECORDS.*
      *  THE ROUTINE CHECKS FIELDS ON THE INPUT RECORD FOR VALID FIELD*
      *  VALUES, FIELD POPULATION AND NUMERIC FIELD TYPES. A RECORD   *
      *  CAN BE FLAGGED FOR MULTIPLE DATA INPUT ERRORS. WHEN ERRORS   *
      *  ARE ENCOUNTERED, CALLS TO PARAGRAPHS TO MOVE FIELDS TO THE   *
      *  ERROR REPORT AND TO WRITE THE ERROR RECORD ARE CALLED. UPON  *
      *  RETURN FROM THE CALLED ROTUINES, THE INPUT DATA ERROR SWITCH *
      *  IS SET TO 'Y' TO INDICATE CLAIMS PAYMENT INFORMATON FOR THE  *
      *  ERRORED RECORD WILL NOT BE CALCULATED.                       *
      * CALLED BY:                                                    *
      *     -  0400-MAIN-PROCESS                                      *
      * CALLS:                                                        *
      *     -  1200-MOVE-FIELDS-TO-ERROR-RPT                          *
      *     -  1400-WRITE-ERROR-RPT-DETAIL                            *
      *****************************************************************
      *
           MOVE 'N' TO INPUT-ERROR-SW.
      *
           IF WS-INSURED-POLICY-NO IS NUMERIC
              NEXT SENTENCE
           ELSE
              MOVE 'POLICY NUMBER'       TO FIELD-IN-ERROR
              MOVE 'POLICY NUMBER MUST BE NUMERIC'
                                         TO FIELD-IN-ERROR-DESC
              MOVE 'Y' TO INPUT-ERROR-SW
              PERFORM 1200-MOVE-FIELDS-TO-ERROR-RPT
              PERFORM 1400-WRITE-ERROR-RPT-DETAIL
              INITIALIZE ERROR-RPT-DETAIL
           END-IF.
      *
           IF WS-INSURED-POLICY-NO EQUAL 0
              MOVE 'POLICY NUMBER'      TO FIELD-IN-ERROR
              MOVE 'POLICY NUMBER CANNOT BE ZEROS'
                                        TO FIELD-IN-ERROR-DESC
              MOVE 'Y'  TO INPUT-ERROR-SW
              PERFORM 1200-MOVE-FIELDS-TO-ERROR-RPT
              PERFORM 1400-WRITE-ERROR-RPT-DETAIL
              INITIALIZE ERROR-RPT-DETAIL
           END-IF.

           IF WS-INSURED-LAST-NAME EQUAL SPACES
              MOVE 'LAST NAME' TO FIELD-IN-ERROR
              MOVE 'LAST NAME MUST BE POPULATED'
                                         TO FIELD-IN-ERROR-DESC
              MOVE 'Y' TO INPUT-ERROR-SW
              PERFORM 1200-MOVE-FIELDS-TO-ERROR-RPT
              PERFORM 1400-WRITE-ERROR-RPT-DETAIL
              INITIALIZE ERROR-RPT-DETAIL
           END-IF.

           IF WS-INSURED-FIRST-NAME EQUAL SPACES
              MOVE 'FIRST NAME'          TO FIELD-IN-ERROR
              MOVE 'FIRST NAME MUST BE POPULATED' TO FIELD-IN-ERROR-DESC
              MOVE 'Y' TO INPUT-ERROR-SW
              PERFORM 1200-MOVE-FIELDS-TO-ERROR-RPT
              PERFORM 1400-WRITE-ERROR-RPT-DETAIL
              INITIALIZE ERROR-RPT-DETAIL
           END-IF.

           IF VALID-POLICY-TYPES
              NEXT SENTENCE
           ELSE
              MOVE 'POLICY TYPE'         TO FIELD-IN-ERROR
              MOVE 'INVALID POLICY TYPE.  VALID VALUES ARE 1, 2 OR 3'
                                         TO FIELD-IN-ERROR-DESC
              MOVE 'Y' TO INPUT-ERROR-SW
              PERFORM 1200-MOVE-FIELDS-TO-ERROR-RPT
              PERFORM 1400-WRITE-ERROR-RPT-DETAIL
              INITIALIZE ERROR-RPT-DETAIL
           END-IF.

           IF WS-POLICY-BENEFIT-DATE-X EQUAL SPACES
              MOVE 'POLICY BENEFIT DATE' TO FIELD-IN-ERROR
              MOVE 'POLICY BENEFIT DATE MUST BE POPULATED'
                                         TO FIELD-IN-ERROR-DESC
              MOVE 'Y' TO INPUT-ERROR-SW
              PERFORM 1200-MOVE-FIELDS-TO-ERROR-RPT
              PERFORM 1400-WRITE-ERROR-RPT-DETAIL
              INITIALIZE ERROR-RPT-DETAIL
           END-IF.

           IF WS-POLICY-AMOUNT IS NUMERIC
              NEXT SENTENCE
           ELSE
              MOVE 'POLICY AMOUNT'       TO FIELD-IN-ERROR
              MOVE 'POLICY AMOUNT MUST BE NUMERIC'
                                         TO FIELD-IN-ERROR-DESC
              MOVE 'Y' TO INPUT-ERROR-SW
              PERFORM 1200-MOVE-FIELDS-TO-ERROR-RPT
              PERFORM 1400-WRITE-ERROR-RPT-DETAIL
           END-IF.


           IF WS-POLICY-DEDUCTIBLE-PAID IS NUMERIC
              NEXT SENTENCE
           ELSE
              MOVE 'POLICY DEDUCTIBLE PAID'  TO FIELD-IN-ERROR
              MOVE 'POLICY DEDUCTIBLE PAID MUST BE NUMERIC'
                                             TO FIELD-IN-ERROR-DESC
              MOVE 'Y' TO INPUT-ERROR-SW
              PERFORM 1200-MOVE-FIELDS-TO-ERROR-RPT
              PERFORM 1400-WRITE-ERROR-RPT-DETAIL
              INITIALIZE ERROR-RPT-DETAIL
           END-IF.


           IF WS-POLICY-COINSURANCE IS NUMERIC
              NEXT SENTENCE
           ELSE
              MOVE 'POLICY COINSURANCE'      TO FIELD-IN-ERROR
              MOVE 'POLICY COINSURANCE MUST BE NUMERIC'
                                             TO FIELD-IN-ERROR-DESC
              MOVE 'Y' TO INPUT-ERROR-SW
              PERFORM 1200-MOVE-FIELDS-TO-ERROR-RPT
              PERFORM 1400-WRITE-ERROR-RPT-DETAIL
              INITIALIZE ERROR-RPT-DETAIL
           END-IF.


           IF WS-CLAIM-AMOUNT IS NUMERIC
              NEXT SENTENCE
           ELSE
              MOVE 'CLAIM AMOUNT'            TO FIELD-IN-ERROR
              MOVE 'CLAIM AMOUNT MUST BE NUMERIC'
                                             TO FIELD-IN-ERROR-DESC
              MOVE 'Y' TO INPUT-ERROR-SW
              PERFORM 1200-MOVE-FIELDS-TO-ERROR-RPT
              PERFORM 1400-WRITE-ERROR-RPT-DETAIL
              INITIALIZE ERROR-RPT-DETAIL
           END-IF.

           IF WS-CLAIM-AMOUNT-PAID IS NUMERIC
              NEXT SENTENCE
           ELSE
              MOVE 'CLAIM AMOUNT PAID'       TO FIELD-IN-ERROR
              MOVE 'CLAIM AMOUNT PAID MUST BE NUMERIC'
                                             TO FIELD-IN-ERROR-DESC
              MOVE 'Y' TO INPUT-ERROR-SW
              PERFORM 1200-MOVE-FIELDS-TO-ERROR-RPT
              PERFORM 1400-WRITE-ERROR-RPT-DETAIL
              INITIALIZE ERROR-RPT-DETAIL
           END-IF.
      *
       0600-PROCESS-CLAIM.
      *    DISPLAY 'ENTERING PARA 0600-PROCESS-CLAIM'.
      *
      *****************************************************************
      * DESCRIPTION:                                                  *
      *  THIS PARAGRAPH:                                              *
      *     - CALLS THE PARAGRAPH TO CALCULATE THE CLAIM DEDUCTIBLE   *
      *     - CALLS THE PARAGRPAH TO CALCULATE THE CLAIM AMOUNT THE   *
      *       INSURANCE COMPANY WILL PAY FOR THE CLAIM.               *
      *     - CALCULATES THE DIFFERENCE BETWEEN THE CALCULATED        *
      *       INSURANCE COMPANY PAYMENT AND THE BALANCE OF THE AMOUNT *
      *       REAMINING ON THE POLICY TO PAY CLAIMS                   *
      *     - IF THE CALCULATED CLAIM PAYMENT AMOUNT IS LESS THAN THE *
      *       BALANCE OF THE AMOUNT REMAINING ON THE POLICY LEFT TO   *
      *       PAY THE CLAIM, CALL PARAGRAPHS TO MOVE FIELDS TO THE    *
      *       CLAIM REPORT AND WRITE THE RECORD.
      *     - IF THE CALCULATED CLAIM PAYMENT AMOUNT IS MORE THAN THE *
      *       BALANCE OF THE AMOUNT ON THE POLICY REMAINING TO PAY    *
      *       THE CLAIM (NO MONEY LEFT ON THE POLCIY TO PAY THE CLAIM)*
      *       CALL THE PARAGRAPHS TO MOVE FIELDS TO THE ERROR REPORT  *
      *       AND WRITE THE ERRRORED RECORD.                          *
      * CALLED BY:                                                    *
      *     -  0400-MAIN-PROCESS                                      *
      * CALLS:                                                        *
      *     -  0700-CALCULATE-DEDUCTIBLE                              *
      *     -  0800-CALCUATE-CLAIM-PAY-AMOUNT                         *
      *     -  1200-MOVE-FIELDS-TO-ERROR-RPT                          *
      *     -  1300-WRITE-CLAIM-RPT-DETAIL                            *
      *****************************************************************
      *
           PERFORM 0700-CALCULATE-DEDUCTIBLE.
           PERFORM 0800-CALCUATE-CLAIM-PAY-AMOUNT.
      *
           COMPUTE WS-CLAIM-BAL-AVAIL ROUNDED = WS-POLICY-AMOUNT -
                                                WS-CLAIM-AMOUNT-TO-PAY
               ON SIZE ERROR
                  DISPLAY 'A VARIABLE SIZE ERROR OCCURRED WHEN CALCULATI
      -              'NG THE POLICY AMOUNT AVAIABLE FOR PAYING CLAIMS'
           END-COMPUTE.
      *
           IF WS-CLAIM-BAL-AVAIL > 0
              PERFORM 1100-MOVE-FIELDS-TO-CLAIM-RPT
              PERFORM 1300-WRITE-CLAIM-RPT-DETAIL
           ELSE
              MOVE 'CLAIM PAYABLE AMOUNT EXCEEDS AVAILABLE POLICY BALANC
      -            'E.' TO FIELD-IN-ERROR-DESC
              MOVE 'CALCULATED CLAIM PAYBLE AMOUNT'
                        TO FIELD-IN-ERROR
              PERFORM 1200-MOVE-FIELDS-TO-ERROR-RPT
              PERFORM 1300-WRITE-CLAIM-RPT-DETAIL
           END-IF.


       0700-CALCULATE-DEDUCTIBLE.
      *    DISPLAY 'ENTERING PARA 0700-CALCULATE-DEDUCTIBLE'.
      *
      *****************************************************************
      * DESCRIPTION:                                                  *
      *  THIS PARAGRAPH CALCULATES THE DEDUCTIBLE AMOUNT FOR A CLAIM  *
      *  BY MULTIPLYING THE POLICY AMOUNT LEFT FOR PAYING CLAIMS BY   *
      *  THE FIXED DEDUCTIBLE PERCENT (.002)                          *
      * CALLED BY:                                                    *
      *     -  0600-PROCESS-CLAIM                                     *
      * CALLS:                                                        *
      *     -  NONE                                                   *
      *****************************************************************
      *
           COMPUTE WS-CALC-DEDUCT-AMOUNT ROUNDED = WS-POLICY-AMOUNT *
                                                   WS-DEDUCT-PERCENT
                 ON SIZE ERROR
                  DISPLAY 'A VARIABLE SIZE ERROR OCCURRED WHEN CALCULATI
      -              'NG THE CLAIM DEDUCTIBLE AMOUNT'
           END-COMPUTE.
      *
       0800-CALCUATE-CLAIM-PAY-AMOUNT.
      *    DISPLAY 'ENTERING PARA 0800-CALCUATE-CLAIM-PAY-AMOUNT'.
      *
      *****************************************************************
      * DESCRIPTION:                                                  *
      *  IF THE CALCULATED DEDUCTIBLE AMOUNT IS >= THE DEDUCTIBLE PAID*
      *  ON PRIOR CLAIMS, CALCULATE THE AMOUNT THE INSURANCE COMPANY  *
      *  WILL PAY FOR THE CLAIM AND SET A SWITCH TO INDICATE THAT THE *
      *  DEDUCTIBLE HAS BEEN MET.  OTHERWISE, CALCULATE THE AMOUNT THE*
      *  INSURANCE COMPANY WILL PAY ON THE CLAIM BY SUBTRACTING THE   *
      *  CALCULATED DEDUCTIBLE AMOUNT FROM THE CLAIM PAYABLE CALCULA- *
      *  TION AND SETTING THE DEDUCTIBLE-MET SWITCH TO 'N'.           *
      * CALLED BY:                                                    *
      *     -  0600-PROCESS-CLAIM                                     *
      * CALLS:                                                        *
      *     -  NONE                                                   *
      *****************************************************************
      *
      *****************************************************************
      *  ROUND THE RESULT OF THE PAYABLE CLAIM AMOUNT CALCULATION AND *
      *  CHECK FOR A SIZE ERROR IF THE CALCULATION PRODUCES A RESULT  *
      *  TOO LARGE FOR THE RESULTING VARIABLE.                        *
      *****************************************************************
      *
           IF WS-CALC-DEDUCT-AMOUNT >= WS-POLICY-DEDUCTIBLE-PAID
              COMPUTE WS-CLAIM-AMOUNT-TO-PAY ROUNDED =
                 WS-CLAIM-AMOUNT -
                   (WS-POLICY-COINSURANCE * WS-CLAIM-AMOUNT)
                      ON SIZE ERROR
                         DISPLAY 'A VARIABLE SIZE ERROR OCCURRED WHEN CA
      -              'LCULATING THE CLAIM AMOUNT INSURANCE WILL PAY'
              END-COMPUTE
              MOVE 'Y' TO WS-DEDUCTIBLE-MET-SW *> DEDUCTIBLE MET
           ELSE
              COMPUTE WS-CLAIM-AMOUNT-TO-PAY ROUNDED =
                 (WS-CLAIM-AMOUNT - WS-CALC-DEDUCT-AMOUNT) -
                 (WS-POLICY-COINSURANCE * WS-CLAIM-AMOUNT)
                      ON SIZE ERROR
                        DISPLAY  'A VARIABLE SIZE ERROR OCCURRED WHEN CA
      -              'LCULATING THE CLAIM AMOUNT INSURANCE WILL PAY'
              END-COMPUTE
              MOVE 'N' TO WS-DEDUCTIBLE-MET-SW *> DEDUCTIBLE NOT MET
           END-IF.
      *
       0900-WRITE-CLAIM-RPT-HEADERS.
      *    DISPLAY 'ENTERING PARA 0900-WRITE-CLAIM-RPT-HEADERS'.
      *
      *****************************************************************
      * DESCRIPTION:                                                  *
      *  THIS PARAGRAPH WRITES REPORT AND COLUMN HEADERS ON THE CLAIM *
      *  REPORT, CHECKS THE FILE STATUS FOR A SUCCESSFUL WRITE        *
      *  OPERATION (STATUS = '00') AND DISPLAYS AN ERROR IF AN ISSUE  *
      *  IS ENCOUNTERED WITH WRITING TO THE FILE.                     *
      * CALLED BY:                                                    *
      *     -  0000-HOUSEKEEPING                                      *
      * CALLS:                                                        *
      *     -  NONE                                                   *
      *****************************************************************
      *
      *
      *****************************************************************
      *  MOVE THE FIRST 8 DIGITS OF THE CURRENT-DATE SYSTEM VARIABLE  *
      *  TO THE CLAIM REPORT HEADER.                                  *
      *****************************************************************
      *
            MOVE FUNCTION CURRENT-DATE(1:8) TO REPORT-DATE.
      *
      *****************************************************************
      *  WRITE THE CLAIM REPORT HEADER RECORDS FROM FORMATTED REPORT  *
      *  LINES DEFINED IN WORKING STORAGE. CHECK THE FILE STATUS FOR  *
      *  EACH WRITE OPERATION.  IF THE WRITE OPERATION IS NOT SUCCESS- *
      *  FUL, DISPLAY AN ERROR MESSAGE.                               *
      *****************************************************************
      *
           WRITE CLAIM-RPT-REC FROM HEADER-REC-1.*> REPORT HEADER
           IF CLAIM-RPT-STATUS EQUAL '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR ENCOUNTERED WRITING TO CLAIMS REPORT FILE'
           END-IF.
     *
           WRITE CLAIM-RPT-REC FROM HEADER-REC-2. *> REPORT HEADER LINES
           IF CLAIM-RPT-STATUS EQUAL '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR ENCOUNTERED WRITING TO CLAIMS REPORT FILE'
           END-IF.
      *
           WRITE CLAIM-RPT-REC FROM HEADER-REC-BLANK. *>BLANK LINE
           IF CLAIM-RPT-STATUS EQUAL '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR ENCOUNTERED WRITING TO CLAIMS REPORT FILE'
           END-IF.
      *
           WRITE CLAIM-RPT-REC FROM HEADER-REC-3 *>COLUMN HEADERS
               AFTER ADVANCING 1 LINE.
           IF CLAIM-RPT-STATUS EQUAL '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR ENCOUNTERED WRITING TO CLAIMS REPORT FILE'
           END-IF.
      *
           WRITE CLAIM-RPT-REC FROM HEADER-REC-4 *>COLUMN HEADERS
               AFTER ADVANCING 1 LINE.

           WRITE CLAIM-RPT-REC FROM HEADER-REC-5 *>COLUMN HEADER LINES
               AFTER ADVANCING 1 LINE.
           IF CLAIM-RPT-STATUS EQUAL '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR ENCOUNTERED WRITING TO CLAIMS REPORT FILE'
           END-IF.
      *
       1000-WRITE-ERROR-RPT-HEADERS.
      *    DISPLAY 'ENTERING PARA 1000-WRITE-ERROR-RPT-HEADERS'.
      *
      *****************************************************************
      * DESCRIPTION:
      *  WRITE THE ERROR REPORT HEADER RECORDS FROM FORMATTED REPORT  *
      *  LINES DEFINED IN WORKING STORAGE. CHECK THE FILE STATUS FOR  *
      *  EACH WRITE OPERATION. IF THE WRITE OPERATION IS NOT SUCCESS- *
      *  FUL, DISPLAY AN ERROR MESSAGE.                               *
      * CALLED BY:                                                    *
      *     -  0000-HOUSEKEEPING                                      *
      * CALLS:                                                        *
      *     -  NONE                                                   *
      *****************************************************************
      *
           WRITE ERRORPT-REC FROM ERROR-RPT-HEADER-BLANK. *> BLANK LINE
           IF ERR-RPT-STATUS EQUAL '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR ENCOUNTERED WRITING TO ERROR REPORT FILE.'
           END-IF.
      *
           WRITE ERRORPT-REC FROM ERROR-RPT-HEADER-1 *>REPORT HEADER
               AFTER ADVANCING 1 LINES
           IF ERR-RPT-STATUS EQUAL '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR ENCOUNTERED WRITING TO ERROR REPORT FILE.'
           END-IF.
      *
           WRITE ERRORPT-REC FROM ERROR-RPT-HEADER-2*>*>REPORT HEADER LN
               AFTER ADVANCING 1 LINES.
           IF ERR-RPT-STATUS EQUAL '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR ENCOUNTERED WRITING TO ERROR REPORT FILE.'
           END-IF.
      *
           WRITE ERRORPT-REC FROM ERROR-RPT-HEADER-BLANK.*>BLANK LINE
           IF ERR-RPT-STATUS EQUAL '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR ENCOUNTERED WRITING TO ERROR REPORT FILE.'
           END-IF.
      *
           WRITE ERRORPT-REC FROM ERROR-RPT-HEADER-3 *>COLUMN HEADERS
               AFTER ADVANCING 1 LINES.
           IF ERR-RPT-STATUS EQUAL '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR ENCOUNTERED WRITING TO ERROR REPORT FILE.'
           END-IF.
      *
           WRITE ERRORPT-REC FROM ERROR-RPT-HEADER-4 *>COLUMN HEADERS LN
               AFTER ADVANCING 1 LINES.
           IF ERR-RPT-STATUS EQUAL '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR ENCOUNTERED WRITING TO ERROR REPORT FILE.'
           END-IF.
      *
       1100-MOVE-FIELDS-TO-CLAIM-RPT.
      *    DISPLAY 'ENTERING PARA 1100-MOVE-FIELDS-TO-CLAIM-RPT'.
      *
      *****************************************************************
      * DESCRIPTION:                                                  *
      *  THIS PARAGRAPH MOVES CALIMS RECORD AND CALCULATED FIELDS TO  *
      *  CLAIMS REPORT FIELDS.                                        *
      * CALLED BY:                                                    *
      *     - 0600-PROCESS-CLAIM                                      *
      * CALLS:                                                        *
      *     - 1225-FORMAT-POLICY-NUMBER                               *
      *****************************************************************
      *
      *
      *****************************************************************
      *  CHECK THE POLICY TYPE VALUE (88 LEVELS) AND MOVE A LITERAL   *
      *  TO THE CLAIM REPORT FIELD, BASED ON THE VALUE OF THE POLICY  *
      *  TYPE.                                                        *
      *****************************************************************
      *
           EVALUATE TRUE
              WHEN PRIVATE
                 MOVE 'PRIVATE' TO INSURED-POLICY-TYPE-O
              WHEN MEDICARE
                 MOVE 'MEDICARE' TO INSURED-POLICY-TYPE-O
              WHEN AFFORDABLE-CARE
                 MOVE 'AFFORDABLE CARE' TO INSURED-POLICY-TYPE-O
              WHEN OTHER
                DISPLAY 'INVALID POLICY TYPE: ' INSURED-POLICY-TYPE-O
           END-EVALUATE.
      *
           PERFORM 1225-FORMAT-POLICY-NUMBER.
      *
           MOVE WS-POLICY-NUMBER-EDITED TO INSURED-POLICY-NUMBER-O.
      *
      *****************************************************************
      *  USE THE UPPER-CASE FUNCTION TO CAPITALIZE ALL LETTERS IN THE *
      *  INSURED'S FIRST AND LAST NAMES                               *
      *****************************************************************
      *
           MOVE FUNCTION UPPER-CASE (INSURED-FIRST-NAME)
                                        TO INSURED-FIRST-NAME-O.
           MOVE FUNCTION UPPER-CASE (INSURED-LAST-NAME)
                                        TO INSURED-LAST-NAME-O.
      *
      *****************************************************************
      *  CALCULATE THE POLICY RENEWAL DATE.  MOVE POLICY-BENEFIT-     *
      *  PERIOD-RDF TO A DECOMPOSED DATE FIELD AND ADD 1 TO THE YEAR. *
      *  THE POLICY RENEWAL DATE IS ONE YEAR AFTER THE POLICY EFFEC-  *
      *  TIVE DATE.                                                   *
      *****************************************************************
      *
           MOVE WS-POLICY-BENEFIT-PERIOD-RDF
                                        TO WS-CALC-RENEW-DATE.
           ADD 1 TO WS-CALC-RENEW-YEAR.
           MOVE WS-CALC-RENEW-YEAR      TO POLICY-RENEW-DATE-YEAR-O.
           MOVE WS-CALC-RENEW-MONTH     TO POLICY-RENEW-DATE-MONTH-O.
           MOVE WS-CALC-RENEW-DAY       TO  POLICY-RENEW-DATE-DAY-O.
      *
      *****************************************************************
      *  IF THE DEDUCTIBLE MET SWITCH IS SET, MOVE 'Y' TO THE DEDUCT- *
      *  IBLE MET FIELD ON THE CLAIM REPORT.  OTHERWISE, MOVE 'N'.    *
      *****************************************************************
      *
           IF DEDUCTIBLE-MET
              MOVE 'Y'  TO POLICY-DEDUCT-MET-O
           ELSE
              MOVE 'N'  TO POLICY-DEDUCT-MET-O
           END-IF.
      *
      *****************************************************************
      *  CONVERT THE DECIMAL VALUE OF THE COINSURANCE TO A PERCENTAGE *
      *  BY MULTIPLYING IT BY 100. ROUND THE RESULT AND CHECK FOR A   *
      *  SIZE ERROR IF THE CALCULATION PRODUCES A RESULT TOO LARGE    *
      *  FOR THE RESULTING VARIABLE.                                  *
      *****************************************************************
      *
           COMPUTE WS-POLICY-COINSURANCE-EDIT ROUNDED =
             (WS-POLICY-COINSURANCE * 100)
                ON SIZE ERROR
                   DISPLAY 'A VARIABLE SIZE ERROR OCCURRED WHEN CONVERTI
      -              'ING THE COINSURANCE TO A PERCENTAGE'
           END-COMPUTE.
      *
           MOVE WS-POLICY-COINSURANCE-EDIT
                                       TO POLICY-COPAY-PERCENT-O.
      *
           MOVE WS-CALC-DEDUCT-AMOUNT  TO POLICY-DEDUCT-AMOUNT-O.
           MOVE WS-CLAIM-AMOUNT        TO CLAIM-AMOUNT-O.
           MOVE WS-CLAIM-AMOUNT-TO-PAY TO
                                     CLAIM-AMOUNT-PAID-O.

           MOVE WS-CALC-DEDUCT-AMOUNT TO WS-POLICY-DED-AMT-EDIT.
           MOVE WS-POLICY-DEDUCTIBLE-PAID TO WS-POLICY-DED-PAID-EDIT
                WS-POLICY-DED-PAID-EDIT.
      *
       1200-MOVE-FIELDS-TO-ERROR-RPT.
      *    DISPLAY 'ENTERING PARA 1200-MOVE-FIELDS-TO-ERROR-RPT'.
      *
      *****************************************************************
      * DESCRIPTION:                                                  *
      *  THIS PARAGRAPH MOVES FIELDS TO THE ERROR REPORT.             *
      * CALLED BY:                                                    *
      *     -  0400-MAIN-PROCESS                                      *
      *     -  0500-VALIDATE-INPUT-DATA                               *
      *     -  0600-PROCESS-CLAIM                                     *
      * CALLS:                                                        *
      *     -  1225-FORMAT-POLICY-NUMBER                              *
      *****************************************************************
      *
           PERFORM 1225-FORMAT-POLICY-NUMBER.
      *
           MOVE WS-POLICY-NUMBER-EDITED TO INSURED-POLICY-NO-ERR.
      *
      *****************************************************************
      *  USE THE UPPER-CASE FUNCTION TO CAPITALIZE ALL LETTERS IN THE *
      *  INSURED'S FIRST AND LAST NAMES                               *
      *****************************************************************
      *
           MOVE FUNCTION UPPER-CASE(WS-INSURED-LAST-NAME)
                                      TO INSURED-LAST-NAME-ERR.
           MOVE FUNCTION UPPER-CASE(WS-INSURED-FIRST-NAME)
                                      TO INSURED-FIRST-NAME-ERR.
      *
       1225-FORMAT-POLICY-NUMBER.
      *    DISPLAY 'ENTERING PARA 1225-FORMAT-POLICY-NUMBER.'
      *
      *****************************************************************
      * DESCRIPTION:                                                  *
      *  THIS PARAGRAPH FORMATS THE POLICY NUMBER FOR THE CLAIM AND   *
      *  ERROR REPORTS BY USING THE STRING FUNCTION AND REERENCE      *
      *  MODIFICATION TO CONCATENATE LITERALS AND SPECIFIC BYTES      *
      *  OF THE INSURED POLICY NUMBER VARIABLE.                       *
      * CALLED BY:                                                    *
      *        -  1100-MOVE-FIELDS-TO-CLAIM-REPORT                    *
      *        -  1200-MOVE-FIELDS-TO-ERROR-RPT                       *
      * CALLS:                                                        *
      *        -  NONE
      *****************************************************************
      *
           STRING INSURED-POLICY-NO(1:1)
                  '-'
                  INSURED-POLICY-NO(2:3)
                  '-'
                  INSURED-POLICY-NO(5:3)  DELIMITED BY SIZE
                  INTO WS-POLICY-NUMBER-EDITED.
      *
       1300-WRITE-CLAIM-RPT-DETAIL.
      *    DISPLAY 'ENTERING PARA 1300-WRITE-CLAIM-RPT-DETAIL'.
      *
      *****************************************************************
      * DESCRIPTION:                                                  *
      *  THIS PARAGRAPH WRITES THE CLAIM DETAIL LINE TO THE CLAIM     *
      *  REPORT, CHECKS THE FILE STATUS FOR A SUCCESSFUL WRITE        *
      *  OPERATION (STATUS = '00') AND DISPLAYS AN ERROR IF AN ISSUE  *
      *  IS ENCOUNTERED WITH WRITING TO THE FILE.                     *
      * CALLED BY:                                                    *
      *     -  0600-PROCESS-CLAIM                                     *
      * CALLS:                                                        *
      *     -  NONE                                                   *
      *****************************************************************
      *
           WRITE CLAIM-RPT-REC FROM CLAIM-RPT-DETAIL *>CLAIM DETAIL LINE
              AFTER ADVANCING 1 LINE.
      *
           IF CLAIM-RPT-STATUS EQUAL '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR ENCOUNTERED WRITING TO CLAIMS REPORT FILE'
           END-IF.
      *
       1400-WRITE-ERROR-RPT-DETAIL.
      *    DISPLAY 'ENTERING PARA 1400-WRITE-ERROR-RPT-DETAIL'.
      *
      *****************************************************************
      * DESCRIPTION:                                                  *
      *  THIS PARAGRAPH WRITES THE ERROR REPORT DETAIL LINE TO THE    *
      *  ERROR REPORT, CHECKS THE FILE STATUS FOR A SUCCESSFUL WRITE  *
      *  OPERATION (STATUS = '00') AND DISPLAYS AN ERROR IF AN ISSUE  *
      *  IS ENCOUNTERED WITH WRITING TO THE FILE.                     *
      * CALLED BY:                                                    *
      *     -  0400-MAIN-PROCESS                                      *
      *     -  0500-VALIDATE-INPUT-DATA                               *
      * CALLS:                                                        *
      *     -  NONE                                                   *
      *****************************************************************
      *
           WRITE ERRORPT-REC FROM ERROR-RPT-DETAIL
              AFTER ADVANCING 1 LINES.
      *
           IF ERR-RPT-STATUS EQUAL '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR ENCOUNTERED WRITING TO ERROR REPORT FILE.'
           END-IF.
      *
       1500-CLOSE-FILES.
      *     DISPLAY 'ENTERING PARA 1500-CLOSE-FILES'.
      *
      *****************************************************************
      * DESCRIPTION:                                                  *
      *  THIS PARAGRAPH CLOSES INPUT AND OUTPUT FILES, CHECKS FOR A   *
      *  SUCCESSFUL FILE STATUS (STATUS = '00') AND DISPLAYS AN ERROR *
      *  MESSAGE IF AN ISSUE IS ENCOUNTERED CLOSING THE FILES.        *
      * CALLED BY:                                                    *
      *        - MAIN PROCEDURE DIVISION STATEMENT                    *
      * CALLS:                                                        *
      *        - NONE                                                 *
      *****************************************************************
      *
           CLOSE CLAIMSIN. *> CLAIMS INPUT FILE
           IF CLAIM-STATUS EQUAL '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR ENCOUNTERED CLOSING CLAIMS INPUT FILE'
           END-IF.
      *
           CLOSE CLAIMRPT.  *> CLAIMS REPORT
           IF CLAIM-RPT-STATUS EQUAL '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR ENCOUNTERED CLOSING CLAIMS REPORT FILE'
           END-IF.
      *
           CLOSE ERRORPT.    *> ERROR REPORT
           IF ERR-RPT-STATUS EQUAL '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR ENCOUNTERED CLOSING ERROR REPORT FILE.'
           END-IF.
