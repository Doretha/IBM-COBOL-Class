       IDENTIFICATION DIVISION.
       PROGRAM-ID.  HOSPEDIT.
       AUTHOR. DORETHA RILEY.
       INSTALLATION. COBOL DEV CENTER.
       DATE-WRITTEN. 01/01/08.
       DATE-COMPILED. 08/16/20.
       SECURITY. NON-CONFIDENTIAL.
.
      *
      *****************************************************************
      *  DESCRIPTION                                                  *
      *    THIS PROGRAM READS A HOSPITAL PATIENT FILE AND AN INSURANCE*
      *    TYPE FILE WITH VALID INSURANCE TYPE CODES. THE INSURANCE   *
      *    TYPE FILE IS LOADED INTO A TABLE (ARRAY). THE INSURANCE    *
      *    TYPE FIELD ON THE HOSPITAL PATIENT RECORD IS USED TO SEARCH*
      *    THE INSURANCE TYPE TABLE. IF THERE IS A MATCH, THE PROGRAM *
      *    CHECKS FOR A VALID PATIENT TYPE ON THE HOSPITAL PATIENT    *
      *    RECORD.  IF THE INSURANCE TYPE IS FOUND ON THE INSURANCE   *
      *    TYPE TABLE AND THE PATIENT TYPE IS VALID, CALCULATIONS AND *
      *    TOTALS ARE PERFORMED ON THE RECORD AND THE RECORD IS       *
      *    WRITTEN TO AN OUTPUT FILE FOR DOWNSTREAM PROCESSING, AN    *
      *    UNFORMATTED REPORT FILE THAT CONTAINS GOOD RECORDS AND A   *
      *    FORMATTED REPORT FILE THAT CONTAINS GOOD RECORDS. IF THE   *
      *    INSURANCE TYPE ON THE HOSPITAL PAIENT RECORD IS NOT        *
      *    CONTAINED IN THE INSURANCE TYPE TABLE OR IF THE PATIENT    *
      *    TYPE IS INVALID ON THE HOSPITAL PATIENT RECORD, THE RECORD *
      *    IS WRITTEN TO A FORMATTED ERROR REPORT.                    *
      *****************************************************************
      *                                                               *
      *    INPUT FILE:                                                *
      *      RTPOT44.HOSPIN.DATA (HOSPITAL PATIENT FILE)              *
      *      INTERNAL FILE NAME:  HOSPIN                              *
      *      JCL DD NAME:         HOSPIN                              *
      *                                                               *
      *      RTPOT44.REUSABLE.COBOL4(INSTYPE), (INSURANCE TYPE FILE)  *
      *      INTERNAL FILE NAME:  INSTFIL                             *
      *      JCL DD NAME:         INSTFIL                             *
      *                                                               *
      *    OUTPUT FILES:                                              *
      *      DD SYSOUT=* (GOOD OUTPUT RECORDS FOR DOWNSTREAM PROCESS) *
      *      INTERNAL FILE NAME:  OUTFILE                             *
      *      JCL DD NAME:         HOSPOUT                             *
      *                                                               *
      *      DD SYSOUT=*  (RECORDS WITH INVALID INSURANCE OR PATIENT) *
      *                   TYPE)                                       *
      *      INTERNAL FILE NAME:  ERRFILE                             *
      *      JCL DD NAME:         ERRFILE                             *
      *                                                               *
      *      DD SYSOUT=*  (UNFORMATTED REPORT WITH GOOD RECORDS)      *
      *      INTERNAL FILE NAME:  RPTFILE                             *
      *      JCL DD NAME:         RPTFILE                             *
      *                                                               *
      *      DD SYSOUT=*  (FORMATTED REPORT WITH GOOD RECORDS)        *
      *      INTERNAL FILE NAME:  NEWRPT                              *
      *      JCL DD NAME:         NEWRPT                              *
      *                                                               *
      *    JCL JOB: RTPOT44.LEARN.REUSABLE.JCL(HOSPEDIT)              *
      *****************************************************************
      *  CHANGE LOG: *                                                *
      ****************                                                *
      *      UPDATED BY:  DORETHA RILEY                               *
      *            DATE:  08/16/2020                                  *
      *     DESCRIPTION:  ADDED A NEWRPT REPORT FILE, INSURANCE TYPE  *
      *                   FILE, TABLE AND SEARCH AND FORMATTED ERROR  *
      *                   REPORT. REORGANIZED PARAGRAPHS.             *
      *                                                               *
      *                                                               *
      *      CREATED BY:  JOHN SAYLES                                 *
      *     DESCRIPTION:  ORIGINAL CREATION OF PROGRAM                *
      *            DATE:  01/01/2008                                  *
      *****************************************************************
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-390.
       OBJECT-COMPUTER. IBM-390.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT HOSPIN           *>HOSPITAL PATIENT INPUT FILE
           ASSIGN TO HOSPIN
             FILE STATUS IS IFCODE.
      *
           SELECT INSTFILE         *>INSURANCE TYPE INPUT FILE
           ASSIGN TO INSTFILE
             FILE STATUS IS TFCODE.
      *
           SELECT OUTFILE          *>OUTPUT FILE OF GOOD INPUT RECOREDS
           ASSIGN TO HOSPOUT
             FILE STATUS IS OFCODE.
      *
           SELECT ERRFILE          *>ERROR REPORT FOR INVALID RECORDS
           ASSIGN TO ERRFILE
             FILE STATUS IS EFCODE.
      *
           SELECT RPTFILE          *>UNFORMATTED REPORT FILE - GOOD RECS
           ASSIGN TO RPTFILE
             FILE STATUS IS RFCODE.

      *
           SELECT NEWRPT           *>FORMATTED REPORT FILE - GOOD RECS
           ASSIGN TO NEWRPT
             FILE STATUS IS NFCODE.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  HOSPIN
           RECORDING MODE IS F
           RECORD CONTAINS 100 CHARACTERS
           DATA RECORD IS HOSPIN-REC.
      *
       01  HOSPIN-REC  PIC X(100).
      *
       FD  INSTFILE
           RECORDING MODE IS F
           RECORD CONTAINS 80 CHARACTERS
           DATA RECORD IS INS-TYPE-REC.
      *
       01  INS-TYPE-REC PIC X(80).
      *
       FD  OUTFILE
           RECORDING MODE IS F
           RECORD CONTAINS 133 CHARACTERS
           DATA RECORD IS OUT-REC.
      *
       01  OUT-REC  PIC X(133).

       FD  ERRFILE
           RECORDING MODE IS F
           RECORD CONTAINS 133 CHARACTERS
           DATA RECORD IS ERR-REC.
      *
       01  ERR-REC  PIC X(133).
      *
       FD  RPTFILE
           RECORDING MODE IS F
           RECORD CONTAINS 133 CHARACTERS
           DATA RECORD IS HOSP-RPT-REC.
      *
       01  HOSP-RPT-REC PIC X(133).

       FD  NEWRPT
           RECORDING MODE IS F
           RECORD CONTAINS 133 CHARACTERS
           DATA RECORD IS NEW-RPT-REC.
      *
       01  NEW-RPT-REC PIC X(133).
      *
       WORKING-STORAGE SECTION.
       01  FILE-STATUS-CODES.
           05  IFCODE              PIC X(02).
               88 I-CODE-READ        VALUE SPACES.
               88 NO-MORE-DATA     VALUE "10".
           05  RFCODE              PIC X(02).
               88 R-CODE-WRITE       VALUE SPACES.
           05  OFCODE              PIC X(02).
               88 O-CODE-WRITE       VALUE SPACES.
           05  EFCODE              PIC X(02).
               88 E-CODE-WRITE       VALUE SPACES.
           05  TFCODE              PIC X(02).
               88 END-OF-INSTFILE  VALUE 'Y'.
           05  NFCODE              PIC X(02).
               88 N-CODE-WRTIE       VALUE SPACES.

       77  INS-COVERAGE-PERC      PIC 9(03) VALUE 10.

       01 WS-NEWRPT-HEADER-1.
          05 FILLER               PIC X(50) VALUE SPACES.
          05 FILLER               PIC X(23) VALUE
                                  'HOSPITAL PATIENT REPORT'.
          05 FILLER               PIC X(50) VALUE SPACES.

       01 WS-NEWRPT-HEADER-2.
          05 FILLER               PIC X(50) VALUE SPACES.
          05 FILLER               PIC X(23) VALUE ALL "=".
          05 FILLER               PIC X(50) VALUE SPACES.

       01 WS-NEWRPT-HEADER-3.
           05 FILLER              PIC X(05) VALUE 'PATNO'.
           05 FILLER              PIC X(02) VALUE SPACES.
           05 FILLER              PIC X(21) VALUE 'PATIENT'.
           05 FILLER              PIC X(02) VALUE SPACES.
           05 FILLER              PIC X(14) VALUE 'PHONE-NO'.
           05 FILLER              PIC X(02) VALUE SPACES.
           05 FILLER              PIC X(04) VALUE 'TYPE'.
           05 FILLER              PIC X(02) VALUE SPACES.
           05 FILLER              PIC X(04) VALUE 'BED'.
           05 FILLER              PIC X(02) VALUE SPACES.
           05 FILLER              PIC X(06) VALUE 'ADMIT'.
           05 FILLER              PIC X(02) VALUE SPACES.
           05 FILLER              PIC X(10) VALUE 'DAILY AMT'.
           05 FILLER              PIC X(02) VALUE SPACES.
           05 FILLER              PIC X(04) VALUE 'DIAG'.
           05 FILLER              PIC X(03) VALUE SPACES.
           05 FILLER              PIC X(03) VALUE 'INS'.
           05 FILLER              PIC X(02) VALUE SPACES.
           05 FILLER              PIC X(04) VALUE 'STAY'.
           05 FILLER              PIC X(04) VALUE SPACES.
           05 FILLER              PIC X(07) VALUE 'NETWORK'.
           05 FILLER              PIC X(02) VALUE SPACES.
           05 FILLER              PIC X(05) VALUE 'COPAY'.
           05 FILLER              PIC X(02) VALUE SPACES.
           05 FILLER              PIC X(06) VALUE 'DEDUCT'.

        01 WS-NEWRPT-HEADER-4.
           05 FILLER              PIC X(05) VALUE ALL '='.
           05 FILLER              PIC X(02) VALUE SPACES.
           05 FILLER              PIC X(21) VALUE ALL '='.
           05 FILLER              PIC X(02) VALUE SPACES.
           05 FILLER              PIC X(14) VALUE ALL '='.
           05 FILLER              PIC X(02) VALUE SPACES.
           05 FILLER              PIC X(04) VALUE ALL '='.
           05 FILLER              PIC X(02) VALUE SPACES.
           05 FILLER              PIC X(04) VALUE ALL '='.
           05 FILLER              PIC X(02) VALUE SPACES.
           05 FILLER              PIC X(06) VALUE ALL '='.
           05 FILLER              PIC X(02) VALUE SPACES.
           05 FILLER              PIC X(10) VALUE ALL '='.
           05 FILLER              PIC X(02) VALUE SPACES.
           05 FILLER              PIC X(04) VALUE ALL '='.
           05 FILLER              PIC X(03) VALUE SPACES.
           05 FILLER              PIC X(03) VALUE ALL '='.
           05 FILLER              PIC X(02) VALUE SPACES.
           05 FILLER              PIC X(04) VALUE ALL '='.
           05 FILLER              PIC X(04) VALUE SPACES.
           05 FILLER              PIC X(07) VALUE ALL '='.
           05 FILLER              PIC X(02) VALUE SPACES.
           05 FILLER              PIC X(05) VALUE ALL '=' .
           05 FILLER              PIC X(02) VALUE SPACES.
           05 FILLER              PIC X(06) VALUE ALL '='.

       01 WS-NEWRPT-BLANK-LINE    PIC X(133) VALUE SPACES.

       01 WS-NEWRPT-DETAIL.
           05  PATIENT-NBR-R        PIC 9(05).
           05  FILLER               PIC X(02) VALUE SPACES.
           05  PATIENT-NAME-R       PIC X(21).
           05  FILLER               PIC X(02).
           05  PATIENT-PHONE-R.
               10 FILLER            PIC X(01) VALUE '('.
               10 NPA-R             PIC X(03).
               10 FILLER            PIC X(02) VALUE ') '.
               10 NXX-R             PIC X(03).
               10 FILLER            PIC X(01) VALUE '-'.
               10 EXCHANGE-R        PIC X(04).
           05  FILLER               PIC X(03) VALUE SPACES.
           05  PATIENT-TYPE-R       PIC X(01).
           05  FILLER               PIC X(04) VALUE SPACES.
           05  BED-IDENTITY-R       PIC 9(04).
           05  FILLER               PIC X(02) VALUE SPACES.
           05  DATE-ADMIT-R         PIC X(06).
           05  FILLER               PIC X(02) VALUE SPACES.
           05  DAILY-AMOUNT-R       PIC $$$,$$$.99.
           05  FILLER               PIC X(02) VALUE SPACES.
           05  DIAGNOSTIC-CODE-R    PIC 999.
           05  FILLER               PIC X(04) VALUE SPACES.
           05  INS-TYPE-R           PIC X(03).
           05  FILLER               PIC X(02) VALUE SPACES.
           05  HOSPITAL-STAY-LTH-R  PIC 999.
           05  FILLER               PIC X(07) VALUE SPACES.
           05  IN-OUT-NETWORK-R     PIC X(03).
           05  FILLER               PIC X(05) VALUE SPACES.
           05  COPAY-R              PIC $$$$.
           05  FILLER               PIC X(02) VALUE SPACES.
           05  DEDUCTIBLE-R         PIC $$$$$.
      *
       01 WS-ERR-REC-HEADER-1.
          05 FILLER                PIC X(51) VALUE SPACES.
          05 FILLER                PIC X(30) VALUE 'HOSPITAL PATIENT ERR
      -                           'OR REPORT'.
          05 FILLER                PIC X(51) VALUE SPACES.
      *
       01 WS-ERR-REC-HEADER-2.
          05 FILLER               PIC X(51) VALUE SPACES.
          05 FILLER               PIC X(30) VALUE ALL '='.
          05 FILLER               PIC X(51) VALUE SPACES.
      *
       01 WS-ERR-REC-HEADER-3.
          05  FILLER               PIC X(11) VALUE 'PATIENT NO.'.
          05  FILLER               PIC X(02) VALUE SPACES.
          05  FILLER               PIC X(21) VALUE 'PATIENT NAME'.
          05  FILLER               PIC X(02) VALUE SPACES.
          05  FILLER               PIC X(15) VALUE 'FIELD IN ERROR'.
          05  FILLER               PIC X(02) VALUE SPACES.
          05  FILLER               PIC X(11) VALUE 'FIELD VALUE'.
          05  FILLER               PIC X(02) VALUE SPACES.
          05  FILLER               PIC X(70) VALUE 'ERROR MESSAGE'.
      *
        01 WS-ERR-REC-HEADER-4.
          05  FILLER               PIC X(11) VALUE ALL '='.
          05  FILLER               PIC X(02) VALUE SPACES.
          05  FILLER               PIC X(21) VALUE ALL '='.
          05  FILLER               PIC X(02) VALUE SPACES.
          05  FILLER               PIC X(15) VALUE ALL '='.
          05  FILLER               PIC X(02) VALUE SPACES.
          05  FILLER               PIC X(11) VALUE ALL '='.
          05  FILLER               PIC X(02) VALUE SPACES.
          05  FILLER               PIC X(70) VALUE ALL '='.
      *
       01 WS-ERR-REC-DETAIL.
          05  PATIENT-NBR-E        PIC 9(05).
          05  FILLER               PIC X(08) VALUE SPACES.
          05  PATIENT-NAME-E       PIC X(21).
          05  FILLER               PIC X(02).
          05  FIELD-IN-ERROR-E     PIC X(15).
          05  FILLER               PIC X(05) VALUE SPACES.
          05  FIELD-IN-ERR-VALUE-E PIC X(05).
          05  FILLER               PIC X(05) VALUE SPACES.
          05  ERROR-MESSAGE-E      PIC X(70) VALUE SPACES.
      *
       01 WS-ERR-REC-BLANK-LINE.
          05  FILLER               PIC X(133).
      *
       01  WS-OUTPUT-REC.
           05  PATIENT-NBR-O          PIC 9(05).
           05  FILLER                 PIC X(02) VALUE SPACES.
           05  PATIENT-NAME-O         PIC X(20).
           05  PATIENT-PHONE-O        PIC X(10).
           05  FILLER                 PIC X(02) VALUE SPACES.
           05  PATIENT-TYPE-O         PIC X(02).
           05  BED-IDENTITY-O         PIC ZZZ9.
           05  FILLER                 PIC X(02) VALUE SPACES.
           05  CURR-DATE-O            PIC X(06).
           05  FILLER                 PIC X(02) VALUE SPACES.
           05  PATIENT-AMT-PER-DAY-O  PIC $$,$$9.99.
           05  FILLER                 PIC X(02) VALUE SPACES.
           05  INS-COVERAGE-PERC-O    PIC 999.
           05  FILLER                 PIC X(02) VALUE SPACES.
           05  INS-TYPE-O             PIC X(04).
           05  HOSPITAL-STAY-LTH-O    PIC 999.
           05  FILLER                 PIC X(07) VALUE SPACES.
      *
       01 WS-TOTALS-REC-1.
          05 FILLER                   PIC X(38)
                                      VALUE "TOTAL RECORDS READ:  ".
           05 TOTAL-RECS-READ-O       PIC 9(3).
      *
       01 WS-TOTALS-REC-2.
          05  FILLER                  PIC X(38)
                 VALUE "TOTAL GOOD RECORDS WRITTEN:  ".
          05  TOTAL-GOOD-RECS-WRITTEN-O  PIC 9(03).
      *
       01 WS-TOTALS-REC-3.
          05  FILLER                  PIC X(38)
                 VALUE "TOTAL ERROR RECORDS:  ".
          05  TOTAL-ERROR-RECS-O      PIC 9(03).
      *
       01 WS-TOTALS-REC-4.
          05  FILLER                  PIC X(38)
                 VALUE "TOTAL INPATIENT RECS:  ".
          05  TOTAL-INPATIENT-RECS-O  PIC 9(03).
      *
       01 WS-TOTALS-REC-5.
          05  FILLER                  PIC X(38)
                 VALUE "TOTAL OUTPATIENT RECS:  ".
          05  TOTAL-OUTPATIENT-RECS-O PIC 9(03).
      *
       01 WS-TOTALS-REC-6.
          05  FILLER                  PIC X(38)
                 VALUE "TOTAL HMO ACCOUNTS:  ".
          05  TOTAL-HMO-ACCOUNTS-O    PIC 9(03).
      *
       01 WS-TOTALS-REC-7.
          05  FILLER                  PIC X(38)
                 VALUE "TOTAL PPO ACCOUNTS:  ".
          05  TOTAL-PPO-ACCOUNTS-O    PIC 9(03).
      *
       01 WS-TOTALS-REC-8.
          05  FILLER                  PIC X(38)
                 VALUE "TOTAL PRIVATE ACCOUNTS:  ".
          05  TOTAL-PRI-ACCOUNTS-O    PIC 9(03).
      *
       01 WS-TOTALS-REC-9.
          05  FILLER                  PIC X(38)
                 VALUE "TOTAL AFFORDABLE ACCOUNTS:  ".
          05  TOTAL-AFF-ACCOUNTS-O    PIC 9(03).
      *
       01 WS-TOTALS-REC-10.
          05  FILLER                  PIC X(38)
                 VALUE "TOTAL STATE/FED (MEDICARE) ACCOUNTS:  ".
          05  TOTAL-STATE-FED-O       PIC 9(03).
      *
       01 WS-TOTALS-REC-11.
          05  FILLER                  PIC X(38)
                 VALUE "TOTAL NO COVERAGE ACCOUNTS:  ".
          05  TOTAL-NO-COVERAGE-O     PIC 9(03).
      *
       01 WS-TOTALS-REC-12.
          05  FILLER                  PIC X(21)
                  VALUE "TOTAL AMOUNT GROSS:  ".
          05  TOTAL-GROSS-O           PIC $,$$$,$99.99.
      *
       01 WS-TOTALS-REC-13.
          05  FILLER                  PIC X(21)
                  VALUE "TOTAL AMOUNT NET:  ".
           05  TOTAL-NET-O            PIC $,$$$,$99.99.
      *
       01 WS-TOTALS-REC-14.
          05  FILLER                  PIC X(22)
                  VALUE "GROSS DAILY AMOUNT:  ".
           05  GROSS-DAILY-AMT-R      PIC $,$$$,$99.99.
      *
       01 WS-TOTALS-BLANK-LINE        PIC X(133) VALUE SPACES.
      *
       77  WS-DATE                     PIC 9(06).
      *
       01 WS-FILE-SWITCHES.
          05 HOSPIN-FILE-SW              PIC X(01) VALUE SPACE.
             88 END-OF-HOSPIN-FILE       VALUE 'Y'.
          05 INS-TYPE-FILE-SW          PIC X(01) VALUE SPACE.
             88 END-OF-INS-TYPE-FILE  VALUE 'Y'.
          05 VALID-INS-TYPE-SW           PIC X(01) VALUE SPACE.
             88 VALID-INS-TYPE           VALUE 'Y'.
      *
       01  COUNTERS-AND-ACCUMULATORS.
           05 RECORDS-READ        PIC S9(04) COMP.
           05 RECORDS-WRITTEN     PIC S9(04) COMP.
           05 ERROR-RECS          PIC S9(04) COMP.
           05 NBR-INPATIENTS      PIC S9(04) COMP.
           05 NBR-OUTPATIENTS     PIC S9(04) COMP.
           05 NBR-HMO             PIC S9(04) COMP.
           05 NBR-STATE-FED       PIC S9(04) COMP.
           05 NBR-NO-COVERAGE     PIC S9(04) COMP.
           05 NBR-PPO             PIC S9(04) COMP.
           05 NBR-PRIVATE         PIC S9(04) COMP.
           05 NBR-AFFORDABLE      PIC S9(04) COMP.
           05 PAT-TOTAL-AMT-NET   PIC S9(07)V99 COMP-3.
           05 TOTAL-AMT-GROSS     PIC S9(07)V99 COMP-3.
           05 TOTAL-AMT-NET       PIC S9(07)V99 COMP-3.
           05 GROSS-DAILY-AMT     PIC S9(07)V99 COMP-3.
      *
       01 WS-TEMP-VARS.
          05 MAX-HOSP-ENTRIES     PIC S9(03) COMP VALUE 14.
          05 WS-DAILY-AMOUNT      PIC S9(7)V99.
          05 WS-6-DIGIT-DATE      PIC X(06) VALUE SPACES.
      *
       01  WS-HOSP-REC.
           05  PATIENT-NBR        PIC 9(05).
           05  PATIENT-NAME.
               10 LAST-NAME       PIC X(10).
               10 FIRST-NAME      PIC X(10).
           05  PATIENT-PHONE      PIC X(10).
           05  PATIENT-PHONE-RDF REDEFINES PATIENT-PHONE.
               10 NPA             PIC X(03).
               10 NXX             PIC X(03).
               10 EXCHANGE        PIC X(04).
           05  PATIENT-TYPE       PIC X(01).
               88 INPATIENT           VALUE "I".
               88 OUTPATIENT          VALUE "O".
               88 VALID-PATIENT-TYPE  VALUES ARE "I", "O".
           05  BED-IDENTITY       PIC 9(04).
           05  DATE-ADMIT         PIC X(10).
           05  AMT-PER-DAY        PIC 9(05)V99.
           05  DIAGNOSTIC-CODE    PIC 999.
           05  INS-TYPE           PIC X(03).
           05  HOSPITAL-STAY-LTH  PIC 999.
           05  PATIENT-TOT-AMT    PIC 9(07)V99.
           05  PCP-ID             PIC X(06).
           05  IN-OUT-NETWORK     PIC X(01).
               88 IN-NETWORK      VALUE "N".
               88 OUT-OF-NETWORK  VALUE "O".
           05  COPAY              PIC S9(03).
           05  DEDUCTIBLE         PIC S9(04).
      *
       01 INS-TYPE-TABLE. *>INSURANCE TYPE TABLE
          05 INS-TYPE-ITEM OCCURS 5 TIMES INDEXED BY T-IDX PIC X(03).
              88  AFF VALUE 'AFF'.
              88  HMO VALUE 'HMO'.
              88  MED VALUE 'MED'.
              88  PPO VALUE 'PPO'.
              88  PRI VALUE 'PRI'.
              88  VALID-INS-TYPES VALUES 'AFF', 'HMO', 'MED', 'PPO',
                                        'PRI'.
          05 FILLER  PIC X(77) VALUE SPACES.
      *
       01 WS-INS-TYPE-REC.  *>WORKING STORAGE AREA FOR INS TYPE FILE
          05 WS-INS-TYPE-CODE     PIC X(03) VALUE SPACES.
          05 FILLER               PIC X(77) VALUE SPACES.

      *
      *****************************************************************
      *  DESCRIPTION:                                                 *
      *    THE MAIN PROCEDURE AREA PERFORMS HOUSEKEEPING INITIALI-    *
      *    ZATIONS, OPEN AND READ OF INPUT FILES, WRITE OF REPORT     *
      *    HEADERS, THE MAIN PROCESS UNTIL END OF FILE, WRITING TOTALS*
      *    AND CLOSING OF OPENED FILES.                               *
      *                                                               *
      *  CALLED BY:                                                   *
      *    - NONE                                                     *
      *                                                               *
      *  CALLS:                                                       *
      *    - 0000-HOUSEKEEPING                                        *
      *    - 0100-OPEN-FILES                                          *
      *    - 0200-READ-LOAD-INS-TYPE-FILE                             *
      *    - 0300-READ-HOSP-PATIENT-FILE                              *
      *    - 1200-WRITE-ERROR-RPT-HEADERS                             *
      *    - 1510-WRITE-NEWRPT-HEADERS                                *
      *    - 0400-MAIN-PROCESS                                        *
      *    - 1600-MOVE-TOTAL-FIELDS                                   *
      *    - 1700-WRIE-HOSPOUT-TOTALS                                 *
      *    - 1750-WRIE-NEWRPT-TOTALS                                  *
      *    - 1800-CLOSE-FILES                                         *
      *****************************************************************
      *
       PROCEDURE DIVISION.
           PERFORM 0000-HOUSEKEEPING.
           PERFORM 0100-OPEN-FILES.
           PERFORM 0200-READ-LOAD-INS-TYPE-FILE.
           PERFORM 0300-READ-HOSP-PATIENT-FILE.
           PERFORM 1200-WRITE-ERROR-RPT-HEADERS.
           PERFORM 1510-WRITE-NEWRPT-HEADERS.
           PERFORM 0400-MAIN-PROCESS
               UNTIL END-OF-HOSPIN-FILE.
           PERFORM 1600-MOVE-TOTAL-FIELDS.
           PERFORM 1700-WRIE-HOSPOUT-TOTALS.
           PERFORM 1750-WRIE-NEWRPT-TOTALS.
           PERFORM 1800-CLOSE-FILES.
           MOVE +0 TO RETURN-CODE.
           GOBACK.
      *
       0000-HOUSEKEEPING.
      *
      *****************************************************************
      *  DESCRIPTION:                                                 *
      *    THIS PARAGRAPH INITIALIZES WORKING STORAGE VARIABLES       *
      *                                                               *
      *  CALLED BY:                                                   *
      *    -  MAIN PROCEDURE AREA                                     *
      *                                                               *
      *  CALLS:                                                       *
      *    -  NONE                                                    *
      *****************************************************************
      *
           ACCEPT      WS-DATE FROM DATE.
           INITIALIZE  COUNTERS-AND-ACCUMULATORS,
                       WS-OUTPUT-REC,
                       INS-TYPE-TABLE,
                       WS-HOSP-REC,
                       WS-ERR-REC-DETAIL,
                       WS-FILE-SWITCHES,
                       WS-TOTALS-REC-1,
                       WS-TOTALS-REC-2,
                       WS-TOTALS-REC-3,
                       WS-TOTALS-REC-4,
                       WS-TOTALS-REC-5,
                       WS-TOTALS-REC-6,
                       WS-TOTALS-REC-7,
                       WS-TOTALS-REC-8,
                       WS-TOTALS-REC-9,
                       WS-TOTALS-REC-10,
                       WS-TOTALS-REC-11,
                       WS-TOTALS-REC-12,
                       WS-TOTALS-REC-13,
                       WS-TOTALS-REC-14.
      *
       0100-OPEN-FILES.
      *
      *****************************************************************
      *  DESCRIPTION:                                                 *
      *    THIS PARAGRAPH OPENS FILES FOR INPUT AND OUTPUT. THE FILE  *
      *    STATUS IS CHECKED AFTER THE OPEN OPERATION AND AN ERROR    *
      *    MESSAGE DISPLAYED IF THERE IS AN ISSUE WITH OPENING THE    *
      *    THE FILE.  THE FOLLOWING FILES ARE PROCESSED               *
      *    1. THE HOSPIN PATIENT FILE FOR INPUT                       *
      *    2. THE INSURANCE TYPE FILE FOR INPUT                       *
      *    3. THE OUTFILE FILE FOR OUTPUT                             *
      *    4. THE UNFORMATTED REPORT FILE FOR OUTPUT                  *
      *    5. THE FORMATTED REPORT FILE FOR OUTPUT                    *
      *    6. THE ERRORFILE REPORT FOR OUTPUT                         *
      *                                                               *
      *  CALLED BY:                                                   *
      *    -  MAIN PROCEDURE AREA                                     *
      *                                                               *
      *  CALLS:                                                       *
      *    -  NONE                                                    *
      *****************************************************************
      *
           OPEN INPUT HOSPIN.
           IF IFCODE = '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR ENCOUNTERED OPENING HOSPITAL INPUT FILE'
           END-IF.

           OPEN INPUT INSTFILE.
           IF TFCODE = '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR ENCOUNTERED OPENING INSURANCE TYPE FILE'
           END-IF.

           OPEN OUTPUT OUTFILE.
           IF OFCODE = '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR ENCOUNTERED OPENING OUTFILE'
           END-IF.

           OPEN OUTPUT ERRFILE.
           IF EFCODE = '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR ENCOUNTERED OPENING ERROR FILE'
           END-IF.

           OPEN OUTPUT RPTFILE.
           IF RFCODE = '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR ENCOUNTERED OPENING REPORT FILE'
           END-IF.

           OPEN OUTPUT NEWRPT.
           IF NFCODE = '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR ENCOUNTERED THE NEW REPORT FILE'
           END-IF.
      *
       0200-READ-LOAD-INS-TYPE-FILE.
      *
      *****************************************************************
      *  DESCRIPTION:                                                 *
      *    THIS PARAGRAPH READS THE INSURANCE TYPE FILE, CHECKS THE   *
      *    FILE STATUS AND DISPLAYS AN ERROR MESSAGE IF THE FILE      *
      *    STATUS IS NOT '00'.  THE INSURANCE TYPE TABLE IS LOADED AS *
      *    RECORDS ARE READ FROM THE INPUT FILE.                      *
      *                                                               *
      *  CALLED BY:                                                   *
      *    - MAIN PROCEDURE AREA                                      *
      *                                                               *
      *  CALLS:                                                       *
      *    -  NONE                                                    *
      *****************************************************************
      *
           READ INSTFILE INTO WS-INS-TYPE-REC
               AT END
                 MOVE 'Y' TO INS-TYPE-FILE-SW
           END-READ.
           IF TFCODE = '00' OR '10'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR ENCOUNTERED READIG INSURANCE TYPE FILE'
           END-IF.

           PERFORM VARYING T-IDX FROM 1 BY 1 UNTIL T-IDX > 5 OR
              END-OF-INS-TYPE-FILE
                 MOVE WS-INS-TYPE-CODE TO INS-TYPE-ITEM (T-IDX)
                 READ INSTFILE INTO WS-INS-TYPE-REC
                   AT END
                     MOVE 'Y' TO INS-TYPE-FILE-SW
                 END-READ
           END-PERFORM.
           IF TFCODE = '00'  OR '10'
              NEXT SENTENCE
           ELSE
              DISPLAY
                 'ERROR ENCOUNTERED READIG INSURANCE TYPE FILE'
           END-IF.
      *
        0300-READ-HOSP-PATIENT-FILE.
      *
      *****************************************************************
      *  DESCRIPTION:                                                 *
      *    THIS PARAGRAPH READS THE HOSPITAL PATIENT FILE, CHECKS THE *
      *    FILE STATUS AND DISPLAYS AN ERROR MESSAGE IF THE FILE      *
      *    STATUS IS NOT '00'. ONE IS ADDED TO THE # OF RECORDS READ. *
      *                                                               *
      *  CALLED BY:                                                   *
      *    -  MAIN PROCEDURE AREA                                     *
      *    -  0400-MAIN-PROCESS                                       *
      *                                                               *
      *  CALLS:                                                       *
      *    -  NONE                                                    *
      *****************************************************************
      *
           READ HOSPIN INTO WS-HOSP-REC
               AT END MOVE 'Y' TO HOSPIN-FILE-SW
           END-READ.

           IF IFCODE = '00'  OR '10'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR ENCOUNTERED READING HOSPITAL INPUT FILE'
           END-IF.
      *
           IF END-OF-HOSPIN-FILE
              NEXT SENTENCE
           ELSE
              ADD +1 TO RECORDS-READ
           END-IF.
      *
       0400-MAIN-PROCESS.
      *
      *****************************************************************
      *  DESCRIPTION:                                                 *
      *    THIS PARAGRAPH CHECKS THE READ RECORD FOR A VALID PATIENT  *
      *    TYPE AND VALID INSURANCE TYPE.  IF BOTH FIELDS ARE VALID,  *
      *    THE RECORD IS WRITTEN TO THE OUTFILE FILE AND PATIENT TOTAL*
      *    AMOUNTS AND REPORT TOTALS ARE CALCULATED. THE RECORD IS    *
      *    THEN WRITTEN TO THE UNFORMATTED AND FORMATTED REPORT FILES.*
      *    IF THE PATIENT TYPE OR INSURANCE TYPE IS NOT VALID, THE    *
      *    RECORD IS WRITTEN TO THE FORMATTED ERROR REPORT. THE NEXT  *
      *    RECORD IS THEN READ.                                       *
      *                                                               *
      *  CALLED BY:                                                   *
      *    -  MAIN PROCEDURE AREA                                     *
      *                                                               *
      *  CALLS:                                                       *
      *    -  0750-WRITE-OUTFILE                                      *
      *    -  0600-CALC-PATIENT-TOT-AMOUNTS                           *
      *    -  0700-ADD-TO-TOTALS                                      *
      *    -  0900-PROCESS-HOSPIN-REPORT                              *
      *    -  1525-PROCESS-NEWRPT-DETAIL                              *
      *    -  1300-PROCESS-ERROR-REPORT                               *
      *    -  0300-READ-HOSP-PATIENT-FILE                             *
      *****************************************************************
      *
           MOVE 'N' TO VALID-INS-TYPE-SW.
           INITIALIZE WS-NEWRPT-DETAIL.
      *
           IF VALID-PATIENT-TYPE
              PERFORM 0500-SEARCH-INS-TYPE-TABLE
              IF VALID-INS-TYPE
                 MOVE WS-HOSP-REC TO OUT-REC
                 PERFORM 0750-WRITE-OUTFILE
                 PERFORM 0600-CALC-PATIENT-TOT-AMOUNTS
                 PERFORM 0700-ADD-TO-TOTALS
                 PERFORM 0900-PROCESS-HOSPIN-REPORT
                 PERFORM 1525-PROCESS-NEWRPT-DETAIL
              ELSE
                 MOVE 'INSURANCE TYPE'    TO FIELD-IN-ERROR-E
                 MOVE 'INVALID INSURANCE TYPE. VALID VALUES ARE AFF, HMO
      -           ' MED, PPO, OR PRI'     TO ERROR-MESSAGE-E
                 MOVE INS-TYPE            TO FIELD-IN-ERR-VALUE-E
                 PERFORM 1300-PROCESS-ERROR-REPORT
              END-IF
           ELSE
              MOVE 'PATIENT TYPE'  TO FIELD-IN-ERROR-E
              MOVE 'INVALID PATIENT TYPE. VALID VALUES ARE I OR O'
                                   TO ERROR-MESSAGE-E
              MOVE PATIENT-TYPE    TO FIELD-IN-ERR-VALUE-E
              PERFORM 1300-PROCESS-ERROR-REPORT
              MOVE 'N' TO VALID-INS-TYPE-SW
           END-IF.
      *
           PERFORM 0300-READ-HOSP-PATIENT-FILE.  *>READ NEXT INPUT REC.
      *
      *****************************************************************
      *  DESCRIPTION:                                                 *
      *    THIS PARAGRAPH SEARCHES THE INSURANCE TYPE TABLE USING THE *
      *    INSURANCE TYPE ON THE INPUT RECORD.  ONCE A MATCH IS FOUND,*
      *    THE VALID INSURANCE TYPE SWITCH IS SET TO 'Y'. IF NO MATCH *
      *    IS FOUND, THE VALID INSURANCE TYPE SWITCH IS SET TO 'N'.   *
      *                                                               *
      *  CALLED BY:                                                   *
      *    -  0400-MAIN-PROCESS                                       *
      *                                                               *
      *  CALLS:                                                       *
      *    -  NONE                                                    *
      *****************************************************************
      *
       0500-SEARCH-INS-TYPE-TABLE.
           SET T-IDX TO 1.
           SEARCH INS-TYPE-ITEM
               AT END MOVE 'N' TO VALID-INS-TYPE-SW
            WHEN INS-TYPE-ITEM (T-IDX) = INS-TYPE
              MOVE 'Y' TO VALID-INS-TYPE-SW
           END-SEARCH.
      *
      *****************************************************************
      *  DESCRIPTION:                                                 *
      *    THIS PARAGRAPH CALCULATES THE PAT-TOTAL-AMT-NET FOR EACH   *
      *    RECORD AND ADDS THE RESULT TO OTHER TOTAL AMOUNTS.         *
      *                                                               *
      *  CALLED BY:                                                   *
      *    -  0400-MAIN-PROCESS                                       *
      *                                                               *
      *  CALLS:                                                       *
      *    -  NONE                                                    *
      *****************************************************************
      *
       0600-CALC-PATIENT-TOT-AMOUNTS.
           COMPUTE PAT-TOTAL-AMT-NET =
                   (PATIENT-TOT-AMT  +
                    AMT-PER-DAY * ((100 - INS-COVERAGE-PERC) / 100))
           END-COMPUTE.

           ADD PAT-TOTAL-AMT-NET TO TOTAL-AMT-NET.
           ADD PATIENT-TOT-AMT TO TOTAL-AMT-GROSS.

           ADD PAT-TOTAL-AMT-NET  TO PATIENT-TOT-AMT
                   GIVING PATIENT-AMT-PER-DAY-O.
      *
      *****************************************************************
      *  DESCRIPTION:                                                 *
      *    THIS PARAGRAPH ADDS TOTAL COUNTS. THE INSURANCE TYPE TABLE *
      *    IS SEARCHED AND 1 ADDED EACH TIME THE INSURANCE TYPE ON THE*
      *    INPUT RECORD IS ENCOUNTERED. TOTAL COUNTS FOR INPATIENT AND*
      *    OUTPATIENT PATIENTS ARE ALSO CALCULATED. WHEN THE INSURANCE*
      *    TYPE IS FOUND IN THE TABLE, THE INDEX IS SET TO +5 SO THAT *
      *    THE PROGRAM LOGIC FALLS OUT OF THE PERFORM LOOP.           *
      *                                                               *
      *  CALLED BY:                                                   *
      *    -  0400-MAIN-PROCESS                                       *
      *                                                               *
      *  CALLS:                                                       *
      *    -  NONE                                                    *
      *****************************************************************
      *
       0700-ADD-TO-TOTALS.
           SET T-IDX TO 1.
           SEARCH INS-TYPE-ITEM
               AT END CONTINUE
            WHEN INS-TYPE-ITEM (T-IDX) = INS-TYPE
              IF HMO(T-IDX)
                 ADD +1 TO NBR-HMO
              ELSE
                 IF MED(T-IDX)
                    ADD +1 TO NBR-STATE-FED
                 ELSE
                    IF AFF(T-IDX)
                       ADD +1 TO NBR-AFFORDABLE
                    ELSE
                       IF PPO(T-IDX)
                          ADD +1 TO NBR-PPO
                          SET T-IDX TO 5
                       ELSE
                          IF PRI(T-IDX)
                            ADD +1 TO NBR-PRIVATE
                           ELSE
                             ADD +1 TO NBR-NO-COVERAGE
                          END-IF
                       END-IF
                    END-IF
                 END-IF
              END-IF
           END-SEARCH.

           IF INPATIENT *>CHECK FOR INPATIENT OR OUTPATIENT PATIENT
              ADD +1 TO NBR-INPATIENTS
           ELSE
              ADD +1 TO NBR-OUTPATIENTS
           END-IF.
      *
      *****************************************************************
      *  DESCRIPTION:                                                 *
      *    THIS PARAGRAPH WRITES THE HOSPITAL PATIENT RECORD TO THE   *
      *    OUTFILE FILE, CHECKS THE FILE STATUS AND DISPLAYS AN ERROR *
      *    MESSAGE IF THE FILE STATUS IS NOT '00'.                    *
      *                                                               *
      *  CALLED BY:                                                   *
      *    -  0400-MAIN-PROCESS                                       *
      *                                                               *
      *  CALLS:                                                       *
      *    -  NONE                                                    *
      *****************************************************************
      *
       0750-WRITE-OUTFILE.
           WRITE OUT-REC.
           IF OFCODE = '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR ENCOUNTERED WRITING TO THE OUTFILE'
           END-IF.
      *
      *****************************************************************
      *  DESCRIPTION:                                                 *
      *    THIS PARAGRAPH CALLS ROUTINES TO MOVE FIELDS TO THE UNFOR- *
      *    MATTED HOSPITAL PATIENT REPORT AND WRITE THE RECORD.       *
      *                                                               *
      *  CALLED BY:                                                   *
      *    -  0400-MAIN-PROCESS                                       *
      *                                                               *
      *  CALLS:                                                       *
      *    -  1000-MOVE-FIELDS-TO-HOSPIN-RPT                          *
      *    -  1100-WRITE-HOSPIN-RPT-DETAIL                            *
      *****************************************************************
      *
       0900-PROCESS-HOSPIN-REPORT.
           PERFORM 1000-MOVE-FIELDS-TO-HOSPIN-RPT.
           PERFORM 1100-WRITE-HOSPIN-RPT-DETAIL.
      *
      *****************************************************************
      *  DESCRIPTION:                                                 *
      *    THIS PARAGRAPH MOVES FIELDS TO THE UNFORMATTED HOSPITAL    *
      *    PATIENT REPORT.                                            *
      *                                                               *
      *  CALLED BY:                                                   *
      *    -  0900-PROCESS-HOSPIN-RPT                                 *
      *                                                               *
      *  CALLS:                                                       *
      *    -  NONE                                                    *
      *****************************************************************
      *
       1000-MOVE-FIELDS-TO-HOSPIN-RPT.
           MOVE PATIENT-NBR        TO PATIENT-NBR-O.
           MOVE PATIENT-NAME       TO PATIENT-NAME-O.
           MOVE PATIENT-PHONE      TO PATIENT-PHONE-O.
           MOVE PATIENT-TYPE       TO PATIENT-TYPE-O.
           MOVE WS-DATE            TO CURR-DATE-O.
           MOVE BED-IDENTITY       TO BED-IDENTITY-O.
           MOVE INS-COVERAGE-PERC  TO INS-COVERAGE-PERC-O.
           MOVE INS-TYPE           TO INS-TYPE-O.
           ADD +1 TO HOSPITAL-STAY-LTH
                         GIVING HOSPITAL-STAY-LTH-O.
      *
      *****************************************************************
      *  DESCRIPTION:                                                 *
      *    THIS PARAGRAPH WRITES A RECORD TO THE UNFORMATTED HOSPITAL *
      *    PATIENT REPORT AND CHECKS THE FILE STATUS.  IF THE STATUS  *
      *    IS NOT '00', AN ERROR MESSAGE IS DISPLAYED.  ONE IS ADDED  *
      *    TO THE NUMBER OF UNFORMATTED RECORDS WRITTEN.              *
      *                                                               *
      *                                                               *
      *  CALLED BY:                                                   *
      *    -  0900-PROCESS-HOSPIN-RPT                                 *
      *                                                               *
      *  CALLS:                                                       *
      *    -  NONE                                                    *
      *****************************************************************
      *
       1100-WRITE-HOSPIN-RPT-DETAIL.
      *
           WRITE HOSP-RPT-REC FROM WS-OUTPUT-REC.
           IF RFCODE = '00'
             NEXT SENTENCE
           ELSE
             DISPLAY 'ERROR ENCOUNTERED WRITING TO HOSPITAL REPORT FILE'
           END-IF.
           ADD +1 TO RECORDS-WRITTEN.
      *
      *****************************************************************
      *  DESCRIPTION:                                                 *
      *    THIS PARAGRAPH WRITES HEADERS TO THE ERROR REPORT AND      *
      *    CHECKS THE FILE STATUS.  IF THE FILE STATUS IS NOT '00',   *
      *    AN ERROR MESSAGE IS DISPLAYED.                             *
      *                                                               *
      *  CALLED BY:                                                   *
      *    -  MAIN PROCEDURE AREA                                     *
      *                                                               *
      *  CALLS:                                                       *
      *    -  NONE                                                    *
      *****************************************************************
      *
       1200-WRITE-ERROR-RPT-HEADERS.
      *    WRITE ERR-REC FROM ERR-HEADER-1.
           WRITE ERR-REC FROM WS-ERR-REC-HEADER-1.
           WRITE ERR-REC FROM WS-ERR-REC-HEADER-2.
           WRITE ERR-REC FROM WS-ERR-REC-BLANK-LINE.
           WRITE ERR-REC FROM WS-ERR-REC-HEADER-3.
           WRITE ERR-REC FROM WS-ERR-REC-HEADER-4.
           IF EFCODE = '00'
             NEXT SENTENCE
           ELSE
             DISPLAY 'ERROR ENCOUNTERED WRITING TO ERROR REPORT'
           END-IF.
      *
      *****************************************************************
      *  DESCRIPTION:                                                 *
      *    THIS PARAGRAPH CALLS ROUTINES THAT MOVE FIELDS TO THE ERROR*
      *    REPORT AND WRITES A RECORD TO THE ERROR REPORT.            *
      *                                                               *
      *  CALLED BY:                                                   *
      *    -  0400-MAIN-PROCESS                                       *
      *                                                               *
      *  CALLS:                                                       *
      *    -  1400-MOVE-ERROR-RPT-FIELDS                              *
      *    -  1500-WRITE-ERR-RPT-DETAIL                               *
      *****************************************************************
      *
       1300-PROCESS-ERROR-REPORT.
           PERFORM 1400-MOVE-ERROR-RPT-FIELDS.
           PERFORM 1500-WRITE-ERR-RPT-DETAIL.
      *
      *****************************************************************
      *  DESCRIPTION:                                                 *
      *    THIS PARAGRAPH MOVE FIELDS TO THE ERROR REPORT.            *
      *                                                               *
      *  CALLED BY:                                                   *
      *    -  1300-PROCESS-ERROR-REPORT                               *
      *                                                               *
      *  CALLS:                                                       *
      *    -  NONE                                                    *
      *****************************************************************
      *
       1400-MOVE-ERROR-RPT-FIELDS.
      *
           MOVE PATIENT-NBR TO PATIENT-NBR-E.
      *
      *****************************************************************
      *  THE STRING FUNCTION IS USED TO CONCATENATE THE FIRST AND LAST*
      *  NAMES INTO THE ERROR REPORT OUTPUT PATIENT-NAME FIELD.       *
      *****************************************************************
      *
           STRING FIRST-NAME DELIMITED BY SPACE
                  ' '                DELIMITED BY SIZE
                  LAST-NAME  DELIMITED BY SPACE
                    INTO  PATIENT-NAME-E.
      *
      *****************************************************************
      *  DESCRIPTION:                                                 *
      *    THIS PARAGRAPH WRITES A RECORD TO THE ERROR REPORT AND     *
      *    CHECKS THE FILE STATUS.  IF THE FILE STATUS IS NOT '00', AN*
      *    ERROR MESSAGE IS DISPLAYED.  ONE IS ADDED TO ERROR RECORDS *
      *    WRITTEN.                                                   *
      *                                                               *
      *  CALLED BY:                                                   *
      *    -  1300-PROCESS-ERROR-REPORT                               *
      *                                                               *
      *  CALLS:                                                       *
      *    -  NONE                                                    *
      *****************************************************************
      *
       1500-WRITE-ERR-RPT-DETAIL.
      *
           WRITE ERR-REC FROM WS-ERR-REC-DETAIL.
           IF EFCODE = '00'
             NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR ENCOUNTERED WRITING TO ERROR REPORT FILE'
           END-IF.

           ADD +1 TO ERROR-RECS.
           INITIALIZE WS-ERR-REC-DETAIL.
      *
      *****************************************************************
      *  DESCRIPTION:                                                 *
      *    THIS PARAGRAPH WRITES HEADER RECORDS TO THE FORMATTED      *
      *    REPORT FILE AND CHECKS THE FILE STATUS. IF THE FILE STATUS *
      *    IS NOT '00', AN ERROR MESSAGE IS DISPLAYED.                *
      *                                                               *
      *  CALLED BY:                                                   *
      *    -  MAIN PROCEDURE AREA                                     *
      *                                                               *
      *  CALLS:                                                       *
      *    -  NONE                                                    *
      *****************************************************************
      *
       1510-WRITE-NEWRPT-HEADERS.
           WRITE NEW-RPT-REC FROM WS-NEWRPT-BLANK-LINE.
           WRITE NEW-RPT-REC FROM WS-NEWRPT-HEADER-1.
           WRITE NEW-RPT-REC FROM WS-NEWRPT-HEADER-2.
           WRITE NEW-RPT-REC FROM WS-NEWRPT-BLANK-LINE.

           WRITE NEW-RPT-REC FROM WS-NEWRPT-HEADER-3.
           WRITE NEW-RPT-REC FROM WS-NEWRPT-HEADER-4.

           IF NFCODE = '00'
             NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR ENCOUNTERED WRITING TO REPORT FILE'
           END-IF.
      *
      *****************************************************************
      *  DESCRIPTION:                                                 *
      *    THIS PARAGRAPH CALLS ROUTINES TO MOVE FIELDS TO THE FORMAT-*
      *    TED DETAIL REPORT AND WRITE THE RECORD.                    *
      *                                                               *
      *  CALLED BY:                                                   *
      *    -  0400-MAIN-PROCESS.                                      *
      *                                                               *
      *  CALLS:                                                       *
      *    -  1550-MOVE-NEWRPT-FIELDS                                 *
      *    -  1575-WRITE-NEWRPT-DETAIL                                *
      *****************************************************************
      *

       1525-PROCESS-NEWRPT-DETAIL.
           PERFORM 1550-MOVE-NEWRPT-FIELDS.
           PERFORM 1575-WRITE-NEWRPT-DETAIL.
      *
      *****************************************************************
      *  DESCRIPTION:                                                 *
      *    THIS PARAGRAPH MOVES FIELDS TO THE FORMATTED REPORT.       *
      *                                                               *
      *  CALLED BY:                                                   *
      *    -  1525-PROCESS-NEWRPT-DETAIL                              *
      *                                                               *
      *  CALLS:                                                       *
      *    -  NONE                                                    *
      *****************************************************************
      *
       1550-MOVE-NEWRPT-FIELDS.
           MOVE PATIENT-NBR TO PATIENT-NBR-R.
      *
      *****************************************************************
      *  USE THE STRING FUNCTION TO CONCATENATE THE FIRST AND LAST    *
      *  NAME INTO THE OUTPUT REPORT FIELD PATIENT-NAME-R.            *
      *****************************************************************
      *
           STRING FIRST-NAME DELIMITED BY SPACE
                  ' '                DELIMITED BY SIZE
                  LAST-NAME  DELIMITED BY SPACE
                    INTO  PATIENT-NAME-R.
      *
      *****************************************************************
      *  MOVE THREE PARTS OF THE PHONE NUMBER INTO A FORMATTED GROUP  *
      *  FIELD IN THE OUTPUT REPORT.                                  *
      *****************************************************************
      *
           MOVE NPA                 TO NPA-R.
           MOVE NXX                 TO NXX-R.
           MOVE EXCHANGE            TO EXCHANGE-R.

           MOVE PATIENT-TYPE        TO PATIENT-TYPE-R.
           MOVE BED-IDENTITY        TO BED-IDENTITY-R.
      *
      *****************************************************************
      *  MOVE SPECIFIC DIGITS OF THE CURRENT DATE INTO A 6-DIGIT      *
      *  WORKING STORAGE FIELD AND THEN MOVE 6-DIGIT DATE TO THE      *
      *  OUTPUT REPORT                                                *
      *****************************************************************
      *
           STRING DATE-ADMIT(9:2)
                  DATE-ADMIT(4:2)
                  DATE-ADMIT(1:2)
                    DELIMITED BY SIZE
                    INTO WS-6-DIGIT-DATE.

           MOVE WS-6-DIGIT-DATE     TO DATE-ADMIT-R.
      *
      *****************************************************************
      *  COMPUTE THE PATIENT DAILY AMOUNT COST AND MOVE TO FIELD IN   *
      *  THE OUTPUT RECORD.                                           *
      *                                                               *
      *****************************************************************
      *
           COMPUTE WS-DAILY-AMOUNT ROUNDED =
               AMT-PER-DAY * ((100 - INS-COVERAGE-PERC) / 100)
      *            AMT-PER-DAY *  HOSPITAL-STAY-LTH.

           MOVE WS-DAILY-AMOUNT     TO DAILY-AMOUNT-R.

           ADD WS-DAILY-AMOUNT      TO GROSS-DAILY-AMT.

           MOVE DIAGNOSTIC-CODE     TO DIAGNOSTIC-CODE-R.

           MOVE INS-TYPE            TO INS-TYPE-R.

           MOVE HOSPITAL-STAY-LTH   TO HOSPITAL-STAY-LTH-R.
      *
      *****************************************************************
      *  IF THE PATIENT IS IN-NETWORK, MOVE 'IN' TO THE REPORT OUTPUT *
      *  IN-OUT-NETWORK-R FIELD, OTHERWISE MOVE 'OUT' TO THIS FIELD.  *
      *                                                               *
      *****************************************************************
      *
           IF IN-NETWORK
              MOVE 'IN' TO IN-OUT-NETWORK-R
           ELSE
              IF OUT-OF-NETWORK
                 MOVE 'OUT' TO IN-OUT-NETWORK-R
              ELSE
                NEXT SENTENCE
              END-IF
           END-IF.

           MOVE COPAY TO COPAY-R.

           MOVE DEDUCTIBLE TO DEDUCTIBLE-R.
      *
      *****************************************************************
      *  DESCRIPTION:                                                 *
      *    THIS PARAGRAPH WRITES THE FORMATTED REPORT DETAIL RECORD   *
      *    AND CHECKS THE FILE STATUS. IF THE FILE STATUS IS NOT '00',*
      *    AN ERROR MESSAGE IS DISPLAYED.                             *
      *                                                               *
      *  CALLED BY:                                                   *
      *    -  1525-PROCESS-NEWRPT-DETAIL                              *
      *                                                               *
      *  CALLS:                                                       *
      *    -  NONE                                                    *
      *****************************************************************
      *
       1575-WRITE-NEWRPT-DETAIL.
      *
           WRITE NEW-RPT-REC FROM WS-NEWRPT-DETAIL.
           IF NFCODE = '00'
             NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR ENCOUNTERED WRITING TO ERROR REPORT FILE'
           END-IF.
      *
      *****************************************************************
      *  DESCRIPTION:                                                 *
      *    THIS PARAGRAPH MOVES TOTAL COUNTS TO TOTAL LINES GROUP     *
      *    ITEMS IN WORKING STORAGE TO BE PRINTED ON THE UNFORMATTED  *
      *    REPORT FILE.                                               *
      *                                                               *
      *  CALLED BY:                                                   *
      *    -  MAIN PROCEDURE AREA                                     *
      *                                                               *
      *  CALLS:                                                       *
      *    -  NONE                                                    *
      *****************************************************************
      *
       1600-MOVE-TOTAL-FIELDS.
           MOVE RECORDS-READ            TO TOTAL-RECS-READ-O.
           MOVE RECORDS-WRITTEN         TO TOTAL-GOOD-RECS-WRITTEN-O.
           MOVE ERROR-RECS              TO TOTAL-ERROR-RECS-O.
           MOVE NBR-INPATIENTS          TO TOTAL-INPATIENT-RECS-O .
           MOVE NBR-OUTPATIENTS         TO TOTAL-OUTPATIENT-RECS-O.
           MOVE NBR-HMO                 TO TOTAL-HMO-ACCOUNTS-O.
           MOVE NBR-PPO                 TO TOTAL-PPO-ACCOUNTS-O.
           MOVE NBR-PRIVATE             TO TOTAL-PRI-ACCOUNTS-O.
           MOVE NBR-AFFORDABLE          TO TOTAL-AFF-ACCOUNTS-O.
           MOVE NBR-STATE-FED           TO TOTAL-STATE-FED-O.
           MOVE NBR-NO-COVERAGE         TO TOTAL-NO-COVERAGE-O.
           MOVE TOTAL-AMT-GROSS         TO TOTAL-GROSS-O.
           MOVE TOTAL-AMT-NET           TO TOTAL-NET-O.
      *
      *****************************************************************
      *  DESCRIPTION:                                                 *
      *    THIS PARAGRAPH WRITES TOTAL LINES TO THE UNFORMATTED REPORT*
      *    FILE AND CHECKS THE FILE STATUS. IF THE FILE STATUS IS NOT *
      *    '00', AN ERROR MESSAGE IS DISPLAYED.                       *
      *                                                               *
      *  CALLED BY:                                                   *
      *    -  MAIN PROCEDURE AREA                                     *
      *                                                               *
      *  CALLS:                                                       *
      *    -  NONE                                                    *
      *****************************************************************
      *
       1700-WRIE-HOSPOUT-TOTALS.

           WRITE HOSP-RPT-REC FROM WS-TOTALS-BLANK-LINE.

           WRITE HOSP-RPT-REC FROM WS-TOTALS-REC-1
              AFTER ADVANCING 1 LINES.

           WRITE HOSP-RPT-REC FROM WS-TOTALS-REC-2
              AFTER ADVANCING 1 LINES.

           WRITE HOSP-RPT-REC FROM WS-TOTALS-REC-3
              AFTER ADVANCING 1 LINES.

           WRITE HOSP-RPT-REC FROM WS-TOTALS-REC-4
              AFTER ADVANCING 1 LINES.

           WRITE HOSP-RPT-REC FROM WS-TOTALS-REC-5
              AFTER ADVANCING 1 LINES.

           WRITE HOSP-RPT-REC FROM WS-TOTALS-REC-6
              AFTER ADVANCING 1 LINES.

           WRITE HOSP-RPT-REC FROM WS-TOTALS-REC-7
              AFTER ADVANCING 1 LINES.

           WRITE HOSP-RPT-REC FROM WS-TOTALS-REC-8
              AFTER ADVANCING 1 LINES.

           WRITE HOSP-RPT-REC FROM WS-TOTALS-REC-9
              AFTER ADVANCING 1 LINES.

           WRITE HOSP-RPT-REC FROM WS-TOTALS-REC-10
              AFTER ADVANCING 1 LINES.

           WRITE HOSP-RPT-REC FROM WS-TOTALS-REC-11
              AFTER ADVANCING 1 LINES.

           WRITE HOSP-RPT-REC FROM WS-TOTALS-BLANK-LINE.

           WRITE HOSP-RPT-REC FROM WS-TOTALS-REC-12
              AFTER ADVANCING 1 LINES.

           WRITE HOSP-RPT-REC FROM WS-TOTALS-REC-13
              AFTER ADVANCING 1 LINES.

           IF RFCODE = '00'
             NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR ENCOUNTERED WRITING TO REPORT FILE'
           END-IF.
      *
      *****************************************************************
      *  DESCRIPTION:                                                 *
      *    THIS PARAGRAPH WRITES TOTAL LINES TO THE FORMMATTED REPORT *
      *    FILE AND CHECKS THE FILE STATUS. IF THE FILE STATUS IS NOT *
      *    '00', AN ERROR MESSAGE IS DISPLAYED.                       *
      *                                                               *
      *  CALLED BY:                                                   *
      *    -  MAIN PROCEDURE AREA                                     *
      *                                                               *
      *  CALLS:                                                       *
      *    -  NONE                                                    *
      *****************************************************************
      *
       1750-WRIE-NEWRPT-TOTALS.
           WRITE NEW-RPT-REC FROM WS-TOTALS-BLANK-LINE.

           WRITE NEW-RPT-REC FROM WS-TOTALS-REC-1
              AFTER ADVANCING 1 LINES.

           WRITE NEW-RPT-REC FROM WS-TOTALS-REC-2
              AFTER ADVANCING 1 LINES.

           WRITE NEW-RPT-REC FROM WS-TOTALS-REC-3
              AFTER ADVANCING 1 LINES.

           WRITE NEW-RPT-REC FROM WS-TOTALS-REC-4
              AFTER ADVANCING 1 LINES.

           WRITE NEW-RPT-REC FROM WS-TOTALS-REC-5
              AFTER ADVANCING 1 LINES.

           WRITE NEW-RPT-REC FROM WS-TOTALS-REC-6
              AFTER ADVANCING 1 LINES.

           WRITE NEW-RPT-REC FROM WS-TOTALS-REC-7
              AFTER ADVANCING 1 LINES.

           WRITE NEW-RPT-REC FROM WS-TOTALS-REC-8
              AFTER ADVANCING 1 LINES.

           WRITE NEW-RPT-REC FROM WS-TOTALS-REC-9
              AFTER ADVANCING 1 LINES.

           WRITE NEW-RPT-REC FROM WS-TOTALS-REC-10
              AFTER ADVANCING 1 LINES.

           WRITE NEW-RPT-REC FROM WS-TOTALS-REC-11
              AFTER ADVANCING 1 LINES.

           WRITE NEW-RPT-REC FROM WS-TOTALS-BLANK-LINE.

           MOVE GROSS-DAILY-AMT TO GROSS-DAILY-AMT-R.

           WRITE NEW-RPT-REC FROM WS-TOTALS-REC-14.

           IF NFCODE = '00'
             NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR ENCOUNTERED WRITING TO REPORT FILE'
           END-IF.
      *
      *****************************************************************
      *  DESCRIPTION:                                                 *
      *    THIS PARAGRAPH CLOSES ALL FILES OPENED FOR INPUT AND OUT-  *
      *    AND CHECKS THE FILE STATUS FOR THE FILES. IF THE FILE      *
      *    STATUS IS NOT '00', AN ERROR MESSAGE IS DISPLAYED.         *
      *                                                               *
      *  CALLED BY:                                                   *
      *    -  MAIN PROCEDURE AREA                                     *
      *                                                               *
      *  CALLS:                                                       *
      *    -  NONE                                                    *
      *****************************************************************
      *
       1800-CLOSE-FILES.

           CLOSE HOSPIN.
           IF IFCODE = '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR ENCOUNTERED CLOSING HOSPITAL INPUT FILE'
           END-IF.
      *
           CLOSE INSTFILE.
           IF TFCODE = '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR ENCOUNTERED CLOSING INSURANCE TYPE FILE'
           END-IF.
      *
           CLOSE OUTFILE.
           IF OFCODE = '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR ENCOUNTERED CLOSING OUTFILE'
           END-IF.
      *
           CLOSE ERRFILE.
           IF EFCODE = '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR ENCOUNTERED CLOSING ERROR FILE'
           END-IF.

           CLOSE RPTFILE.
           IF RFCODE = '00'
              NEXT SENTENCE
           ELSE
             DISPLAY 'ERROR ENCOUNTERED CLOSING UNFORMATTED REPORT FILE'
           END-IF.

           CLOSE NEWRPT.
           IF NFCODE = '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR ENCOUNTERED CLOSING FORMATTED REPORT FILE'
           END-IF.
