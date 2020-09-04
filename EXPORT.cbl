       IDENTIFICATION DIVISION.
       PROGRAM-ID.  EXPORT.
       AUTHOR. DORETHA RILEY.
       INSTALLATION. COBOL DEV CENTER.
       DATE-WRITTEN. 01/01/08.
       DATE-COMPILED. 07/30/20.
       SECURITY. NON-CONFIDENTIAL.
.
      *
      *****************************************************************
      *  DESCRIPTION                                                  *
      *    THIS PROGRAM <FILL IN HERE>                                *
      *                                                               *
      *    INPUT FILE:                                                *
      *      <USERID>.<FILE IN HERE>                                  *
      *      INTERNAL FILE NAME:  INPUT-FILE                          *
      *      JCL DD NAME:         EMPROJ                              *
      *                                                               *
      *    OUTPUT FILES:  <FILL IN HERE>
      *                                                               *
      *    DD SYSOUT=* - DISPLAYS THE FOLLOWIMG                       *
      *    1. THE EMPLOYEE PROJECT ENTRIES IN THE TABLE (ARRAY)       *
      *    2. THE NAMES OF EMPLOYEES WORKING ON PROJECT A111          *
      *    3. THE NAMES OF EMPLOYEES IN NORTH CAROLINA WHO ARE        *
      *       PROGRAMMER/ANALYSTS AND WHO ARE ALLOWED TO BILL OVERTIME*
      *    4. TOTAL COST FOR ALL PROJECTS                             *
      *                                                               *
      *    JCL JOB: <USERID>.LEARN.REUSABLE.JCL(TABLES01)             *
      *****************************************************************
      *  CHANGE LOG:                                                  *
      *      UPDATED BY:  DORETHA RILEY                               *
      *            DATE:  07/30/2020                                  *
      *     DESCRIPTION:  ADDED A NEW OUTPUT FILE                     *
      *                                                               *
      *      UPDATED BY:  JOHN SAYLES                                 *
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
           SELECT INFILE
           ASSIGN TO INFILE
             FILE STATUS IS IFCODE.
      *
           SELECT OUTFILE
           ASSIGN TO OUTFILE
             FILE STATUS IS OFCODE.

      *
       DATA DIVISION.
       FILE SECTION.
       FD  INFILE
           RECORDING MODE IS F
           RECORD CONTAINS 100 CHARACTERS
           DATA RECORD IS INFILE-REC.
      *
       01  INFILE-REC  PIC X(100).
      *
      *
       FD  OUTFILE
           RECORDING MODE IS F
           RECORD CONTAINS 133 CHARACTERS
           DATA RECORD IS OUTFILE-REC.
      *
       01  OUTFILE-REC  PIC X(133).

      *
       WORKING-STORAGE SECTION.
       01 WS-FILE-SWITCHES.
          05 INFILE-FILE-SW   PIC X(01) VALUE SPACE.
             88 END-OF-FILE   VALUE 'Y'.
          05 IFCODE           PIC X(02).
          05 OFCODE           PIC X(02).

       01  WS-INFILE-REC.
           05  PATIENT-NBR        PIC 9(05).
           05  PATIENT-NAME.
               10 LAST-NAME       PIC X(10).
               10 FIRST-NAME      PIC X(10).
           05  PATIENT-PHONE.
               10 NPA             PIC X(03).
               10 NXX             PIC X(03).
               10 EXCHANGE        PIC X(04).
           05  PATIENT-TYPE       PIC X(01).
               88 INPATIENT           VALUE "I".
               88 OUTPATIENT          VALUE "0".
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

       01  WS-OUTFILE-REC.
           05  PATIENT-NBR-O        PIC 9(05).
           05  FILLER            PIC X(01) VALUE '|'.
           05  PATIENT-NAME-O.
               10 LAST-NAME-O       PIC X(10).
               10 FILLER            PIC X(01) VALUE '|'.
               10 FIRST-NAME-O      PIC X(10).
               10 FILLER            PIC X(01) VALUE '|'.
           05  PATIENT-PHONE-O.
               10 FILLER            PIC X(01) VALUE '('.
               10 NPA-O             PIC X(03).
               10 FILLER            PIC X(02) VALUE ') '.
               10 NXX-O             PIC X(03).
               10 FILLER            PIC X(01) VALUE '-'.
               10 EXCHANGE-O        PIC X(04).
           05  FILLER               PIC X(01) VALUE '|'.
           05  PATIENT-TYPE-O       PIC X(01).
           05  FILLER               PIC X(01) VALUE '|'.
           05  BED-IDENTITY-O       PIC 9(04).
           05  FILLER               PIC X(01) VALUE '|'.
           05  DATE-ADMIT-O         PIC X(10).
           05  FILLER               PIC X(01) VALUE '|'.
           05  AMT-PER-DAY-O        PIC $$$,$$$.99.
           05  FILLER               PIC X(01) VALUE '|'.
           05  DIAGNOSTIC-CODE-O    PIC 999.
           05  FILLER               PIC X(01) VALUE '|'.
           05  INS-TYPE-O           PIC X(03).
           05  FILLER               PIC X(01) VALUE '|'.
           05  HOSPITAL-STAY-LTH-O  PIC 999.
           05  FILLER               PIC X(01) VALUE '|'.
           05  PATIENT-TOT-AMT-O    PIC $$,$$$,$$$.99.
           05  FILLER               PIC X(01) VALUE '|'.
           05  PCP-ID-O             PIC X(06).
           05  FILLER               PIC X(01) VALUE '|'.
           05  IN-OUT-NETWORK-O     PIC X(01).
           05  FILLER               PIC X(01) VALUE '|'.
           05  COPAY-O              PIC 9(03).
           05  FILLER               PIC X(01) VALUE '|'.
           05  DEDUCTIBLE-O         PIC 9(04).


      *
      *****************************************************************
      *  DESCRIPTION:                                                 *
      *    THIS PROGRAM READS AN INPUT FILE AND WRITES IT TO AN OUTPUT*
      *    FILE TO BE EXPORTED TO THE LOCAL DRIVE AND USED FOR INPUT  *
      *    TO AN EXCEL FILE.                                          *
      *****************************************************************

       PROCEDURE DIVISION.
           PERFORM 0000-HOUSEKEEPING.
           PERFORM 0100-OPEN-FILES.
           PERFORM 0200-READ-INPUT-FILE.
           PERFORM 0300-MAIN-PROCESS
               UNTIL END-OF-FILE.
           PERFORM 0600-CLOSE-FILES.
           GOBACK.
      *
       0000-HOUSEKEEPING.
      *
      *****************************************************************
      *  DESCRIPTION:                                                 *
      *    THIS PARAGRAPH INITIALIZES WORKING STORAGE VARIABLES       *
      *                                                               *
      *  CALLED BY:                                                   *
      *    MAIN PROCEDURE AREA                                        *
      *                                                               *
      *  CALLS:                                                       *
      *    -  NONE                                                    *
      *****************************************************************
      *
      *    DISPLAY    "HOUSEKEEPING".
      *    ACCEPT      WS-DATE FROM DATE.
           INITIALIZE  WS-FILE-SWITCHES.
      *
       0100-OPEN-FILES.
      *
      *****************************************************************
      *  DESCRIPTION:                                                 *
      *    THIS PARAGRAPH OPENS THE FOLLOWING FILES:                  *
      *    1. INFILE - INPUT FILE                                     *
      *    2. OUTFILE - FORMATTED OUTPUT FILE                         *
      *                                                               *
      *  CALLED BY:                                                   *
      *    MAIN PROCEDURE AREA                                        *
      *                                                               *
      *  CALLS:                                                       *
      *    -  NONE                                                    *
      *****************************************************************
      *
           OPEN INPUT INFILE.
           IF IFCODE = '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR ENCOUNTERED OPENING INPUT FILE'
           END-IF.

           OPEN OUTPUT OUTFILE.
           IF OFCODE = '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR ENCOUNTERED OPENING OUTFILE'
           END-IF.
      *
       0200-READ-INPUT-FILE.
      *
      *****************************************************************
      *  DESCRIPTION:                                                 *
      *    THIS PARAGRAPH READS THE INSURANCE TYPE FILE, CHECKS THE   *
      *    FILE STATUS AND DISPLAYS AN ERROR MESSAGE IF THE FILE      *
      *    STATUS IS NOT '00'.                                        *
      *****************************************************************
      *
           READ INFILE INTO WS-INFILE-REC
               AT END
                 MOVE 'Y' TO INFILE-FILE-SW
           END-READ.
           IF IFCODE = '00' OR '10'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR ENCOUNTERED READIG INPUT FILE'
           END-IF.

      *
       0300-MAIN-PROCESS.
      *
      *****************************************************************
      *  DESCRIPTION:                                                 *
      *    THIS PARAGRAPH CALLS ROUTINES TO:                          *
      *
      *                                                               *
      *  CALLED BY:                                                   *
      *    MAIN PROCEDURE AREA                                        *
      *                                                               *
      *  CALLS:                                                      *
      *    -  200-FIND-PROJECT                                        *
      *    -  300-FIND-NC-OT-SKILL                                    *
      *    -  400-TOTAL-PROJ-EXPENSE                                  *
      *    -  500-TOTAL-ALL-PROJECTS-EXPENSE                          *
      *****************************************************************
      *
           PERFORM 0400-MOVE-FIELDS-TO-OUTFILE.
           PERFORM 0500-WRITE-OUTFILE.
           PERFORM 0200-READ-INPUT-FILE.

       0400-MOVE-FIELDS-TO-OUTFILE.
           MOVE PATIENT-NBR       TO PATIENT-NBR-O.
           MOVE LAST-NAME         TO LAST-NAME-O.
           MOVE FIRST-NAME        TO FIRST-NAME-O.
           MOVE NPA               TO NPA-O.
           MOVE NXX               TO NXX-O.
           MOVE EXCHANGE          TO EXCHANGE-O.
           MOVE PATIENT-TYPE      TO PATIENT-TYPE-O.
           MOVE BED-IDENTITY      TO BED-IDENTITY-O.
           MOVE DATE-ADMIT        TO DATE-ADMIT-O.
           MOVE AMT-PER-DAY       TO AMT-PER-DAY-O.
           MOVE DIAGNOSTIC-CODE   TO DIAGNOSTIC-CODE-O.
           MOVE INS-TYPE          TO INS-TYPE-O.
           MOVE HOSPITAL-STAY-LTH TO HOSPITAL-STAY-LTH-O.
           MOVE PATIENT-TOT-AMT   TO PATIENT-TOT-AMT-O.
           MOVE PCP-ID            TO PCP-ID-O.
           MOVE IN-OUT-NETWORK    TO IN-OUT-NETWORK-O.
           MOVE COPAY             TO COPAY-O.
           MOVE DEDUCTIBLE        TO DEDUCTIBLE-O.

       0500-WRITE-OUTFILE.
           WRITE OUTFILE-REC FROM WS-OUTFILE-REC.
           IF OFCODE = '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR ENCOUNTERED WRITING TO THE OUTFILE'
           END-IF.

       0600-CLOSE-FILES.
      *
      *****************************************************************
      *  DESCRIPTION:                                                 *
      *    THIS PARAGRAPH CLOSES THE LISTED FILES, CHECKS THE FILE    *
      *    STATUS, AND DISPLAYS AN ERROR MESSAGE IF THE FILE STATUS   *
      *    IS NOT '00'.                                               *
      *    1. THE HOSPIN PATIENT FILE                                 *
      *    2. THE INSURANCE TYPE FILE                                 *
      *    3. THE OUTFILE FILE OF GOOD RECORDS                        *
      *    4. THE ERRORFILE OF RECORDS                                *
      *    5. THE RPTFILE GOOD RECORDS FORMATTED IN SYSOUT            *
      *                                                               *
      *  CALLED BY:                                                   *
      *    MAIN PROCEDURE AREA                                        *
      *                                                               *
      *  CALLS:                                                       *
      *    -  NONE                                                    *
      *****************************************************************
      *
           CLOSE INFILE
           IF IFCODE = '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR ENCOUNTERED CLOSING INPUT FILE'
           END-IF.
      *
           CLOSE OUTFILE.
           IF OFCODE = '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR ENCOUNTERED CLOSING OUTFILE'
           END-IF.
