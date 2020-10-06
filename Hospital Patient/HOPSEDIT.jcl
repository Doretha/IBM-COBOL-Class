//HOSPEDT JOB ,
// MSGCLASS=H,MSGLEVEL=(1,1),TIME=(,4),REGION=144M,COND=(16,LT)
//*
//********************************************************************
//*  THIS JCL COMPILES, LINKS AND RUNS THE HOSPEDIT.jcl PROGRAM, WHICH
//*  READS A HOSPITAL PAITENT FILE, READS AN INSURANCE TYPE FILE AND
//*  LOADS IT INTO A TABLE, CALCULATES THE PATIENT DAILY AMOUNT AND
//*  PRODUCES OUTPUT FILES AND REPORTS TO SYSOUT=*
//********************************************************************
// SET COBPGM='HOSPEDIT'
//**** Compile JCL ******
//STP0000 EXEC PROC=ELAXFCOC,
// CICS=,
// DB2=,
// COMP=
//COBOL.SYSPRINT DD SYSOUT=*
//SYSLIN DD DISP=SHR,
//        DSN=&SYSUID..COBOBJS.OBJ4(&COBPGM.)
//COBOL.SYSLIB DD DISP=SHR,
//        DSN=&SYSUID..COBOL.COPYLIB
//COBOL.SYSXMLSD DD DUMMY
//COBOL.SYSIN DD DISP=SHR,DSN=&SYSUID..REUSABLE.COBOL4(&COBPGM.)
//****Link/Edit Step ******
//LKED EXEC PROC=ELAXFLNK
//LINK.SYSLIB DD DSN=CEE.SCEELKED,
//        DISP=SHR
//        DD DSN=&SYSUID..LEARN.LOAD,
//        DISP=SHR
//LINK.OBJ0000 DD DISP=SHR,
//        DSN=&SYSUID..COBOBJS.OBJ4(&COBPGM.)
//LINK.SYSLIN DD *
     INCLUDE OBJ0000
/*
//LINK.SYSLMOD   DD  DISP=SHR,
//        DSN=&SYSUID..LEARN.LOAD(&COBPGM.)
//*
//** Go (Run) Step. Add //DD cards when needed ******
//GO    EXEC   PROC=ELAXFGO,GO=&COBPGM.,
//        LOADDSN=&SYSUID..LEARN.LOAD
//CEEOPTS DD *
  TEST(,,,DBMDT%RTPOT44:*)
/*
//******* ADDITIONAL RUNTIME JCL HERE ******
//********************************************************************
//*  HOSPITAL PATIENT INPUT FILE
//********************************************************************
//HOSPIN   DD DSN=RTPOT44.HOSPIN.DATA,DISP=SHR
//********************************************************************
//*  INSURANCE TYPE INPUT FILE
//********************************************************************
//INSTFILE DD DSN=RTPOT44.REUSABLE.COBOL4(INSTYPE),DISP=SHR
//********************************************************************
//*  OUTPUT OF HOSPITAL PATIENT RECORDS WITH NO ERRORS
//********************************************************************
//HOSPOUT  DD SYSOUT=*
//********************************************************************
//*  OUTPUT OF RECORDS WITH ERRORS
//********************************************************************
//ERRFILE  DD SYSOUT=*
//********************************************************************
//*  REPORT OF RECORDS WITHOUT ERRORS - UNFORMATTED
//********************************************************************
//RPTFILE  DD SYSOUT=*
//********************************************************************
//*  REPORT OF RECORDS WITHOUT ERRORS - FORMATTED
//********************************************************************
//NEWRPT   DD SYSOUT=*
