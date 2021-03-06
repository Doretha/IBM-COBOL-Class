//TABLES01 JOB ,
// MSGCLASS=H,MSGLEVEL=(1,1),TIME=(,4),REGION=144M,COND=(16,LT)
//*
//********************************************************************
//*  THIS JCL COMPILES, LINKS AND RUNS THE TABLES01.cbl PROGRAM
//*  THAT LOADS AN INPUT FILE INTO AN ARRAY AND PERFORMS CALCULATIONS
//*  ON SPECIFIC RECORDS.
//********************************************************************
// SET COBPGM='TABLES01'
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
//*   EMPLOYEE PROJECT INPUT FILE
//********************************************************************
//EMPROJ   DD DSN=RTPOT44.LEARN.EMP1.PROJ,DISP=SHR
