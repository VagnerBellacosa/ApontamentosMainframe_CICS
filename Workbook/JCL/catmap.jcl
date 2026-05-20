//CATMAPA   JOB    ('00'),TSO.&SYSUID,                                  00010000
//          REGION=4M,NOTIFY=&SYSUID,                                   00020000
//          MSGCLASS=T,MSGLEVEL=1,CLASS=A                               00030000
//*-----------------------------------------------------------------*   00040000
//* COMPILA E LINKEDITA PROGRAMA COBOL CICS                             00040100
//*-----------------------------------------------------------------*   00041000
//DFHMAPS PROC INDEX='DFH410.CICS',      FOR SDFHMAC                    00045000
//             MAPLIB='DFH410.CICS.USERLOAD',                           00046000
//             MAPNAME=IBMT001,             NAME OF MAPSET - REQUIRED   00047052
//             A=,                           A=A FOR ALIGNED MAP        00048000
//*            RMODE=24,                     24/ANY                     00049000
//             ASMBLR=ASMA90,                ASSEMBLER PROGRAM NAME     00050000
//             REG=2048K,                    REGION FOR ASSEMBLY        00060000
//             OUTC=*,                       PRINT SYSOUT CLASS         00070000
//             WORK=SYSDA                    WORK FILE UNIT             00080000
//*-----------------------------------------------------------------    00090000
//COPY     EXEC PGM=IEBGENER                                            00100000
//SYSPRINT DD SYSOUT=&OUTC                                              00110000
//SYSUT1   DD DSN=SYS1.MACLIB(&MAPNAME),DISP=SHR                        00120036
//         DD DSN=CEE.SCEEMAC,DISP=SHR                                  00120151
//         DD DSN=IBMUSER.WORKBOOK.CICS(&MAPNAME),DISP=SHR              00121051
//SYSUT2   DD DSN=&&TEMPM,UNIT=&WORK,DISP=(,PASS),                      00130000
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=400),                      00140000
//            SPACE=(400,(50,50))                                       00150000
//SYSIN    DD DUMMY                                                     00160000
//*-----------------------------------------------------------------    00170000
//ASMMAP   EXEC PGM=&ASMBLR,REGION=&REG,                                00180000
//  PARM='SYSPARM(&A.MAP),DECK,NOOBJECT'                                00190000
//SYSPRINT DD SYSOUT=&OUTC                                              00200000
//SYSLIB   DD DSN=&INDEX..SDFHMAC,DISP=SHR                              00210000
//         DD DSN=SYS1.MACLIB,DISP=SHR                                  00221022
//SYSUT1   DD UNIT=&WORK,SPACE=(CYL,(5,5))                              00230000
//SYSUT2   DD UNIT=&WORK,SPACE=(CYL,(5,5))                              00240000
//SYSUT3   DD UNIT=&WORK,SPACE=(CYL,(5,5))                              00250000
//SYSPUNCH DD DSN=&&MAP,DISP=(,PASS),UNIT=&WORK,                        00260000
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=400),                      00270000
//            SPACE=(400,(50,50))                                       00280000
//SYSIN    DD DSN=&&TEMPM,DISP=(OLD,PASS)                               00290000
//*-----------------------------------------------------------------    00300000
//LINKMAP  EXEC PGM=IEWL,PARM='LIST,LET,XREF,AMODE=31,RMODE=ANY'        00310000
//SYSPRINT DD SYSOUT=&OUTC                                              00320000
//SYSLMOD  DD DSN=&MAPLIB(&MAPNAME),DISP=SHR                            00330000
//SYSUT1   DD UNIT=&WORK,SPACE=(1024,(20,20))                           00340000
//SYSLIN   DD DSN=&&MAP,DISP=(OLD,DELETE)                               00350000
//*-----------------------------------------------------------------    00360000
// PEND                                                                 00510000
/*                                                                      00520000
//COBCURSO  EXEC DFHMAPS                                                00530012
