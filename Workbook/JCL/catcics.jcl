//CATCCIC   JOB    ('00'),TSO.&SYSUID,    ç                             00010099
//          REGION=4M,NOTIFY=&SYSUID,                                   00020000
//          MSGCLASS=T,MSGLEVEL=1,CLASS=A                               00030000
//*-----------------------------------------------------------------*   00040000
//* COMPILA E LINKEDITA PROGRAMA COBOL CICS                             00040100
//*-----------------------------------------------------------------*   00041000
//COMPCOB PROC MOD=CICP0007,                                            00050099
//             DSNCOB='IBMUSER.WORKBOOK.CICS',                          00060099
//             COPY='IBMUSER.WORKBOOK.COPY',                            00060199
//             INDEX='DFH410.CICS',                                     00060299
//             INDEX2='DFH410.CICS',                                    00061054
//             INDCOB='IGY420',                                         00070000
//             INDLE='CEE'                                              00080000
//*******************************************************************   00081099
//TRN    EXEC PGM=DFHECP1$,                                             00090000
//            PARM='SP,COBOL3,NUM',                                     00100000
//            REGION=4M                                                 00110000
//STEPLIB  DD DSN=&INDEX..SDFHLOAD,DISP=SHR                             00120000
//SYSPRINT DD SYSOUT=A                                                  00130000
//SYSPUNCH DD DSN=&&SYSCIN,                                             00140000
//            DISP=(,PASS),UNIT=SYSDA,                                  00150000
//            DCB=BLKSIZE=800,                                          00160000
//            SPACE=(800,(1600,200))                                    00170000
//SYSIN    DD DSN=&DSNCOB(&MOD),DISP=SHR                                00180000
//*******************************************************************   00190000
//COB    EXEC PGM=IGYCRCTL,REGION=2M,                                   00200000
//  PARM='BUFSIZE(32760),,OPT(STD),EXIT(NOPRTEXIT),MAP,LIST,LIB'        00210099
//* PARM='BUFSIZE(32760),,OPT(STD),EXIT(NOPRTEXIT),NUMPROC(MIG),MAP'    00211000
//* PARM='BUFSIZE(32760),LIB,,NOOPT,FLAG(I,W),RENT,LIST,ARITH(EXTEND)'  00212099
//STEPLIB  DD DISP=SHR,DSN=&INDCOB..SIGYCOMP                            00220000
//*        DD DISP=SHR,DSN=&INDCOB..LOAD                                00230000
//SYSLIB   DD DSN=&INDEX..SDFHSAMP,DISP=SHR                             00240000
//         DD DSN=&INDEX..SDFHCOB,DISP=SHR                              00250000
//         DD DSN=&DSNCOB,DISP=SHR                                      00260037
//         DD DSN=&COPY,DISP=SHR                                        00261099
//SYSPRINT DD SYSOUT=*                                                  00270000
//SYSIN    DD DSN=&&SYSCIN,DISP=(OLD,DELETE)                            00280000
//SYSLIN   DD DSN=&&LOADSET,DISP=(MOD,PASS),                            00290000
//            UNIT=SYSDA,SPACE=(180,(250,100))                          00300000
//SYSUT1   DD UNIT=SYSDA,SPACE=(960,(550,100))                          00310000
//SYSUT2   DD UNIT=SYSDA,SPACE=(960,(550,100))                          00320000
//SYSUT3   DD UNIT=SYSDA,SPACE=(960,(550,100))                          00330000
//SYSUT4   DD UNIT=SYSDA,SPACE=(960,(550,100))                          00340000
//SYSUT5   DD UNIT=SYSDA,SPACE=(960,(550,100))                          00350000
//SYSUT6   DD UNIT=SYSDA,SPACE=(960,(550,100))                          00360000
//SYSUT7   DD UNIT=SYSDA,SPACE=(960,(550,100))                          00370000
//*                                                                     00380000
//********************************************************************  00390000
//LKED   EXEC PGM=IEWL,REGION=2M,                                       00400000
//            PARM='XREF',COND=(5,LT,COB)                               00410000
//SYSLIB   DD DSN=&INDEX..SDFHLOAD,DISP=SHR                             00420000
//         DD DSN=&INDLE..SCEELKED,DISP=SHR                             00430000
//         DD DSN=ADCD.Z112.USERLOAD,DISP=SHR                           00431000
//SYSLMOD  DD DSN=&INDEX2..USERLOAD(&MOD),DISP=SHR                      00440054
//SYSUT1   DD UNIT=SYSDA,DCB=BLKSIZE=1024,                              00450000
//            SPACE=(1024,(200,20))                                     00460000
//SYSPRINT DD SYSOUT=A                                                  00470000
//SYSLIN   DD DSN=&INDEX..SDFHCOB(DFHEILIC),DISP=SHR                    00480000
//         DD DSN=&&LOADSET,DISP=(OLD,DELETE)                           00490000
//         DD DDNAME=SYSIN                                              00500000
// PEND                                                                 00510000
/*                                                                      00520000
//COBCURSO  EXEC COMPCOB                                                00530037
//************************** FIM DO JCL     ****************************00540099
