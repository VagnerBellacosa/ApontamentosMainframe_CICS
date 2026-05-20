//CATMAPA   JOB    ('00'),TSO.&SYSUID,                                  00020053
//          REGION=4M,NOTIFY=&SYSUID,                                   00030053
//          MSGCLASS=T,MSGLEVEL=1,CLASS=A                               00040053
//*-----------------------------------------------------------------*   00050053
//* COMPILA E LINKEDITA PROGRAMA COBOL CICS                             00060053
//*-----------------------------------------------------------------*   00070053
//DFHMAPS PROC INDEX='DFH410.CICS',      FOR SDFHMAC                    00080053
//             MAPLIB='DFH410.CICS.USERLOAD',                           00090053
//             MAPNAME='IBMT001',            NAME OF MAPSET - REQUIRED  00100053
//             SRCLIB='IBMUSER.WORKBOOK.CICS',                          00110053
//             DSCTLIB='IBMUSER.WORKBOOK.COPY',                         00120053
//             A=,                           A=A FOR ALIGNED MAP        00130053
//*            RMODE=24,                     24/ANY                     00140053
//             ASMBLR=ASMA90,                ASSEMBLER PROGRAM NAME     00150053
//             REG=2048K,                    REGION FOR ASSEMBLY        00160053
//             OUTC=*,                       PRINT SYSOUT CLASS         00170053
//             WORK=SYSDA                    WORK FILE UNIT             00180053
//*-----------------------------------------------------------------    00190053
//*- COPIA MAPA PARA TEMPORARIO ------------------------------------    00200053
//*-----------------------------------------------------------------    00210053
//COPY     EXEC PGM=IEBGENER                                            00220053
//SYSPRINT DD SYSOUT=&OUTC                                              00230053
//*SYSUT1   DD DSN=SYS1.MACLIB(&MAPNAME),DISP=SHR                       00240053
//SYSUT1   DD DSN=&SRCLIB(&MAPNAME),DISP=SHR                            00250053
//SYSUT2   DD DSN=&&TEMPM,UNIT=&WORK,DISP=(,PASS),                      00260053
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=400),                      00270053
//            SPACE=(400,(50,50))                                       00280053
//SYSIN    DD DUMMY                                                     00290053
//BSTERR DD SYSOUT=*                                                    00291054
//*-----------------------------------------------------------------    00300053
//*- COMPILA MAPA ASSEMBLER ----------------------------------------    00310053
//*-----------------------------------------------------------------    00320053
//ASMMAP   EXEC PGM=&ASMBLR,REGION=&REG,                                00330053
//  PARM='SYSPARM(&A.MAP),DECK,NOOBJECT'                                00340053
//SYSPRINT DD SYSOUT=&OUTC                                              00350053
//SYSLIB   DD DSN=&INDEX..SDFHMAC,DISP=SHR                              00360053
//         DD DSN=SYS1.MACLIB,DISP=SHR                                  00370053
//         DD DSN=CEE.SCEEMAC,DISP=SHR                                  00380053
//         DD DSN=&SRCLIB,DISP=SHR                                      00390053
//SYSUT1   DD UNIT=&WORK,SPACE=(CYL,(5,5))                              00400053
//SYSUT2   DD UNIT=&WORK,SPACE=(CYL,(5,5))                              00410053
//SYSUT3   DD UNIT=&WORK,SPACE=(CYL,(5,5))                              00420053
//SYSPUNCH DD DSN=&&MAP,DISP=(,PASS),UNIT=&WORK,                        00430053
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=400),                      00440053
//            SPACE=(400,(50,50))                                       00450053
//SYSIN    DD DSN=&&TEMPM,DISP=(OLD,PASS)                               00460053
//SYSLIN   DD DUMMY                                                     00470053
//BSTERR DD SYSOUT=*                                                    00471054
//*-----------------------------------------------------------------    00480053
//*- LINKEDITA O MAPA ----------------------------------------------    00490053
//*-----------------------------------------------------------------    00500053
//LINKMAP  EXEC PGM=IEWL,COND=(4,LT),                                   00510053
//  PARM='LIST,LET,XREF,AMODE=31,RMODE=ANY'                             00520053
//SYSPRINT DD SYSOUT=&OUTC                                              00530053
//SYSLMOD  DD DSN=&MAPLIB(&MAPNAME),DISP=SHR                            00540053
//SYSUT1   DD UNIT=&WORK,SPACE=(1024,(20,20))                           00550053
//SYSLIN   DD DSN=&&MAP,DISP=(OLD,DELETE)                               00560053
//BSTERR DD SYSOUT=*                                                    00561054
//*------------------------------------------------------------------*  00570053
//*- DSECT GENERATION                                                *  00580053
//*------------------------------------------------------------------*  00590053
//ASMDSECT EXEC PGM=&ASMBLR,REGION=&REG,COND=(4,LT),                    00600053
//  PARM='SYSPARM(DSECT),DECK,NOLOAD'                                   00610053
//SYSPRINT DD SYSOUT=&OUTC                                              00620053
//SYSLIB   DD DSN=&INDEX..SDFHMAC,DISP=SHR                              00630053
//         DD DSN=SYS1.MACLIB,DISP=SHR                                  00640053
//         DD DSN=&SRCLIB,DISP=SHR                                      00650053
//SYSUT1   DD UNIT=&WORK,SPACE=(CYL,(5,5))                              00660053
//SYSUT2   DD UNIT=&WORK,SPACE=(CYL,(5,5))                              00670053
//SYSUT3   DD UNIT=&WORK,SPACE=(CYL,(5,5))                              00680053
//SYSPUNCH DD DSN=&DSCTLIB(&MAPNAME),DISP=OLD                           00690053
//SYSIN    DD DSN=&&TEMPM,DISP=(OLD,DELETE)                             00700053
//SYSLIN   DD DUMMY                                                     00710053
//BSTERR DD SYSOUT=*                                                    00711054
// PEND                                                                 00720053
/*                                                                      00730053
//COBCURSO  EXEC DFHMAPS                                                00740053
