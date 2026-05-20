      ******************************************************************00000105
      * PROGRAMA    : CICP0011                                          00000218
      * CRIAÇÃO     : 02/03/2025                                        00000418
      * PROGRAMADOR : VAGNER BELLACOSA                                  00000518
      * SISTEMA     : CIC - CICS                                        00000605
      * TIPO OBJETO : PROGRAMA COBOL/CICS                               00000705
      * AMBIENTE    : ONLINE                                            00000905
      * FINALIDADE  : GRAVA UMA TS COM O NOME DO PROGRAMADOR            00001005
      ******************************************************************00001105
      *                                                                 00004005
      *------------------------                                         00010000
       IDENTIFICATION DIVISION.                                         00020000
      *------------------------                                         00030000
      *                                                                 00031005
       PROGRAM-ID. CICP0011.                                            00040018
      *                                                                 00041005
      *---------------------                                            00050000
       ENVIRONMENT DIVISION.                                            00070000
      *---------------------                                            00071000
      *                                                                 00071105
      *--------------                                                   00072000
       DATA DIVISION.                                                   00080000
      *--------------                                                   00081000
      *                                                                 00081105
      *------------------------                                         00082000
       WORKING-STORAGE SECTION.                                         00090000
      *------------------------                                         00091000
      *                                                                 00091105
       77  CTE-INICIO              PIC X(015) VALUE 'WSS COMECA AQUI'.  00092002
       77  CTE-VERSAO              PIC X(006) VALUE 'VRS001'.           00093002
       77  WSS-NOME-PROGRAMADOR    PIC X(040) VALUE SPACES.             00094011
       77  WSS-DATE-SYSTEM         PIC X(080) VALUE SPACES.             00094113
       77  WSS-DISP-MESSAGE        PIC X(40).                           00094214
       77  WSS-DISP-LENGTH         PIC S9(4) COMP.                      00094314
       77  WSS-USERID              PIC X(8).                            00094422
       77  WSS-APPLID              PIC X(4).                            00094522
       77  WSS-DATATIME            PIC X(32).                           00094622
       77  WSS-DATA                PIC X(10).                           00094722
       77  WSS-TIME                PIC X(10).                           00094822
       77  WSS-GENERICA            PIC X(60).                           00094922
      *----                                                             00096021
       01  MSG-AVISO               PIC X(60).                           00097021
      *                                                                 00097121
       01  MSG-ERRO.                                                    00097221
           03  FILLER                PIC  X(005) VALUE 'CMD= '.         00098021
           03  COMANDO               PIC  X(008) VALUE SPACES.          00099021
           03  FILLER                PIC  X(008) VALUE ', RESP= '.      00100021
           03  RESP1                 PIC  9(005).                       00110021
           03  FILLER                PIC  X(009) VALUE ', RESP2= '.     00120021
           03  RESP2                 PIC  9(005).                       00130021
                                                                        00140021
      *----                                                             00150021
      *-------------------                                              00220000
       PROCEDURE DIVISION.                                              00230000
      *-------------------                                              00231000
      *                                                                 00232005
      ******************************************************************00233005
      * ROTINA GRAVA UMA TS (UCOBXX) COM O CONEúDO DA VARIáVEL          00234005
      * WSS-NOME-PROGRAMADOR                                            00234111
      ******************************************************************00235005
      *                                                                 00236005
           DISPLAY ' '                                                  00236111
           DISPLAY '***************************************************'00236211
           DISPLAY '***       ME99 - CICP0011          ****************'00236323
           DISPLAY '***************************************************'00236411
           DISPLAY ' '                                                  00236523
                                                                        00237011
      *                                                                 00238011
      *    LIMPA AREA DE TS                                             00238116
      *    EXEC CICS                                                    00238221
      *           DELETEQ TS QUEUE ('INEFEXXX')                         00238321
      *                   RESP(RESP1)                                   00238421
      *                   RESP2(RESP2)                                  00238521
      *    END-EXEC.                                                    00238621
      *----                                                             00238719
      *    IF RESP1 NOT EQUAL DFHRESP(NORMAL)                           00238821
      *      DISPLAY 'ERRO NO DELETEQ TS'                               00238921
      *      MOVE    'ERRO NO DELETEQ TS' TO MSG-AVISO                  00239021
      *      PERFORM 9888-ALERTA                                        00239121
      *    END-IF.                                                      00239221
      *                                                                 00239319
           MOVE SPACE           TO  WSS-DATE-SYSTEM.                    00239424
                                                                        00239524
           EXEC CICS WRITEQ TS QUEUE('SYSPROG9')                        00239624
                              FROM(WSS-DATE-SYSTEM)                     00239724
                     RESP(RESP1)                                        00239824
                     RESP2(RESP2)                                       00239924
           END-EXEC.                                                    00240024
                                                                        00240124
           MOVE 'INICIO MSA1 '  TO  WSS-DATE-SYSTEM.                    00240224
                                                                        00240319
           EXEC CICS WRITEQ TS QUEUE('SYSPROG9')                        00240423
                              FROM(WSS-DATE-SYSTEM)                     00240519
                     RESP(RESP1)                                        00240620
                     RESP2(RESP2)                                       00240720
           END-EXEC.                                                    00240820
      *----                                                             00240920
           IF RESP1 NOT EQUAL DFHRESP(NORMAL)                           00241020
             DISPLAY 'ERRO NO WRITEQ TS'                                00241120
             MOVE    'ERRO NO WRITEQ TS' TO MSG-AVISO                   00241220
             PERFORM 9888-ALERTA                                        00241320
           END-IF.                                                      00241420
      *                                                                 00242020
           MOVE 'INEFE00' TO WSS-NOME-PROGRAMADOR.                      00560018
                                                                        00561011
           EXEC CICS WRITEQ TS QUEUE('SYSPROG9')                        00570023
                              FROM(WSS-NOME-PROGRAMADOR)                00580011
                     RESP(RESP1)                                        00581020
                     RESP2(RESP2)                                       00582020
           END-EXEC.                                                    00583020
      *----                                                             00584020
           IF RESP1 NOT EQUAL DFHRESP(NORMAL)                           00585020
             DISPLAY 'ERRO NO WRITEQ TS 1'                              00586020
             MOVE    'ERRO NO WRITEQ TS 1' TO MSG-AVISO                 00587020
             PERFORM 9888-ALERTA                                        00588020
           END-IF.                                                      00589020
      *                                                                 00589120
           MOVE '       ' TO WSS-DATE-SYSTEM.                           00591113
                                                                        00591213
           EXEC CICS WRITEQ TS QUEUE('SYSPROG9')                        00591323
                              FROM(WSS-DATE-SYSTEM)                     00591413
                     RESP(RESP1)                                        00591520
                     RESP2(RESP2)                                       00591620
           END-EXEC.                                                    00591720
      *----                                                             00591820
           PERFORM 9777-GET-INFO.                                       00591922
      *----                                                             00592022
           IF RESP1 NOT EQUAL DFHRESP(NORMAL)                           00592120
             DISPLAY 'ERRO NO WRITEQ TS 2'                              00592220
             MOVE    'ERRO NO WRITEQ TS 2' TO MSG-AVISO                 00592320
             PERFORM 9888-ALERTA                                        00592420
           ELSE                                                         00592521
             PERFORM 9999-FIM                                           00592621
           END-IF.                                                      00592720
                                                                        00592822
      *                                                                 00592920
      ******************************************************************00601205
      * ROTINA ENCERRA O PROGRAMA COM OS SEGUINTES PARÂMETROS:          00601305
      * FREEKB = LIBERA O TECLADO                                       00601405
      * ERASE  = LIMPA A TELA                                           00601505
      * RETURN = RETORNA AO CICS                                        00601605
      ******************************************************************00601705
      *                                                                 00601805
       9666-GRAVA-TS-GENERICA.                                          00601922
                                                                        00602022
           EXEC CICS WRITEQ TS QUEUE('SYSPROG9')                        00602523
                              FROM(WSS-GENERICA)                        00602622
                     RESP(RESP1)                                        00602722
                     RESP2(RESP2)                                       00602822
           END-EXEC.                                                    00602922
      *----                                                             00603022
           IF RESP1 NOT EQUAL DFHRESP(NORMAL)                           00603122
             DISPLAY 'ERRO NO WRITEQ GENE'                              00603222
             MOVE    'ERRO NO WRITEQ GENE' TO MSG-AVISO                 00603322
             PERFORM 9888-ALERTA                                        00603422
           END-IF.                                                      00603522
                                                                        00603622
       9777-GET-INFO.                                                   00603722
                                                                        00603822
           EXEC CICS ASSIGN USERID(WSS-USERID) END-EXEC                 00603922
                                                                        00604022
           STRING 'USER-ID = '  DELIMITED BY SIZE                       00604122
                  WSS-USERID    DELIMITED BY SIZE                       00604222
               INTO  WSS-GENERICA                                       00604322
           END-STRING.                                                  00604422
                                                                        00604522
           PERFORM 9666-GRAVA-TS-GENERICA                               00604622
                                                                        00604722
           EXEC CICS ASSIGN APPLID(WSS-APPLID) END-EXEC                 00604822
                                                                        00604922
           STRING 'APPLID = '  DELIMITED BY SIZE                        00605022
                   WSS-APPLID  DELIMITED BY SIZE                        00605122
               INTO  WSS-GENERICA                                       00605222
           END-STRING.                                                  00605322
                                                                        00605422
           PERFORM 9666-GRAVA-TS-GENERICA                               00605522
                                                                        00605622
           EXEC CICS ASKTIME ABSTIME (WSS-DATATIME) END-EXEC            00605722
                                                                        00605822
           STRING 'DATA/HORA = ' DELIMITED BY SIZE                      00605922
                   WSS-DATATIME  DELIMITED BY SIZE                      00606022
             INTO  WSS-GENERICA                                         00606122
           END-STRING.                                                  00606222
                                                                        00606322
           PERFORM 9666-GRAVA-TS-GENERICA                               00606422
                                                                        00606522
           EXEC CICS FORMATTIME ABSTIME (WSS-DATATIME)                  00606622
                                DATESEP                                 00606722
                                DDMMYYYY (WSS-DATA)                     00606822
                                TIME (WSS-TIME)                         00606922
                                TIMESEP                                 00607022
           END-EXEC.                                                    00607122
                                                                        00607222
           STRING 'DATA = ' DELIMITED BY SIZE                           00607322
                  WSS-DATA  DELIMITED BY SIZE                           00607422
              INTO  WSS-GENERICA                                        00607522
           END-STRING.                                                  00607622
                                                                        00607722
           PERFORM 9666-GRAVA-TS-GENERICA                               00607822
                                                                        00607922
           STRING 'HORA = '  DELIMITED BY SIZE                          00608022
                   WSS-TIME  DELIMITED BY SIZE                          00608122
              INTO  WSS-GENERICA                                        00608222
           END-STRING.                                                  00608322
                                                                        00608422
           PERFORM 9666-GRAVA-TS-GENERICA.                              00608522
                                                                        00608622
       9888-ALERTA.                                                     00608722
                                                                        00608822
           EXEC CICS SEND CONTROL FREEKB ERASE                          00608922
           END-EXEC.                                                    00609022
      *----                                                             00609122
                                                                        00609220
           EXEC CICS SEND  FROM   (MSG-AVISO)                           00609320
           END-EXEC                                                     00609420
      *----                                                             00609520
           EXEC CICS RETURN                                             00609620
           END-EXEC.                                                    00609720
                                                                        00609820
       9999-FIM.                                                        00609920
                                                                        00610020
           DISPLAY 'FIM DO PROGRAMA '.                                  00610113
                                                                        00610213
           MOVE 'FIM DO PROGRAMA'  TO  WSS-DATE-SYSTEM.                 00610313
                                                                        00610413
           EXEC CICS WRITEQ TS QUEUE('SYSPROG9')                        00610523
                              FROM(WSS-DATE-SYSTEM)                     00610613
           END-EXEC.                                                    00610713
                                                                        00610813
           EXEC CICS WRITEQ TS QUEUE('SYSPROG9')                        00610923
                              FROM(MSG-ERRO)                            00611021
           END-EXEC.                                                    00611121
                                                                        00612021
                                                                        00620013
           EXEC CICS SEND CONTROL FREEKB ERASE END-EXEC.                00630005
                                                                        00640012
           EXEC CICS RETURN                    END-EXEC.                00650005
                                                                        00650113
      ********************** FIM DO PROGRAMA ***************************00660012
