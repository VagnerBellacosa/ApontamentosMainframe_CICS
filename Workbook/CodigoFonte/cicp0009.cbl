      ******************************************************************00010016
      * PROGRAMA    : CICP0009                                          00020034
      * CRIAÇÃO     : 23/02/2026                                        00030034
      * PROGRAMADOR : VAGNER R BELLACOSA                                00040016
      * SISTEMA     : CIC - CICS                                        00050016
      * TIPO OBJETO : PROGRAMA COBOL/CICS                               00060016
      * AMBIENTE    : ONLINE                                            00070016
      * FINALIDADE  : HELLO WORLD EM MAPA BMS                           00080018
      ******************************************************************00090016
      *                                                                 00100016
      *------------------------                                         00110016
       IDENTIFICATION DIVISION.                                         00120016
      *------------------------                                         00130016
      *                                                                 00140016
       PROGRAM-ID. CICP0009.                                            00150034
      *                                                                 00160016
      *---------------------                                            00170016
       ENVIRONMENT DIVISION.                                            00180016
      *---------------------                                            00190016
      *                                                                 00200016
      *--------------                                                   00210016
       DATA DIVISION.                                                   00220016
      *--------------                                                   00230016
      *                                                                 00240016
      *------------------------                                         00250016
       WORKING-STORAGE SECTION.                                         00260016
      *------------------------                                         00270016
      *                                                                 00280016
      *----                                                             00281028
       01  MSG-ERRO.                                                    00282028
           03  FILLER                PIC  X(005) VALUE 'CMD= '.         00283028
           03  COMANDO               PIC  X(008) VALUE SPACES.          00284028
           03  FILLER                PIC  X(008) VALUE ', RESP= '.      00285028
           03  RESP1-DIS             PIC  9(005).                       00286028
           03  FILLER                PIC  X(009) VALUE ', RESP2= '.     00287028
           03  RESP2-DIS             PIC  9(005).                       00288028
      *----                                                             00289028
       77  CTE-INICIO              PIC X(015) VALUE 'WSS COMECA AQUI'.  00290016
       77  CTE-VERSAO              PIC X(006) VALUE 'VRS001'.           00300016
       77  WSS-NOME-PROGRAMADOR    PIC X(040) VALUE SPACES.             00310016
       77  WSS-DATE-SYSTEM         PIC X(080) VALUE SPACES.             00320016
       77  WSS-DISP-MESSAGE        PIC X(40).                           00330016
       77  WSS-DISP-LENGTH         PIC S9(4)  COMP.                     00340023
       77  MAPA                    PIC X(8)   VALUE 'MAPS005'.          00340128
       77  CONT-CHAVE              PIC X(016) VALUE 'CONTCHAVE'.        00341028
       77  CONT-NAME               PIC X(016) VALUE 'TESTE1'.           00341128
       77  CHANNEL-NAME            PIC X(016) VALUE 'CANAL1'.           00342028
       77  RESP1                   PIC S9(4)  COMP VALUE ZEROS.         00344023
       77  RESP2                   PIC S9(4)  COMP VALUE ZEROS.         00345023
       77  WSS-ABS                 PIC S9(015) COMP-3.                  00346032
      *                                                                 00350016
      *                                                                 00351019
      *---- AREAS DO MAPA CICS BMS                                      00352019
          COPY MAPS005.                                                 00353022
      *                                                                 00354019
      *---- AREAS DE RETORNO E CONTROLE DO CICS                         00355019
          COPY DFHAID.                                                  00356019
      *                                                                 00356133
      *---- AREAS DE RETORNO E CONTROLE DO CICS                         00356233
          COPY DFHBMSCA.                                                00356333
                                                                        00356426
      *                                                                 00357021
      *-------------------                                              00360016
       PROCEDURE DIVISION.                                              00370016
      *-------------------                                              00380016
      *                                                                 00390016
                                                                        00391129
      *  OBTEM O NOME DO CANAL                                          00392026
           EXEC CICS                                                    00393026
              ASSIGN CHANNEL(CHANNEL-NAME)                              00393126
           END-EXEC.                                                    00394026
                                                                        00397026
      *  CASO DE ERRO NO CANAL ABENDA O PROGRAMA                        00398026
           IF CHANNEL-NAME = SPACES THEN                                00399026
             MOVE 'TESTE1'   TO CHANNEL-NAME                            00399126
      *                                                                 00399226
      *         EXEC CICS ABEND                                         00400026
      *            ABCODE('ERCH') NODUMP                                00401026
      *         END-EXEC                                                00410026
           ELSE                                                         00411029
             DISPLAY 'OLACB02 ENTERED WITH CHANNEL ' CHANNEL-NAME       00412029
           END-IF.                                                      00420026
      *                                                                 00951019
      ******************************************************************00951119
      * ROTINA VERIFICA SE É A PRIMEIRA VEZ QUE O PROGRAMA EXECUTA      00951219
      ******************************************************************00951319
           EXEC CICS GET CONTAINER(CONT-NAME)                           00952126
                               CHANNEL(CHANNEL-NAME)                    00952226
      ***                      FROM(MAPA)                               00952330
              NODATA           FLENGTH(LENGTH OF MAPA)                  00952530
      *****   INTOCCSID(037)                                            00952629
                               RESP(RESP1)                              00952724
                               RESP2(RESP2)                             00952824
           END-EXEC.                                                    00952924
                                                                        00953624
      *    EXEC CICS GET CONTAINER(CONT-NAME)                           00953726
      *                     CHANNEL(CHANNEL-NAME)                       00953826
      *                     NODATA FLENGTH(LENGTH OF MAPS005I)          00953926
      *                     INTOCCSID(037)                              00954026
      *                     RESP(RESP1)                                 00954126
      *                     RESP2(RESP2)                                00954226
      *    END-EXEC.                                                    00954326
                                                                        00954426
      *----                                                             00954519
           IF RESP1 NOT EQUAL DFHRESP(NORMAL)                           00954619
              MOVE RESP1 TO RESP1-DIS                                   00954729
              DISPLAY ' RESP1 = ' RESP1-DIS                             00954829
              PERFORM  0000-PRIMEIRA-VEZ                                00954919
           ELSE                                                         00955019
              PERFORM  0100-SEGUNDA-VEZ                                 00955119
           END-IF.                                                      00955219
                                                                        00955319
      ******************************************************************00955425
      * CHAMA TELA DE HELLO WORLD                                       00955525
      ******************************************************************00955625
      *                                                                 00955725
           DISPLAY ' '                                                  00955825
           DISPLAY '***************************************************'00955925
           DISPLAY '***       OIXX - CICP0009          ****************'00956034
           DISPLAY '***************************************************'00956125
           DISPLAY ' '                                                  00956225
           DISPLAY ' HELLO WORLD NO MAPA'                               00956325
           DISPLAY ' '.                                                 00956425
                                                                        00956525
      ******************************************************************00956619
      * ROTINA EXECUTADA SE O PROGRAMA É CARREGADO PELA PRIMEIRA VEZ    00956719
      ******************************************************************00956819
       0000-PRIMEIRA-VEZ.                                               00956919
                                                                        00957029
           DISPLAY '0000-PRIMEIRA-VEZ'.                                 00957129
                                                                        00957220
           EXEC CICS SEND CONTROL FREEKB ERASE END-EXEC.                00957319
                                                                        00957420
           EXEC CICS PUT CONTAINER(CONT-NAME)                           00957526
                               CHANNEL(CHANNEL-NAME)                    00957626
                               FROM(MAPA)                               00957730
                               FLENGTH(LENGTH OF MAPA)                  00957830
                               RESP(RESP1)                              00957926
                               RESP2(RESP2)                             00958026
           END-EXEC.                                                    00958126
                                                                        00958226
           IF RESP1 NOT EQUAL DFHRESP(NORMAL)                           00958326
              DISPLAY ' ERRO NO PUT CONTAINER'                          00958426
           END-IF.                                                      00958526
                                                                        00958626
      *----                                                             00959530
           PERFORM 0001-LIMPA-CAMPOS.                                   00959630
      *----                                                             00959730
                                                                        00960124
      *----                                                             00960219
           PERFORM 0200-ROTINA-SEND.                                    00960319
      *----                                                             00960419
           PERFORM 1000-FIM.                                            00960519
      *----                                                             00960619
      ******************************************************************00960731
      * ROTINA PARA LIMPAR OS CAMPOS DA TELA                            00960831
      ******************************************************************00960931
       0001-LIMPA-CAMPOS.                                               00961031
                                                                        00961131
           DISPLAY '0001-LIMPA-CAMPOS'.                                 00961231
                                                                        00961331
           INITIALIZE MAPS005I.                                         00961431
                                                                        00961531
           EXEC CICS ASSIGN USERID(DFH0001I) END-EXEC                   00962431
                                                                        00962531
           EXEC CICS ASSIGN APPLID(DFH0002I) END-EXEC                   00962631
                                                                        00962731
           EXEC CICS ASKTIME ABSTIME (WSS-ABS) END-EXEC                 00962832
                                                                        00963031
           EXEC CICS FORMATTIME ABSTIME (WSS-ABS)                       00963132
                                DATESEP                                 00963231
                                DDMMYYYY (DFH0003I)                     00963331
                                TIME (DFH0004I)                         00963431
                                TIMESEP                                 00963531
           END-EXEC.                                                    00963631
                                                                        00963731
      ******************************************************************00963831
      ******************************************************************00963919
      * ROTINA EXECUTADA SE O PROGRAMA É CARREGADO PELA SEGUNDA VEZ     00964019
      ******************************************************************00964119
       0100-SEGUNDA-VEZ.                                                00964219
                                                                        00964320
           DISPLAY '0100-SEGUNDA-VEZ'.                                  00964429
                                                                        00964529
           PERFORM 0300-ROTINA-RECE                                     00964619
      *----                                                             00964719
           EVALUATE EIBAID                                              00964819
               WHEN  DFHPF3                                             00964919
                     PERFORM  2000-ENCERRA                              00965019
               WHEN  OTHER                                              00965119
                     IF MSGERROO = 'TECLA INVALIDA.'                    00965221
                        MOVE 'PARA SAIR PRESSIONE F3'                   00965321
                                               TO MSGERROO              00965421
                     ELSE                                               00965521
                        MOVE 'TECLA INVALIDA.' TO MSGERROO              00965621
                     END-IF                                             00965721
                                                                        00965821
                     PERFORM 0200-ROTINA-SEND                           00965919
                     PERFORM  1000-FIM                                  00966019
           END-EVALUATE.                                                00966119
                                                                        00966220
      ******************************************************************00966319
      * ROTINA SEND MAP                                                 00966419
      ******************************************************************00966519
       0200-ROTINA-SEND.                                                00966619
                                                                        00966720
           DISPLAY '0200-ROTINA-SEND'.                                  00966829
                                                                        00966929
           EXEC CICS SEND MAP(MAPA)                                     00967029
                          MAPSET('MAPS005')                             00967129
                          FROM(MAPS005O)                                00967229
                          NOHANDLE                                      00967329
           END-EXEC.                                                    00967429
                                                                        00967529
      ******************************************************************00967629
      * ROTINA RECEIVE MAP                                              00967729
      ******************************************************************00967829
       0300-ROTINA-RECE.                                                00967929
                                                                        00968029
           DISPLAY '0300-ROTINA-RECE'.                                  00968129
                                                                        00968229
           EXEC CICS RECEIVE MAP(MAPA)                                  00968329
                             MAPSET('MAPS005')                          00968429
                             INTO(MAPS005I)                             00968529
                             NOHANDLE                                   00968629
           END-EXEC.                                                    00968729
                                                                        00968829
      ******************************************************************00968929
      * ROTINA ENCERRA O PROGRAMA COM OS SEGUINTES PARÂMETROS:          00969029
      * FREEKB = LIBERA O TECLADO                                       00969129
      * ERASE  = LIMPA A TELA                                           00969229
      * RETURN = RETORNA AO CICS                                        00969329
      ******************************************************************00969429
                                                                        00969529
      ******************************************************************00969629
      * ROTINA ENCERRA O PROGRAMA                                       00969729
      ******************************************************************00969829
       1000-FIM.                                                        00969929
                                                                        00970029
           DISPLAY '1000-FIM'.                                          00970129
                                                                        00970229
           EXEC CICS SEND CONTROL FREEKB END-EXEC                       00970329
                                                                        00970429
           EXEC CICS RETURN TRANSID(EIBTRNID) CHANNEL(CHANNEL-NAME)     00970529
           END-EXEC.                                                    00970629
                                                                        00970729
                                                                        00970829
      ******************************************************************00970929
      * ROTINA ENCERRA O PROGRAMA                                       00971029
      ******************************************************************00971129
       2000-ENCERRA.                                                    00971229
                                                                        00971329
           DISPLAY '2000-ENCERRA'.                                      00971429
                                                                        00971529
           EXEC CICS                                                    00971629
              SEND CONTROL FREEKB ERASE                                 00971729
           END-EXEC.                                                    00971829
                                                                        00971929
           EXEC CICS                                                    00972029
              RETURN                                                    00972129
           END-EXEC.                                                    00972229
                                                                        00972329
      ******************************************************************00972419
       9999-FIM.                                                        00973016
                                                                        00980016
           DISPLAY 'FIM DO PROGRAMA '.                                  00990016
                                                                        01000016
           MOVE 'FIM DO PROGRAMA'  TO  WSS-DATE-SYSTEM.                 01010016
                                                                        01020016
           EXEC CICS SEND CONTROL FREEKB ALARM END-EXEC.                01080020
                                                                        01090016
           EXEC CICS RETURN                    END-EXEC.                01100016
                                                                        01110016
      ********************** FIM DO PROGRAMA ***************************01120016
