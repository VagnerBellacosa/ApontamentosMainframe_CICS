      ******************************************************************00010005
      * PROGRAMA    : CICP0001                                          00020021
      * CRIAÇÃO     : 11/02/2026                                        00030021
      * PROGRAMADOR : VAGNER R BELLACOSA                                00040011
      * SISTEMA     : CIC - CICS                                        00050005
      * TIPO OBJETO : PROGRAMA COBOL/CICS                               00060005
      * AMBIENTE    : ONLINE                                            00070005
      * FINALIDADE  : ESCREVE HELLO WORLD NA CONSOLA DO CICS            00080015
      * TRANSACAO   : ME01                                              00090023
      ******************************************************************00100016
      *                                                                 00110005
      *------------------------                                         00120000
       IDENTIFICATION DIVISION.                                         00130000
      *------------------------                                         00140000
      *                                                                 00150005
       PROGRAM-ID. CICP0001.                                            00160021
      *                                                                 00170005
      *---------------------                                            00180000
       ENVIRONMENT DIVISION.                                            00190000
      *---------------------                                            00200000
      *                                                                 00210005
      *--------------                                                   00220000
       DATA DIVISION.                                                   00230000
      *--------------                                                   00240000
      *                                                                 00250005
      *------------------------                                         00260000
       WORKING-STORAGE SECTION.                                         00270000
      *------------------------                                         00280000
      *                                                                 00290005
       77  CTE-INICIO              PIC X(015) VALUE 'WSS COMECA AQUI'.  00300002
       77  CTE-VERSAO              PIC X(006) VALUE 'VRS001'.           00310002
       77  WSS-NOME-PROGRAMADOR    PIC X(040) VALUE SPACES.             00320011
       77  WSS-MESSAGE             PIC X(040) VALUE SPACES.             00330013
      *                                                                 00340005
      *-------------------                                              00350000
       PROCEDURE DIVISION.                                              00360000
      *-------------------                                              00370000
      *                                                                 00380005
           DISPLAY ' '                                                  00390011
           DISPLAY '***************************************************'00400011
           DISPLAY '***       ME01 - CICP0001          ****************'00410023
           DISPLAY '***************************************************'00420011
                                                                        00430011
      * ENVIANDO UMA MENSAGEM AO TERMINAL                               00440013
      *                                                                 00450013
      ***  MOVE 'HELLO WORLD!!! BOA NOITE' TO WSS-MESSAGE               00460024
           MOVE 'MSA1  OLA MUNDO!!! BOM DIA' TO WSS-MESSAGE             00461025
      *                                                                 00470013
           DISPLAY 'TESTE TRANSACAO CICS : '                            00471022
               ' EIBTRNID : '    EIBTRNID                               00471122
               ' EIBTASKN : '    EIBTASKN                               00471222
               ' EIBTRMID : '    EIBTRMID                               00471322
                                                                        00471422
      *                                                                 00472022
           EXEC CICS SEND TEXT                                          00480013
                FROM (WSS-MESSAGE)                                      00490013
           END-EXEC.                                                    00500014
                                                                        00510013
      * TASK TERMINATES WITHOUT ANY INTERACTION FROM THE USER           00520013
       0000-FIM.                                                        00530000
                                                                        00540012
      *                                                                 00550013
      ******************************************************************00560013
      * ROTINA ENCERRA O PROGRAMA COM OS SEGUINTES PARÂMETROS:          00570013
      * FREEKB = LIBERA O TECLADO                                       00580013
      * RETURN = RETORNA AO CICS                                        00590013
      ******************************************************************00600013
      *                                                                 00610013
           EXEC CICS SEND CONTROL FREEKB END-EXEC.                      00620013
                                                                        00630012
           EXEC CICS RETURN              END-EXEC.                      00640019
                                                                        00650012
           GOBACK.                                                      00660019
      ********************** FIM DO PROGRAMA ***************************00670012
