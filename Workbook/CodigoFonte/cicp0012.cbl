      ******************************************************************        
      * PROGRAMA    : CICP0012                                                  
      * CRIAÇÃO     : 02/03/2026                                                
      * PROGRAMADOR : VAGNER R BELLACOSA                                        
      * SISTEMA     : CIC - CICS                                                
      * TIPO OBJETO : PROGRAMA COBOL/CICS/BMS                                   
      * AMBIENTE    : ONLINE                                                    
      * FINALIDADE  : EXIBE TELA DE ABERTURA                                    
      ******************************************************************        
      *                                                                         
      *------------------------                                               00
       IDENTIFICATION DIVISION.                                               00
      *------------------------                                               00
       PROGRAM-ID. CICP0012.                                                  00
                                                                                
      *---------------------                                                  00
       ENVIRONMENT DIVISION.                                                  00
      *---------------------                                                  00
        CONFIGURATION SECTION.                                                  
         SPECIAL-NAMES.  DECIMAL-POINT IS COMMA.                                
                                                                                
      *--------------                                                         00
       DATA DIVISION.                                                         00
      *--------------                                                         00
                                                                                
      *------------------------                                               00
       WORKING-STORAGE SECTION.                                               00
      *------------------------                                               00
       77  W-ABS                     PIC S9(015) COMP-3.                      00
       77  MEDIA-ARIT                PIC  9(002)V99 VALUE 0.                  00
       77  MATRIC                    PIC  9(011) VALUE 0.                     00
       77  MASCARA                   PIC  Z9,99.                              00
                                                                                
      *----                                                                   00
       01  MSG-ERRO.                                                          00
           03  FILLER                PIC  X(005) VALUE 'CMD= '.               00
           03  COMANDO               PIC  X(008) VALUE SPACES.                00
           03  FILLER                PIC  X(008) VALUE ', RESP= '.            00
           03  RESP1                 PIC  9(005).                             00
           03  FILLER                PIC  X(009) VALUE ', RESP2= '.           00
           03  RESP2                 PIC  9(005).                             00
      *----                                                                   00
       01  MAP-ABERT                 PIC  X(008) VALUE 'MAPS003 '.            00
       01  MAP-MENU                  PIC  X(008) VALUE 'MAPS004 '.            00
       01  MAPA                      PIC  X(008) VALUE 'MAPS003 '.            00
       01  WSS-ABERTURA              PIC  X(001) VALUE 'S'.                   00
       01  WSS-MAPS-OUT              PIC  X(008) VALUE SPACES.                00
       01  WSS-MAPS                  PIC  X(008) VALUE SPACES.                00
       01  WSS-SUBPROG               PIC  X(008) VALUE 'CICP0013'.            00
       01  ARQ-VSAM                  PIC  X(008) VALUE 'IOCALCME'.            00
       01  CONT-CHAVE                PIC  X(016) VALUE 'CONTCHAVE'.           00
       01  CONT-NAME                 PIC  X(016) VALUE 'CONTKAZ'.             00
       01  CHANNEL-NAME              PIC  X(016) VALUE 'CHANNKAZ'.            00
      *                                                                         
      *---- AREAS DO MAPA CICS BMS TELA DE ABERTURA                           00
          COPY MAPS003.                                                       00
      *                                                                         
      *---- AREAS DE RETORNO E CONTROLE DO CICS                               00
          COPY DFHAID.                                                        00
                                                                                
      *-------------------                                                    00
       PROCEDURE DIVISION.                                                    00
      *-------------------                                                    00
                                                                                
       MAIN-SECTION                      SECTION.                               
                                                                                
           DISPLAY '********************************************'               
           DISPLAY '* PROGRAMA CICP0012                        *'               
           DISPLAY '* EXIBE O MENU DE SELECAO E CARREGA PGM    *'               
           DISPLAY '* EXEMPLO DE TELA ABERTURA                 *'               
           DISPLAY '*                                          *'               
           DISPLAY '********************************************'.              
                                                                                
      ******************************************************************      00
      * ROTINA VERIFICA SE É A PRIMEIRA VEZ QUE O PROGRAMA EXECUTA            00
      ******************************************************************      00
           EXEC CICS GET CONTAINER(CONT-NAME)                                 00
                               CHANNEL(CHANNEL-NAME)                          00
                               INTO(MAPA)                                     00
                               FLENGTH(LENGTH OF MAPA)                        00
                               RESP(RESP1)                                    00
                               RESP2(RESP2)                                   00
           END-EXEC.                                                          00
      *----                                                                     
           DISPLAY ' CONT-NAME    : ' CONT-NAME                                 
                   ' CHANNEL-NAME : ' CHANNEL-NAME                              
                   ' MAPA         : ' MAPA                                      
                   ' RESP1        : ' RESP1                                     
      *----                                                                     
           IF RESP1 NOT EQUAL DFHRESP(NORMAL)                                 00
              PERFORM  0000-PRIMEIRA-VEZ                                      00
           ELSE                                                               00
              PERFORM  0100-SEGUNDA-VEZ  UNTIL  EIBAID  =  DFHPF3             00
           END-IF.                                                            00
                                                                                
      ******************************************************************      00
      * ROTINA EXECUTADA SE O PROGRAMA É CARREGADO PELA PRIMEIRA VEZ          00
      ******************************************************************      00
       0000-PRIMEIRA-VEZ.                                                     00
                                                                                
           DISPLAY '0000-PRIMEIRA-VEZ'.                                         
                                                                                
           EXEC CICS SEND CONTROL FREEKB ERASE                                  
           END-EXEC.                                                            
                                                                                
           EXEC CICS PUT CONTAINER(CONT-NAME)                                 00
                               CHANNEL(CHANNEL-NAME)                          00
                               FROM(MAPA)                                     00
                               FLENGTH(LENGTH OF MAPA)                        00
                               RESP(RESP1)                                    00
                               RESP2(RESP2)                                   00
           END-EXEC.                                                          00
      *----                                                                   00
           DISPLAY ' CONT-NAME    : ' CONT-NAME                                 
                   ' CHANNEL-NAME : ' CHANNEL-NAME                              
                   ' MAPA         : ' MAPA                                      
                   ' RESP1        : ' RESP1                                     
                                                                                
           IF RESP1 NOT EQUAL DFHRESP(NORMAL)                                   
             EXEC CICS SEND FROM(MSG-ERRO) ERASE                                
             END-EXEC                                                           
           END-IF.                                                              
      *----                                                                   00
           PERFORM 0500-ABERTURA.                                             00
      *----                                                                   00
           IF EIBAID = DFHPF3                                                   
              DISPLAY 'PF3 - SAINDO A QUENTE'                                   
              PERFORM 9999-ENCERRA                                              
           ELSE                                                                 
              PERFORM 6000-FIM                                                00
           END-IF.                                                              
                                                                                
      *----                                                                   00
      ******************************************************************      00
      * ROTINA EXECUTADA SE O PROGRAMA É CARREGADO PELA SEGUNDA VEZ           00
      ******************************************************************      00
       0100-SEGUNDA-VEZ.                                                      00
                                                                                
           DISPLAY '0100-SEGUNDA-VEZ'.                                          
                                                                                
           PERFORM  0300-ROTINA-RECE                                          00
                                                                                
           DISPLAY ' EIBAID : ' EIBAID                                          
           MOVE SPACES            TO MSGERROO                                 00
      *--                                                                     00
           EVALUATE EIBAID                                                    00
                                                                                
               WHEN  DFHENTER                                                 00
                     DISPLAY ' ENTER '                                          
                     DISPLAY ' WSS-SUBPROG : ' WSS-SUBPROG                      
                     EXEC CICS XCTL                                             
                          PROGRAM(WSS-SUBPROG)                                  
                          RESP  (RESP1)                                         
                          RESP2 (RESP2)                                         
                     END-EXEC                                                   
                                                                                
                     IF RESP1 NOT EQUAL DFHRESP(NORMAL)                         
                       DISPLAY ' MSG-ERRO : ' MSG-ERRO                          
                       EXEC CICS SEND FROM(MSG-ERRO) ERASE                      
                       END-EXEC                                                 
                     END-IF                                                     
                     MOVE SPACES TO  EIBAID                                     
                                                                                
               WHEN  DFHPF3                                                     
                     DISPLAY 'PF3 - SAINDO'                                     
                     PERFORM 9999-ENCERRA                                       
                                                                                
               WHEN  OTHER                                                    00
                     MOVE 'TECLA INVÁLIDA.' TO MSGERROO                       00
                     PERFORM 0200-ROTINA-SEND                                 00
                     PERFORM  6000-FIM                                        00
                                                                                
           END-EVALUATE.                                                      00
                                                                                
                                                                                
      ******************************************************************      00
      * ROTINA SEND MAP                                                       00
      ******************************************************************      00
       0200-ROTINA-SEND.                                                      00
                                                                                
           DISPLAY '0200-ROTINA-SEND'.                                          
                                                                                
           DISPLAY 'LIMPA MAPA'                                                 
                                                                                
           EXEC CICS SEND CONTROL FREEKB ERASE                                  
           END-EXEC.                                                            
                                                                                
           MOVE 'MAPS003'      TO WSS-MAPS                                      
           MOVE 'MAPS003O'     TO WSS-MAPS-OUT                                  
                                                                                
           DISPLAY ' WSS-MAPS : ' WSS-MAPS                                      
           DISPLAY ' MSGERROO : ' MSGERROO                                      
                                                                                
           EXEC CICS SEND MAP(WSS-MAPS)                                       00
                          MAPSET(WSS-MAPS)                                    00
                          FROM(WSS-MAPS-OUT)                                  00
                          NOHANDLE                                            00
           END-EXEC.                                                          00
                                                                                
           EXEC CICS SEND FROM(MSGERROO)                                        
           END-EXEC.                                                            
                                                                                
      ******************************************************************      00
      * ROTINA RECEIVE MAP                                                    00
      ******************************************************************      00
       0300-ROTINA-RECE.                                                      00
                                                                                
           DISPLAY '0300-ROTINA-RECE'.                                          
                                                                                
           MOVE 'MAPS003'      TO WSS-MAPS                                      
           MOVE 'MAPS003I'     TO WSS-MAPS-OUT                                  
                                                                                
           EXEC CICS RECEIVE MAP(WSS-MAPS)                                    00
                             MAPSET(WSS-MAPS)                                 00
                             INTO(WSS-MAPS-OUT)                               00
                             NOHANDLE                                         00
                     RESP  (RESP1)                                              
                     RESP2 (RESP2)                                              
           END-EXEC                                                             
                                                                                
           IF RESP1 NOT EQUAL DFHRESP(NORMAL)                                   
             DISPLAY ' MSG-ERRO : ' MSG-ERRO                                    
             EXEC CICS SEND FROM(MSG-ERRO) ERASE                                
             END-EXEC                                                           
           END-IF                                                               
                                                                                
           DISPLAY ' LOGINL : ' LOGINL.                                         
                                                                                
      *    EXEC CICS RECEIVE END-EXEC.                                          
                                                                                
      ******************************************************************      00
      * ROTINA PARA CARREGAR MAPA DE ABERTURA                                 00
      ******************************************************************      00
       0500-ABERTURA.                                                         00
                                                                                
           DISPLAY '0500-ABERTURA'                                              
                                                                                
           MOVE 'MAPS003'      TO WSS-MAPS                                      
           MOVE 'MAPS003O'     TO WSS-MAPS-OUT                                  
                                                                                
           EXEC CICS SEND MAP(WSS-MAPS)                                       00
                          MAPSET(WSS-MAPS)                                    00
                          FROM(WSS-MAPS-OUT)                                  00
                     RESP  (RESP1)                                              
                     RESP2 (RESP2)                                              
           END-EXEC.                                                          00
                                                                                
           IF RESP1 NOT EQUAL DFHRESP(NORMAL)                                   
             EXEC CICS SEND FROM(MSG-ERRO) ERASE                                
             END-EXEC                                                           
           END-IF.                                                              
                                                                                
      ******************************************************************      00
      * ROTINA FIM DA PSEUDO-CONVERSACAO                                      00
      ******************************************************************      00
       6000-FIM.                                                              00
                                                                                
           DISPLAY '6000-FIM'.                                                  
                                                                                
           EXEC CICS SEND CONTROL FREEKB                                        
           END-EXEC                                                             
                                                                                
           DISPLAY 'EIBTRNID    : ' EIBTRNID.                                   
           DISPLAY 'CHANNEL-NAME: ' CHANNEL-NAME.                               
                                                                                
           IF CHANNEL-NAME NOT EQUAL 'CHANNKAZ'                                 
             MOVE 'CHANNKAZ'      TO CHANNEL-NAME                               
           END-IF.                                                              
                                                                                
           EXEC CICS RETURN TRANSID(EIBTRNID) CHANNEL(CHANNEL-NAME)           00
           END-EXEC.                                                          00
                                                                                
      ******************************************************************      00
      * ROTINA ENCERRA O PROGRAMA                                             00
      ******************************************************************      00
       9999-ENCERRA.                                                          00
                                                                                
           DISPLAY '9999-ENCERRA'.                                              
                                                                                
           EXEC CICS SEND CONTROL FREEKB ERASE                                  
           END-EXEC                                                             
                                                                                
           EXEC CICS RETURN                                                   00
           END-EXEC.                                                          00
                                                                                
           GOBACK.                                                              
      ********************** FIM DO PROGRAMA ***************************        
