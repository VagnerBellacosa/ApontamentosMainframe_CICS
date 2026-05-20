      ******************************************************************        
      * PROGRAMA    : CICP0013                                                  
      * CRIAÇÃO     : 02/03/2026                                                
      * PROGRAMADOR : VAGNER R BELLACOSA                                        
      * SISTEMA     : CIC - CICS                                                
      * TIPO OBJETO : PROGRAMA COBOL/CICS/BMS                                   
      * AMBIENTE    : ONLINE                                                    
      * FINALIDADE  : EXIBE TELA DE ABERTURA, TELA DE MENU E                    
      *               CARREGA APLICATIVO DE USUARIO                             
      ******************************************************************        
      *                                                                         
      *------------------------                                               00
       IDENTIFICATION DIVISION.                                               00
      *------------------------                                               00
       PROGRAM-ID. CICP0013.                                                  00
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
                                                                                
      *---------------------------------------                                00
      * AREA DE VARIAVEIS ARQUIVO VSAM ALUNOS.                                00
      *---------------------------------------                                00
       COPY CPYALUNO.                                                           
                                                                                
      *----                                                                   00
       01  MSG-ERRO.                                                          00
           03  FILLER                PIC  X(005) VALUE 'CMD= '.               00
           03  COMANDO               PIC  X(008) VALUE SPACES.                00
           03  FILLER                PIC  X(008) VALUE ', RESP= '.            00
           03  RESP1                 PIC  9(005).                             00
           03  FILLER                PIC  X(009) VALUE ', RESP2= '.           00
           03  RESP2                 PIC  9(005).                             00
      *----                                                                   00
       01  MAP-MENU                  PIC  X(008) VALUE 'MAPS004 '.            00
       01  MAPA                      PIC  X(008) VALUE 'MAPS004 '.            00
       01  WSS-ABERTURA              PIC  X(001) VALUE 'S'.                   00
       01  WSS-MAPS-OUT              PIC  X(008) VALUE SPACES.                00
       01  WSS-MAPS                  PIC  X(008) VALUE SPACES.                00
       01  WSS-SUBPROG               PIC  X(008) VALUE SPACES.                00
       01  ARQ-VSAM                  PIC  X(008) VALUE 'IOCALCME'.            00
       01  CONT-CHAVE                PIC  X(016) VALUE 'CONTCHAVE'.           00
       01  CONT-NAME                 PIC  X(016) VALUE 'CONTKAZ'.             00
       01  CHANNEL-NAME              PIC  X(016) VALUE 'CHANNKAZ'.            00
      *                                                                         
      *---- AREAS DO MAPA CICS BMS TELA DE ABERTURA                           00
      *   COPY MAPS003.                                                       00
      *                                                                         
      *---- AREAS DO MAPA CICS BMS MENU GERAL                                 00
          COPY MAPS004.                                                       00
      *                                                                         
      *---- AREAS DE RETORNO E CONTROLE DO CICS                               00
          COPY DFHAID.                                                        00
                                                                                
      *-------------------                                                    00
       PROCEDURE DIVISION.                                                    00
      *-------------------                                                    00
                                                                                
           DISPLAY '********************************************'               
           DISPLAY '*                                          *'               
           DISPLAY '* PROGRAMA CICP0013                        *'               
           DISPLAY '* EXIBE O MENU DE SELECAO E CARREGA PGM    *'               
           DISPLAY '* EXEMPLO DE TELA ABERTURA E TELA MENU     *'               
           DISPLAY '*                                          *'               
           DISPLAY '********************************************'.              
                                                                                
      ******************************************************************      00
      * ROTINA VERIFICA SE É A PRIMEIRA VEZ QUE O PROGRAMA EXECUTA            00
      ******************************************************************      00
           EXEC CICS GET CONTAINER(CONT-NAME)                                 00
                               CHANNEL(CHANNEL-NAME)                          00
                               INTO(MAP-MENU)                                 00
                               FLENGTH(LENGTH OF MAP-MENU)                    00
                               RESP(RESP1)                                    00
                               RESP2(RESP2)                                   00
           END-EXEC.                                                          00
      *----                                                                     
           DISPLAY ' CONT-NAME    : ' CONT-NAME                                 
                   ' CHANNEL-NAME : ' CHANNEL-NAME                              
                   ' MAPA         : ' MAP-MENU                                  
                   ' RESP1        : ' RESP1                                     
      *----                                                                     
           IF RESP1 NOT EQUAL DFHRESP(NORMAL)                                 00
              PERFORM  0000-PRIMEIRA-VEZ                                      00
           ELSE                                                               00
              MOVE 'N' TO WSS-ABERTURA                                          
              PERFORM  0100-SEGUNDA-VEZ UNTIL DFHENTER  = DFHPF3              00
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
                               FROM(MAP-MENU)                                 00
                               FLENGTH(LENGTH OF MAP-MENU)                    00
                               RESP(RESP1)                                    00
                               RESP2(RESP2)                                   00
           END-EXEC.                                                          00
      *----                                                                   00
           DISPLAY ' CONT-NAME    : ' CONT-NAME                                 
                   ' CHANNEL-NAME : ' CHANNEL-NAME                              
                   ' MAPA         : ' MAP-MENU                                  
                   ' RESP1        : ' RESP1                                     
                                                                                
           IF RESP1 NOT EQUAL DFHRESP(NORMAL)                                   
              MOVE 'PUT     ' TO COMANDO                                      00
              EXEC CICS SEND FROM(MSG-ERRO) ERASE                               
              END-EXEC                                                          
           END-IF.                                                              
      *----                                                                   00
           PERFORM 0001-LIMPA-CAMPOS.                                         00
      *----                                                                   00
           PERFORM 0500-ABERTURA.                                             00
      *----                                                                   00
      *    PERFORM 6000-WAIT-USER-ACTION.                                     00
      *----                                                                   00
      ******************************************************************      00
      * ROTINA EXECUTADA SE O PROGRAMA É CARREGADO PELA SEGUNDA VEZ           00
      ******************************************************************      00
       0100-SEGUNDA-VEZ.                                                      00
                                                                                
           DISPLAY '0100-SEGUNDA-VEZ'.                                          
                                                                                
           PERFORM  0300-ROTINA-RECE                                          00
      *----                                                                   00
           EVALUATE EIBAID                                                    00
               WHEN  DFHENTER                                                 00
                     DISPLAY ' ENTER '                                          
                     PERFORM  4100-RETORNA-MENU                                 
               WHEN  DFHPF4                                                   00
                     PERFORM  2000-CADASTRO-ALUNO                             00
               WHEN  DFHPF3                                                   00
                     PERFORM  9999-ENCERRA                                    00
               WHEN  DFHPF5                                                   00
                     PERFORM  3000-INSERE-NOTA                                00
               WHEN  DFHPF6                                                   00
                     PERFORM  4000-CALCULO-MEDIA                              00
               WHEN  DFHPF8                                                   00
                     PERFORM  5000-RELATORIO                                  00
               WHEN  OTHER                                                    00
                     DISPLAY ' TECLA INVALIDA '                                 
                     MOVE 'TECLA INVÁLIDA.' TO MSGERROO                       00
                     PERFORM 0200-ROTINA-SEND                                 00
                     PERFORM 6000-WAIT-USER-ACTION                            00
           END-EVALUATE.                                                      00
                                                                                
      ******************************************************************      00
      * ROTINA PARA LIMPAR OS CAMPOS DA TELA                                  00
      ******************************************************************      00
       0001-LIMPA-CAMPOS.                                                     00
                                                                                
           DISPLAY '0001-LIMPA-CAMPOS'.                                         
                                                                                
           MOVE 'N'              TO WSS-ABERTURA.                               
                                                                                
           EXEC CICS ASSIGN USERID(USERIDO)                                     
           END-EXEC                                                             
                                                                                
           EXEC CICS ASSIGN APPLID(CICSO)                                       
           END-EXEC                                                             
                                                                                
      *----                                                                   00
           EXEC CICS ASKTIME ABSTIME (W-ABS)                                    
           END-EXEC                                                             
      *----                                                                   00
           EXEC CICS FORMATTIME ABSTIME (W-ABS)                               00
                                DATESEP                                       00
                                DDMMYYYY (DATAO)                              00
                                TIME (HORAO)                                  00
                                TIMESEP                                       00
           END-EXEC.                                                          00
                                                                                
      ******************************************************************      00
      * ROTINA SEND MAP                                                       00
      ******************************************************************      00
       0200-ROTINA-SEND.                                                      00
                                                                                
           DISPLAY '0200-ROTINA-SEND'.                                          
                                                                                
           DISPLAY 'LIMPA MAPA'                                                 
                                                                                
      *    EXEC CICS SEND CONTROL FREEKB ERASE                                  
      *    END-EXEC.                                                            
                                                                                
           MOVE MAP-MENU   TO WSS-MAPS                                          
                                                                                
           DISPLAY ' WSS-MAPS     : ' WSS-MAPS                                  
           DISPLAY ' WSS-MAPS-OUT : ' WSS-MAPS-OUT                              
           DISPLAY ' MSG-ERRO     : ' MSG-ERRO                                  
           DISPLAY ' MSGERROO     : ' MSGERROO                                  
           DISPLAY ' MAPS004I     : ' MAPS004I                                  
           DISPLAY ' MAPS004O     : ' MAPS004O                                  
                                                                                
           EXEC CICS SEND MAP(WSS-MAPS)                                       00
                          MAPSET(WSS-MAPS)                                    00
                          FROM(MAPS004O)                                      00
                          NOHANDLE                                            00
           END-EXEC.                                                          00
      *                   ERASE                                                 
                                                                                
      ******************************************************************      00
                                                                                
      ******************************************************************      00
      * ROTINA RECEIVE MAP                                                    00
      ******************************************************************      00
       0300-ROTINA-RECE.                                                      00
                                                                                
           DISPLAY '0300-ROTINA-RECE'.                                          
                                                                                
           MOVE 'MAPS004'      TO WSS-MAPS                                      
           MOVE 'MAPS004I'     TO WSS-MAPS-OUT                                  
                                                                                
           EXEC CICS RECEIVE MAP(WSS-MAPS)                                    00
                             MAPSET(WSS-MAPS)                                 00
                             INTO(WSS-MAPS-OUT)                               00
                             NOHANDLE                                         00
           END-EXEC.                                                          00
                                                                                
      ******************************************************************      00
      * ROTINA PARA CARREGAR MAPA DE ABERTURA                                 00
      ******************************************************************      00
       0500-ABERTURA.                                                         00
                                                                                
           DISPLAY '0500-ABERTURA'                                              
                                                                                
           MOVE MAP-MENU       TO WSS-MAPS                                      
                                                                                
           DISPLAY ' WSS-MAPS : ' WSS-MAPS                                      
                                                                                
           EXEC CICS SEND MAP(WSS-MAPS)                                       00
                          MAPSET(WSS-MAPS)                                    00
                          FROM(WSS-MAPS-OUT)                                  00
                          ERASE                                                 
                     RESP  (RESP1)                                              
                     RESP2 (RESP2)                                              
           END-EXEC.                                                          00
                                                                                
           IF RESP1 NOT EQUAL DFHRESP(NORMAL)                                   
             DISPLAY 'MSG-ERRO : ' MSG-ERRO                                     
             EXEC CICS SEND FROM(MSG-ERRO) ERASE                                
             END-EXEC                                                           
           END-IF.                                                              
                                                                                
      ******************************************************************      00
      * ROTINA PARA CARREGAR MAPA DE MENU                                     00
      ******************************************************************      00
       1000-EXIBE-MENU.                                                       00
                                                                                
           DISPLAY '1000-EXIBE-MENU'                                            
                                                                                
           MOVE MAP-MENU       TO WSS-MAPS                                      
           MOVE 'MAPS004O'     TO WSS-MAPS-OUT                                  
                                                                                
           PERFORM 0200-ROTINA-SEND.                                          00
                                                                                
      ******************************************************************      00
      * ROTINA CADASTRO DE ALUNO                                              00
      ******************************************************************      00
       2000-CADASTRO-ALUNO.                                                     
                                                                                
           DISPLAY '2000-CADASTRO-ALUNO'.                                       
                                                                                
      *    DISPLAY ' CAD ALUNO INVALIDO'                                        
      *    MOVE 'CAD ALUNO NAO CRIADO' TO MSGERROO                            00
                                                                                
           EXEC CICS LINK                                                       
                     PROGRAM('CICP0014')                                        
                     RESP  (RESP1)                                              
                     RESP2 (RESP2)                                              
           END-EXEC.                                                            
                                                                                
           IF RESP1 NOT EQUAL DFHRESP(NORMAL)                                   
             MOVE 'ERRO NA CHAMADA CICP0014' TO MSG-ERRO                        
             DISPLAY 'MSG-ERRO : ' MSG-ERRO                                     
             EXEC CICS SEND FROM(MSG-ERRO) ERASE                                
             END-EXEC                                                           
           END-IF.                                                              
                                                                                
           PERFORM 0200-ROTINA-SEND                                           00
           PERFORM 6000-WAIT-USER-ACTION.                                     00
                                                                                
      ******************************************************************      00
      * ROTINA INSERE NOTA                                                    00
      ******************************************************************      00
       3000-INSERE-NOTA.                                                        
                                                                                
           DISPLAY '3000-INSERE-NOTA'.                                          
                                                                                
           DISPLAY ' INS NOTA  INVALIDO'                                        
           MOVE 'INS NOTA NAO CRIADO' TO MSGERROO                             00
           PERFORM 0200-ROTINA-SEND                                           00
           PERFORM 6000-WAIT-USER-ACTION.                                     00
                                                                                
                                                                                
      ******************************************************************      00
      * ROTINA CALCULO DE MEDIA                                               00
      ******************************************************************      00
       4000-CALCULO-MEDIA.                                                      
                                                                                
           DISPLAY '4000-CALCULO-MEDIA'.                                        
                                                                                
           MOVE 'COBCXX95'    TO WSS-SUBPROG.                                   
                                                                                
           EXEC CICS LINK                                                       
                     PROGRAM(WSS-SUBPROG)                                       
                     RESP  (RESP1)                                              
                     RESP2 (RESP2)                                              
           END-EXEC.                                                            
                                                                                
           IF RESP1 NOT EQUAL DFHRESP(NORMAL)                                   
             DISPLAY 'MSG-ERRO : ' MSG-ERRO                                     
             EXEC CICS SEND FROM(MSG-ERRO) ERASE                                
             END-EXEC                                                           
           ELSE                                                                 
             PERFORM 9999-ENCERRA                                               
           END-IF.                                                              
                                                                                
      ******************************************************************      00
      * ROTINA VOLTA PARA O MENU                                              00
      ******************************************************************      00
       4100-RETORNA-MENU.                                                       
                                                                                
           DISPLAY '4100-RETORNA-MENU'.                                         
                                                                                
           MOVE 'CICP0013'    TO WSS-SUBPROG.                                   
                                                                                
           EXEC CICS XCTL                                                       
                     PROGRAM(WSS-SUBPROG)                                       
                     RESP  (RESP1)                                              
                     RESP2 (RESP2)                                              
           END-EXEC.                                                            
                                                                                
           IF RESP1 NOT EQUAL DFHRESP(NORMAL)                                   
             DISPLAY 'MSG-ERRO : ' MSG-ERRO                                     
             EXEC CICS SEND FROM(MSG-ERRO) ERASE                                
             END-EXEC                                                           
           ELSE                                                                 
             PERFORM 9999-ENCERRA                                               
           END-IF.                                                              
                                                                                
      ******************************************************************      00
      * ROTINA IMPRIME RELATORIO DE ALUNO                                     00
      ******************************************************************      00
       5000-RELATORIO.                                                          
                                                                                
           DISPLAY '5000-RELATORIO'.                                            
                                                                                
                                                                                
           DISPLAY ' RELATORIO INVALIDO'                                        
           MOVE 'RELATORIO NAO CRIADO' TO MSGERROO                            00
           PERFORM 0200-ROTINA-SEND                                           00
           PERFORM 6000-WAIT-USER-ACTION.                                     00
                                                                                
                                                                                
      ******************************************************************      00
      * ROTINA FIM DA PSEUDO-CONVERSACAO                                      00
      ******************************************************************      00
       6000-WAIT-USER-ACTION.                                                 00
                                                                                
           DISPLAY '6000-WAIT-USER-ACTION'.                                     
                                                                                
           EXEC CICS SEND CONTROL FREEKB                                        
           END-EXEC                                                             
                                                                                
           IF EIBTRNID NOT EQUAL 'CADW'                                         
             MOVE 'ME04'   TO  EIBTRNID                                         
           END-IF.                                                              
                                                                                
           MOVE 'CHANNKAZ'            TO CHANNEL-NAME                           
                                                                                
           DISPLAY ' EIBTRNID : ' EIBTRNID                                      
           DISPLAY ' CHANNEL-NAME : ' CHANNEL-NAME                              
                                                                                
                                                                                
           EXEC CICS RETURN TRANSID(EIBTRNID) CHANNEL(CHANNEL-NAME)           00
           END-EXEC.                                                          00
                                                                                
      ******TO *********************************************************      00
      * ROTINA ENCERRA O PROGRAMA                                             00
      ******************************************************************      00
       9999-ENCERRA.                                                          00
                                                                                
           DISPLAY '9999-ENCERRA'.                                              
                                                                                
           EXEC CICS SEND CONTROL FREEKB ERASE                                  
           END-EXEC                                                             
                                                                                
           EXEC CICS RETURN                                                   00
           END-EXEC.                                                          00
      ********************** FIM DO PROGRAMA ***************************        
