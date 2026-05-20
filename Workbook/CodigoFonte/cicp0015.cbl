      ******************************************************************        
      * PROGRAMA    : CICP0015                                                  
      * CRIAÇÃO     : 04/05/2026                                                
      * PROGRAMADOR : VAGNER R BELLACOSA                                        
      * SISTEMA     : CIC - CICS                                                
      * TIPO OBJETO : PROGRAMA COBOL/CICS/BMS                                   
      * AMBIENTE    : ONLINE                                                    
      * FINALIDADE  : PROGRAMA EXEMPLO USO DE COMMAREA ENVIADA ENTRE            
      *               PROGRAMA CICP0015 E RECEBIDA NO CICP0016 VIA              
      *               TEXTO                                                     
      ******************************************************************        
      *                                                                         
      *------------------------                                               00
       IDENTIFICATION DIVISION.                                               00
      *------------------------                                               00
       PROGRAM-ID. CICP0015.                                                  00
                                                                                
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
       77  LNK-TID                   PIC X(4).                                  
       77  LNK-MSG                   PIC X(50)                                  
                            VALUE 'ATENCAO LINKANDO PROGRAMA'.                  
       77  LNK-A                     PIC X(40)                                  
                            VALUE 'ME05 EH O PROGRAMA PRINCIPAL'.               
       77  LNK-B                     PIC X(40)                                  
                            VALUE 'AGORA MOVEREI O CONTROLE PARA O SUB'.        
                                                                                
      *----                                                                   00
       01  MSG-ERRO.                                                          00
           03  FILLER                PIC  X(005) VALUE 'CMD= '.               00
           03  COMANDO               PIC  X(008) VALUE SPACES.                00
           03  FILLER                PIC  X(008) VALUE ', RESP= '.            00
           03  RESP1                 PIC  9(005).                             00
           03  FILLER                PIC  X(009) VALUE ', RESP2= '.           00
           03  RESP2                 PIC  9(005).                             00
                                                                                
      *----                                                                   00
       01  MAPA                      PIC  X(008) VALUE 'MAPS007 '.            00
       01  ARQ-VSAM                  PIC  X(008) VALUE 'IOCALCME'.            00
       01  WSS-SUBPROG               PIC  X(008) VALUE 'CICP0016'.            00
       01  CONT-CHAVE                PIC  X(016) VALUE 'CONTCHAVE'.           00
       01  CONT-NAME                 PIC  X(016) VALUE 'CONTKAZ'.             00
       01  CHANNEL-NAME              PIC  X(016) VALUE 'CHANNKAZ'.            00
      *                                                                         
       01  LKG-COMMAREA.                                                        
         05 WSS-AREA-COMUNICACAO     PIC  X(050) VALUE                          
         'AREA DE MEMORIA ENVIADA VIA LINK'.                                    
      *                                                                         
      *---- AREAS DE RETORNO E CONTROLE DO CICS                               00
          COPY DFHAID.                                                        00
                                                                                
      *-------------------                                                    00
       PROCEDURE DIVISION.                                                    00
      *-------------------                                                    00
                                                                                
           DISPLAY '********************************************'               
           DISPLAY '*                                          *'               
           DISPLAY '* PROGRAMA CICP0015                        *'               
           DISPLAY '* CHAMA PROGRAMA CICP0016 VIA LINK         *'               
           DISPLAY '* VAI E VOLTA                              *'               
           DISPLAY '* ENVIO DE COMMAREA PARA SUB-PROGRAMA      *'               
           DISPLAY '*                                          *'               
           DISPLAY '********************************************'.              
                                                                                
           DISPLAY ' RECEBO TELA '.                                             
                                                                                
           EXEC CICS RECEIVE INTO (LNK-TID)                                     
                     LENGTH(LENGTH OF LNK-TID)                                  
                        RESP      (RESP1)                                       
                        RESP2     (RESP2)                                       
           END-EXEC.                                                            
                                                                                
           IF RESP1 NOT EQUAL DFHRESP(NORMAL)                                   
              DISPLAY 'ERRO RESP1 = ' RESP1                                     
              DISPLAY 'ERRO RESP2 = ' RESP2                                     
              MOVE 'TEXTO 001'  TO LNK-TID                                      
           END-IF.                                                              
                                                                                
           EXEC CICS SEND CONTROL CURSOR(1140)                                  
           END-EXEC                                                             
                                                                                
           DISPLAY ' ME05 - PRIMEIRA ITERACAO: MANDO MENSAGEM TELA'.            
           DISPLAY ' LNK-MSG : ' LNK-MSG.                                       
                                                                                
           EXEC CICS SEND FROM(LNK-MSG)                                         
                    RESP      (RESP1)                                           
                    RESP2     (RESP2)                                           
           END-EXEC.                                                            
      *                                                                         
           IF RESP1 NOT EQUAL DFHRESP(NORMAL)                                   
              DISPLAY 'ERRO RESP1 = ' RESP1                                     
              DISPLAY 'ERRO RESP2 = ' RESP2                                     
              MOVE 'TEXTO 002'  TO LNK-TID                                      
           END-IF.                                                              
                                                                                
           DISPLAY ' ME05 - RETURN DA TELA'.                                    
                                                                                
           EXEC CICS RECEIVE END-EXEC.                                          
                                                                                
           EXEC CICS SEND CONTROL CURSOR(1140)                                  
           END-EXEC                                                             
                                                                                
           DISPLAY ' ME05 - MANDO MENSAGEM PARA A TELA'.                        
           DISPLAY ' LNK-A : ' LNK-A                                            
                                                                                
           EXEC CICS SEND FROM(LNK-A) ERASE                                     
           END-EXEC.                                                            
                                                                                
           DISPLAY ' ME05 - RECEBO A TELA'                                      
                                                                                
           EXEC CICS RECEIVE                                                    
           END-EXEC.                                                            
                                                                                
           EXEC CICS SEND CONTROL CURSOR(1140)                                  
           END-EXEC                                                             
                                                                                
           EXEC CICS SEND FROM(LNK-B)                                           
           END-EXEC.                                                            
                                                                                
           DISPLAY ' ME05 - MANDO MENSAGEM PARA A TELA'.                        
           DISPLAY ' LNK-B : ' LNK-B                                            
                                                                                
           EXEC CICS RECEIVE                                                    
           END-EXEC.                                                            
                                                                                
           DISPLAY 'ME05 - LINK '                                               
           DISPLAY 'PONTEIRO SAI DO PROGRAMA'.                                  
           DISPLAY 'E VOLTARA QUANDO O USUARIO APERTAR '.                       
           DISPLAY 'ENTER E SAIR DO SUBPROGRAMA.'.                              
           DISPLAY ' WSS-SUBPROG = ' WSS-SUBPROG                                
           DISPLAY ' LKG-COMMAREA= ' LKG-COMMAREA                               
      *                                                                         
           EXEC CICS LINK PROGRAM   (WSS-SUBPROG)                               
                          RESP      (RESP1)                                     
                          RESP2     (RESP2)                                     
                          COMMAREA  (LKG-COMMAREA)                              
                          LENGTH    (50)                                        
           END-EXEC.                                                            
                                                                                
           IF RESP1 = DFHRESP(NORMAL)                                           
              DISPLAY ' DEU TUDO CERTO '                                        
              PERFORM 2000-ENCERRA                                              
           ELSE                                                                 
              DISPLAY 'ERRO RESP1 = ' RESP1                                     
              DISPLAY 'ERRO RESP2 = ' RESP2                                     
              DISPLAY ' DEU RUIM ERRO NA CHAMADA'                               
      *   *     PERFORM ERROR-HANDLING                                          
           END-IF.                                                              
                                                                                
           IF RESP1 NOT EQUAL DFHRESP(NORMAL)                                   
             MOVE 'ERRO NO FLUXO ME05' TO MSG-ERRO                              
             EXEC CICS SEND FROM(MSG-ERRO) ERASE                                
             END-EXEC                                                           
             PERFORM 2000-ENCERRA                                               
           END-IF.                                                              
                                                                                
                                                                                
      ******************************************************************      00
      * ROTINA ENCERRA O PROGRAMA                                             00
      ******************************************************************      00
       2000-ENCERRA.                                                          00
                                                                                
           DISPLAY '2000-ENCERRA'.                                              
                                                                                
           DISPLAY 'ME05 - SAINDO'.                                             
                                                                                
           EXEC CICS SEND CONTROL FREEKB ERASE                                  
           END-EXEC                                                             
                                                                                
           EXEC CICS RETURN                                                   00
           END-EXEC.                                                          00
                                                                                
           GOBACK.                                                              
      ********************** FIM DO PROGRAMA ***************************        
