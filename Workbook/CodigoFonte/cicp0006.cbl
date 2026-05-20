      ******************************************************************        
      * PROGRAMA    : CICP0006                                                  
      * CRIAÇÃO     : 18/02/2026                                                
      * PROGRAMADOR : VAGNER R BELLACOSA                                        
      * SISTEMA     : CIC - CICS                                                
      * TIPO OBJETO : PROGRAMA COBOL/CICS/BMS                                   
      * AMBIENTE    : ONLINE                                                    
      * FINALIDADE  : PROGRAMA CHAMADOR VIA LINK FLUXO DE COMUNICACAO           
      ******************************************************************        
      *                                                                         
      *------------------------                                               00
       IDENTIFICATION DIVISION.                                               00
      *------------------------                                               00
       PROGRAM-ID. CICP0006.                                                  00
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
                            VALUE 'XX94 EH O PROGRAMA PRINCIPAL'.               
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
       01  WSS-SUBPROG               PIC  X(008) VALUE 'CICP0007'.            00
       01  CONT-CHAVE                PIC  X(016) VALUE 'CONTCHAVE'.           00
       01  CONT-NAME                 PIC  X(016) VALUE 'CONTKAZ'.             00
       01  CHANNEL-NAME              PIC  X(016) VALUE 'CHANNKAZ'.            00
      *                                                                         
      *                                                                         
      *---- AREAS DE RETORNO E CONTROLE DO CICS                               00
          COPY DFHAID.                                                        00
                                                                                
      *-------------------                                                    00
       PROCEDURE DIVISION.                                                    00
      *-------------------                                                    00
                                                                                
           DISPLAY '********************************************'               
           DISPLAY '* PROGRAMA CICP0006                        *'               
           DISPLAY '* CHAMA OUTRO PROGRAMA VIA LINK            *'               
           DISPLAY '* E RETORNA AO PROGRAMA PRINCIPAL          *'               
           DISPLAY '*                                          *'               
           DISPLAY '********************************************'.              
                                                                                
           EXEC CICS RECEIVE INTO (LNK-TID)                                     
                     LENGTH(LENGTH OF LNK-TID)                                  
           END-EXEC.                                                            
                                                                                
           EXEC CICS SEND CONTROL CURSOR(1140)                                  
           END-EXEC                                                             
                                                                                
           MOVE 'LINK - CARREGANDO PROXIMO PASSO' TO LNK-MSG.                   
                                                                                
           EXEC CICS SEND FROM(LNK-MSG)                                         
           END-EXEC.                                                            
                                                                                
           EXEC CICS RECEIVE END-EXEC.                                          
                                                                                
           EXEC CICS SEND CONTROL CURSOR(1140)                                  
           END-EXEC                                                             
                                                                                
           MOVE 'LINK - SEGUNDO PASSO CARREGADO ' TO LNK-MSG.                   
                                                                                
           EXEC CICS SEND FROM(LNK-MSG)                                         
           END-EXEC.                                                            
                                                                                
           EXEC CICS RECEIVE END-EXEC.                                          
                                                                                
           EXEC CICS SEND CONTROL CURSOR(1140)                                  
           END-EXEC                                                             
                                                                                
                                                                                
           MOVE 'LINK - SAINDO DO CICP0006'  TO LNK-A.                          
                                                                                
           EXEC CICS SEND FROM(LNK-A) ERASE                                     
           END-EXEC.                                                            
                                                                                
           EXEC CICS RECEIVE                                                    
           END-EXEC.                                                            
                                                                                
           EXEC CICS SEND CONTROL CURSOR(1140)                                  
           END-EXEC                                                             
                                                                                
           DISPLAY ' LINK '                                                     
                                                                                
           MOVE 'CARREGANDO O PROXIMO PGM' TO MSG-ERRO.                         
                                                                                
           EXEC CICS SEND FROM(MSG-ERRO) ERASE                                  
           END-EXEC                                                             
                                                                                
           EXEC CICS LINK                                                       
                     PROGRAM(WSS-SUBPROG)                                       
                     RESP  (RESP1)                                              
                     RESP2 (RESP2)                                              
           END-EXEC.                                                            
                                                                                
           IF RESP1 NOT EQUAL ZEROES                                            
             DISPLAY 'ABENDOUUUUUU'                                             
           END-IF                                                               
                                                                                
           MOVE 'DANCANDO LAMBADA        ' TO MSG-ERRO.                         
                                                                                
           DISPLAY ' DANCANDO LAMBADA'                                          
                                                                                
           EXEC CICS SEND FROM(MSG-ERRO) ERASE                                  
           END-EXEC                                                             
                                                                                
           EXEC CICS RECEIVE                                                    
           END-EXEC.                                                            
                                                                                
           MOVE 'DANCANDO MACARENA       ' TO MSG-ERRO.                         
                                                                                
           EXEC CICS SEND FROM(MSG-ERRO) ERASE                                  
           END-EXEC                                                             
                                                                                
           EXEC CICS RECEIVE                                                    
           END-EXEC.                                                            
                                                                                
           EXEC CICS SEND CONTROL CURSOR(1140)                                  
           END-EXEC                                                             
                                                                                
           PERFORM 1000-FIM.                                                    
                                                                                
      ******************************************************************      00
      * ROTINA FIM DA PSEUDO-CONVERSACAO                                      00
      ******************************************************************      00
       1000-FIM.                                                              00
                                                                                
           DISPLAY '1000-FIM'.                                                  
                                                                                
           EXEC CICS SEND CONTROL FREEKB                                        
           END-EXEC                                                             
                                                                                
           EXEC CICS RETURN TRANSID(EIBTRNID) CHANNEL(CHANNEL-NAME)           00
           END-EXEC.                                                          00
                                                                                
      ******************************************************************      00
      * ROTINA ENCERRA O PROGRAMA                                             00
      ******************************************************************      00
       2000-ENCERRA.                                                          00
                                                                                
           DISPLAY '2000-ENCERRA'.                                              
                                                                                
           DISPLAY ' XX94 - SAINDO'.                                            
                                                                                
           EXEC CICS SEND CONTROL FREEKB ERASE                                  
           END-EXEC                                                             
                                                                                
           EXEC CICS RETURN                                                   00
           END-EXEC.                                                          00
      ********************** FIM DO PROGRAMA ***************************        
