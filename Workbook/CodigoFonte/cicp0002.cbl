      ******************************************************************        
      * PROGRAMA    : CICP0002                                                  
      * CRIAÇÃO     : 11/02/2026                                                
      * PROGRAMADOR : VAGNER R BELLACOSA                                        
      * SISTEMA     : CIC - CICS                                                
      * TIPO OBJETO : PROGRAMA COBOL/CICS                                       
      * AMBIENTE    : ONLINE                                                    
      * TRANSACAO   : OLHA                                                      
      * FINALIDADE  : PROGRAMA CHAMADO VIA LINK FLUXO DE COMUNICACAO            
      ******************************************************************        
      *                                                                         
      *------------------------                                               00
       IDENTIFICATION DIVISION.                                               00
      *------------------------                                               00
       PROGRAM-ID. CICP0002.                                                  00
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
       77  LNK-D                     PIC X(50)                                  
                            VALUE 'ESTE EH O SUB-PROGRAMA'.                     
       77  LNK-E                     PIC X(40)                                  
                            VALUE 'O CONTROLE EH DELE E VOLTA'.                 
       77  LNK-C                     PIC X(40)                                  
                            VALUE 'OBRIGADO E ATE MAIS'.                        
                                                                                
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
       01  WSS-SUBPROG               PIC  X(008) VALUE 'COZSOLHA'.            00
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
           DISPLAY '* PROGRAMA CICP0002                        *'               
           DISPLAY '* CHAMADO PELO PROGRAMA OLHA VIA LINK      *'               
           DISPLAY '*                                          *'               
           DISPLAY '********************************************'.              
                                                                                
           DISPLAY 'LA LA LA LA LA LA LA    '.                                  
           DISPLAY 'MANDO MENSAGEM PARA TELA'.                                  
           DISPLAY 'LNK-D :  ' LNK-D.                                           
                                                                                
           EXEC CICS SEND FROM(LNK-D)                                           
           END-EXEC.                                                            
                                                                                
           DISPLAY 'RECEBO A TELA'.                                             
                                                                                
           EXEC CICS RECEIVE END-EXEC.                                          
                                                                                
           EXEC CICS SEND CONTROL CURSOR(1140)                                  
           END-EXEC                                                             
                                                                                
           DISPLAY 'MANDO MENSAGEM PARA TELA'.                                  
           DISPLAY 'LNK-E :  ' LNK-E.                                           
                                                                                
           EXEC CICS SEND FROM(LNK-E) ERASE                                     
           END-EXEC.                                                            
                                                                                
           DISPLAY 'RECEBO A TELA'.                                             
                                                                                
           EXEC CICS RECEIVE                                                    
           END-EXEC.                                                            
                                                                                
           EXEC CICS SEND CONTROL CURSOR(1140)                                  
           END-EXEC                                                             
                                                                                
           DISPLAY 'MANDO MENSAGEM PARA TELA'.                                  
           DISPLAY 'LNK-C :  ' LNK-C.                                           
                                                                                
           EXEC CICS SEND FROM(LNK-C)                                           
           END-EXEC.                                                            
                                                                                
           DISPLAY 'RECEBO A TELA'.                                             
                                                                                
           EXEC CICS RECEIVE                                                    
           END-EXEC.                                                            
                                                                                
           DISPLAY ' VOLTANDO PARA CASA (OLHA)'.                                
                                                                                
      * DEVOLVE O PONTEIRO DA EXECUCAO AO PGM CHAMADOR                          
           GOBACK.                                                              
                                                                                
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
                                                                                
           DISPLAY '2000-ENCERRA ZOLHA'.                                        
                                                                                
           EXEC CICS SEND CONTROL FREEKB ERASE                                  
           END-EXEC                                                             
                                                                                
           EXEC CICS RETURN                                                   00
           END-EXEC.                                                          00
      ********************** FIM DO PROGRAMA ***************************        
