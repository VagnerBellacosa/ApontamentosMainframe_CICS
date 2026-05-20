      ******************************************************************        
      * PROGRAMA    : CICP0003                                                  
      * CRIAÇÃO     : 11/02/2026                                                
      * PROGRAMADOR : VAGNER R BELLACOSA                                        
      * SISTEMA     : CIC - CICS                                                
      * TIPO OBJETO : PROGRAMA COBOL/CICS                                       
      * AMBIENTE    : ONLINE                                                    
      * TRANSACAO   : LE01                                                      
      * FINALIDADE  : LÊ UM REGISTRO, PASSADO PELO TERMINAL,                    
      *               FAZ DISPLAY NA TELA                                       
      ******************************************************************        
      *                                                                         
      *------------------------                                         00010000
       IDENTIFICATION DIVISION.                                         00020000
      *------------------------                                         00030000
      *                                                                         
       PROGRAM-ID. CICP0003.                                            00040000
      *---------------------                                                    
      *                                                                         
      *---------------------                                                    
       ENVIRONMENT DIVISION.                                            00070000
      *---------------------                                                    
      *                                                                         
      *--------------                                                           
       DATA DIVISION.                                                   00080000
      *--------------                                                           
      *                                                                         
      *------------------------                                                 
       WORKING-STORAGE SECTION.                                         00090000
      *------------------------                                                 
      *                                                                         
       77  CTE-INICIO              PIC  X(15) VALUE 'WSS COMECA AQUI'.          
       77  CTE-VERSAO              PIC  X(06) VALUE 'VRS001'.                   
       77  MEDIA-ARIT              PIC  9(04)V99 VALUE 0.                       
       77  WSS-MEDIA-OLD           PIC  9(04)V99 VALUE 0.                       
      *----                                                                     
       01  MATRIC.                                                              
           03  FILLER              PIC  X(05).                                  
           03  MAT                 PIC  9(05).                                  
      *----                                                                     
       01  MSG-AVISO               PIC  X(40).                          00100000
      *----                                                                     
       01  MSG-ERRO.                                                    00100000
           03  FILLER              PIC  X(05) VALUE 'CMD= '.            00110000
           03  COMANDO             PIC  X(08) VALUE SPACES.             00120000
           03  FILLER              PIC  X(08) VALUE ', RESP= '.         00130000
           03  RESP                PIC  9(05).                          00140000
           03  FILLER              PIC  X(09) VALUE ', RESP2= '.        00150000
           03  RESP2               PIC  9(05).                          00160000
      *----                                                             00170000
       01  COMP-REG                PIC S9(04) COMP.                     00220000
      *                                                                         
      *-------------------                                              00230000
       PROCEDURE DIVISION.                                              00240000
      *-------------------                                              00250000
      *                                                                         
                                                                                
           DISPLAY ' '.                                                         
           DISPLAY '**************************************************'         
           DISPLAY '***      LE01 - CICP0003                       ***'         
           DISPLAY '**************************************************'         
           DISPLAY ' '.                                                         
                                                                                
                                                                                
           EXEC CICS RECEIVE    INTO     (MATRIC)                               
                                LENGTH   (LENGTH OF MATRIC)                     
           END-EXEC.                                                            
                                                                                
           EVALUATE TRUE                                                        
             WHEN MAT IS NOT NUMERIC                                            
               DISPLAY '  MATRICULA NAO NUMERICA'                               
               MOVE ZEROES                      TO WSS-MEDIA-OLD                
               MOVE '   MATRICULA NAO NUMERICA'    TO MSG-AVISO                 
               PERFORM FIM                                                      
             WHEN MAT EQUAL ZEROES                                              
               DISPLAY '  CAMPO ZERADO          '                               
               MOVE ZEROES                      TO WSS-MEDIA-OLD                
               MOVE '   CAMPO ZERADO          '    TO MSG-AVISO                 
               PERFORM FIM                                                      
             WHEN MAT > 50000                                                   
               MOVE 1 TO WSS-MEDIA-OLD                                          
               MOVE 'NUMERO OK                                '                 
                   TO MSG-AVISO                                                 
             WHEN OTHER                                                         
               MOVE 0 TO WSS-MEDIA-OLD                                          
               MOVE 'NUMERO INVALIDO                          '                 
                   TO MSG-AVISO                                                 
           END-EVALUATE.                                                        
                                                                                
           DISPLAY ' TRANSACAO + NUMERO = ' MATRIC                              
           DISPLAY ' NUMERO = ' MAT.                                            
           DISPLAY ' WSS-MEDIA-OLD = ' WSS-MEDIA-OLD                            
                                                                                
           PERFORM FIM.                                                         
                                                                                
      *----                                                                   00
       TRATAR-ERRO.                                                             
           DISPLAY ' '                                                          
           DISPLAY ' ERRO NA TRANSACAO LE01 - CONTATE ANALISTA RESP.'           
           DISPLAY MSG-ERRO.                                                    
                                                                                
           IF RESP EQUAL 13                                                     
             MOVE 99                 TO WSS-MEDIA-OLD                           
             MOVE 'ALUNO NAO EXISTE' TO MSG-AVISO                               
           END-IF.                                                              
                                                                                
           PERFORM FIM.                                                         
      *----                                                                   00
       FIM.                                                             00560000
                                                                                
           EXEC CICS SEND  FROM   (MSG-AVISO)                                   
           END-EXEC                                                             
      *----                                                                     
           EXEC CICS RECEIVE    INTO     (MATRIC)                               
                                LENGTH   (LENGTH OF MATRIC)                     
           END-EXEC.                                                            
      *----                                                                     
                                                                                
           EXEC CICS SEND CONTROL FREEKB ERASE                                  
           END-EXEC.                                                            
      *----                                                                     
           EXEC CICS RETURN                                             00580000
           END-EXEC.                                                    00590000
                                                                                
           GOBACK.                                                              
      **********************FIM DO PROGRAMA ****************************        
