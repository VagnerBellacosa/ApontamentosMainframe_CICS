      ******************************************************************        
      * PROGRAMA    : CICP0010                                                  
      * CRIAÇÃO     : 23/02/2026                                                
      * PROGRAMADOR : VAGNER R BELLACOSA                                        
      * SISTEMA     : CIC - CICS                                                
      * TIPO OBJETO : PROGRAMA COBOL/CICS/BMS                                   
      * AMBIENTE    : ONLINE                                                    
      * FINALIDADE  : ATUALIZA ARQUIVO ALUNOS E EXIBE NA TELA                   
      *               PGM EXEMPLO CICS COM BMS E VSAM                           
      ******************************************************************        
      *                                                                         
      *------------------------                                               00
       IDENTIFICATION DIVISION.                                               00
      *------------------------                                               00
       PROGRAM-ID. CICP0010.                                                  00
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
       77  MASCARA-DOIDA             PIC  99.                                 00
                                                                                
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
       01  MAPA                      PIC  X(008) VALUE 'MAPS007 '.            00
       01  ARQ-VSAM                  PIC  X(008) VALUE 'IOALUNOV'.            00
       01  CONT-CHAVE                PIC  X(016) VALUE 'CONTCHAVE'.           00
       01  CONT-NAME                 PIC  X(016) VALUE 'CONTKAZ'.             00
       01  CHANNEL-NAME              PIC  X(016) VALUE 'CHANNKAZ'.            00
      *                                                                         
      *---- AREAS DO MAPA CICS BMS                                            00
          COPY MAPS007.                                                       00
      *                                                                         
      *---- AREAS DE RETORNO E CONTROLE DO CICS                               00
          COPY DFHAID.                                                        00
                                                                                
      *-------------------                                                    00
       PROCEDURE DIVISION.                                                    00
      *-------------------                                                    00
                                                                                
       ROT-PRINCIPAL               SECTION.                                     
                                                                                
           DISPLAY '    '                                                       
           DISPLAY ' OI '                                                       
           DISPLAY '    '                                                       
           DISPLAY '********************************************'               
           DISPLAY '* PROGRAMA CICP0010                        *'               
           DISPLAY '* RECEBE MATRICULA DE NOME E PROCURA VSAM *'                
           DISPLAY '* EXEMPLO DE CRUD COM NAVEGACAO EM TELA BMS*'               
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
           IF RESP1 NOT EQUAL DFHRESP(NORMAL)                                 00
              PERFORM  0000-PRIMEIRA-VEZ                                      00
           ELSE                                                               00
              PERFORM  0100-SEGUNDA-VEZ                                       00
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
           PERFORM 0001-LIMPA-CAMPOS.                                         00
      *----                                                                   00
           PERFORM 0200-ROTINA-SEND.                                          00
      *----                                                                   00
           PERFORM 1000-FIM.                                                  00
      *----                                                                   00
       0000-PRIMEIRA-VEZ-EXIT.                                                00
           EXIT.                                                                
                                                                                
      ******************************************************************      00
      * ROTINA EXECUTADA SE O PROGRAMA É CARREGADO PELA SEGUNDA VEZ           00
      ******************************************************************      00
       0100-SEGUNDA-VEZ.                                                      00
                                                                                
           DISPLAY '0100-SEGUNDA-VEZ'.                                          
                                                                                
           PERFORM 0300-ROTINA-RECE                                           00
                                                                                
           DISPLAY ' RETORNO DA TELA EIBAID = ' EIBAID                          
                                                                                
      *----                                                                   00
           EVALUATE EIBAID                                                    00
               WHEN  DFHENTER                                                 00
                     DISPLAY ' APERTEI ENTER'                                   
                     PERFORM 1000-LER-ARQUIVO                                 00
               WHEN  DFHPF4                                                   00
                     DISPLAY ' APERTEI F4'                                      
                     PERFORM 4000-SALVA-ARQUIVO                            00   
                     MOVE 'REGISTRO SALVO.' TO MSGERROO                    00   
                     MOVE MEDIA             TO MASCARA                     00   
                     MOVE MASCARA           TO MEDIAO                      00   
                     MOVE SITUACAO          TO SITUACAOO                   00   
                     PERFORM 0200-ROTINA-SEND                                 00
                     PERFORM  1000-FIM                                        00
               WHEN  DFHPF3                                                   00
                     DISPLAY ' APERTEI F3'                                      
                     PERFORM  2000-ENCERRA                                    00
               WHEN  DFHPF5                                                   00
                     DISPLAY ' APERTEI F5'                                      
                     PERFORM 3000-RECALCULA-ARQUIVO                           00
                     MOVE 'REGISTRO RECALCULADO.' TO MSGERROO              00   
                     PERFORM 0200-ROTINA-SEND                                 00
                     PERFORM  1000-FIM                                        00
               WHEN  DFHPF6                                                   00
                     DISPLAY ' APERTEI F6'                                      
                     PERFORM 0001-LIMPA-CAMPOS                                00
                     PERFORM 0200-ROTINA-SEND                                 00
                     PERFORM  1000-FIM                                        00
               WHEN  DFHPF7                                                   00
                     DISPLAY ' APERTEI F7'                                      
                     PERFORM 4000-SALVA-ARQUIVO                            00   
                     MOVE 'REGISTRO SALVO.' TO MSGERROO                    00   
                     MOVE MEDIA             TO MASCARA                     00   
                     MOVE MASCARA           TO MEDIAO                      00   
                     MOVE SITUACAO          TO SITUACAOO                   00   
                     PERFORM 0200-ROTINA-SEND                                 00
                     PERFORM  1000-FIM                                        00
               WHEN  OTHER                                                    00
                     MOVE 'TECLA INVÁLIDA.' TO MSGERROO                       00
                     PERFORM 0200-ROTINA-SEND                                 00
                     PERFORM  1000-FIM                                        00
           END-EVALUATE.                                                      00
                                                                                
       0100-SEGUNDA-VEZ-EXIT.                                                   
           EXIT.                                                                
                                                                                
      ******************************************************************      00
      * ROTINA PARA LIMPAR OS CAMPOS DA TELA                                  00
      ******************************************************************      00
       ROT-NAVEGACAO               SECTION.                                     
                                                                                
           DISPLAY ' TO DOIDAO ENTREI NA ROT-NAVEGACAO'.                        
                                                                                
           PERFORM  0100-SEGUNDA-VEZ UNTIL EIBAID = DFHPF3.                   00
                                                                                
                                                                                
      ******************************************************************      00
      * ROTINA SEND MAP                                                       00
      ******************************************************************      00
       0200-ROTINA-SEND.                                                      00
                                                                                
           DISPLAY '0200-ROTINA-SEND'.                                          
                                                                                
           EXEC CICS SEND MAP(MAPA)                                           00
                          MAPSET('MAPS007')                                   00
                          FROM(MAPS007O)                                      00
                          NOHANDLE                                            00
           END-EXEC.                                                          00
                                                                                
       0200-ROTINA-SEND-EXIT.                                                 00
           EXIT.                                                                
                                                                                
      ******************************************************************      00
      * ROTINA PARA LIMPAR OS CAMPOS DA TELA                                  00
      ******************************************************************      00
       0001-LIMPA-CAMPOS.                                                     00
                                                                                
           DISPLAY '0001-LIMPA-CAMPOS'.                                         
                                                                                
           EXEC CICS ASSIGN USERID(USERIDO)                                     
           END-EXEC                                                             
                                                                                
           EXEC CICS ASSIGN APPLID(CICSO)                                       
           END-EXEC                                                             
                                                                                
           MOVE SPACES TO MATRICO                                             00
           MOVE SPACES TO ALUNOO                                              00
           MOVE SPACES TO SEXOO                                               00
           MOVE SPACES TO NOTA01O                                             00
           MOVE SPACES TO NOTA02O                                             00
           MOVE SPACES TO NOTA03O                                             00
           MOVE SPACES TO NOTA04O                                             00
           MOVE SPACES TO NOTA05O                                             00
           MOVE SPACES TO MEDIAO                                              00
           MOVE SPACES TO SITUACAOO                                           00
           MOVE SPACES TO MSGERROO                                            00
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
                                                                                
       0001-LIMPA-CAMPOS-EXIT.                                                00
           EXIT.                                                                
                                                                                
      ******************************************************************      00
      * ROTINA RECEIVE MAP                                                    00
      ******************************************************************      00
       0300-ROTINA-RECE.                                                      00
                                                                                
           DISPLAY '0300-ROTINA-RECE'.                                          
                                                                                
           EXEC CICS RECEIVE MAP(MAPA)                                        00
                             MAPSET('MAPS007')                                00
                             INTO(MAPS007I)                                   00
                             NOHANDLE                                         00
           END-EXEC.                                                          00
                                                                                
       0300-ROTINA-RECE-EXIT.                                                 00
           EXIT.                                                                
                                                                                
      ******************************************************************      00
      * ROTINA FIM DA PSEUDO-CONVERSACAO                                      00
      ******************************************************************      00
       1000-FIM.                                                              00
                                                                                
           DISPLAY '1000-FIM'.                                                  
                                                                                
           EXEC CICS SEND CONTROL FREEKB                                        
           END-EXEC                                                             
                                                                                
           MOVE 'DEF1'     TO EIBTRNID                                          
                                                                                
           DISPLAY ' EIBTRNID     = ' EIBTRNID                                  
           DISPLAY ' CHANNEL-NAME = ' CHANNEL-NAME.                             
                                                                                
      *    EXEC CICS RETURN TRANSID(EIBTRNID) CHANNEL(CHANNEL-NAME)             
      *    END-EXEC.                                                            
                                                                                
       1000-FIM-EXIT.                                                         00
           EXIT.                                                                
                                                                                
      ******************************************************************      00
      * ROTINA ENCERRA O PROGRAMA                                             00
      ******************************************************************      00
       2000-ENCERRA.                                                          00
                                                                                
           DISPLAY '2000-ENCERRA'.                                              
                                                                                
           EXEC CICS SEND CONTROL FREEKB ERASE                                  
           END-EXEC                                                             
                                                                                
           IF EIBAID = DFHPF3                                                   
             EXEC CICS RETURN                                                 00
             END-EXEC                                                         00
                                                                                
             GOBACK                                                             
           END-IF.                                                              
                                                                                
                                                                                
       2000-ENCERRA-EXIT.                                                     00
           EXIT.                                                                
                                                                                
      ******************************************************************      00
      * SERVICO DE ACESSO DADOS VSAM                                          00
      ******************************************************************      00
       SERVICO-DADOS               SECTION.                                     
                                                                                
      ******************************************************************      00
      * ROTINA PARA LER O ARQUIVO VSAM                                        00
      ******************************************************************      00
       1000-LER-ARQUIVO.                                                      00
                                                                                
           DISPLAY '1000-LER-ARQUIVO'.                                          
                                                                                
                                                                                
      *    UNSTRING                                                             
      *       MATRICI  DELIMIT'D BY SPACES INTO MATRIC                          
      *    END-UNSTRING                                                         
                                                                                
           MOVE MATRICI       TO MATRIC.                                        
                                                                                
           DISPLAY ' NOME RECEBIDO   : ' MATRICI.                               
           DISPLAY ' NOME PESQUISADO : ' MATRIC.                                
           DISPLAY ' '                                                          
                                                                                
      *----                                                                     
           EXEC CICS READ     FILE (ARQ-VSAM)                                 00
                              RIDFLD(MATRIC)                                  00
                              INTO (REGISTRO-VSAM)                            00
                              LENGTH (LENGTH OF REGISTRO-VSAM)                00
                              RESP(RESP1)                                     00
                              RESP2(RESP2)                                    00
           END-EXEC                                                           00
      *----                                                                     
           IF RESP1 NOT EQUAL DFHRESP(NORMAL)                                 00
              DISPLAY 'RESP1 = ' RESP1                                          
              DISPLAY 'NOVO ALUNO'                                              
              MOVE    'NOVO ALUNO' TO MSG-ERRO                                  
              MOVE 'READ    '      TO COMANDO                                 00
              MOVE MSG-ERRO        TO MSGERROO                                00
              MOVE SPACE           TO ALUNOO                                    
              MOVE SPACE           TO SEXOO                                     
              MOVE SPACE           TO NOTA01O                                   
              MOVE SPACE           TO NOTA02O                                   
              MOVE SPACE           TO NOTA03O                                   
              MOVE SPACE           TO NOTA04O                                   
              MOVE SPACE           TO NOTA05O                                   
              MOVE SPACE           TO MEDIAO                                    
              MOVE SPACE           TO SITUACAOO                                 
           ELSE                                                               00
              DISPLAY 'ALUNO EXISTENTE'                                         
              MOVE NOME     TO ALUNOO                                         00
              MOVE SEXO     TO SEXOO                                          00
              MOVE NOTA01   TO MASCARA                                        00
              MOVE MASCARA  TO NOTA01O                                        00
              MOVE NOTA02   TO MASCARA                                        00
              MOVE MASCARA  TO NOTA02O                                        00
              MOVE NOTA03   TO MASCARA                                        00
              MOVE MASCARA  TO NOTA03O                                        00
              MOVE NOTA04   TO MASCARA                                        00
              MOVE MASCARA  TO NOTA04O                                        00
              MOVE NOTA05   TO MASCARA                                        00
              MOVE MASCARA  TO NOTA05O                                        00
              MOVE MEDIA    TO MASCARA                                        00
              DISPLAY 'MEDIA NO VSAM    = ' MEDIA                               
              MOVE MASCARA  TO MEDIAO                                         00
              DISPLAY 'MEDIA NA MASCARA = ' MASCARA                             
              DISPLAY 'MEDIA NA MEDIAO  = ' MEDIAO                              
              MOVE SITUACAO TO SITUACAOO                                      00
              MOVE SPACES   TO MSGERROO                                       00
      *----                                                                     
              DISPLAY ' MSGERROO  = '  MSGERROO                                 
              DISPLAY ' ALUNOO     = ' ALUNOO                                   
              DISPLAY ' SEXOO      = ' SEXOO                                    
              DISPLAY ' NOTA01O    = ' NOTA01O                                  
              DISPLAY ' NOTA02O    = ' NOTA02O                                  
              DISPLAY ' NOTA03O    = ' NOTA03O                                  
              DISPLAY ' NOTA04O    = ' NOTA04O                                  
              DISPLAY ' NOTA05O    = ' NOTA05O                                  
              DISPLAY ' MEDIAO     = ' MEDIAO                                   
              DISPLAY ' SITUACAOO  = ' SITUACAOO                                
              DISPLAY '   '                                                     
                                                                                
              EXEC CICS PUT CONTAINER(CONT-CHAVE)                             00
                            CHANNEL(CHANNEL-NAME)                             00
                            FROM(MATRICULA)                                   00
                            FLENGTH(LENGTH OF MATRICULA)                      00
              END-EXEC                                                        00
                                                                                
           END-IF                                                             00
                                                                                
           MOVE DFHENTER   TO EIBAID                                            
                                                                                
           PERFORM 0200-ROTINA-SEND                                           00
                                                                                
           PERFORM  1000-FIM.                                                 00
                                                                                
       1000-LER-ARQUIVO-EXIT.                                                 00
           EXIT.                                                                
                                                                                
      ******************************************************************      00
      * ROTINA PARA ATUALIZAR O ARQUIVO VSAM                                  00
      ******************************************************************      00
       3000-RECALCULA-ARQUIVO.                                                00
                                                                                
           DISPLAY '3000-RECALCULA-ARQUIVO'.                                    
                                                                                
           COMPUTE MEDIA-ARIT = (NOTA01 + NOTA02 + NOTA03 +                   00
                                 NOTA04 + NOTA05) / 5                         00
                                                                                
           MOVE MEDIA-ARIT TO MASCARA                                         00
           MOVE MASCARA    TO MEDIAO                                          00
                                                                                
           IF MEDIA-ARIT < 7 THEN                                             00
              MOVE 'REPROVADO' TO SITUACAOO                                   00
           ELSE                                                               00
              MOVE 'APROVADO ' TO SITUACAOO                                   00
           END-IF.                                                            00
                                                                                
           DISPLAY '*************************'.                                 
           DISPLAY '*** CALCULEI A MEDIA  ***'.                                 
           DISPLAY '*************************'.                                 
           DISPLAY '*** MEDIA     = ' MEDIAO            '    ***'.              
           DISPLAY '*** SITUACAOO = ' SITUACAOO         '    ***'.              
           DISPLAY '*************************'.                                 
                                                                                
       3000-RECALCULA-ARQUIVO-EXIT.                                           00
           EXIT.                                                                
                                                                                
      ******************************************************************      00
      * ROTINA PARA SALVAR O ARQUIVO VSAM                                     00
      ******************************************************************      00
       4000-SALVA-ARQUIVO.                                                    00
                                                                                
           DISPLAY '4000-SALVA-ARQUIVO'.                                        
                                                                                
           DISPLAY ' NOME RECEBIDO : ' MATRICI.                                 
           DISPLAY ' NOME PESQUISADO : ' MATRIC.                                
           DISPLAY ' '                                                          
                                                                                
           EXEC CICS GET CONTAINER(CONT-CHAVE)                                00
                               CHANNEL(CHANNEL-NAME)                          00
                               INTO(MATRICULA)                                00
                               FLENGTH(LENGTH OF MATRICULA)                   00
           END-EXEC                                                           00
      *----                                                                   00
           EXEC CICS READ     FILE (ARQ-VSAM)                                 00
                              RIDFLD(MATRICULA)                               00
                              INTO (REGISTRO-VSAM)                            00
                              LENGTH (LENGTH OF REGISTRO-VSAM)                00
                              UPDATE                                          00
                              RESP(RESP1)                                     00
                              RESP2(RESP2)                                    00
           END-EXEC                                                           00
      *----                                                                   00
      * TRATA ERRO                                                              
      *----                                                                   00
           IF RESP1 NOT EQUAL DFHRESP(NORMAL)                                 00
              DISPLAY ' ERRO NO ACESSO VSAM RESP = ' RESP1                      
                      ' RESP2 = ' RESP2                                         
              MOVE 'REWRITE ' TO COMANDO                                      00
              MOVE MSG-ERRO TO MSGERROO                                       00
              PERFORM 0200-ROTINA-SEND                                        00
              PERFORM  1000-FIM                                               00
           END-IF.                                                            00
                                                                                
           COMPUTE MEDIA-ARIT = (NOTA01 + NOTA02 + NOTA03 +                   00
                                 NOTA04 + NOTA05) / 5                         00
                                                                                
           MOVE MEDIA-ARIT       TO MASCARA-DOIDA                               
           MOVE MASCARA-DOIDA    TO MEDIA                                     00
                                                                                
           DISPLAY ' VALOR DA MEDIA = ' MEDIA.                                  
                                                                                
           IF MEDIA-ARIT < 7 THEN                                             00
              MOVE 'REPROVADO' TO SITUACAO                                    00
           ELSE                                                               00
              MOVE 'APROVADO ' TO SITUACAO                                    00
           END-IF                                                             00
      *----                                                                   00
           DISPLAY ' ANTES DO ERRO DO REWRITE 16'                               
           DISPLAY ' MATRICULA  = '  MATRICULA                                  
           DISPLAY ' NOME       = '  NOME                                       
           DISPLAY ' SEXO       = '  SEXO                                       
           DISPLAY ' NOTA01     = '  NOTA01                                     
           DISPLAY ' NOTA02     = '  NOTA02                                     
           DISPLAY ' NOTA03     = '  NOTA03                                     
           DISPLAY ' NOTA04     = '  NOTA04                                     
           DISPLAY ' NOTA05     = '  NOTA05                                     
           DISPLAY ' MEDIA      = '  MEDIA                                      
           DISPLAY ' SITUACAO   = '  SITUACAO                                   
      *----                                                                   00
           EXEC CICS REWRITE FILE(ARQ-VSAM)                                   00
                             FROM(REGISTRO-VSAM)                              00
                             LENGTH(LENGTH OF REGISTRO-VSAM)                  00
                             RESP(RESP1)                                      00
                             RESP2(RESP2)                                     00
           END-EXEC                                                           00
      *----                                                                   00
                                                                                
           IF RESP1 NOT EQUAL DFHRESP(NORMAL)                                 00
              DISPLAY ' ERRO NO VSAM REWRITE -> RESP1= ' RESP1                  
                      ' RESP2 = ' RESP2                                         
              MOVE 'REWRITE ' TO COMANDO                                      00
              MOVE MSG-ERRO TO MSGERROO                                       00
              PERFORM 0200-ROTINA-SEND                                        00
              PERFORM  1000-FIM                                               00
           END-IF.                                                            00
                                                                                
       4000-SALVA-ARQUIVO-EXIT.                                               00
           EXIT.                                                                
                                                                                
      ********************** FIM DO PROGRAMA ***************************        
