 1         2         3         4         5         6         7   
123456789012345678901234567890123456789012345678901234567890123456789012

      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.                                         
      *-----------------------------------------------------------------
       PROGRAM-ID.      CADBALAN.                                       
       AUTHOR.          CARLOS ALBERTO DORNELLES.                       
      *-----------------------------------------------------------------
      * SISTEMA       : PROGRAMAS EXEMPLOS                              
      * PROGRAMA      : CADBALAN                                        
      * OBJETIVO      : BALANCE LINE DE DOIS ARQUIVOS                   
      * ENTRADA       : ARQUIVO COM NOME DO FUNCIONARIO                 
      *               : ARQUIVO COM ENDERECO DO FUNCIONARIO             
      * SAIDA         : ARQUIVO COM OS DADOS DO FUNCIONARIO             
      * ANALISTA      : CARLOS ALBERTO DORNELLES                        
      * LINGUAGEM     : COBOL II - COBOL 85                             
      * MODO OPERACAO : BATCH                                           
      *-----------------------------------------------------------------
      *  VERSAO DD.MM.AAAA HISTORICO/AUTOR                              
      *  ------ ---------- ---------------                              
      *   V.001 16.08.2007 PROGRAMA INICIAL/DORNELLES                   
      *                                                                 
      *-----------------------------------------------------------------
                                                                        
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.                                            
      *-----------------------------------------------------------------
                                                                        
      *-----------------------------------------------------------------
       CONFIGURATION SECTION.                                           
      *-----------------------------------------------------------------
       SPECIAL-NAMES.                                                   
                        DECIMAL-POINT IS COMMA.                         
                                                                        
      *-----------------------------------------------------------------
       INPUT-OUTPUT SECTION.                                            
      *-----------------------------------------------------------------
       FILE-CONTROL.                                                    
                                                                        
           SELECT ENTNOME ASSIGN TO ENTNOME                             
                  FILE STATUS IS WS-FS-ENTNOME.                         
           SELECT ENTENDE ASSIGN TO ENTENDE                             
                  FILE STATUS IS WS-FS-ENTENDE.                         
           SELECT SAIFUNC ASSIGN TO SAIFUNC                             
                  FILE STATUS IS WS-FS-SAIFUNC.                         
                                                                        
      *-----------------------------------------------------------------
       DATA DIVISION.                                                   
      *-----------------------------------------------------------------
                                                                        
      *-----------------------------------------------------------------
       FILE SECTION.                                                    
      *-----------------------------------------------------------------
                                                                        
       FD  ENTNOME                                                      
           BLOCK CONTAINS 0 RECORDS                                     
           RECORDING MODE IS F                                          
           RECORD CONTAINS 060 CHARACTERS.                              
                                                                        
       01  REG-ENTNOME.                                                 
           03  ENTNOME-MATRICULA         PIC 9(010).                    
           03  ENTNOME-NOME              PIC X(050).                    
                                                                        
       FD  ENTENDE                                                      
           BLOCK CONTAINS 0 RECORDS                                     
           RECORDING MODE IS F                                          
           RECORD CONTAINS 120 CHARACTERS.                              
                                                                        
       01  REG-ENTENDE.                                                 
           03  ENTENDE-MATRICULA         PIC 9(010).                    
           03  ENTENDE-ENDERECO          PIC X(050).                    
           03  ENTENDE-CIDADE            PIC X(050).                    
           03  ENTENDE-UF                PIC X(002).                    
           03  ENTENDE-CEP               PIC 9(008).                    
                                                                        
       FD  SAIFUNC                                                      
           BLOCK CONTAINS 0 RECORDS                                     
           RECORDING MODE IS F                                          
           RECORD CONTAINS 170 CHARACTERS.                              
                                                                        
       01  REG-SAIFUNC.                                                 
           03  SAIFUNC-MATRICULA         PIC 9(010).                    
           03  SAIFUNC-NOME              PIC X(050).                    
           03  SAIFUNC-ENDERECO          PIC X(050).                    
           03  SAIFUNC-CIDADE            PIC X(050).                    
           03  SAIFUNC-UF                PIC X(002).                    
           03  SAIFUNC-CEP               PIC 9(008).                    
                                                                        
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.                                         
      *-----------------------------------------------------------------
                                                                        
       01  WS-AREA-AUXILIAR.                                            
           05  WS-COD-PROGRAMA           PIC X(008)  VALUE 'CADBALAN'.  
           05  WS-COD-VER                PIC X(008)  VALUE '001/2007'.  
           05  WS-FS-ENTNOME             PIC X(002)  VALUE SPACES.      
           05  WS-FS-ENTENDE             PIC X(002)  VALUE SPACES.      
           05  WS-FS-SAIFUNC             PIC X(002)  VALUE SPACES.      
           05  WS-LIDOS-NOME             PIC 9(010)  VALUE ZEROES.      
           05  WS-LIDOS-ENDE             PIC 9(010)  VALUE ZEROES.      
           05  WS-GRAVA-FUNC             PIC 9(010)  VALUE ZEROES.      
           05  WS-MENSAGEM               PIC X(070)  VALUE SPACES.      
           05  WS-PROCESSO               PIC X(070)  VALUE SPACES.      
                                                                        
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.                                              
      *-----------------------------------------------------------------
                                                                        
           PERFORM P0000-INICIAL
           PERFORM P1000-PRINCIPAL
           PERFORM P9000-FINAL
           GOBACK.
                                                                        
      *-----------------------------------------------------------------
       P0000-INICIAL.                                                   
      *-----------------------------------------------------------------
                                                                        
           MOVE 'P0000-INICIAL' TO        WS-PROCESSO.                  
                                                                        
           OPEN INPUT ENTNOME.                                          
           IF WS-FS-ENTNOME NOT EQUAL '00'                              
              MOVE SPACES TO WS-MENSAGEM                                
              STRING 'ERRO ABERTURA ARQUIVO ENTNOME FILE STATUS: '      
                     WS-FS-ENTNOME                                      
                     DELIMITED BY SIZE  INTO WS-MENSAGEM                
              END-STRING                                                
              PERFORM P8000-ERRO THRU P8000-FIM                         
           END-IF.                                                      
                                                                        
           OPEN INPUT ENTENDE.                                          
           IF WS-FS-ENTENDE NOT EQUAL '00'                              
              STRING 'ERRO ABERTURA ARQUIVO ENTENDE FILE STATUS: '      
                     WS-FS-ENTENDE                                      
                     DELIMITED BY SIZE  INTO WS-MENSAGEM                
              END-STRING                                                
              PERFORM P8000-ERRO THRU P8000-FIM                         
           END-IF.                                                      
                                                                        
           OPEN OUTPUT SAIFUNC.                                         
           IF WS-FS-SAIFUNC NOT EQUAL '00'                              
              STRING 'ERRO ABERTURA ARQUIVO SAIFUNC FILE STATUS: '      
                     WS-FS-SAIFUNC                                      
                     DELIMITED BY SIZE  INTO WS-MENSAGEM                
              END-STRING                                                
              PERFORM P8000-ERRO THRU P8000-FIM                         
           END-IF.                                                      
                                                                        
        P0000-FIM.                                                      
            EXIT.                                                       
                                                                        
      *-----------------------------------------------------------------
       P1000-PRINCIPAL.                                                 
      *-----------------------------------------------------------------
                                                                        
           MOVE 'P1000-PRINCIPAL      ' TO  WS-PROCESSO.                
                                                                        
           PERFORM P2000-LER-ENTNOME THRU P2000-FIM                     
           PERFORM P3000-LER-ENTENDE THRU P3000-FIM                     
           PERFORM UNTIL WS-FS-ENTNOME EQUAL '10'                       
                     AND WS-FS-ENTENDE EQUAL '10'                       
              EVALUATE TRUE                                             
                 WHEN ENTNOME-MATRICULA EQUAL        ENTENDE-MATRICULA  
                      PERFORM P4000-GRAVA-SAIFUNC THRU P4000-FIM        
                      PERFORM P2000-LER-ENTNOME   THRU P2000-FIM        
                      PERFORM P3000-LER-ENTENDE   THRU P3000-FIM        
                 WHEN ENTNOME-MATRICULA LESS THAN    ENTENDE-MATRICULA  
                      PERFORM P2000-LER-ENTNOME   THRU P2000-FIM        
                 WHEN ENTNOME-MATRICULA GREATER THAN ENTENDE-MATRICULA  
                      PERFORM P3000-LER-ENTENDE   THRU P3000-FIM        
              END-EVALUATE                                              
           END-PERFORM.                                                 
                                                                        
       P1000-FIM.                                                       
           EXIT.                                                        
                                                                        
      *-----------------------------------------------------------------
       P2000-LER-ENTNOME.                                               
      *-----------------------------------------------------------------
                                                                        
           MOVE 'P2000-LER-ENTNOME' TO WS-PROCESSO                      
           READ ENTNOME                                                 
                AT END                                                  
                MOVE '10'       TO WS-FS-ENTNOME                        
                MOVE 9999999999 TO ENTNOME-MATRICULA                    
                NOT AT END                                              
                IF WS-FS-ENTNOME NOT EQUAL '00' AND '10'                
                   MOVE SPACES TO WS-MENSAGEM                           
                   STRING 'ERRO LEITURA ARQUIVO ENTNOME FILE STATUS: '  
                           WS-FS-ENTNOME                                
                           DELIMITED BY SIZE  INTO WS-MENSAGEM          
                   END-STRING                                           
                   PERFORM P8000-ERRO THRU P8000-FIM                    
                END-IF                                                  
                IF WS-FS-ENTNOME EQUAL '00'                             
                   ADD 1 TO WS-LIDOS-NOME                               
                END-IF                                                  
           END-READ.                                                    
                                                                        
       P2000-FIM.                                                       
           EXIT.                                                        
                                                                        
      *-----------------------------------------------------------------
       P3000-LER-ENTENDE.                                               
      *-----------------------------------------------------------------
                                                                        
           MOVE 'P3000-LER-ENTENDE' TO WS-PROCESSO                      
           READ ENTENDE                                                 
                AT END                                                  
                MOVE '10'       TO WS-FS-ENTENDE                        
                MOVE 9999999999 TO ENTENDE-MATRICULA                    
                NOT AT END                                              
                IF WS-FS-ENTENDE NOT EQUAL '00' AND '10'                
                   MOVE SPACES TO WS-MENSAGEM                           
                   STRING 'ERRO LEITURA ARQUIVO ENTENDE FILE STATUS: '  
                           WS-FS-ENTENDE                                
                           DELIMITED BY SIZE  INTO WS-MENSAGEM          
                   END-STRING                                           
                   PERFORM P8000-ERRO THRU P8000-FIM                    
                END-IF                                                  
                IF WS-FS-ENTENDE EQUAL '00'                             
                   ADD 1 TO WS-LIDOS-ENDE                               
                END-IF                                                  
           END-READ.                                                    
                                                                        
       P3000-FIM.                                                       
           EXIT.                                                        
                                                                        
                                                                        
      *-----------------------------------------------------------------
       P4000-GRAVA-SAIFUNC.                                             
      *-----------------------------------------------------------------
                                                                        
           MOVE 'P4000-GRAVA-SAIFUNC' TO WS-PROCESSO                    
           INITIALIZE REG-SAIFUNC                                       
                      REPLACING ALPHANUMERIC BY SPACES                  
                                     NUMERIC BY ZEROES                  
                                                                        
           MOVE ENTENDE-MATRICULA TO SAIFUNC-MATRICULA                  
           MOVE ENTENDE-ENDERECO  TO SAIFUNC-ENDERECO                   
           MOVE ENTENDE-CIDADE    TO SAIFUNC-CIDADE                     
           MOVE ENTENDE-UF        TO SAIFUNC-UF                         
           MOVE ENTENDE-CEP       TO SAIFUNC-CEP                        
           MOVE ENTNOME-NOME      TO SAIFUNC-NOME                       
           WRITE REG-SAIFUNC         END-WRITE                          
                                                                        
           IF WS-FS-SAIFUNC NOT EQUAL '00'                              
              MOVE SPACES TO WS-MENSAGEM                                
              STRING 'ERRO GRAVACAO ARQUIVO SAIFUNC FILE STATUS: '      
                      WS-FS-SAIFUNC                                     
                      DELIMITED BY SIZE  INTO WS-MENSAGEM               
              END-STRING                                                
              PERFORM P8000-ERRO THRU P8000-FIM                         
           END-IF                                                       
                                                                        
           ADD 1 TO WS-GRAVA-FUNC.                                      
                                                                        
       P4000-FIM.                                                       
           EXIT.                                                        
                                                                        
                                                                        
      *-----------------------------------------------------------------
       P8000-ERRO.                                                      
      *-----------------------------------------------------------------
                                                                        
           DISPLAY '---------------------------------------------'      
           DISPLAY 'PROGRAMA CADBALAN CANCELADO'                        
           DISPLAY 'PARAGRAFO   - ' WS-PROCESSO                         
           DISPLAY 'VERSAO      - ' WS-COD-VER                          
           DISPLAY 'MENSAGEM    - ' WS-MENSAGEM                         
           DISPLAY '---------------------------------------------'      
           MOVE 99 TO RETURN-CODE                                       
           GOBACK.                                                      
                                                                        
       P8000-FIM.                                                       
           EXIT.                                                        
                                                                        
      *-----------------------------------------------------------------
       P9000-FINAL.                                                     
      *-----------------------------------------------------------------
                                                                        
           DISPLAY '---------------------------------------------'      
           DISPLAY 'PROGRAMA CADBALAN - TERMINO OK'                     
           DISPLAY '                                             '      
           DISPLAY 'TOTAL DE LIDOS NOMES .. - ' WS-LIDOS-NOME           
           DISPLAY 'TOTAL DE LIDOS ENDERECO - ' WS-LIDOS-ENDE           
           DISPLAY 'TOTAL GRAVADOS ........ - ' WS-GRAVA-FUNC.          
                                                                        
       P9000-FIM.                                                       
           EXIT.                                                        


