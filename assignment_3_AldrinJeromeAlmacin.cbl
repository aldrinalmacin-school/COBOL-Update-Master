      *NAME: ALDRIN JEROME ALMACIN
      *DATE: AUGUST 9, 2013
      *PURPOSE: TO UPDATE OR DELETE A MASTER FILE RECORD IN PLACE USING
      *A TRANSACTION FILE.
      *THERE CAN BE NONE, ONE OR MULTIPLE TRANSACTION RECORD(IF ANY) 
      *CORRESPONDING TO A MASTER RECORD.
      
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ASSIGN3.
      ***********************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-FILE
             ASSIGN TO 'MASTER.DAT'
             ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TRANSACTION-FILE
             ASSIGN TO 'TRANS.DAT'
             ORGANIZATION IS LINE SEQUENTIAL.
          
      ***********************************************************
       DATA DIVISION.
       FILE SECTION.
       FD  MASTER-FILE
           RECORD CONTAINS 13 CHARACTERS.
       01  MASTER-REC.
           05 M-ACCT-NO       PIC X(5).
           05 M-AMOUNT        PIC 9(5)V99.  
           05 M-ACTIVE        PIC X.
              88 ACTIVE               VALUE 'Y'.
              88 NOT-ACTIVE           VALUE 'N'.  
           
       FD  TRANSACTION-FILE
           RECORD CONTAINS 13 CHARACTERS.
       01  TRANS-REC.
           05 T-ACCT-NO       PIC X(5).
           05 T-AMOUNT        PIC 9(5)V99.
           05 T-CODE          PIC X.
              88 UPDATE-R              VALUE 'U'.
              88 DELETE-R              VALUE 'D'.  
          
       WORKING-STORAGE SECTION.
       01  MORE-RECORDS       PIC X    VALUE 'Y'.   
           
      **********************************************************
       PROCEDURE DIVISION.
      *OPENS THE FILES, PROCESSES THE RECORDS AND CLOSES THE FILES 
       100-MAIN-PARA.
           PERFORM 200-OPEN-PARA
           PERFORM 400-READ-TRANS-PARA
           PERFORM 500-U-OR-D-MASTER-PARA
             UNTIL MORE-RECORDS = 'N'
           PERFORM 600-CLOSE-PARA 
          
           STOP RUN.
      *----------------------------------------------------       
      *OPEN THE FILES IN THE REQUIRED MODE 
       200-OPEN-PARA.
           OPEN   I-O     MASTER-FILE
                  INPUT   TRANSACTION-FILE.
                  
      *----------------------------------------------------
      *READ THE NEXT RECORD FROM THE OLD MASTER FILE
       300-READ-MASTER-PARA.
           READ MASTER-FILE
             AT END 
               MOVE HIGH-VALUES TO M-ACCT-NO
           END-READ.
           
       400-READ-TRANS-PARA.
           READ TRANSACTION-FILE
             AT END 
              MOVE 'N' TO MORE-RECORDS.
            
       500-U-OR-D-MASTER-PARA.
           PERFORM 300-READ-MASTER-PARA UNTIL
              M-ACCT-NO = T-ACCT-NO
              OR
              M-ACCT-NO > T-ACCT-NO
              OR
              M-ACCT-NO = HIGH-VALUES
              
            EVALUATE TRUE
              WHEN M-ACCT-NO = T-ACCT-NO
                EVALUATE TRUE
                  WHEN T-CODE = 'U'
                    ADD T-AMOUNT TO M-AMOUNT
                    REWRITE MASTER-REC
                  WHEN T-CODE = 'D'
                    MOVE 'N' TO M-ACTIVE
                    REWRITE MASTER-REC
                END-EVALUATE
              WHEN M-ACCT-NO > T-ACCT-NO
      *CANNOT DO A WRITE ON A SEQUENTIAL FILE OPENED IN I-O MODE        
      *          WRITE OLD-REC FROM TRANS-REC 
                DISPLAY T-ACCT-NO, ' NOT ON THE MASTER FILE. '
                'NO OPERATION PERFORMED.'
            END-EVALUATE 
            
            PERFORM 400-READ-TRANS-PARA.
          
      *CLOSE THE FILES.
       600-CLOSE-PARA.
           CLOSE  MASTER-FILE
                  TRANSACTION-FILE.
                       
      *********************************************************     
       
            
                                                
                    
                