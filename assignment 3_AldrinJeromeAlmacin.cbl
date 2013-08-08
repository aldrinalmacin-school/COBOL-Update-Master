      *NAME: ANJU CHAWLA
      *DATE: AUGUST 2, 2013
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
           PERFORM 400-READ-TRANS
           PERFORM 500-UPDATE-PARA
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
       300-READ-MASTER.
           READ OLD-MASTER-FILE
             AT END 
               MOVE HIGH-VALUES TO M-ACCT-NO
           END-READ.
      *----------------------------------------------------
      *READ THE NEXT RECORD FROM THE TRANSACTION FILE
       400-READ-TRANS.
           READ TRANSACTION-FILE
             AT END 
              MOVE 'N' TO MORE-RECORDS
           END-READ. 
      *---------------------------------------------------
      *COMPARE THE KEY FILEDS OF THE MASTER AND TRANSACTION RECORDS
      *AND PERFORM THE REQUIRED OPERATION
       500-UPDATE-PARA.
           PERFORM 300-READ-MASTER UNTIL
              M-ACCT-NO = T-ACCT-NO
              OR
              M-ACCT-NO > T-ACCT-NO
              OR
              M-ACCT-NO = HIGH-VALUES
              
            EVALUATE TRUE
              WHEN M-ACCT-NO = T-ACCT-NO
                ADD T-AMOUNT TO M-AMOUNT
                REWRITE OLD-REC
              WHEN M-ACCT-NO > T-ACCT-NO
      *CANNOT DO A WRITE ON A SEQUENTIAL FILE OPENED IN I-O MODE        
      *          WRITE OLD-REC FROM TRANS-REC 
                DISPLAY T-ACCT-NO, ' NOT ON THE MASTER FILE. '
                'NO OPERATION PERFORMED.'
            END-EVALUATE 
            
             PERFORM 400-READ-TRANS.
       
      *------------------------------------------------------ 
       
          
      *CLOSE THE FILES.
       600-CLOSE-PARA.
           CLOSE  MASTER-FILE
                  TRANSACTION-FILE.
                       
      *********************************************************     
       
            
                                                
                    
                