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
           SELECT NEW-MASTER-FILE
             ASSIGN TO 'NEW-MAST.DAT'
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
              
       FD  NEW-MASTER-FILE
           RECORD CONTAINS 13 CHARACTERS. 
       01  NEW-MASTER-REC.
           05 MN-ACCT-NO       PIC X(5).
           05 MN-AMOUNT        PIC 9(5)V99.  
           05 MN-ACTIVE        PIC X.
          
       WORKING-STORAGE SECTION.
       01  MORE-RECORDS       PIC X    VALUE 'Y'.
       01  RESET-FILE         PIC X    VALUE 'N'.   
           
      **********************************************************
       PROCEDURE DIVISION.
       100-MAIN-PARA.
           PERFORM 200-OPEN-PARA
          
           PERFORM UNTIL MORE-RECORDS = 'N'
             READ TRANSACTION-FILE
               AT END
                 MOVE 'N' TO MORE-RECORDS
               NOT AT END
                 PERFORM 800-READ-MASTER-PARA
             END-READ      
           END-PERFORM
           
           PERFORM 900-MOVE-TO-NEW-FILE-PARA
           PERFORM 600-CLOSE-PARA 
           
           STOP RUN.
      
       200-OPEN-PARA.
           OPEN  INPUT   TRANSACTION-FILE
           OPEN  I-O     MASTER-FILE.
                  
       300-PROCESS-PARA.
           EVALUATE TRUE
             WHEN UPDATE-R
               PERFORM 400-UPDATE-PARA
             WHEN DELETE-R
               PERFORM 500-DELETE-PARA
             WHEN OTHER
               DISPLAY 'ERROR IN TRANSACTION CODE FOR TRANSACTION '
                'TRANSACTION NO ', T-ACCT-NO
           END-EVALUATE.
       
       400-UPDATE-PARA.
           ADD T-AMOUNT TO M-AMOUNT
           REWRITE MASTER-REC.
       
       500-DELETE-PARA.
           MOVE 'N' TO M-ACTIVE
           REWRITE MASTER-REC.
       
       600-CLOSE-PARA.
           CLOSE TRANSACTION-FILE
                 MASTER-FILE.
       
       700-RESET-PARA.
           MOVE 'N' TO RESET-FILE
           CLOSE MASTER-FILE
           OPEN  I-O MASTER-FILE.
           
       800-READ-MASTER-PARA.
           PERFORM UNTIL RESET-FILE = 'Y'
             READ MASTER-FILE
               AT END 
                 MOVE 'Y' TO RESET-FILE
               NOT AT END
                 IF M-ACCT-NO = T-ACCT-NO
                   PERFORM 300-PROCESS-PARA
                   MOVE 'Y' TO RESET-FILE
                 END-IF
             END-READ
           END-PERFORM
           
           PERFORM 700-RESET-PARA.
           
       900-MOVE-TO-NEW-FILE-PARA.
           OPEN OUTPUT NEW-MASTER-FILE
           PERFORM UNTIL RESET-FILE = 'Y'
             READ MASTER-FILE
               AT END 
                 MOVE 'Y' TO RESET-FILE
               NOT AT END
                 IF M-ACTIVE = 'Y'
                   MOVE MASTER-REC TO NEW-MASTER-REC
                   WRITE NEW-MASTER-REC
                 END-IF
             END-READ
           END-PERFORM
           CLOSE NEW-MASTER-FILE.