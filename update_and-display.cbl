      *NAME: ANJU CHAWLA
      *DATE: AUGUST 9, 2013
      *PURPOSE:UPDATE RECORDS IN AN MASTER INDEXED FILE.TRANSACTION FILE
      *CAN CONTAIN NONE, ONE OR MULTIPLE RECORDS CORRESPONDING TO A 
      *MASTER RECORD. THE RECORDS IN THE TRANSACTION FILE NEED NOT BE
      *IN SEQUENCE.CREATE A SEQUENTIAL FILE FROM THIS INDEXED FILE.
      
       IDENTIFICATION DIVISION.
       PROGRAM-ID. UPDISP.
      *************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRANSACTION-FILE
             ASSIGN TO 'TRANS.DAT'
             ORGANIZATION IS LINE SEQUENTIAL.
             
           SELECT MASTER-FILE
             ASSIGN TO 'CH1501.NDX'
             ORGANIZATION IS INDEXED
             ACCESS IS DYNAMIC
             RECORD KEY IS I-SSNO-OUT. 
             
           SELECT PAYROLL-MASTER
             ASSIGN TO 'CH1501.OUT'
             ORGANIZATION IS LINE SEQUENTIAL.   
      *********************************************************
       DATA DIVISION.
       FILE SECTION.
       FD  TRANSACTION-FILE
           RECORD CONTAINS 35 CHARACTERS.
       01  TRANS-RECORD.
           05 SSNO-IN             PIC X(9).
           05 NAME-IN             PIC X(20).
           05 SALARY-IN           PIC 9(5).
           05 CODE-IN             PIC X.
             88 ADD-R                     VALUE '1'.
             88 UPDATE-R                  VALUE '2'.
             88 DELETE-R                  VALUE '3'.
               
       FD  MASTER-FILE
           RECORD CONTAINS 34 CHARACTERS.
       01  MASTER-RECORD.
           05 I-SSNO-OUT          PIC X(9).
           05 I-NAME-OUT          PIC X(20).
           05 I-SALARY-OUT        PIC 9(5). 
           
       FD  PAYROLL-MASTER
           RECORD CONTAINS 34 CHARACTERS.
       01  PAYROLL-RECORD.
           05 SSNO-OUT            PIC X(9).
           05 NAME-OUT            PIC X(20).
           05 SALARY-OUT          PIC 9(5).
      *---------------------------------------------------------       
       WORKING-STORAGE SECTION.
       01  MORE-DATA              PIC X    VALUE 'Y'.
       01  RECORD-COUNTER         PIC 9(3) VALUE 0.  
      
      **********************************************************
       PROCEDURE DIVISION.
      *OPEN THE FILES, PROCESS ALL RECORDS IN THE TRANSACTION FILE
      *CLOSE THE FILES
       100-MAIN-PARA.
           OPEN  INPUT   TRANSACTION-FILE
                 I-O     MASTER-FILE
                 
           PERFORM UNTIL MORE-DATA = 'N'
             READ TRANSACTION-FILE
               AT END
                 MOVE 'N' TO MORE-DATA
               NOT AT END
                 ADD 1 TO RECORD-COUNTER
                 PERFORM 200-PROCESS-PARA
             END-READ      
           END-PERFORM
           
           CLOSE TRANSACTION-FILE
           
           PERFORM 600-PRINT-PARA
                
           STOP RUN.
      *-------------------------------------------------------------
      *CHECKS THE CODE IN THE TRANSACTION RECORD AND PERFORMS THE 
      *REQUIRED OPERATION - ADD, UPDATE, DELETE.
       200-PROCESS-PARA.
           EVALUATE TRUE
             WHEN ADD-R
               PERFORM 300-ADD-PARA
             WHEN UPDATE-R
               PERFORM 400-UPDATE-PARA
             WHEN DELETE-R
               PERFORM 500-DELETE-PARA
             WHEN OTHER
                DISPLAY 'ERROR IN TRANSACTION CODE FOR TRANSACTION '
                'RECORD NUMBER ', RECORD-COUNTER
                DISPLAY 'THE KEY OF THE ERRONEOUS RECORD IS ',SSNO-IN
                DISPLAY '--------------------------------------------' 
           END-EVALUATE.
      *--------------------------------------------------------------
      *A NEW RECORD IS ADDED TO THE MASTER IF THERE IT DOES NOT CONTAIN
      *ONE WITH THE SAME KEY. DISPLAY AN ERROR MESSAGE OTHERWISE
       300-ADD-PARA.
           MOVE SPACES TO MASTER-RECORD
           MOVE SSNO-IN   TO I-SSNO-OUT
           MOVE NAME-IN   TO I-NAME-OUT
           MOVE SALARY-IN TO I-SALARY-OUT
           WRITE MASTER-RECORD
              INVALID KEY PERFORM 350-ERROR-PARA
              NOT INVALID KEY
                 DISPLAY 'RECORD WITH KEY ', SSNO-IN, ' ADDED.'
                 DISPLAY '-------------------------------------------'
           END-WRITE.
      *---------------------------------------------------------------
      *DISPLAY AN ERROR MESSAGE IF RECORD CANNOT BE ADDED  
       350-ERROR-PARA.
           DISPLAY 'ERROR IN TRANSACTION RECORD NUMBER ',
            RECORD-COUNTER
           DISPLAY 'THE KEY OF THE ERRONEOUS RECORD IS ',SSNO-IN
           DISPLAY ' RECORD WITH THE SAME KEY EXISTS IN MASTER FILE. '
               'ADD UNSUCCESSFUL.'
           DISPLAY '--------------------------------------------'.
      *---------------------------------------------------------------
      *UPDATE THE OLD SALARY TO A NEW SALARY IF THE RECORD EXISTS 
      *IN THE MASTER FILE
       400-UPDATE-PARA.
           MOVE SSNO-IN TO I-SSNO-OUT
           READ MASTER-FILE
             INVALID KEY
               PERFORM 450-ERROR-PARA
             NOT INVALID KEY  
               MOVE SALARY-IN TO I-SALARY-OUT
               REWRITE MASTER-RECORD
      *            INVALID KEY 
      *              PERFORM 450-ERROR-PARA 
                  NOT INVALID KEY
                    DISPLAY 'RECORD WITH KEY ',SSNO-IN, ' UPDATED.'
                    DISPLAY '--------------------------------------'
               END-REWRITE        
           END-READ.
      *-------------------------------------------------------------     
      *DISPLAY AN ERROR MESSAGE IF RECORD CANNOT BE FOUND AND UPDATED  
       450-ERROR-PARA.
           DISPLAY 'ERROR IN TRANSACTION RECORD NUMBER ',
            RECORD-COUNTER
           DISPLAY 'THE KEY OF THE ERRONEOUS RECORD IS ',SSNO-IN
           DISPLAY 'RECORD WITH THE KEY DOES NOT EXIST IN MASTER FILE.'
               'UPDATE UNSUCCESSFUL.'
           DISPLAY '--------------------------------------------'.
      *---------------------------------------------------------------                          
      *DELETE A RECORD FROM THE INDEXED FILE, IF FOUND, ELSE DISPLAY AN 
      *ERROR MESSAGE.   
       500-DELETE-PARA.
           MOVE SSNO-IN TO I-SSNO-OUT
           DELETE MASTER-FILE
             INVALID KEY
               PERFORM 550-ERROR-PARA
             NOT INVALID KEY
               DISPLAY 'RECORD WITH KEY ', SSNO-IN, ' DELETED.'
               DISPLAY '-------------------------------------------'
          END-DELETE.
      *-------------------------------------------------------------     
      *DISPLAY AN ERROR MESSAGE IF RECORD CANNOT BE DELETED SINCE NOT 
      *FOUND  
       550-ERROR-PARA.
           DISPLAY 'ERROR IN TRANSACTION RECORD NUMBER ', 
                       RECORD-COUNTER
           DISPLAY 'THE KEY OF THE ERRONEOUS RECORD IS ',SSNO-IN
           DISPLAY 'RECORD WITH THE KEY DOES NOT EXIST IN MASTER FILE.'
               'DELETE UNSUCCESSFUL.'
           DISPLAY '--------------------------------------------'.
      *--------------------------------------------------------------
      *MOVES AND WRITES RECORDS FROM THE UPDATED INDEXED FILE TO 
      *A SEQUENTIAL FILE
       600-PRINT-PARA.
           OPEN OUTPUT PAYROLL-MASTER
      *CLOSE THE MASTER AND OPEN IT AGAIN SO THAT FILE POINTER
      *IS POSITIONED AT THE FIRST RECORD     
      *     CLOSE MASTER-FILE
      *     OPEN INPUT MASTER-FILE
      *THE OTHER OPTION IS TO USE THE START VERB TO POSITION THE 
      *FILE POINTER
           MOVE LOW-VALUES TO I-SSNO-OUT
           START MASTER-FILE
             KEY > I-SSNO-OUT 
             INVALID KEY
                DISPLAY 'NO RECORDS IN THE INDEXED FILE.'
                DISPLAY '-------------------------------------'
             NOT INVALID KEY
                CONTINUE
           END-START
      *READ RECORD FROM MASTER INDEXED AND WRITE TO MASTER SEQUENTIAL     
           MOVE 'Y' TO MORE-DATA
            
           PERFORM UNTIL MORE-DATA = 'N'
      *IF ACCESS IS DYNAMIC, USE READ-NEXT TO READ THE RECORDS
      *SEQUENTIALLY     
             READ MASTER-FILE NEXT RECORD
               AT END
                 MOVE 'N' TO MORE-DATA
               NOT AT END
                 WRITE PAYROLL-RECORD FROM MASTER-RECORD
             END-READ
           
           END-PERFORM
           
           CLOSE PAYROLL-MASTER
      *     CLOSE MASTER-FILE
            .          
           
      ****************************************************************                                      