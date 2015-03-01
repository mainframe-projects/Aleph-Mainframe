      *****************************************************************
      * PROGRAM    : CALC1000  
      * DATE       : 05 NOVEMBER 2014
      * AUTHOR     : JULIO CESAR TORRES  
      * PROGRAMMER : JULIO CESAR TORRES
      * OBJECTIVE  : CALCULATE THE CANADIAN SALES TAX AMOUNTS (FOR QC) 
      *****************************************************************
       IDENTIFICATION DIVISION. 
      * 
       PROGRAM-ID. CALC1001. 
      * 
       ENVIRONMENT DIVISION. 
      * 
       INPUT-OUTPUT SECTION. 
      * 
       DATA DIVISION. 
      * 
       FILE SECTION. 
      * 
       WORKING-STORAGE SECTION. 
      * 
       77    END-OF-SALES-SWITCH        PIC X VALUE 'N'. 
       77    SALES-AMOUNT               PIC S9(005)V99. 
       77    SALES-TAX                  PIC S9(005)V99.
       77    SALES-TPS                  PIC S9(005)V99.
       77    SALES-TVQ                  PIC S9(005)V99. 
       77    TPS-QUOTA                  PIC S9(005)V99 VALUE +0.05 .
       77    TVQ-QUOTA                  PIC S9(005)V99 VALUE +0.1.
       77    SALES-TAXES                PIC S9(005)V99. 
      * 
      *
       PROCEDURE DIVISION. 
      * 
       000-CALCULATE-SALES-TAX. 
      * 
           PERFORM 100-CALCULATE-ONE-SALES-TAX
        		UNTIL END-OF-SALES-SWITCH = 'Y'. 
           DISPLAY 'END OF SESSION.'.
           STOP RUN. 
      * 
       100-CALCULATE-ONE-SALES-TAX. 
           DISPLAY '---------------------------------------------------'. 
           DISPLAY '------------ TO END PROGRAM, ENTER 0.--------------'. 
           DISPLAY 'TO CALCULATE THE SALES TAX, ENTER THE SALES AMOUNT:'. 
           ACCEPT SALES-AMOUNT. 

           IF NOT IS NUMERIC SALES-AMOUNT 
               DISPLAY 'ERRO : DADO NAO NUMERICO!'
               STOP RUN
           END-IF. 

               IF SALES-AMOUNT = ZERO 
                   MOVE "Y" TO END-OF-SALES-SWITCH 
               ELSE 
             	   COMPUTE SALES-TPS ROUNDED = SALES-AMOUNT * TPS-QUOTA

             	   COMPUTE SALES-TVQ ROUNDED = SALES-AMOUNT * TVQ-QUOTA

                   ADD SALES-TPS TO SALES-TVQ GIVING SALES-TAX

       			   COMPUTE SALES-TAXES ROUNDED = SALES-AMOUNT + 
       			       SALES-TPS + SALES-TVQ

                   DISPLAY 'SOUS-TOTAL   : ' SALES-AMOUNT
       			   DISPLAY 'TPS 5%       : ' SALES-TPS
       			   DISPLAY 'TVQ 9,975%   : ' SALES-TVQ
           		   DISPLAY 'TAXES        : ' SALES-TAX 
           		   DISPLAY '==============================' 
           		   DISPLAY 'TOTAL        : ' SALES-TAXES 
           		END-IF. 
