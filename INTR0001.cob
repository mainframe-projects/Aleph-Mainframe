      *-----------------------------------------------------------------
      * PROGRAMA  : INTR0001
      * ANALISTA  : JULIO CESAR TORRES DOS SANTOS 
      * AUTOR     : JULIO CESAR TORRES 
      * OBJETIVO  : CALCUTATE THE COMPOUND INTEREST RATE
      * COMPILACAO: COBOL II 
      *-----------------------------------------------------------------
      * VRS001  -  2014-11-07  - IMPLANTACAO
      *-----------------------------------------------------------------
      *
      *------------------------
       IDENTIFICATION DIVISION. 
      *------------------------
      * 
       PROGRAM-ID. INTR0001.
       AUTHOR. JULIO.CESAR.TORRES.
       INSTALLATION. IBM Z/OS 390.  
       DATE-WRITTEN. 2014-11-07. 
       DATE-COMPILED.
      *
      *---------------------
       ENVIRONMENT DIVISION. 
      *---------------------
      * 
      *--------------
       DATA DIVISION.
      *--------------
      *
      *------------------------
       WORKING-STORAGE SECTION. 
      *------------------------
       77  CTE-INICIO-SS               PIC  X(040) VALUE 
           '*** STORAGE SECTION COMECA AQUI ***'. 
       77  CTE-PROG                    PIC  X(018) VALUE 
           '*** INTR0001 ***'.     
       77  CTE-VERS                    PIC  X(006) VALUE 'VRS001'.  
       77  SBVERSAO                    PIC  X(008) VALUE 'SBVERSAO'. 
       01  USER-ENTRIES. 
           03    NUMBER-ENTERED        PIC  9      VALUE 1. 
           03    INVESTMENT-AMOUNT     PIC  99999. 
           03    NUMBER-OF-YEARS       PIC  99. 
           03    YEARLY-INTEREST-RATE  PIC  99V9. 
      * 
       01  WORK-FIELDS. 
           03    FUTURE-VALUE          PIC  9(7)V99. 
           03    YEAR-COUNTER          PIC  999. 
           03    EDITED-FUTURE-VALUE   PIC  Z,ZZZ,ZZZ.99.
      * 
      *-------------------
       PROCEDURE DIVISION.
      *-------------------
      * 
      *------------------------
       000000-ROTINA-PRINCIPAL.
      *------------------------
      * 
           DISPLAY 'SAUDACAO DO COBOL.'. 
      * 
           DISPLAY                        WHEN-COMPILED. 
           MOVE ZEROS                  TO RETURN-CODE.
           PERFORM 100000-CALCULATE-FUTURE-VALUE.  
      * 
      *------------
       000099-EXIT.
      *------------
      * 
      *------------------------------
       100000-CALCULATE-FUTURE-VALUE. 
      *------------------------------
      *
           PERFORM 200000-CALCULATE-FUTURE-VALUES 
               UNTIL NUMBER-ENTERED = ZERO. 
           DISPLAY 'END-OF-SESSION.'. 
           STOP RUN. 
      *------------
       100099-EXIT.
      *------------
      * 
      *-------------------------------
       200000-CALCULATE-FUTURE-VALUES. 
      *-------------------------------
      *
           DISPLAY '-----------------------------------------------'.
           DISPLAY 'To end program, enter 0. '. 
           DISPLAY 'To perform another calculation, enter 1. '. 
           ACCEPT NUMBER-ENTERED. 
           DISPLAY '-----------------------------------------------'
      * 
           IF NUMBER-ENTERED = 1 
               PERFORM 300000-GET-USER-VALUES 
               MOVE INVESTMENT-AMOUNT TO FUTURE-VALUE 
               MOVE 1 TO YEAR-COUNTER 
               PERFORM 400000-CALCULATE-NEXT-FV 
                   UNTIL YEAR-COUNTER > NUMBER-OF-YEARS 
               MOVE FUTURE-VALUE TO EDITED-FUTURE-VALUE
               DISPLAY '=====>>> FUTURE VALUE = ' EDITED-FUTURE-VALUE. 
      *
      *------------
       200099-EXIT.
      *------------
      * 
      *-----------------------
       300000-GET-USER-VALUES. 
      *-----------------------
      *         
           DISPLAY '==> Enter investment amount (xxxxx). '. 
           ACCEPT INVESTMENT-AMOUNT. 
           DISPLAY '==> Enter number of years (xx). '. 
           ACCEPT NUMBER-OF-YEARS. 
           DISPLAY '==> Enter yearly interest rate (xx.x) .'. 
           ACCEPT YEARLY-INTEREST-RATE. 
      * 
      *------------
       300099-EXIT.
      *------------
      * 
      *-------------------------
       400000-CALCULATE-NEXT-FV. 
      *-------------------------
      *
           COMPUTE FUTURE-VALUE ROUNDED = 
               FUTURE-VALUE + 
                   (FUTURE-VALUE * YEARLY-INTEREST-RATE / 100). 
           ADD +1 TO YEAR-COUNTER. 
      * 
      *------------
       400099-EXIT.
      *------------
      * 
      *----------------------------------------------------------------*
      *         F I M   D O   P R O G R A M A   I N T R 0 0 0 1        *
      *----------------------------------------------------------------*
