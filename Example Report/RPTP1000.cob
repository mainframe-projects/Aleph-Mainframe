      *================================================================
      * PROGRAMA    : RPTP1000
      * ANALISTA    : F6015650 JULIO CESAR TORRES DOS SANTOS
      * PROGRAMADOR : F6015650 JULIO CESAR TORRES DOS SANTOS
      * SISTEMA     : ICD -INFRA-ESTRUTURA DE CERTIFICACAO DIGITAL
      * TIPO OBJETO : PROGRAMA COBOL/CICS/DB2
      * LINGUAGEM   : COBOL II
      * DATA        : 09/11/2014
      * AMBIENTE    : ON LINE/BATCH
      * FINALIDADE  : EXEMPLO DE RELATORIO EM COBOL. CAPITULO 3 DO 
      *               LIVRO DO MURACH. 
      *================================================================
      * VRS AUTOR             ALTERACAO                     DATA
      * ---------------------------------------------------------------
      * 001 F6015650-JULIO    IMPLANTACAO                   09/11/2014
      *================================================================
      *
       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.  RPTP1000.
      *AUTHOR. JULIO.C.
      *DATE-WRITTEN. NOV/2014.
      *DATE-COMPILED. 
      *
       ENVIRONMENT DIVISION.
      *
       INPUT-OUTPUT SECTION. 
      *
       FILE-CONTROL. 
      * 
       SELECT CUSTMAST ASSIGN TO 'CUSTMAST.TXT'
           ORGANIZATION IS LINE SEQUENTIAL. 
       SELECT SALESRPT ASSIGN TO 'SALESRPT.TXT' 
           ORGANIZATION IS LINE SEQUENTIAL. 
      *
       DATA DIVISION. 
      *
       FILE SECTION. 
      *
       FD  CUSTMAST. 
       01  CUSTOMER-MASTER-RECORD. 
           03  CM-BRANCH-NUMBER        PIC 9(002). 
           03  CM-SALESREP-NUMBER      PIC 9(002). 
           03  CM-CUSTOMER-NUMBER      PIC 9(005). 
           03  CM-CUSTOMER-NAME        PIC X(020). 
           03  CM-SALES-THIS-YTD       PIC S9(005)V9(002). 
           03  CM-SALES-LAST-YTD       PIC S9(005)V9(002). 
      *     
       FD  SALESRPT. 
       01  PRINT-AREA                  PIC X(132). 
      *
       WORKING-STORAGE SECTION. 
      *
       77  RPTP1000                    PIC X(008) VALUE 'RPTP1000'. 
      *
       01  SWITCHES. 
           03  CUSTMAST-EOF-SWITCH     PIC X(001) VALUE 'N'.
      *    
       01  PRINT-FIELDS. 
           03  PAGE-COUNT              PIC S9(003) VALUE ZEROS. 
           03  LINES-ON-PAGE           PIC S9(003) VALUE +55. 
           03  LINE-COUNT              PIC S9(003) VALUE +99. 
           03  SPACE-CONTROL           PIC S9. 
      *    
       01  TOTAL-FIELDS. 
    	   03  GRAND-TOTAL-THIS-YTD    PIC S9(007)V99. 
    	   03  GRAND-TOTAL-LAST-YTD    PIC S9(007)V99. 
      *   
       01  CURRENT-DATE-AND-TIME. 
           03  CD-YEAR                 PIC 9999. 
           03  CD-MONTH                PIC 99. 
           03  CD-DAY                  PIC 99. 
           03  CD-HOURS                PIC 99. 
           03  CD-MINUTES              PIC 99. 
           03  FILLER                  PIC X(009). 
      *
       01  HEADING-LINE-1. 
           03  FILLER                  PIC X(007) VALUE 'DATE:  '. 
           03  HL1-MONTH               PIC 9(002). 
           03  FILLER                  PIC X(001) VALUE '/'. 
           03  HL1-DAY                 PIC 9(002). 
           03  FILLER                  PIC X(001) VALUE '/'. 
           03  HL1-YEAR                PIC 9(004). 
           03  FILLER                  PIC X(011) VALUE SPACES. 
           03  FILLER          PIC X(020) VALUE 'YEAR-TO DATE SALES R'.
           03  FILLER          PIC X(020) VALUE 'EPORT               '. 
           03  FILLER                  PIC X(008) VALUE '  PAGE: '. 
           03  HL1-PAGE-NUMBER         PIC ZZZ9. 
           03  FILLER                  PIC X(052) VALUE SPACES. 
      *    
       01  HEADING-LINE-2. 
           03  FILLER                  PIC X(007) VALUE 'TIME:  '. 
           03  HL2-HOURS               PIC 9(002). 
           03  FILLER                  PIC X(001) VALUE ':'. 
           03  HL2-MINUTES             PIC 9(002). 
           03  FILLER                  PIC X(058) VALUE SPACES. 
           03  FILLER                  PIC X(010) VALUE 'RPTP1000'. 
           03  FILLER                  PIC X(058) VALUE SPACES. 
      *    
       01  HEADING-LINE-3. 
           03  FILLER          PIC X(020) VALUE 'CUST                '. 
           03  FILLER          PIC X(020) VALUE '            SALES   '.
           03  FILLER          PIC X(020) VALUE '      SALES         '.       
           03  FILLER          PIC X(72)  VALUE SPACES. 
      *
       01  HEADING-LINE-4.  
           03  FILLER          PIC X(020) VALUE 'NUM    CUSTOMER NAME'. 
           03  FILLER          PIC X(020) VALUE '           THIS YTD '. 
           03  FILLER          PIC X(020) VALUE '     LAST YTD       '. 
           03  FILLER          PIC X(020) VALUE SPACES. 
      *     
       01  CUSTOMER-LINE. 
           03  CL-CUSTOMER-NUMBER      PIC 9(005). 
           03  FILLER                  PIC X(002) VALUE SPACES. 
           03  CL-CUSTOMER-NAME        PIC X(020). 
           03  FILLER                  PIC X(003) VALUE SPACES. 
           03  CL-SALES-THIS-YTD       PIC ZZ,ZZ9.99-. 
           03  FILLER                  PIC X(004) VALUE SPACES. 
           03  CL-SALES-LAST-YTD       PIC ZZ,ZZ9.99-. 
           03  FILLER                  PIC X(078) VALUE SPACES. 
      *   
       01  GRAND-TOTAL-LINE. 
           03  FILLER                  PIC X(27) VALUE SPACES. 
           03  GTL-SALES-THIS-YTD      PIC Z,ZZZ,ZZ9.99-. 
           03  FILLER                  PIC X(001) VALUE SPACES. 
           03  GTL-SALES-LAST-YTD      PIC Z,ZZZ,ZZ9.99-. 
           03  FILLER                  PIC X(078). 
      *    
       PROCEDURE DIVISION. 
      * 
       0000-PREPARE-SALES-REPORT. 
       
           OPEN INPUT  CUSTMAST
                OUTPUT SALESRPT. 
        
           PERFORM 100000-FORMAT-REPORT-HEADING. 
           PERFORM 200000-PREPARE-SALES-LINES  
               UNTIL CUSTMAST-EOF-SWITCH = 'Y'.
           PERFORM 300000-PRINT-GRAND-TOTALS. 
           
           CLOSE CUSTMAST 
                 SALESRPT.
           DISPLAY 'Arquivo de saida SALESRPT.TXT gerado. '.
           STOP RUN. 
      *     
       100000-FORMAT-REPORT-HEADING. 
           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-AND-TIME. 
           MOVE CD-MONTH               TO HL1-MONTH. 
           MOVE CD-DAY                 TO HL1-DAY. 
           MOVE CD-YEAR                TO HL1-YEAR. 
           MOVE CD-HOURS               TO HL2-HOURS. 
           MOVE CD-MINUTES             TO HL2-MINUTES. 
      *     
       200000-PREPARE-SALES-LINES. 
           PERFORM 210000-READ-CUSTOMER-RECORD.
           IF CUSTMAST-EOF-SWITCH = 'N'
               PERFORM 220000-PRINT-CUSTOMER-LINE
           END-IF. 
      *
       210000-READ-CUSTOMER-RECORD. 
           READ CUSTMAST 
               AT END 
                   MOVE 'Y'            TO CUSTMAST-EOF-SWITCH. 
      *
       220000-PRINT-CUSTOMER-LINE. 
           IF LINE-COUNT >= LINES-ON-PAGE 
               PERFORM 230000-PRINT-HEADING-LINES 
           END-IF. 
           MOVE CM-CUSTOMER-NUMBER     TO CL-CUSTOMER-NUMBER. 
           MOVE CM-CUSTOMER-NAME       TO CL-CUSTOMER-NAME. 
           MOVE CM-SALES-THIS-YTD      TO CL-SALES-THIS-YTD. 
           MOVE CM-SALES-LAST-YTD      TO CL-SALES-LAST-YTD. 
           MOVE CUSTOMER-LINE          TO PRINT-AREA. 
           WRITE PRINT-AREA AFTER ADVANCING SPACE-CONTROL LINES. 
           ADD 1 TO LINE-COUNT. 
           ADD CM-SALES-THIS-YTD       TO GRAND-TOTAL-THIS-YTD. 
           ADD CM-SALES-LAST-YTD       TO GRAND-TOTAL-LAST-YTD. 
           MOVE 1                      TO SPACE-CONTROL. 
      *
       230000-PRINT-HEADING-LINES. 
           ADD +1                      TO PAGE-COUNT. 
           MOVE PAGE-COUNT             TO HL1-PAGE-NUMBER.
           MOVE HEADING-LINE-1         TO PRINT-AREA. 
           WRITE PRINT-AREA AFTER ADVANCING PAGE. 
           MOVE HEADING-LINE-2         TO PRINT-AREA. 
           WRITE PRINT-AREA AFTER ADVANCING 1 LINE. 
           MOVE HEADING-LINE-3         TO PRINT-AREA. 
           WRITE PRINT-AREA AFTER ADVANCING 2 LINES.
           MOVE HEADING-LINE-4         TO PRINT-AREA. 
           WRITE PRINT-AREA AFTER ADVANCING 1 LINES. 
           MOVE ZERO                   TO LINE-COUNT. 
           MOVE 2 TO SPACE-CONTROL. 
      *
       300000-PRINT-GRAND-TOTALS.  
           MOVE GRAND-TOTAL-THIS-YTD   TO GTL-SALES-THIS-YTD. 
           MOVE GRAND-TOTAL-LAST-YTD   TO GTL-SALES-LAST-YTD. 
           MOVE GRAND-TOTAL-LINE       TO PRINT-AREA. 
           WRITE PRINT-AREA AFTER ADVANCING 2 LINES. 
      *     
      *================================================================
      *       F I M   D O   P R O G R A M A   R P T P 1 0 0 0         *
      *================================================================
