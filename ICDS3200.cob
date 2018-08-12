      *-----------------------*
       IDENTIFICATION DIVISION.
      *-----------------------*
       PROGRAM-ID.   ICDS3200.
       AUTHOR.       F5127685 JORGE ANDRE
       DATE-WRITTEN. 18/09/2008
       DATE-COMPILED.
      *----------------------------------------------------------------*
      *          ICDS3200 - COBOL II / CICS / SQL - BATCH & ONLINE     *
      *          ( COMPILAR COM SOS 13 - OPCAO 4 )                     *
      *----------------------------------------------------------------*
      * VRS003 24.09.2010 F7120693 - Inclui conversao de base de codifi-
      *                              cacao (b64/bin)
      * VRS002 12.02.2009 F5127685 - Passa a usar a ICDSUTC0 e ICDS7100.
      * VRS001 18.09.2008 F5127685 - IMPLANTACAO.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *  ICD - Infra-estrutura de Certificacao Digital
      *----------------------------------------------------------------*
      *  Acolher Certificados Digitais de Usuario Final.
      *----------------------------------------------------------------*
      *  Chama:
      *    --------
      *    ICDS0100 - Conferência de assinatura (criptograma)
      *    ICDS2100 - Parsing de Certificados X.509
      *    ICDS2200 - Vincular certificado X codigo MCI - Usuario Final
      *    ICDS7100 - Buscar arquivo de LCR e baixar certificado(s)     
      *    ICDS7200 - Baixar certificado(s) revogado(s) na LCR atual
      *    ICDS9100 - Gravar log
      *    --------
      *    ICDS002P - Interface Tabela DB2ICD.IDFR_CHV_ADD_CTFR - CURSOR
      *    ICDS003C - Interface Tabela DB2ICD.CTFD_DGTL         - SELECT
      *    ICDS003I - Interface Tabela DB2ICD.CTFD_DGTL         - INSERT
      *    ICDS004I - Interface Tabela DB2ICD.HST_CTFD_DGTL     - INSERT
      *    ICDS006C - Interface Tabela DB2ICD.URL_LS_CTFD_CNCD  - SELECT
      *    ICDS006I - Interface Tabela DB2ICD.URL_LS_CTFD_CNCD  - INSERT
      *    ICDS006P - Interface Tabela DB2ICD.URL_LS_CTFD_CNCD  - CURSOR
      *    --------
      *    ICDSL03A - Interface Tabela Logica EST_CTFD_DGTL
      *    --------
      *    ICDSUTC0 - Timestamp UTC (Universal Time, Coordinated)       
      *    --------                                                     
      *    SBCALLER - Identifica programa chamador (ou cadeia de progs.)
      *    SBCNVB64 - Converte de/para codificacao base64
      *----------------------------------------------------------------*
      *
      *--------------------*
       ENVIRONMENT DIVISION.
      *--------------------*
      *
      *--------------------------------------*
       CONFIGURATION                  SECTION.
      *--------------------------------------*
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *SOURCE-COMPUTER.                                                 
      *    IBM-390 WITH DEBUGGING MODE.                                 
      *
      *-------------*
       DATA DIVISION.
      *-------------*
      *
      *--------------------------------------*
       WORKING-STORAGE                SECTION.
      *--------------------------------------*
      *
       01  CTE-PRGM                        PIC  X(008) VALUE 'ICDS3200'.
       01  CTE-VERS                        PIC  X(006) VALUE 'VRS003'.
       01  FILLER                REDEFINES CTE-VERS.
           03  FILLER                      PIC  X(003).
           03  CTE-NR-VRS-PGM              PIC  9(003).
       01  CTE-DESL                        PIC S9(009) COMP.
       01  GDA-CD-RTN                      PIC S9(009) COMP.
           88  CD-RTN-FIM-NML                          VALUE ZEROS.
       01  GDA-LIM                         PIC S9(009) COMP.
       01  GDA-TAM                         PIC S9(009) COMP.
      *
       01  GDA-DFHEIBLK                    PIC  X(085) VALUE SPACES.
       01  GDA-RESP                        PIC S9(008) COMP.            
       01  GDA-RESP2                       PIC S9(008) COMP.            
      *
       01  GDA-CD-USU                      PIC  X(008) VALUE SPACES.
       01  NDX                             PIC S9(009) COMP.
       01  KDPY-CD-RTN                     PIC  +9999999999.
       01  KDPY-INTEGER                    PIC  +9999999999.
       01  KDPY-SMALLINT                   PIC  +99999.
       01  KDPY-AUX-SIZE                   PIC  +99999.
      *
      *D01  FIXME-FLG                       PIC  X(003) VALUE SPACES.
      *D   88  FIXME-CTNR-X509                         VALUE  'F01'.
      *D   88  FIXME-OBG-ICP-BR                        VALUE  'F02'.
      *D   88  FIXME-LCR-KEY-ID                        VALUE  'F03'.
      *D   88  FIXME-CHV-PBCO                          VALUE  'F04'.
      *D   88  FIXME-UTZO-CHV                          VALUE  'F05'.
      *D   88  FIXME-TBS-ASS-AC                        VALUE  'F06'.
      *
       01  GDA-OID-TBS.
           49  OID-TBS-SIZE                PIC S9(009) COMP.
           49  OID-TBS-TEXT                PIC  X(032).
       01  GDA-OID-CTNR.
           49  OID-CTNR-SIZE               PIC S9(009) COMP.
           49  OID-CTNR-TEXT               PIC  X(032).
      *
       01  INICIO-DADOS                    PIC  X(02)  VALUE SPACES.
       01  FILLER REDEFINES INICIO-DADOS.
           03 INICIO-DADOS-1               PIC  X(01).
           03 FILLER                       PIC  X(01).
      *
      *----------------------------------------------------------------*
      *    Area da sub-rotina ICDS0100 - Confere assinatura(criptograma)
      *----------------------------------------------------------------*
       01  ICDS0100                        PIC  X(008) VALUE 'ICDS0100'.
      *
       01  S0100-CD-RTN                    PIC S9(009) COMP.
           88  S0100-ERRO-SBICSF12                     VALUE        +18.
      *
       01  S0100-CND-VLD-ASS-DGTL          PIC  X(003).
           88  S0100-LCR-ATZD                          VALUE      '000'.
           88  S0100-LCR-N-ATZD                        VALUE      '001'.
           88  S0100-LCR-N-LCZD                        VALUE      '002'.
           88  S0100-ASS-VLDO                          VALUE      '000'
                                                                  '001'
                                                                  '002'.
           88  S0100-ASS-INVD                          VALUE      '015'.
      *
       01  ICDS0100-DADOS.
-INC   ICDKRTNW                                                         
-INC   ICDK0100                                                         
      *    03  FIXME-ENTD-CHV-PBCO         PIC X(2056).
      *
       01  ICSF-RETURN-CODE-X.
           03  ICSF-RETURN-CODE            PIC S9(009) COMP.
      *
       01  ICSF-REASON-CODE-X.
           03  ICSF-REASON-CODE            PIC S9(009) COMP.
      *
      *----------------------------------------------------------------*
      *    Area da sub-rotina ICDS2100 - Parsing de Certificados X.509
      *----------------------------------------------------------------*
       01  ICDS2100                        PIC  X(008) VALUE 'ICDS2100'.
      *
       01  ICDS2100-DADOS.
-INC   ICDKRTNW                                                         
-INC   ICDK2100                                                         
           03  S2100-SAID-FNC              PIC X(5201).
      *
       01  S2100-SAID-F01.
-INC   ICDK2101                                                         
      *
       01  S2100-SAID-F02.
-INC   ICDK2102                                                         
      *
       01  S2100-SAID-F03.
-INC   ICDK2103                                                         
      *
       01  S2100-SAID-F04.
-INC   ICDK2104                                                         
      *
       01  S2100-SAID-F05.
-INC   ICDK2105                                                         
      *
       01  S2100-SAID-F06.
-INC   ICDK2106                                                         
      *
      *----------------------------------------------------------------*
      *    Area da sub-rotina ICDS2200 - Vincular ao MCI - Usuario Final
      *----------------------------------------------------------------*
       01  ICDS2200                        PIC  X(008) VALUE 'ICDS2200'.
      *
       01  S2200-CD-RTN                    PIC S9(009) COMP.
           88  S2200-FIM-NML                           VALUE      ZEROS.
      *
       01  ICDS2200-DADOS.
-INC   ICDKRTNW                                                         
-INC   ICDK2200                                                         
      *
      *----------------------------------------------------------------*
      *    Area da sub-rotina ICDS7100 - Buscar LCR e baixar certificado
      *----------------------------------------------------------------*
       01  ICDS7100                        PIC  X(008) VALUE 'ICDS7100'.
      *                                                                 
       01  ICDS7100-DADOS.                                              
-INC   ICDKRTNW                                                         
-INC   ICDK7100                                                         
      *                                                                 
       01  S7100-CND-EXT-URL               PIC  X(001) VALUE SPACES.    
           88  S7100-URL-EXTT                          VALUE SPACES.    
           88  S7100-URL-NOVO                          VALUE 'N'.       
      *                                                                 
      *----------------------------------------------------------------*
      *    Area da sub-rotina ICDS7200 - Baixar certificados revogados
      *----------------------------------------------------------------*
       01  ICDS7200                        PIC  X(008) VALUE 'ICDS7200'.
      *
       01  ICDS7200-DADOS.
-INC   ICDKRTNW                                                         
-INC   ICDK7200                                                         
      *
      *----------------------------------------------------------------*
      *    Area da sub-rotina ICDS9100 - Gravacao de log do sistema
      *----------------------------------------------------------------*
       01  ICDS9100                        PIC  X(008) VALUE 'ICDS9100'.
      *
       01  ICDS9100-DADOS.
-INC   ICDKRTNW                                                         
-INC   ICDK9100                                                         
      *
      *----------------------------------------------------------------*
      *    Area da sub-rotina ICDS002P - Tabela DB2ICD.IDFR_CHV_ADD_CTFR
      *----------------------------------------------------------------*
       01  ICDS002P                        PIC  X(008) VALUE 'ICDS002P'.
      *
       01  ICDS002W-DADOS.
-INC   ICDKRTNW                                                         
-INC   ICDK002W                                                         
      *
       01  GDA-IN-FIM-002P                 PIC  9(001) VALUE ZERO.
           88  IN-FIM-002P                             VALUE 1.
      *
       01  ICDS002P-DADOS.
           03  P002-QT-REG                        PIC S9(009) COMP.
           03  P002-IN-FIM                        PIC  9(001).
           03  P002-TABELA                        OCCURS  200.
               05  P002-CD-ADD-CTFR               PIC S9(0009) COMP.
               05  P002-CD-NR-SRE-CTFD            PIC  X(0016).
               05  P002-CD-ADD-CTFR-DTTR          PIC S9(0009) COMP.
               05  P002-TX-IDFR-CHV-DTTR.
                   49  P002-IDFR-CHV-SIZE         PIC S9(0004) COMP.
                   49  P002-IDFR-CHV-TEXT         PIC  X(0512).
      *
      *----------------------------------------------------------------*
      *    Area da sub-rotina ICDS003* - Tabela DB2ICD.CTFD_DGTL
      *----------------------------------------------------------------*
       01  ICDS003C                        PIC  X(008) VALUE 'ICDS003C'.
       01  ICDS003I                        PIC  X(008) VALUE 'ICDS003I'.
      *
       01  ICDS003W-DADOS.
-INC   ICDKRTNW                                                         
-INC   ICDK003W                                                         
      *
      *----------------------------------------------------------------*
      *    Area da sub-rotina ICDS004* - Tabela DB2ICD.HST_CTFD_DGTL
      *----------------------------------------------------------------*
       01  ICDS004I                        PIC  X(008) VALUE 'ICDS004I'.
      *
       01  ICDS004W-DADOS.
-INC   ICDKRTNW                                                         
-INC   ICDK004W                                                         
      *
      *----------------------------------------------------------------*
      *    Area da sub-rotina ICDS006* - Tabela DB2ICD.URL_LS_CTFD_CNCD
      *----------------------------------------------------------------*
       01  ICDS006C                        PIC  X(008) VALUE 'ICDS006C'.
       01  ICDS006I                        PIC  X(008) VALUE 'ICDS006I'.
       01  ICDS006P                        PIC  X(008) VALUE 'ICDS006P'.
      *
       01  ICDS006W-DADOS.
-INC   ICDKRTNW                                                         
-INC   ICDK006W                                                         
      *
       01  GDA-IN-FIM-006P                 PIC  9(001) VALUE ZERO.
           88  IN-FIM-006P                             VALUE 1.
      *
       01  ICDS006P-DADOS.
           03  P006-QT-REG                 PIC  9(009) COMP.
           03  P006-IN-FIM                 PIC  9(001).
           03  P006-TABELA                 OCCURS   70.
               05  P006-CD-ADD-CTFR        PIC S9(009) COMP.
               05  P006-CD-URL-LS-CNCD     PIC S9(004) COMP.
               05  P006-CD-EST-LS-CNCD     PIC  X(001).
               05  P006-TS-EST-LS-CNCD     PIC  X(026).
               05  P006-TX-URL-LS-CNCD.
                   49  P006-TX-URL-LS-SIZE PIC S9(004) COMP.
                   49  P006-TX-URL-LS-TEXT PIC  X(256).
      *
      *----------------------------------------------------------------*
      *    Area da sub-rotina ICDSUTC0 - Busca (TIMESTAMP-TIMEZONE) no  
      *    DB-2 para uso de timestamp UTC (Universal Time, Coordinated) 
      *----------------------------------------------------------------*
       01  ICDSUTC0                        PIC  X(008) VALUE 'ICDSUTC0'.
      *                                                                 
       01  ICDSUTCW-DADOS.                                              
-INC   ICDKRTNW                                                         
-INC   ICDKUTCW                                                         
      *                                                                 
      *----------------------------------------------------------------*
      *    Area da sub-rotina ICDSL03A - Logica EST_CTFD_DGTL
      *----------------------------------------------------------------*
       01  ICDSL03A                        PIC  X(008) VALUE 'ICDSL03A'.
      *
       01  ICDSL03A-DADOS.
-INC   ICDKRTNW                                                         
-INC   ICDKL03A                                                         
      *
      *----------------------------------------------------------------*
      *    Area da sub-rotina SBCALLER - Cadeia de ativacao do programa
      *----------------------------------------------------------------*
-INC   ICDKPGMW                                                         
      *
      *----------------------------------------------------------------*
      *    Area das sub-rotinas de conversao:
      *    SBCNVB64, SBCNVCOD, SBCNVHEX, SBCNVOID e SBCNVUNI
      *----------------------------------------------------------------*
-INC   ICDKCNVW                                                         
      *
      *----------------------------------------------------------------*
      *    OID (Identificadores de Objetos)
      *----------------------------------------------------------------*
-INC   ICDKOIDW                                                         
      *
      *--------------------------------------*
       LOCAL-STORAGE                  SECTION.
      *--------------------------------------*
      *
      *----------------------------------------------------------------*
      *    Books acesso tabelas DB2
      *----------------------------------------------------------------*
-INC   ICDK002D                                                         
      *
-INC   ICDK006D                                                         
      *
      *----------------------------------------------------------------*
      *    Definicao do cursor da tabela DB2ICD.IDFR_CHV_ADD_CTFR
      *----------------------------------------------------------------*
           EXEC SQL
                DECLARE  IDFR-CHV-ADD CURSOR FOR
                 SELECT  CD_ADD_CTFR
                      ,  CD_NR_SRE_CTFD
                      ,  CD_ADD_CTFR_DTTR
                      ,  TX_IDFR_CHV_DTTR
                   FROM  DB2ICD.IDFR_CHV_ADD_CTFR
                  WHERE  CD_ADD_CTFR_DTTR = :K002-CD-ADD-CTFR-DTTR
      *----------------------------------------------------------------*
      *    FIXME [BEGIN]
      *----------------------------------------------------------------*
                    AND  TX_IDFR_CHV_DTTR = :K002-TX-IDFR-CHV-DTTR
      *----------------------------------------------------------------*
      *    FIXME [END]
      *----------------------------------------------------------------*
           END-EXEC.
      *
      *----------------------------------------------------------------*
      *    Definicao do cursor da tabela DB2ICD.URL_LS_CTFD_CNCD
      *----------------------------------------------------------------*
           EXEC SQL
                DECLARE  LS-CTFD-CNCD CURSOR FOR
                 SELECT  CD_ADD_CTFR
                      ,  CD_URL_LS_CNCD
                      ,  CD_EST_LS_CNCD
                      ,  TS_EST_LS_CNCD
                      ,  TX_URL_LS_CNCD
                   FROM  DB2ICD.URL_LS_CTFD_CNCD
                  WHERE  CD_ADD_CTFR    = :K006-CD-ADD-CTFR
                    AND  TX_URL_LS_CNCD = :K006-TX-URL-LS-CNCD
           END-EXEC.
      *
      *----------------------------------------------------------------*
      *    O erro de SQL sera guardado na SQLCA
      *----------------------------------------------------------------*
      *
           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.
      *
      *----------------------------------------------------------------*
      *    Variaveis da linkage
      *----------------------------------------------------------------*
      *
       01  GDA-LKS-ENTD.
-INC   ICDKRTNW                                                         
-INC   ICDK3200                                                         
      *
      *--------------------------------------*
       LINKAGE                        SECTION.
      *--------------------------------------*
      *
       01  DFHCOMMAREA.
           03  LKS-BOOK-ENTD.
               05  LKS-BOOK-RTNW      PIC X(0110).
               05  LKS-BOOK-3200      PIC X(4126).
      *
      *--------------------------------------*
       PROCEDURE DIVISION   USING DFHCOMMAREA.
      *--------------------------------------*
      *
      *--------------------------------------*
       000000-PRINCIPAL               SECTION.
      *--------------------------------------*
      D    DISPLAY '000 ' CTE-PRGM ' - 000000-PRINCIPAL              '.
      *
      D    DISPLAY '000 ' CTE-PRGM ' - LKS-BOOK-RTNW                 '.
      D    DISPLAY '(' LKS-BOOK-RTNW ')'.
      D    DISPLAY '000 ' CTE-PRGM ' - LKS-BOOK-3200                 '.
      *D   DISPLAY '(' LKS-BOOK-3200 ')'.
      D    DISPLAY '(' LKS-BOOK-3200 (1:5) ')'.
      *
           PERFORM 906100-SBCALLER-F01.
           PERFORM 906900-SBCALLER-TX-RTN.
           MOVE CALLER-NM-PGM TO CALLER-NM-PGM-RSP.
      *
           PERFORM 906000-SBCALLER.
      *
      D    DISPLAY CALLER-FUC ' ' SBCALLER ' Ret-Code = ' CALLER-CD-RTN.
      D    DISPLAY '    ' CALLER-TX-RTN-SIZE '(' CALLER-TX-RTN-TEXT ')'.
      D    DISPLAY '    Programa Responsavel: (' CALLER-NM-PGM-RSP  ')'.
      D    DISPLAY '    Sistema  Responsavel: (' CALLER-SG-SIS-RSP  ')'.
      D    DISPLAY '    Programa Chamador   : (' CALLER-NM-PGM-CHMR ')'.
      D    DISPLAY '    Programas Chamadores: (' CALLER-LS-PGM-CHMR ')'.
      *
      *--------------------------------------*
      *    Identifica usuario responsavel.
      *--------------------------------------*
      D    DISPLAY '000 ' CTE-PRGM ' - Identifica usuario responsavel'.
      *
      *D   PERFORM 906300-SBCALLER-F03.
      *D   PERFORM 906900-SBCALLER-TX-RTN.
      *
      D    EVALUATE TRUE
      D       WHEN CALLER-AMB-BATCH
      D            DISPLAY '000 ' CTE-PRGM ' - SBCALLER - Batch'
      D       WHEN CALLER-AMB-CICS
      D            DISPLAY '000 ' CTE-PRGM ' - SBCALLER - CICS'
      D       WHEN CALLER-AMB-GRI
      D            DISPLAY '000 ' CTE-PRGM ' - SBCALLER - GRI'
      D       WHEN CALLER-AMB-DB2-SQL
      D            DISPLAY '000 ' CTE-PRGM ' - SBCALLER - DB2/SQL'
      D       WHEN CALLER-AMB-NTRL-BCH
      D            DISPLAY '000 ' CTE-PRGM ' - SBCALLER - Natural/Batch'
      D    END-EVALUATE.
      *
           IF  CALLER-AMB-CICS
               EXEC CICS
                    ASSIGN USERID (GDA-CD-USU)
               END-EXEC
           ELSE
               MOVE 'ctm-user'  TO GDA-CD-USU
           END-IF.
      *
      D    DISPLAY '000 ' CTE-PRGM ' - UserId: (' GDA-CD-USU ')'.
      *
      *--------------------------------------*
      *    Inicializa log de erro.
      *--------------------------------------*
      D    DISPLAY '000 ' CTE-PRGM ' - Inicializa log de erro'.
      *
           MOVE CALLER-NM-PGM-CHMR TO S9100-NM-PGM-CHMR.
           MOVE CTE-NR-VRS-PGM     TO S9100-NR-VRS-PGM.
      *
      *--------------------------------------*
      *    Rotina principal.
      *--------------------------------------*
      D    DISPLAY '000 ' CTE-PRGM ' - Rotina principal'.
      *
           PERFORM 100000-VALIDAR-LINKAGE.
      *
      D    DISPLAY '000 ' CTE-PRGM ' - Inicializa book de retorno'.
      *
           SET  CD-RTN-FIM-NML TO TRUE.
           MOVE GDA-CD-RTN     TO KRTN-CD-RTN OF GDA-LKS-ENTD.
           MOVE SPACES         TO S3200-SAID.
      *
      D    DISPLAY '000 ' CTE-PRGM ' - Processa certificado digital'
      D                              ' de Usuario Final'.
      *
           PERFORM 200000-EXTRAI-DADOS-CTFD-DGTL.
           PERFORM 300000-VERIFICA-CADM-ANT.
           IF  S3200-SAID-CD-EST-ACLT EQUAL SPACES
               PERFORM 400000-VERIFICA-BASC-X509
               PERFORM 500000-VERIFICA-SEQ-CTFC
               PERFORM 600000-ARMAZENA-CTFD-USU-FIM
           END-IF.
           PERFORM 700000-FINALIZA.
      *
           MOVE S3200-SAID  TO KL03-TAB(1).
           PERFORM 812000-EXECUTA-ICDSL03A.
           MOVE KL03-TAB(1) TO S3200-SAID.
      *
      D    DISPLAY '000 ' CTE-PRGM ' - Prepara dados para retorno'.
      *
           MOVE 'SUCESSO'    TO KRTN-TX-LVRE    OF GDA-LKS-ENTD.
      D    DISPLAY '000 ' CTE-PRGM ' - TX-LVRE.: ' KRTN-TX-LVRE
      D                                            OF GDA-LKS-ENTD.
      *
      D    DISPLAY '000 ' CTE-PRGM ' - S3200-SAID  ('
      D                                S3200-SAID-CD-EST-ACLT       '-'
      D                                S3200-SAID-TX-EST-ACLT       ')'.
      D    DISPLAY '000 ' CTE-PRGM ' - RETURN-CODE (' GDA-CD-RTN    ')'.
      *
           MOVE GDA-CD-RTN   TO KRTN-CD-RTN   OF GDA-LKS-ENTD.
           MOVE GDA-LKS-ENTD TO LKS-BOOK-ENTD.
           MOVE KRTN-CD-RTN  OF GDA-LKS-ENTD  TO RETURN-CODE.
      *
       000099-FINAL.
           GOBACK.
      *
      *--------------------------------------*
       100000-VALIDAR-LINKAGE         SECTION.
      *--------------------------------------*
      D    DISPLAY '000 ' CTE-PRGM ' - 100000-VALIDAR-LINKAGE        '.
      *
           IF  DFHEIBLK     NOT EQUAL     SPACES
           AND EIBCALEN GREATER LENGTH OF DFHCOMMAREA
               MOVE 'EIBCALEN' TO S9100-CD-LCZC-ERRO-FON
               GO TO 999000-ERRO-001
           END-IF.
      *
           MOVE LKS-BOOK-ENTD TO GDA-LKS-ENTD.
      *
      *----------------------------------------------------------------*
      D    DISPLAY '000 ' CTE-PRGM ' - DFHCOMMAREA.                 '.
      D    DISPLAY 'KRTN-CFG        (' KRTN-CFG     OF GDA-LKS-ENTD ')'.
      D    DISPLAY 'KRTN-PRM-RTN    (' KRTN-PRM-RTN OF GDA-LKS-ENTD ')'.
      D    DISPLAY 'IDFR-VRS-PRM    (' S3200-ENTD-IDFR-VRS-PRM      ')'.
      D    DISPLAY 'IDFR-SEQ-CTFC   (' S3200-CD-IDFR-SEQ-CTFC       ')'.
      *D   DISPLAY 'TX-CTFD-DGTL    (' S3200-TX-CTFD-DGTL-SIZE
      *D                           ':' S3200-TX-CTFD-DGTL-TEXT
      *D                            (1:S3200-TX-CTFD-DGTL-SIZE )
      *D                           ')'.
      *----------------------------------------------------------------*
      *
      D    DISPLAY '000 ' CTE-PRGM ' - Valida versao da API (S3200)   '.
      *
           IF  NOT S3200-IDFR-VRS-PRM-ATU-OK
               MOVE 'VLDR-LKS' TO S9100-CD-LCZC-ERRO-FON
               GO TO 999000-ERRO-002
           END-IF.
      *
      D    DISPLAY '000 ' CTE-PRGM ' - Valida tamanho do certificado  '.
      *
      D    DISPLAY '000 ' CTE-PRGM ' - Antes do INSPECT : SIZE = '
      D                                S3200-TX-CTFD-DGTL-SIZE.
      *
           IF          S3200-TX-CTFD-DGTL-SIZE EQUAL ZEROS
               INSPECT S3200-TX-CTFD-DGTL-TEXT TALLYING
                       S3200-TX-CTFD-DGTL-SIZE
                   FOR CHARACTERS BEFORE '        '
           END-IF.
      *
      D    DISPLAY '000 ' CTE-PRGM ' - Depois do INSPECT             '.
      D    DISPLAY 'TAM-CTFD-DGTL   (' S3200-TX-CTFD-DGTL-SIZE      ')'.
      *
      *----------------------------------------------------------
      * Verifica formato do Certificado e converte para B64.
      *----------------------------------------------------------

           DISPLAY CTE-PRGM ' - INCIO DA SECTION DE CONVERSAO'
           MOVE S3200-TX-CTFD-DGTL-TEXT(1:2)      TO INICIO-DADOS.
           DISPLAY CTE-PRGM ' - INICIO-DADOS: ' INICIO-DADOS.

           DISPLAY CTE-PRGM ' - INICIO DO EVALUATE.'

           IF  INICIO-DADOS EQUAL X'4D49'
               DISPLAY '100000 RECEBIDO CTFD B64 ASCII'
               PERFORM 101000-CONVERTE-ASCII-EBCDIC
               MOVE CNVCOD-DADO-SIZE      TO S3200-TX-CTFD-DGTL-SIZE
               MOVE CNVCOD-DADO-TEXT      TO S3200-TX-CTFD-DGTL-TEXT
           ELSE
               IF INICIO-DADOS EQUAL 'MI'
                   DISPLAY '100000 RECEBIDO CTFD B64 EBCDIC'
                   CONTINUE
               ELSE
                   IF INICIO-DADOS-1 NOT EQUAL X'30'
                       DISPLAY '100000 RECEBIDO CTFD FMTO DESCONHECIDO'
                       MOVE 'VLDR-LKS' TO S9100-CD-LCZC-ERRO-FON
                       GO TO 999000-ERRO-016
                   ELSE
                       DISPLAY '100000 RECEBIDO CTFD BINARIO'
                       DISPLAY 'ANTES DE CHAMAR SECTION CONVERSAO'
                       PERFORM 102000-CONVERTE-BIN-EM-B64
                       DISPLAY 'CNVB64-SAID-SIZE: ' CNVB64-SAID-SIZE
                       DISPLAY 'CNVB64-SAID-TEXT: ' CNVB64-SAID-TEXT

                       DISPLAY ' ANTES DA MOVIMENTACAO SB64 -> S3200'
                       MOVE CNVB64-SAID-SIZE  TO S3200-TX-CTFD-DGTL-SIZE
                       MOVE CNVB64-SAID-TEXT  TO S3200-TX-CTFD-DGTL-TEXT
                   END-IF
               END-IF
           END-IF.

      *    EVALUATE INICIO-DADOS

      *        FORMATO BASE64 EM EBCDIC
      *        ---------------------------
      *        WHEN 'MI'
      *            DISPLAY '100000 RECEBIDO CTFD B64 EBCDIC'
      *            CONTINUE

      *        FORMATO BASE64 EM ASCII
      *        ---------------------------
      *        WHEN X'4D49'
      *        DISPLAY '100000 RECEBIDO CTFD B64 ASCII'
      *        PERFORM 101000-CONVERTE-ASCII-EBCDIC
      *        MOVE CNVCOD-DADO-SIZE      TO S3200-TX-CTFD-DGTL-SIZE
      *        MOVE CNVCOD-DADO-TEXT      TO S3200-TX-CTFD-DGTL-TEXT

      *        FORMATO BINARIO
      *        ---------------------------
      *        WHEN X'30'
      *            DISPLAY '100000 RECEBIDO CTFD BINARIO'
      *            PERFORM 102000-CONVERTE-BIN-EM-B64
      *            MOVE CNVB64-SAID-SIZE      TO S3200-TX-CTFD-DGTL-SIZE
      *            MOVE CNVB64-SAID-TEXT      TO S3200-TX-CTFD-DGTL-TEXT

      *        FORMATO DESCONHECIDO - ERRO
      *        -----------------------------
      *        WHEN OTHER
      *            DISPLAY '100000 RECEBIDO CTFD FORMATO DESCONHECIDO'
      *            MOVE 'VLDR-LKS' TO S9100-CD-LCZC-ERRO-FON
      *            GO TO 999000-ERRO-016

      *    END-EVALUATE.

           DISPLAY CTE-PRGM ' - FIM DO EVALUATE.'

           MOVE S3200-TX-CTFD-DGTL-SIZE           TO GDA-TAM.
           MOVE LENGTH OF S3200-TX-CTFD-DGTL-TEXT TO GDA-LIM.
      *
           IF  GDA-TAM    LESS THAN ZEROS
               MOVE 'VLDR-LKS' TO S9100-CD-LCZC-ERRO-FON
               GO TO 999000-ERRO-003
           END-IF.
      *
           IF  GDA-TAM GREATER THAN GDA-LIM
               MOVE 'VLDR-LKS' TO S9100-CD-LCZC-ERRO-FON
               GO TO 999000-ERRO-003
           END-IF.
      *
      D    DISPLAY '000 ' CTE-PRGM ' - Valida Base64 do certificado'.
      *
           MOVE S3200-TX-CTFD-DGTL-SIZE TO WCNV-TAM-CMP-TAB.
           MOVE S3200-TX-CTFD-DGTL-TEXT TO WCNV-CMP-TAB.
      *
      D    DISPLAY '000 ' CTE-PRGM ' - WCNV-TAB-VALIDA-ENCODE.        '.
      D    DISPLAY 'TAM-CMP-TAB     (' WCNV-TAM-CMP-TAB             ')'.
      *D   DISPLAY 'CMP-TAB         (' WCNV-CMP-TAB                 ')'.
      *
           PERFORM 901100-VALIDA-BASE64.
      *
           IF  WCNV-ENCODE-IS-NOT-VALID
               MOVE 'VLDR-LKS' TO S9100-CD-LCZC-ERRO-FON
               GO TO 999000-ERRO-004
           END-IF.
      *----------*
       100099-SAI.
      *----------*
           EXIT.
      *
      *----------------------------------------*
       101000-CONVERTE-ASCII-EBCDIC     SECTION.
      *----------------------------------------*
      *
      D    DISPLAY '000 ' CTE-PRGM ' -102000-CONVERTE-ASCII-EM-EBCDIC'.
      *

           MOVE S3200-TX-CTFD-DGTL-SIZE        TO CNVCOD-DADO-SIZE.
           MOVE S3200-TX-CTFD-DGTL-TEXT        TO CNVCOD-DADO-TEXT.
           MOVE 'F01'                          TO CNVCOD-FUC.
           MOVE 4                              TO CNVCOD-TAB-CNV.

      *    DISPLAY CTE-PRGM ' - CALL SBCNVCOD USING: '
      *    DISPLAY 'FUC: ' CNVCOD-FUC
      *    DISPLAY 'TAB: ' CNVCOD-TAB-CNV
      *    DISPLAY 'DADO-SIZE: ' CNVCOD-DADO-SIZE
      *    DISPLAY 'DADO-TEXT: ' CNVCOD-DADO-TEXT(1:CNVCOD-DADO-SIZE)

           CALL SBCNVCOD USING CNVCOD-FUC
                               CNVCOD-TAB-CNV
                               CNVCOD-DADO-SIZE
                               CNVCOD-DADO-TEXT
      *
      *    DISPLAY CTE-PRGM ' - SAIDA DA SUBROTINA'
      *    DISPLAY 'CD-RTN: ' CNVCOD-CD-RTN
      *    DISPLAY 'DADO-SIZE: ' CNVCOD-DADO-SIZE
      *    DISPLAY 'DADO-TEXT: ' CNVCOD-DADO-TEXT(1:CNVCOD-DADO-SIZE)

           IF CNVCOD-CD-RTN EQUAL +0
      D      DISPLAY '000 ' CTE-PRGM ' - CONV. ASCII EM EBCDIC OK'
             CONTINUE
      *
           ELSE
      D      DISPLAY '000 ' CTE-PRGM ' -ERRO NA CONV. ASCII EM EBCDIC'
             PERFORM 999000-ERRO-017

           END-IF.

      *    DISPLAY CTE-PRGM ' - SBCNVCOD: ' RETURN-CODE.

      *    DISPLAY CTE-PRGM ' - FIM DA SECTION CONVERTE-ASCII-EBCDIC'.
      *
      *----------*
       101999-SAI.
      *----------*
            EXIT.
      *
      *----------------------------------------*
       102000-CONVERTE-BIN-EM-B64      SECTION.
      *----------------------------------------*
      *
           DISPLAY '000 ' CTE-PRGM ' - 102000-CONVERTE-BIN-EM-B64'.
      *
           DISPLAY '===> 1 '
           DISPLAY 'S3200-CTFD-SIZE: ' S3200-TX-CTFD-DGTL-SIZE.
           DISPLAY 'S3200-CTFD-TEXT: ' S3200-TX-CTFD-DGTL-TEXT.

           DISPLAY '===> 2 '
           MOVE 'F01'                          TO CNVB64-FUC.
           MOVE S3200-TX-CTFD-DGTL-SIZE        TO CNVB64-ENTD-SIZE.
           MOVE S3200-TX-CTFD-DGTL-TEXT        TO CNVB64-ENTD-TEXT.

           DISPLAY '===> 3 '
      *    DISPLAY CTE-PRGM ' - CALL SBCNVB64 USING: '
      *    DISPLAY 'FUC: ' CNVB64-FUC
      *    DISPLAY 'ENTD-SIZE: ' CNVB64-ENTD-SIZE
      *    DISPLAY 'ENTD-TEXT: ' CNVB64-ENTD-TEXT(1:CNVB64-ENTD-SIZE)

           DISPLAY '===> 4 '
           CALL SBCNVB64 USING CNVB64-FUC
                               CNVB64-ENTD-SIZE
                               CNVB64-ENTD-TEXT
                               CNVB64-SAID-SIZE
                               CNVB64-SAID-TEXT

           DISPLAY '===> 5 '
      *    DISPLAY CTE-PRGM ' - SAIDA DA SUBROTINA SBCNVB64: '
      *    DISPLAY 'CD-RTN: ' CNVB64-CD-RTN
      *    DISPLAY 'SAID-SIZE: ' CNVB64-SAID-SIZE
      *    DISPLAY 'SAID-TEXT: ' CNVB64-SAID-TEXT(1:CNVB64-SAID-SIZE)

           DISPLAY 'RETURN-CODE: ' RETURN-CODE
           DISPLAY '===> 6 '
           IF RETURN-CODE  EQUAL ZERO
      *    IF CNVB64-CD-RTN EQUAL ZERO
           DISPLAY '===> 7 '
             DISPLAY '000 ' CTE-PRGM ' - CONVERSAO BIN EM B64 OK'
             CONTINUE
           ELSE
           DISPLAY '===> 8 '
             DISPLAY '000 ' CTE-PRGM ' - ERRO NA CONVERSAO BIN EM B64'
             PERFORM 999000-ERRO-018
           END-IF.

           DISPLAY '===> 9 '
           DISPLAY CTE-PRGM ' - FIM DA SECTION CONVERTE-ASCII-EBCDIC'.
      *
      *----------*
       102999-SAI.
      *----------*
            EXIT.
      *
      *--------------------------------------*
       200000-EXTRAI-DADOS-CTFD-DGTL  SECTION.
      *--------------------------------------*
      D    DISPLAY '000 ' CTE-PRGM ' - 200000-EXTRAI-DADOS-CTFD-DGTL '.
      *
      *----------------------------------------------------------------*
      *
      D    DISPLAY '000 ' CTE-PRGM ' - Parsing certificado X.509'
      D                            ' - dados basicos'.
      *
           INITIALIZE     ICDS2100-DADOS
                          S2100-SAID-F01
                          S2100-SAID-F02
                          S2100-SAID-F03
                          S2100-SAID-F04
                          S2100-SAID-F05
                          S2100-SAID-F06
               REPLACING  NUMERIC BY  ZEROS
                     ALPHANUMERIC BY SPACES.
      *
           SET  S2100-IDFR-VRS-PRM-ATU-OK    TO TRUE.
           SET  S2100-FUC-CTNR-X509          TO TRUE.
           SET  S2100-FMT-PEM                TO TRUE.
           MOVE S3200-ENTD-TX-CTFD-DGTL      TO S2100-ENTD-TX-CTFD-DGTL.
      *
           PERFORM 210000-EXECUTA-ICDS2100.
      *
           MOVE S2100-SAID-FNC          TO S2100-SAID-F01.
      *
      D    DISPLAY 'Numero de Serie (' S2100-NR-SRE-BIN              ')'
      D                            '(' S2100-NR-SRE-HEX              ')'
      D    DISPLAY 'TS de Emissao   (' S2100-TS-EMS-CTFD-DGTL        ')'
      D    DISPLAY 'TS de Expiracao (' S2100-TS-EXPC-CTFD-DGTL       ')'
      D    DISPLAY 'Versao X.509    (' S2100-NR-VRS-X509             ')'
      D    DISPLAY 'Emissor - cod.  (' S2100-CD-IDFR-EMT-CTFD        ')'
      D                            '(' S2100-TX-IDFR-EMT-CTFD        ')'
      *D   DISPLAY 'Emissor - nome  (' S2100-NM-EMT-CTFD-SIZE        ')'
      D    DISPLAY 'Emissor - nome  (' S2100-NM-EMT-CTFD-TEXT
      D                             (1:S2100-NM-EMT-CTFD-SIZE      ) ')'
      D    DISPLAY 'Titular - cod.  (' S2100-CD-IDFR-DTTR-CTFD       ')'
      D                            '(' S2100-TX-IDFR-DTTR-CTFD       ')'
      *D   DISPLAY 'Titular - nome  (' S2100-NM-DTTR-CTFD-SIZE       ')'
      D    DISPLAY 'Titular - nome  (' S2100-NM-DTTR-CTFD-TEXT
      D                             (1:S2100-NM-DTTR-CTFD-SIZE     ) ')'
      D    DISPLAY 'Natureza        (' S2100-CD-NTZ-TITR-CTFD        ')'
      D                            '(' S2100-TX-NTZ-TITR-CTFD        ')'
      *
      *----------------------------------------------------------------*
      *
      D    DISPLAY '000 ' CTE-PRGM ' - Parsing certificado X.509'
      D                            ' - ICP-Brasil'.
      *
           INITIALIZE     ICDS2100-DADOS
               REPLACING  NUMERIC BY  ZEROS
                     ALPHANUMERIC BY SPACES.
      *
           SET  S2100-IDFR-VRS-PRM-ATU-OK    TO TRUE.
           SET  S2100-FUC-OBG-ICP-BR         TO TRUE.
           SET  S2100-FMT-PEM                TO TRUE.
           MOVE S3200-ENTD-TX-CTFD-DGTL      TO S2100-ENTD-TX-CTFD-DGTL.
      *
      *----------------------------------------------------------------*
      *    FIXME [BEGIN]
      *----------------------------------------------------------------*
      *D   MOVE S2100-ENTD-CD-FUC        TO FIXME-FLG.
      *D   DISPLAY     'Codigo da funcao (' FIXME-FLG                ')'
      *D   IF  FIXME-OBG-ICP-BR
      *D       MOVE 'PF'                 TO S2100-CD-FNLD-CTFD-DGTL
      *D       MOVE 'Pessoa Fisica     ' TO S2100-TX-FNLD-CTFD-DGTL
      *D       MOVE 'A3'                 TO S2100-CD-TIP-PSS-CTFD
      *D       MOVE 'Assinatura Digital' TO S2100-TX-TIP-PSS-CTFD
      *
      *D       DISPLAY 'Finalidade       (' S2100-CD-FNLD-CTFD-DGTL  ')'
      *D                                '(' S2100-TX-FNLD-CTFD-DGTL  ')'
      *D       DISPLAY 'Tipo Pessoa      (' S2100-CD-TIP-PSS-CTFD    ')'
      *D                                '(' S2100-TX-TIP-PSS-CTFD    ')'
      *D       GO TO 200099-FIXME-OBG-ICP-BR
      *D   END-IF.
      *----------------------------------------------------------------*
      *    FIXME [END]
      *----------------------------------------------------------------*
      *
           PERFORM 210000-EXECUTA-ICDS2100.
      *
           MOVE S2100-SAID-FNC          TO S2100-SAID-F02.
      *
      D    DISPLAY 'Finalidade       (' S2100-CD-FNLD-CTFD-DGTL      ')'
      D                             '(' S2100-TX-FNLD-CTFD-DGTL      ')'
      D    DISPLAY 'Tipo Pessoa      (' S2100-CD-TIP-PSS-CTFD        ')'
      D                             '(' S2100-TX-TIP-PSS-CTFD        ')'
      D    DISPLAY 'Data nascimento  (' S2100-ICP-BR-DT-NSC          ')'
      D    DISPLAY 'Numero do CPF    (' S2100-ICP-BR-NR-CPF          ')'
      D    DISPLAY 'NIS (INSS)       (' S2100-ICP-BR-NR-NIS          ')'
      D    DISPLAY 'Identidade (RG)  (' S2100-ICP-BR-NR-RG           ')'
      D    DISPLAY 'RG: emissor e UF (' S2100-ICP-BR-SG-UF           ')'
      D    DISPLAY 'Titulo Eleitoral (' S2100-ICP-BR-TELT-NR-ISCR    ')'
      D    DISPLAY '- Zona           (' S2100-ICP-BR-TELT-ZONA       ')'
      D    DISPLAY '- Secao          (' S2100-ICP-BR-TELT-SCAO       ')'
      D    DISPLAY '- Municipio e UF (' S2100-ICP-BR-TELT-MUN-UF     ')'
      D    DISPLAY 'Numero CEI       (' S2100-ICP-BR-NR-CEI          ')'
      D    DISPLAY 'Numero CNPJ      (' S2100-ICP-BR-NR-CNPJ         ')'
      *D   DISPLAY 'Nome responsavel (' S2100-ICP-BR-NM-RSP-SIZE     ')'
      D    DISPLAY 'Nome responsavel (' S2100-ICP-BR-NM-RSP-TEXT
      D                              (1:S2100-ICP-BR-NM-RSP-SIZE   ) ')'
      D    .
      *----------------------------------------------------------------*
      *    FIXME [BEGIN]
      *----------------------------------------------------------------*
      D200099-FIXME-OBG-ICP-BR.
      *----------------------------------------------------------------*
      *    FIXME [END]
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *
      D    DISPLAY '000 ' CTE-PRGM ' - Parsing certificado X.509'
      D                            ' - LCR e Key Id'.
      *
           INITIALIZE     ICDS2100-DADOS
               REPLACING  NUMERIC BY  ZEROS
                     ALPHANUMERIC BY SPACES.
      *
           SET  S2100-IDFR-VRS-PRM-ATU-OK    TO TRUE.
           SET  S2100-FUC-LCR-KEY-ID         TO TRUE.
           SET  S2100-FMT-PEM                TO TRUE.
           MOVE S3200-ENTD-TX-CTFD-DGTL      TO S2100-ENTD-TX-CTFD-DGTL.
      *
      *----------------------------------------------------------------*
      *    FIXME [BEGIN]
      *----------------------------------------------------------------*
      *D   MOVE S2100-ENTD-CD-FUC     TO FIXME-FLG.
      *D   DISPLAY 'Codigo da funcao ('  FIXME-FLG ')'.
      *D   IF  FIXME-LCR-KEY-ID
      *D       GO TO 200099-FIXME-TBS-ASS-AC
      *D   END-IF.
      *----------------------------------------------------------------*
      *    FIXME [END]
      *----------------------------------------------------------------*
      *
           PERFORM 210000-EXECUTA-ICDS2100.
      *
           MOVE S2100-SAID-FNC          TO S2100-SAID-F03.
      *
      D    MOVE +1 TO NDX.
      D    PERFORM UNTIL NDX NOT LESS S2100-URL-LS-CTFD-CNCD-QT-OCR
      D        DISPLAY 'URL da LCR       ('
      D                          S2100-URL-LS-CTFD-CNCD-SIZE (NDX)   ')'
      D                      '(' S2100-URL-LS-CTFD-CNCD-TEXT (NDX)
      D                       (1:S2100-URL-LS-CTFD-CNCD-SIZE (NDX) ) ')'
      D        ADD +1 TO NDX
      D    END-PERFORM.
      D    DISPLAY 'KeyId - Emissor  (' S2100-IDFR-CHV-EMT-SIZE      ')'
      D                             '(' S2100-IDFR-CHV-EMT-TEXT
      D                              (1:S2100-IDFR-CHV-EMT-SIZE    ) ')'
      D    DISPLAY 'KeyId - Detentor (' S2100-IDFR-CHV-DTTR-SIZE     ')'
      D                             '(' S2100-IDFR-CHV-DTTR-TEXT
      D                              (1:S2100-IDFR-CHV-DTTR-SIZE   ) ')'
      *
      *----------------------------------------------------------------*
      *
      D    DISPLAY '000 ' CTE-PRGM ' - Parsing certificado X.509'
      D                            ' - chave publica'.
      *
           INITIALIZE     ICDS2100-DADOS
               REPLACING  NUMERIC BY  ZEROS
                     ALPHANUMERIC BY SPACES.
      *
           SET  S2100-IDFR-VRS-PRM-ATU-OK    TO TRUE.
           SET  S2100-FUC-CHV-PBCO           TO TRUE.
           SET  S2100-FMT-PEM                TO TRUE.
           MOVE S3200-ENTD-TX-CTFD-DGTL      TO S2100-ENTD-TX-CTFD-DGTL.
      *
           PERFORM 210000-EXECUTA-ICDS2100.
      *
           MOVE S2100-SAID-FNC          TO S2100-SAID-F04.
      *
      D    DISPLAY 'Modulo Chave RSA (' S2100-CHV-PBCO-MDU-SIZE      ')'
      D                             '(' S2100-CHV-PBCO-MDU-TEXT
      D                              (1:S2100-CHV-PBCO-MDU-SIZE    ) ')'
      D    DISPLAY 'Expoente Publico (' S2100-CHV-PBCO-EXPT-SIZE     ')'
      D                             '(' S2100-CHV-PBCO-EXPT-TEXT
      D                              (1:S2100-CHV-PBCO-EXPT-SIZE   ) ')'
      *
      *----------------------------------------------------------------*
      *
      D    DISPLAY '000 ' CTE-PRGM ' - Parsing certificado X.509'
      D                            ' - Utilizacao da Chave'.
      *
           INITIALIZE     ICDS2100-DADOS
               REPLACING  NUMERIC BY  ZEROS
                     ALPHANUMERIC BY SPACES.
      *
           SET  S2100-IDFR-VRS-PRM-ATU-OK    TO TRUE.
           SET  S2100-FUC-UTZO-CHV           TO TRUE.
           SET  S2100-FMT-PEM                TO TRUE.
           MOVE S3200-ENTD-TX-CTFD-DGTL      TO S2100-ENTD-TX-CTFD-DGTL.
      *
           PERFORM 210000-EXECUTA-ICDS2100.
      *
           MOVE S2100-SAID-FNC          TO S2100-SAID-F05.
      *
      D    DISPLAY 'X.509 Key Usage  (' S2100-UTZO-CHV-SIZE          ')'
      D                             '(' S2100-UTZO-CHV-TEXT
      D                              (1:S2100-UTZO-CHV-SIZE        ) ')'
      D    MOVE +1 TO NDX.
      D    PERFORM UNTIL NDX NOT LESS S2100-UTZO-ETS-QT-OCR
      D        DISPLAY '<Ext> Key Usage  ('
      D                                 S2100-UTZO-ETS-CHV-SIZE(NDX) ')'
      D                             '(' S2100-UTZO-ETS-CHV-TEXT(NDX)
      D                              (1:S2100-UTZO-ETS-CHV-SIZE(NDX))')'
      D        ADD +1 TO NDX
      D    END-PERFORM.
      *
      *----------------------------------------------------------------*
      *    FIXME [BEGIN]
      *----------------------------------------------------------------*
      D200099-FIXME-TBS-ASS-AC.
      *----------------------------------------------------------------*
      *    FIXME [END]
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *
      D    DISPLAY '000 ' CTE-PRGM ' - Parsing certificado X.509'
      D                            ' - TBS e Assinatura do Emissor'.
      *
           INITIALIZE     ICDS2100-DADOS
               REPLACING  NUMERIC BY  ZEROS
                     ALPHANUMERIC BY SPACES.
      *
           SET  S2100-IDFR-VRS-PRM-ATU-OK    TO TRUE.
           SET  S2100-FUC-TBS-ASS-AC         TO TRUE.
           SET  S2100-FMT-PEM                TO TRUE.
           MOVE S3200-ENTD-TX-CTFD-DGTL      TO S2100-ENTD-TX-CTFD-DGTL.
      *
           PERFORM 210000-EXECUTA-ICDS2100.
      *
           MOVE S2100-SAID-FNC          TO S2100-SAID-F06.
      *
      D    DISPLAY 'Algoritmo (TBS)  (' S2100-AGRM-CTU-ASNR-SIZE     ')'
      D                             '(' S2100-AGRM-CTU-ASNR-TEXT
      D                              (1:S2100-AGRM-CTU-ASNR-SIZE   ) ')'
      D    DISPLAY 'Algoritmo (CNTR) (' S2100-AGRM-ASS-CTNR-SIZE     ')'
      D                             '(' S2100-AGRM-ASS-CTNR-TEXT
      D                              (1:S2100-AGRM-ASS-CTNR-SIZE   ) ')'
      D    DISPLAY 'Assinatura da AC (' S2100-ASS-DGTL-ADD-SIZE      ')'
      D                             '(' S2100-ASS-DGTL-ADD-TEXT
      D                              (1:S2100-ASS-DGTL-ADD-SIZE    ) ')'
      D    DISPLAY 'TBS-certificate  (' S2100-CTU-ASNR-SIZE          ')'
      D                             '(' S2100-CTU-ASNR-TEXT
      D                              (1:S2100-CTU-ASNR-SIZE        ) ')'
      D    .
      *
      *----------------------------------------------------------------*
      *
       200099-SAI.
           EXIT.
      *
      *--------------------------------------*
       210000-EXECUTA-ICDS2100        SECTION.
      *--------------------------------------*
      D    DISPLAY '000 ' CTE-PRGM ' - 210000-EXECUTA-ICDS2100       '.
      *
      D    DISPLAY '000 ' CTE-PRGM ' - Antes da sub-rotina ICDS2100'
      D                            '(' S2100-ENTD-CD-FUC ')'.
      *
      D    DISPLAY '000 ' CTE-PRGM
      D             ' - ' CTE-VERS
      D             ' - ' ICDS2100
      D               '(' S2100-ENTD-IDFR-VRS-PRM
      D             ') (' S2100-ENTD-CD-FUC
      D             ') (' S2100-ENTD-IN-FMT-ENTD
      D             ') (' S2100-TX-CTFD-DGTL-SIZE
      *D            '):(' S2100-TX-CTFD-DGTL-TEXT
      *D               (1:S2100-TX-CTFD-DGTL-SIZE )
      D             ')'.
      *
           MOVE SPACES      TO GDA-DFHEIBLK.
      *
           CALL ICDS2100 USING GDA-DFHEIBLK
                               ICDS2100-DADOS.
           MOVE RETURN-CODE TO GDA-CD-RTN.
      *
      D    MOVE GDA-CD-RTN  TO KDPY-CD-RTN.
      D    DISPLAY '000 ' CTE-PRGM ' - Volta da sub-rotina ICDS2100'
      D            ' - RETURN-CODE = ' KDPY-CD-RTN.
      *
           IF  KRTN-CD-RTN OF ICDS2100-DADOS NOT EQUAL ZEROS
               MOVE 'DET-X509' TO S9100-CD-LCZC-ERRO-FON
               GO TO 999000-ERRO-ICDS2100
           END-IF.
      *
       210099-SAI.
           EXIT.
      *
      *--------------------------------------*
       300000-VERIFICA-CADM-ANT       SECTION.
      *--------------------------------------*
      D    DISPLAY '000 ' CTE-PRGM ' - 300000-VERIFICA-CADM-ANT      '.
      *
           INITIALIZE     ICDS003W-DADOS
               REPLACING  NUMERIC BY  ZEROS
                     ALPHANUMERIC BY SPACES.
      *
           MOVE S2100-CD-IDFR-EMT-CTFD      TO K003-CD-ADD-CTFR.
           MOVE S2100-NR-SRE-BIN            TO K003-CD-NR-SRE-CTFD.
      D    MOVE S2100-NR-SRE-HEX            TO K003-TX-NR-SRE-CTFD.
      *
           DISPLAY 'DISPREI : ' KRTN-CD-RTN OF ICDS003W-DADOS
           PERFORM 811000-EXECUTA-ICDS003C.
      *
           DISPLAY 'DISPRAY: 'KRTN-CD-RTN OF ICDS003W-DADOS
           EVALUATE KRTN-CD-RTN OF ICDS003W-DADOS
      *
      *        Certificado localizado
               WHEN +0
                    MOVE K003-CD-EST-CTFD-DGTL TO KL03-TAB (1)
                    PERFORM 812000-EXECUTA-ICDSL03A
                    IF  KRTN-CD-RTN OF ICDSL03A-DADOS NOT EQUAL ZERO
                        MOVE 'CADM-ANT' TO S9100-CD-LCZC-ERRO-FON
                        GO TO 999000-ERRO-ICDSL03A
                    END-IF
                    MOVE KL03-TAB(1) TO S3200-SAID
      *
      *        Certificado nao localizado
               WHEN +100
                    MOVE SPACES TO S3200-SAID
      *
      *        Outros retornos
               WHEN OTHER
                    MOVE 'CADM-ANT' TO S9100-CD-LCZC-ERRO-FON
                    GO TO 999000-ERRO-ICDS003C
           END-EVALUATE.
      *
       300099-SAI.
           EXIT.
      *
      *--------------------------------------*
       400000-VERIFICA-BASC-X509      SECTION.
      *--------------------------------------*
      D    DISPLAY '000 ' CTE-PRGM ' - 400000-VERIFICA-BASC-X509     '.
      *
      *----------------------------------------------------------------*
      *    (1) ICDS2100 - OID (TBS) = OID (CNTR) ?
      *    (2) ICDS2100 - authKeyId X subjectKeyId ?
      *    (3) ICDS2100 - versao X.509 = 3 ?
      *    (4) ICDKUTC* - emissao < curUTC < validade ?
      *----------------------------------------------------------------*
      *
           IF  S2100-CD-NTZ-TITR-CTFD EQUAL 'AC'
               MOVE 'BASCX509' TO S9100-CD-LCZC-ERRO-FON
               GO TO 999000-ERRO-005
           END-IF.
      *
      *----------------------------------------------------------------*
      *    (1) ICDS2100 - OID (TBS) = OID (CNTR) ?
      *----------------------------------------------------------------*
           MOVE S2100-AGRM-CTU-ASNR-TEXT (1:S2100-AGRM-CTU-ASNR-SIZE)
                                         TO OID-TBS-TEXT.
           MOVE S2100-AGRM-CTU-ASNR-SIZE TO OID-TBS-SIZE.
           MOVE S2100-AGRM-ASS-CTNR-TEXT (1:S2100-AGRM-ASS-CTNR-SIZE)
                                         TO OID-CTNR-TEXT.
           MOVE S2100-AGRM-ASS-CTNR-SIZE TO OID-CTNR-SIZE.
      *
           IF  OID-TBS-SIZE EQUAL OID-CTNR-SIZE
           AND OID-TBS-TEXT EQUAL OID-CTNR-TEXT
               NEXT SENTENCE
           ELSE
               MOVE 'BASCX509' TO S9100-CD-LCZC-ERRO-FON
               GO TO 999000-ERRO-006
           END-IF.
      *
      *----------------------------------------------------------------*
      *    (2) ICDS2100 - authKeyId X subjectKeyId ?
      *----------------------------------------------------------------*
      *
           IF  S2100-IDFR-CHV-EMT-SIZE      EQUAL ZEROS
           OR  S2100-IDFR-CHV-DTTR-SIZE NOT EQUAL ZEROS
               MOVE 'BASCX509' TO S9100-CD-LCZC-ERRO-FON
               GO TO 999000-ERRO-007
           END-IF.
      *
      *----------------------------------------------------------------*
      *    (3) ICDS2100 - versao X.509 = 3 ?
      *----------------------------------------------------------------*
           IF  S2100-NR-VRS-X509 NOT EQUAL +3
               MOVE 'BASCX509' TO S9100-CD-LCZC-ERRO-FON
               GO TO 999000-ERRO-008
           END-IF.
      *
      *----------------------------------------------------------------*
      *    (4) ICDKUTC* - emissao < curUTC < validade ?
      *----------------------------------------------------------------*
           PERFORM 907000-EXECUTA-ICDSUTC0.                             
      *
           IF  UTCW-GEN-TIME NOT GREATER S2100-TS-EMS-CTFD-DGTL         
               MOVE 'BASCX509'        TO S9100-CD-LCZC-ERRO-FON         
               GO TO 999000-ERRO-009
           END-IF.
      *
           IF  UTCW-GEN-TIME NOT    LESS S2100-TS-EXPC-CTFD-DGTL        
               MOVE 'E'               TO S3200-SAID-CD-EST-ACLT         
           END-IF.
      *
       400099-SAI.
           EXIT.
      *
      *--------------------------------------*
       500000-VERIFICA-SEQ-CTFC       SECTION.
      *--------------------------------------*
      D    DISPLAY '000 ' CTE-PRGM ' - 500000-VERIFICA-SEQ-CTFC      '.
      *
      *----------------------------------------------------------------*
      *    (1) ICDS002P - localiza o certificado do emissor
      *    (2) ICDS003C - certificado do emissor ok ?
      *    (3) ICDS0100 - assinatura do emissor confere ?
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    (1) ICDS002P - localiza o certificado do emissor
      *----------------------------------------------------------------*
      *
           MOVE S2100-CD-IDFR-EMT-CTFD    TO K002-CD-ADD-CTFR-DTTR.
           MOVE S2100-IDFR-CHV-EMT-SIZE   TO K002-TX-IDFR-CHV-DTTR-SIZE.
           MOVE S2100-IDFR-CHV-EMT-TEXT   TO K002-TX-IDFR-CHV-DTTR-TEXT.
      *
           PERFORM 811000-EXECUTA-ICDS002P.
      *
           IF  P002-QT-REG NOT GREATER ZEROS
               MOVE 'SEQ-CTFC' TO S9100-CD-LCZC-ERRO-FON
               GO TO 999000-ERRO-010
           END-IF.
      *
           MOVE P002-QT-REG TO GDA-LIM.
           MOVE SPACES TO K0100-CND-VLD-ASS-DGTL.
           MOVE ZEROS  TO NDX.
           PERFORM  UNTIL NDX NOT LESS GDA-LIM
                       OR K0100-CND-VLD-ASS-DGTL EQUAL ZEROS
      *
               ADD +1 TO NDX
      *
               INITIALIZE     ICDS003W-DADOS
                   REPLACING  NUMERIC BY  ZEROS
                         ALPHANUMERIC BY SPACES
      *
               MOVE P002-CD-ADD-CTFR    (NDX) TO K003-CD-ADD-CTFR
               MOVE P002-CD-NR-SRE-CTFD (NDX) TO K003-CD-NR-SRE-CTFD
      *
               PERFORM 811000-EXECUTA-ICDS003C
      *
      *----------------------------------------------------------------*
      *        (2) ICDS003C - certificado do emissor ok ?
      *----------------------------------------------------------------*
      *
      D        DISPLAY '000 ' CTE-PRGM ' - CD-RTN        '
      D                '('    KRTN-CD-RTN OF ICDS003W-DADOS          ')'
      D        DISPLAY '000 ' CTE-PRGM ' - EST-CTFD-DGTL '
      D                '('    K003-CD-EST-CTFD-DGTL                  ')'
      D        DISPLAY '000 ' CTE-PRGM ' - IDFR-SEQ-CTFC '
      D                '('                 K003-CD-IDFR-SEQ-CTFC     ')'
      D                '('                 S3200-CD-IDFR-SEQ-CTFC    ')'
      *
               IF  K003-CD-EST-CTFD-DGTL EQUAL 'P'
                   MOVE 'P' TO S3200-SAID-CD-EST-ACLT
               END-IF
      *
               IF  KRTN-CD-RTN OF ICDS003W-DADOS
                                         EQUAL ZEROS
               AND K003-CD-EST-CTFD-DGTL EQUAL 'A'
               AND K003-CD-IDFR-SEQ-CTFC EQUAL S3200-CD-IDFR-SEQ-CTFC
      *            ----------------------------------------------
      *            (3) ICDS0100 - assinatura do emissor confere ?
      *            ----------------------------------------------
                   MOVE 'F01'                 TO K0100-CD-FUC
                   MOVE K003-CD-ADD-CTFR      TO K0100-CD-ADD-CTFR
                   MOVE K003-CD-NR-SRE-CTFD   TO K0100-CD-NR-SRE-CTFD
                   MOVE S2100-SAID-CTU-ASNR   TO K0100-TEXTO
                   MOVE S2100-SAID-ASS-DGTL-ADD-CTFR
                                              TO K0100-ASS
                   MOVE S2100-AGRM-ASS-CTNR   TO K0100-ALG
      *
                   PERFORM 510000-ICDS0100-CONF-ASS-AC
      *
      D            DISPLAY '000 ' CTE-PRGM ' - CD-RTN-S0100  '
      D                    '('    K0100-CND-VLD-ASS-DGTL             ')'
      D                    '('    K0100-LS-CNCD                      ')'
      *
      *----------------------------------------------------------------*
      *    FIXME [BEGIN]
      *----------------------------------------------------------------*
      *            IF  S0100-ASS-VLDO
      *            AND S0100-LCR-ATZD
      *            AND S3200-SAID-CD-EST-ACLT EQUAL SPACES
      *----------------------------------------------------------------*
      *    FIXME [END]
      *----------------------------------------------------------------*
                   IF  S0100-ASS-VLDO
                   AND S3200-SAID-CD-EST-ACLT EQUAL SPACES
      *                [L]CR nao atualizada                             
                       MOVE 'L' TO S3200-SAID-CD-EST-ACLT
                   END-IF
                   IF  S0100-ASS-INVD
                   AND S3200-SAID-CD-EST-ACLT EQUAL SPACES
                       MOVE 'I' TO S3200-SAID-CD-EST-ACLT
                       MOVE S3200-SAID TO KL03-TAB(1)
                       PERFORM 812000-EXECUTA-ICDSL03A
                       MOVE KL03-TAB(1) TO S3200-SAID
                       MOVE 'SEQ-CTFC' TO S9100-CD-LCZC-ERRO-FON
                       GO TO 999000-ERRO-011
                   END-IF
      *
      D            DISPLAY '000 ' CTE-PRGM ' - CD-EST-ACLT   '
      D                    '('    S3200-SAID-CD-EST-ACLT             ')'
      *
               END-IF
      *
      *        -----------------------
      *        tenta outro certificado
      *        -----------------------
      *
           END-PERFORM.
      *
           IF  NDX GREATER GDA-LIM
      *        ----------------------------------------------
      *        nenhum certificado encontrado (AC + authKeyId)
      *        ----------------------------------------------
               MOVE 'SEQ-CTFC' TO S9100-CD-LCZC-ERRO-FON
               GO TO 999000-ERRO-012
           END-IF.
      *
           IF  S3200-SAID-CD-EST-ACLT EQUAL SPACES
      *        ----------------------------------------------
      *        nenhum certificado encontrado (AC + authKeyId)
      *        ----------------------------------------------
               MOVE 'SEQ_CTFC' TO S9100-CD-LCZC-ERRO-FON
               GO TO 999000-ERRO-012
           END-IF.
      *
           IF  S3200-SAID-CD-EST-ACLT EQUAL 'P'
               MOVE S3200-SAID TO KL03-TAB(1)
               PERFORM 812000-EXECUTA-ICDSL03A
               MOVE KL03-TAB(1) TO S3200-SAID
               MOVE 'SEQ-CTFC' TO S9100-CD-LCZC-ERRO-FON
               GO TO 999000-ERRO-013
           END-IF.
      *
       500099-SAI.
           EXIT.
      *
      *--------------------------------------*
       510000-ICDS0100-CONF-ASS-AC    SECTION.
      *--------------------------------------*
      D    DISPLAY '000 ' CTE-PRGM ' - 510000-ICDS0100-CONF-ASS-AC   '.
      *
      D    DISPLAY '000 ' CTE-PRGM ' - Antes da sub-rotina ICDS0100'
      D                            '(' K0100-CD-FUC ')'.
      *
      D    DISPLAY '000 ' CTE-PRGM
      D             ' - ' CTE-VERS
      D             ' - ' ICDS0100
      D               '(' K0100-ENTD-IDFR-VRS-PRM
      D             ') (' K0100-CD-FUC
      D             ')'.
      D    DISPLAY '000 ' CTE-PRGM
      D             ' - ' CTE-VERS
      D             ' - ' ICDS0100
      D               '(' K0100-CD-ADD-CTFR
      D             ') (' K003-TX-NR-SRE-CTFD
      D             ') (' K0100-CD-NR-SRE-CTFD
      D             ')'.
      D    DISPLAY '000 ' CTE-PRGM
      D             ' - ' CTE-VERS
      D             ' - ' ICDS0100
      D               '(' K0100-CD-ADD-CTFR
      D             ') (' S2100-NR-SRE-HEX
      D             ') (' K0100-CD-NR-SRE-CTFD
      D             ')'.
      D    DISPLAY '000 ' CTE-PRGM
      D             ' - ' CTE-VERS
      D             ' - ' ICDS0100
      D               '(' K0100-ALG-SIZE
      D             ') (' K0100-ALG-TEXT
      D                (1:K0100-ALG-SIZE )
      D             ')'.
      *
      *----------------------------------------------------------------*
      *    FIXME [BEGIN]
      *----------------------------------------------------------------*
      *D   DISPLAY '000 ' CTE-PRGM
      *D            ' - ' CTE-VERS
      *D            ' - ' ICDS0100
      *D            ') (' K003-TX-MDU-CHV-PBCO-SIZE
      *D            ') (' K003-TX-MDU-CHV-PBCO-TEXT
      *D               (1:K003-TX-MDU-CHV-PBCO-SIZE  )
      *D            ')'.
      *D   DISPLAY '000 ' CTE-PRGM
      *D            ' - ' CTE-VERS
      *D            ' - ' ICDS0100
      *D            ') (' K003-TX-EXPT-CHV-PBCO-SIZE
      *D            ') (' K003-TX-EXPT-CHV-PBCO-TEXT
      *D               (1:K003-TX-EXPT-CHV-PBCO-SIZE )
      *D            ')'.
      *
      *D   MOVE K003-TX-MDU-CHV-PBCO-SIZE   TO S2100-CHV-PBCO-MDU-SIZE.
      *D   MOVE K003-TX-MDU-CHV-PBCO-TEXT   TO S2100-CHV-PBCO-MDU-TEXT.
      *D   MOVE K003-TX-EXPT-CHV-PBCO-SIZE  TO S2100-CHV-PBCO-EXPT-SIZE.
      *D   MOVE K003-TX-EXPT-CHV-PBCO-TEXT  TO S2100-CHV-PBCO-EXPT-TEXT.
      *D   MOVE S2100-SAID-F04-CHV-PBCO     TO FIXME-ENTD-CHV-PBCO.
      *
      *D   DISPLAY '000 ' CTE-PRGM
      *D            ' - ' CTE-VERS
      *D            ' - ' ICDS0100
      *D              '(' S2100-CHV-PBCO-MDU-SIZE
      *D            ') (' S2100-CHV-PBCO-MDU-TEXT
      *D               (1:S2100-CHV-PBCO-MDU-SIZE  )
      *D            ')'.
      *D   DISPLAY '000 ' CTE-PRGM
      *D            ' - ' CTE-VERS
      *D            ' - ' ICDS0100
      *D              '(' S2100-CHV-PBCO-EXPT-SIZE
      *D            ') (' S2100-CHV-PBCO-EXPT-TEXT
      *D               (1:S2100-CHV-PBCO-EXPT-SIZE )
      *D            ')'.
      *----------------------------------------------------------------*
      *    FIXME [END]
      *----------------------------------------------------------------*
      *
           MOVE SPACES      TO GDA-DFHEIBLK.
      *
           CALL ICDS0100 USING GDA-DFHEIBLK
                               ICDS0100-DADOS.
           MOVE KRTN-CD-RTN OF ICDS0100-DADOS TO S0100-CD-RTN.
           MOVE K0100-CND-VLD-ASS-DGTL        TO S0100-CND-VLD-ASS-DGTL.
      *
      D    MOVE S0100-CD-RTN TO KDPY-CD-RTN.
      D    DISPLAY '000 ' CTE-PRGM ' - Volta da sub-rotina ICDS0100'
      D            ' - RETURN-CODE = ' KDPY-CD-RTN.
      D    DISPLAY '000 ' CTE-PRGM ' - K0100-SAIDA (' K0100-SAIDA ')'.
      *
           IF  NOT S0100-ASS-VLDO
           AND NOT S0100-ASS-INVD
               MOVE 'SEQ-CTFC' TO S9100-CD-LCZC-ERRO-FON
               GO TO 999000-ERRO-ICDS0100
           END-IF.
      *
      D    DISPLAY 'VLD-ASS-DGTL (' K0100-CND-VLD-ASS-DGTL          ')'
      D                      ' - (' K0100-TS-CND-VLD                ')'.
      D    DISPLAY 'LS-CTFD-CNCD (' K0100-TS-EMS-LS-CNCD            ')'
      D                      ' - (' K0100-TS-VLD-LS-CNCD            ')'
      D                      ' - (' K0100-NR-SEQL-LS-CNCD           ')'.
      *
       510099-SAI.
           EXIT.
      *
      *--------------------------------------*
       600000-ARMAZENA-CTFD-USU-FIM   SECTION.
      *--------------------------------------*
      D    DISPLAY '000 ' CTE-PRGM ' - 600000-ARMAZENA-CTFD-USU-FIM  '.
      *
      *----------------------------------------------------------------*
      *    (1) ICDS006P - LCR existe ?
      *    (2) ICDS006I - cria registro da LCR nova
      *    (3) ICDS003I - armazena certificado
      *    (4) ICDS004I - armazena historico do certificado
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    (1) ICDS006P - LCR existe ?
      *    (2) ICDS006I - cria registro da LCR nova
      *----------------------------------------------------------------*
      *
           IF  S2100-URL-LS-CTFD-CNCD-QT-OCR NOT GREATER ZEROS
               MOVE 'URL-LCR ' TO S9100-CD-LCZC-ERRO-FON
               GO TO 999000-ERRO-014
           END-IF.
      *
      *----------------------------------------------------------------*
      *    FIXME [BEGIN]
      *----------------------------------------------------------------*
           MOVE S2100-CD-IDFR-EMT-CTFD      TO K006-CD-ADD-CTFR.
           MOVE S2100-URL-LS-CTFD-CNCD-SIZE (1)
                                            TO K006-TX-URL-LS-CNCD-SIZE.
           MOVE S2100-URL-LS-CTFD-CNCD-TEXT (1)
                                            TO K006-TX-URL-LS-CNCD-TEXT.
      *----------------------------------------------------------------*
      *    FIXME [END]
      *----------------------------------------------------------------*
      *
           MOVE SPACES TO S7100-CND-EXT-URL.                            
      *                                                                 
           PERFORM 811000-EXECUTA-ICDS006P.
      *
           IF  P006-QT-REG NOT GREATER ZEROS
               MOVE S2100-CD-IDFR-EMT-CTFD  TO K006-CD-ADD-CTFR
               MOVE S2100-URL-LS-CTFD-CNCD-SIZE (1)
                                            TO K006-TX-URL-LS-CNCD-SIZE
               MOVE S2100-URL-LS-CTFD-CNCD-TEXT (1)
                                            TO K006-TX-URL-LS-CNCD-TEXT
               MOVE 'M'                     TO K006-CD-EST-LS-CNCD
               PERFORM 907000-EXECUTA-ICDSUTC0                          
               MOVE UTCW-DB2-TIME           TO K006-TS-EST-LS-CNCD      
               PERFORM 811000-EXECUTA-ICDS006I
               MOVE 'N' TO S7100-CND-EXT-URL                            
           END-IF.
      *
      *----------------------------------------------------------------*
      *    (5) ICDS003I - armazena certificado
      *----------------------------------------------------------------*
      *
           INITIALIZE     ICDS003W-DADOS
               REPLACING  NUMERIC BY  ZEROS
                     ALPHANUMERIC BY SPACES.
      *
           MOVE K006-CD-ADD-CTFR         TO K003-CD-ADD-CTFR.
           MOVE S2100-NR-SRE-BIN         TO K003-CD-NR-SRE-CTFD.
           MOVE S3200-CD-IDFR-SEQ-CTFC   TO K003-CD-IDFR-SEQ-CTFC.
           MOVE S2100-CD-FNLD-CTFD-DGTL  TO K003-CD-FNLD-CTFD-DGTL.
           MOVE S2100-NTZ-TITR-CTFD-DGTL TO K003-CD-NTZ-TITR-CTFD.
           MOVE K006-CD-URL-LS-CNCD      TO K003-CD-URL-LS-CNCD.
           MOVE S2100-NR-SRE-HEX         TO K003-TX-NR-SRE-CTFD.
           MOVE S2100-TS-EMS-CTFD-DGTL   TO UTCW-GEN-TIME.              
           PERFORM 907100-CNV-UTC-TS.
           MOVE UTCW-DB2-TIME            TO K003-TS-EMS-CTFD-DGTL.      
           MOVE S2100-TS-EXPC-CTFD-DGTL  TO UTCW-GEN-TIME.              
           PERFORM 907100-CNV-UTC-TS.
           MOVE UTCW-DB2-TIME            TO K003-TS-EXPC-CTFD-DGTL.     
           MOVE SPACES                   TO K003-TS-CNCT-CTFD-DGTL.
           MOVE S3200-SAID-CD-EST-ACLT   TO K003-CD-EST-CTFD-DGTL.
           PERFORM 907000-EXECUTA-ICDSUTC0.                             
           MOVE UTCW-DB2-TIME            TO K003-TS-EST-CTFD-DGTL.      
           MOVE GDA-CD-USU               TO K003-CD-USU-RSP-EST.
           MOVE CALLER-NM-PGM-RSP        TO K003-NM-PGM-RSP-EST.
           MOVE S2100-NM-DTTR-CTFD-TEXT  TO K003-NM-DTTR-CTFD-DGTL.
           MOVE S2100-CHV-PBCO-MDU-SIZE  TO K003-TX-MDU-CHV-PBCO-SIZE.
           MOVE S2100-CHV-PBCO-MDU-TEXT  TO K003-TX-MDU-CHV-PBCO-TEXT.
           MOVE S2100-CHV-PBCO-EXPT-SIZE TO K003-TX-EXPT-CHV-PBCO-SIZE.
           MOVE S2100-CHV-PBCO-EXPT-TEXT TO K003-TX-EXPT-CHV-PBCO-TEXT.
           MOVE S2100-TX-CTFD-DGTL-SIZE  TO K003-TX-CTFD-DGTL-SIZE.
           MOVE S2100-TX-CTFD-DGTL-TEXT  TO K003-TX-CTFD-DGTL-TEXT.
      *
           PERFORM 811000-EXECUTA-ICDS003I.
      *
      *----------------------------------------------------------------*
      *    (6) ICDS004I - armazena historico do certificado
      *----------------------------------------------------------------*
      *
      *    MOVE ICDS003W-DADOS            TO ICDS004W-DADOS.
           MOVE UTCW-DB2-TIME             TO K004-TS-ALT-TAB.           
           MOVE 'I'                       TO K004-CD-ALT-TAB.
           MOVE GDA-CD-USU                TO K004-CD-RSP-ALT-TAB.
      *----------------------------------------------------------------*
           MOVE K003-CD-ADD-CTFR          TO K004-CD-ADD-CTFR.
           MOVE K003-CD-NR-SRE-CTFD       TO K004-CD-NR-SRE-CTFD.
           MOVE K003-CD-IDFR-SEQ-CTFC     TO K004-CD-IDFR-SEQ-CTFC.
           MOVE K003-CD-FNLD-CTFD-DGTL    TO K004-CD-FNLD-CTFD-DGTL.
           MOVE K003-CD-NTZ-TITR-CTFD     TO K004-CD-NTZ-TITR-CTFD.
           MOVE K003-CD-URL-LS-CNCD       TO K004-CD-URL-LS-CNCD.
           MOVE K003-TX-NR-SRE-CTFD       TO K004-TX-NR-SRE-CTFD.
           MOVE K003-TS-EMS-CTFD-DGTL     TO K004-TS-EMS-CTFD-DGTL.
           MOVE K003-TS-EXPC-CTFD-DGTL    TO K004-TS-EXPC-CTFD-DGTL.
           MOVE K003-TS-CNCT-CTFD-DGTL    TO K004-TS-CNCT-CTFD-DGTL.
           MOVE K003-CD-EST-CTFD-DGTL     TO K004-CD-EST-CTFD-DGTL.
           MOVE K003-TS-EST-CTFD-DGTL     TO K004-TS-EST-CTFD-DGTL.
           MOVE K003-CD-USU-RSP-EST       TO K004-CD-USU-RSP-EST.
           MOVE K003-NM-PGM-RSP-EST       TO K004-NM-PGM-RSP-EST.
           MOVE K003-NM-DTTR-CTFD-DGTL    TO K004-NM-DTTR-CTFD-DGTL.
           MOVE K003-TX-MDU-CHV-PBCO      TO K004-TX-MDU-CHV-PBCO.
           MOVE K003-TX-EXPT-CHV-PBCO     TO K004-TX-EXPT-CHV-PBCO.
           MOVE K003-TX-CTFD-DGTL         TO K004-TX-CTFD-DGTL.
      *----------------------------------------------------------------*
           PERFORM 811000-EXECUTA-ICDS004I.
      *
       600099-SAI.
           EXIT.
      *
      *--------------------------------------*
       700000-FINALIZA                SECTION.
      *--------------------------------------*
      D    DISPLAY '000 ' CTE-PRGM ' - 700000-FINALIZA               '.
      *
      D    DISPLAY '000 ' CTE-PRGM ' - Certificado'
      D               '(' S2100-TX-IDFR-EMT-CTFD
      D             ') (' K003-TX-NR-SRE-CTFD
      D             ')'.
      *
      *----------------------------------------------------------------*
      *    (1) ICDS7100 - URL de LCR nova ?                             
      *    (2) ICDS006P - LCR estah ativa ?                             
      *    (3) ICDS7200 - baixa o certificado, se revogado              
      *    (4) ICDS2200 - vincula certificado ao codigo MCI             
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    (1) ICDS7100 - URL de LCR nova ?                             
      *----------------------------------------------------------------*
      *                                                                 
           IF  S7100-URL-NOVO                                           
               PERFORM 710000-EXECUTA-ICDS7100                          
           END-IF.                                                      
      *
      *----------------------------------------------------------------*
      *    (2) ICDS006P - LCR estah ativa ?                             
      *----------------------------------------------------------------*
      *
           MOVE K003-CD-ADD-CTFR          TO K006-CD-ADD-CTFR.
           MOVE K003-CD-URL-LS-CNCD       TO K006-CD-URL-LS-CNCD.
      *
           PERFORM 811000-EXECUTA-ICDS006C.
      *
           IF  K006-CD-EST-LS-CNCD NOT EQUAL 'A'
      *        [L]CR nao atualizada
               MOVE 'L' TO S3200-SAID-CD-EST-ACLT
               GO TO 700000-FIM-LCR                                     
           END-IF.
      *
      *----------------------------------------------------------------*
      *    (3) ICDS7200 - baixa o certificado, se revogado              
      *        Somente para os estados da URL de LCR:                   
      *        - 'A' = LCR [A]tiva                                      
      *        - 'L' = [L]CR nao atualizada                             
      *----------------------------------------------------------------*
      *
           SET  S7200-FUC-UM-CTFD         TO TRUE.
           SET  S7200-IDFR-VRS-PRM-ATU-OK TO TRUE.
           MOVE K003-CD-ADD-CTFR          TO S7200-CD-ADD-CTFR.
           MOVE K003-CD-URL-LS-CNCD       TO S7200-CD-URL-LS-CNCD.
           MOVE K003-CD-NR-SRE-CTFD       TO S7200-NR-SRE-CTFD-DGTL.
      *
           IF  S3200-SAID-CD-EST-ACLT EQUAL 'A'
           OR  S3200-SAID-CD-EST-ACLT EQUAL 'L'
               PERFORM 720000-EXECUTA-ICDS7200
               MOVE S7200-CD-EST-CTFD-DGTL TO S3200-SAID
           END-IF.                                                      
      *                                                                 
       700000-FIM-LCR.                                                  
      *                                                                 
      *----------------------------------------------------------------*
      *    (4) ICDS2200 - vincula certificado ao codigo MCI             
      *----------------------------------------------------------------*
      *                                                                 
           PERFORM 730000-EXECUTA-ICDS2200.                             
      *                                                                 
       700099-SAI.                                                      
           EXIT.                                                        
      *                                                                 
      *--------------------------------------*                          
       710000-EXECUTA-ICDS7100        SECTION.                          
      *--------------------------------------*                          
      D    DISPLAY '000 ' CTE-PRGM ' - 710000-EXECUTA-ICDS7100       '. 
      *                                                                 
           IF  NOT CALLER-AMB-CICS                                      
      D        DISPLAY '000 ' CTE-PRGM ' - NAO EH CICS'                 
      D                                ' - SBCALLER(AMB=' CALLER-AMB ')'
               MOVE SPACES TO S7100-CND-EXT-URL                         
               GO TO 710099-SAI                                         
           END-IF.                                                      
      *                                                                 
           INITIALIZE     ICDS7100-DADOS                                
               REPLACING  NUMERIC BY  ZEROS                             
                     ALPHANUMERIC BY SPACES.                            
      *                                                                 
           SET  S7100-IDFR-VRS-PRM-ATU-OK TO TRUE.                      
           SET  S7100-FUC-ATLR-URL        TO TRUE.                      
      *                                                                 
      D    DISPLAY '000 ' CTE-PRGM                                      
      D             ' - ' CTE-VERS                                      
      D             ' - ' ICDS7100                                      
      D               '(' S7100-ENTD-IDFR-VRS-PRM                       
      D             ') (' S7100-ENTD-CD-FUC                             
      D             ')'.                                                
      *                                                                 
           EXEC CICS LINK                                               
                     PROGRAM      ( ICDS7100 )                          
                     COMMAREA     ( ICDS7100-DADOS )                    
                     LENGTH       ( LENGTH OF ICDS7100-DADOS )          
                     RESP         ( GDA-RESP )                          
                     RESP2        ( GDA-RESP2 )                         
                     SYNCONRETURN                                       
           END-EXEC.                                                    
      *                                                                 
           IF  GDA-RESP NOT EQUAL ZERO                                  
               MOVE 'URL-LCR ' TO S9100-CD-LCZC-ERRO-FON                
               PERFORM 999000-ERRO-015                                  
           END-IF.                                                      
      *                                                                 
           IF  KRTN-CD-RTN OF ICDS7100-DADOS NOT EQUAL ZEROS            
               MOVE 'URL-LCR ' TO S9100-CD-LCZC-ERRO-FON                
               PERFORM 999000-ERRO-ICDS7100                             
           END-IF.
      *
       710099-SAI.
           EXIT.
      *
      *--------------------------------------*
       720000-EXECUTA-ICDS7200        SECTION.
      *--------------------------------------*
      D    DISPLAY '000 ' CTE-PRGM ' - 720000-EXECUTA-ICDS7200       '.
      *
      D    DISPLAY '000 ' CTE-PRGM ' - Antes da sub-rotina ICDS7200'.
      *                                                                 
           SET  S7200-IDFR-VRS-PRM-ATU-OK TO TRUE.                      
           SET  S7200-FUC-UM-CTFD         TO TRUE.                      
      *
      D    DISPLAY '000 ' CTE-PRGM
      D             ' - ' CTE-VERS
      D             ' - ' ICDS7200
      D               '(' S7200-ENTD-IDFR-VRS-PRM
      D             ') (' S7200-ENTD-CD-FUC
      D             ') (' S7200-CD-ADD-CTFR
      D             ') (' S7200-CD-URL-LS-CNCD
      D             ') (' S7200-NR-SRE-CTFD-DGTL
      D             ')'.
      *
           MOVE SPACES      TO GDA-DFHEIBLK.
      *
           CALL ICDS7200 USING GDA-DFHEIBLK
                               ICDS7200-DADOS.
           MOVE KRTN-CD-RTN OF ICDS7200-DADOS TO GDA-CD-RTN.
      *
      D    MOVE GDA-CD-RTN  TO KDPY-CD-RTN.
      D    DISPLAY '000 ' CTE-PRGM ' - Volta da sub-rotina ICDS7200'
      D            ' - RETURN-CODE = ' KDPY-CD-RTN.
      *
           IF  KRTN-CD-RTN OF ICDS7200-DADOS NOT EQUAL ZEROS
               MOVE 'SEQ-CTFC' TO S9100-CD-LCZC-ERRO-FON
               PERFORM 999000-ERRO-ICDS7200
           END-IF.
      *
      D    DISPLAY 'EST-CTFD-DGTL (' S7200-CD-EST-CTFD-DGTL         ')'.
      D    DISPLAY 'EST-LS-CNCD   (' S7200-CD-EST-LS-CNCD           ')'
      D                       ' - (' S7200-TS-EST-LS-CNCD           ')'.
      *
       720099-SAI.
           EXIT.
      *
      *--------------------------------------*
       730000-EXECUTA-ICDS2200        SECTION.
      *--------------------------------------*
      D    DISPLAY '000 ' CTE-PRGM ' - 730000-EXECUTA-ICDS2200       '.
      *
      *    SET  S2200-IDFR-VRS-PRM-ATU-OK   TO TRUE.
      *    SET  S2200-FUC-REF               TO TRUE.
      *    MOVE S2100-CD-IDFR-EMT-CTFD      TO S2200-CD-ADD-CTFR.
      *    MOVE S2100-NR-SRE-BIN            TO S2200-CD-NR-SRE-CTFD.
      *
           SET  S2200-IDFR-VRS-PRM-ATU-OK   TO TRUE.
           SET  S2200-FUC-TX                TO TRUE.
           SET  S2200-FMT-PEM               TO TRUE.
           MOVE S3200-ENTD-TX-CTFD-DGTL     TO S2200-ENTD-TX-CTFD-DGTL.
      *
      D    DISPLAY '000 ' CTE-PRGM ' - Antes da sub-rotina ICDS2200'.
      *
      D    DISPLAY '000 ' CTE-PRGM
      D             ' - ' CTE-VERS
      D             ' - ' ICDS2200
      D               '(' S2200-ENTD-IDFR-VRS-PRM
      D             ') (' S2200-ENTD-CD-FUC
      D             ')'.
      D    DISPLAY '000 ' CTE-PRGM
      D             ' - ' CTE-VERS
      D             ' - ' ICDS2200
      D               '(' S2100-TX-IDFR-EMT-CTFD
      D             ') (' S2200-CD-ADD-CTFR
      D             ')'.
      D    DISPLAY '000 ' CTE-PRGM
      D             ' - ' CTE-VERS
      D             ' - ' ICDS2200
      D               '(' S2100-NR-SRE-HEX
      D             ') (' S2200-CD-NR-SRE-CTFD
      D             ')'.
      D    DISPLAY '000 ' CTE-PRGM
      D             ' - ' CTE-VERS
      D             ' - ' ICDS2200
      D               '(' S2200-ENTD-IN-FMT-ENTD
      D             ') (' S2200-TX-CTFD-DGTL-TEXT
      D                (1:S2200-TX-CTFD-DGTL-SIZE)
      D             ')'.
      *
           MOVE SPACES      TO GDA-DFHEIBLK.
      *
           CALL ICDS2200 USING GDA-DFHEIBLK
                               ICDS2200-DADOS.
           MOVE KRTN-CD-RTN OF ICDS2200-DADOS TO S2200-CD-RTN.
      *
      D    MOVE S2200-CD-RTN TO KDPY-CD-RTN.
      D    DISPLAY '000 ' CTE-PRGM ' - Volta da sub-rotina ICDS2200'
      D            ' - RETURN-CODE = ' KDPY-CD-RTN.
      D    MOVE KRTN-CD-RTN-AUX OF ICDS2200-DADOS TO KDPY-INTEGER.
      D    DISPLAY '000 ' CTE-PRGM ' - Cod. MCI = '  KDPY-INTEGER.
      *
           IF  NOT S2200-FIM-NML
               MOVE 'VCLR-MCI' TO S9100-CD-LCZC-ERRO-FON
               PERFORM 999000-ERRO-ICDS2200
           END-IF.
      *
      D    DISPLAY 'MCI-CD-IDFC (' KRTN-CD-RTN-AUX OF ICDS2200-DADOS ')'
      D    .
      *
       730099-SAI.
           EXIT.
      *
      *--------------------------------------*
      *800000-SUBROTINAS              SECTION.
      *--------------------------------------*
      *
      *--------------------------------------*
      *810000-ACESSO-TABELAS-SISTEMA  SECTION.
      *--------------------------------------*
      *
      *--------------------------------------*
      *811000-TABELAS-FISICAS         SECTION.
      *--------------------------------------*
      *
      *--------------------------------------*
       811000-EXECUTA-ICDS002P        SECTION.
      *--------------------------------------*
      D    DISPLAY '000 ' CTE-PRGM ' - 811000-EXECUTA-ICDS002P'
      D                            ' - DB2ICD.IDFR_CHV_ADD_CTFR'.
      *
      D    DISPLAY '000 ' CTE-PRGM ' - Antes da sub-rotina ICDS002P'.
      *
      D    DISPLAY '000 ' CTE-PRGM
      D            ' - '  CTE-VERS
      D            ' - '  ICDS002P.
      D    PERFORM 811000-DISPLAY-ICDK002W.
      *
      *    Open cursor DB2ICD.IDFR_CHV_ADD_CTFR
           EXEC SQL
                OPEN IDFR-CHV-ADD
           END-EXEC.
      *
           MOVE ZERO TO P002-QT-REG.
           MOVE ZERO TO P002-IN-FIM.
      *
           PERFORM UNTIL IN-FIM-002P
      *
      *        Pesquisa tabela DB2ICD.IDFR_CHV_ADD_CTFR
               EXEC SQL
                    FETCH  IDFR-CHV-ADD
                     INTO  :K002-CD-ADD-CTFR
                        ,  :K002-CD-NR-SRE-CTFD
                        ,  :K002-CD-ADD-CTFR-DTTR
                        ,  :K002-TX-IDFR-CHV-DTTR
               END-EXEC
      *
               EVALUATE SQLCODE
      *
      *        Registro localizado
                   WHEN +0
                        PERFORM 811000-RETORNO-ICDS002P
      *
      *        Registro nao localizado
                   WHEN +100
                        IF  P002-QT-REG EQUAL ZEROS
                            MOVE 'ICDS002P' TO S9100-CD-LCZC-ERRO-FON
                            GO TO 999000-ERRO-ICDS002P
                        ELSE
                            MOVE 1 TO GDA-IN-FIM-002P
                        END-IF
      *
      *        Registro nao localizado
                   WHEN OTHER
                        MOVE 'ICDS002P' TO S9100-CD-LCZC-ERRO-FON
                        GO TO 999000-ERRO-ICDS002P
               END-EVALUATE
           END-PERFORM.
      *
      *    Close cursor DB2ICD.IDFR_CHV_ADD_CTFR
           EXEC SQL
                CLOSE IDFR-CHV-ADD
           END-EXEC.
      *
      D    PERFORM 811000-DISPLAY-ICDK002W.
      *
       811099-SAI-EXECUTA-ICDS002P.
           EXIT.
      *
      *--------------------------------------*
      D811000-DISPLAY-ICDK002W        SECTION.
      *--------------------------------------*
      *
      D    DISPLAY 'ADD-CTFR      (' K002-CD-ADD-CTFR               ')'.
      D    DISPLAY 'NR-SRE-CTFD   (' K002-CD-NR-SRE-CTFD            ')'.
      D    DISPLAY 'ADD-CTFR-DTTR (' K002-CD-ADD-CTFR-DTTR          ')'.
      D    DISPLAY 'IDFR-CHV-PRTR (' K002-TX-IDFR-CHV-DTTR-SIZE     ')'
      D                         '-(' K002-TX-IDFR-CHV-DTTR-TEXT
      D                           (1:K002-TX-IDFR-CHV-DTTR-SIZE   ) ')'.
      *
      D811099-SAI-DISPLAY-ICDS002W.
      D    EXIT.
      *
      *--------------------------------------*
       811000-RETORNO-ICDS002P        SECTION.
      *--------------------------------------*
      D    DISPLAY '000 ' CTE-PRGM ' - 811000-RETORNO-ICDS002P'
      D                            ' - DB2ICD.IDFR_CHV_ADD_CTFR'.
      *
           INITIALIZE     ICDS002P-DADOS
               REPLACING  NUMERIC BY  ZEROS
                     ALPHANUMERIC BY SPACES.
      *
            IF  P002-QT-REG GREATER 200
                MOVE  1 TO  P002-IN-FIM
      *         MOVE  1 TO  GDA-IN-FIM-002P
            ELSE
                ADD  +1 TO  P002-QT-REG
      *
                MOVE        K002-CD-ADD-CTFR
                        TO  P002-CD-ADD-CTFR      ( P002-QT-REG )
                MOVE        K002-CD-NR-SRE-CTFD
                        TO  P002-CD-NR-SRE-CTFD   ( P002-QT-REG )
                MOVE        K002-CD-ADD-CTFR-DTTR
                        TO  P002-CD-ADD-CTFR-DTTR ( P002-QT-REG )
                MOVE        K002-TX-IDFR-CHV-DTTR
                        TO  P002-TX-IDFR-CHV-DTTR ( P002-QT-REG )
            END-IF.
      *
       811099-SAI-RETORNO-ICDS002P.
           EXIT.
      *
      *--------------------------------------*
      *D811000-DISPLAY-ICDK002P        SECTION.                         
      *--------------------------------------*
      *
      *D   MOVE +1 TO NDX.                                              
      *D   PERFORM UNTIL NDX NOT LESS P002-QT-REG                       
      *D       DISPLAY 'CD-ADD-CTFR(' P002-CD-ADD-CTFR      (NDX)    ')'
      *D       DISPLAY 'NR-SRE-CTFD(' P002-CD-NR-SRE-CTFD   (NDX)    ')'
      *D       DISPLAY 'ADD-DTTR   (' P002-CD-ADD-CTFR-DTTR (NDX)    ')'
      *D       DISPLAY 'IDFR-CHV   (' P006-TX-URL-LS-SIZE   (NDX)    ')'
      *D                          '(' P006-TX-URL-LS-TEXT   (NDX)       
      *D                           (1:P006-TX-URL-LS-SIZE   (NDX)  ) ')'
      *D       ADD +1 TO NDX                                            
      *D   END-PERFORM.                                                 
      *
      *D811099-SAI-DISPLAY-ICDS002P.                                    
      *D   EXIT.                                                        
      *
      *--------------------------------------*
       811000-EXECUTA-ICDS003C        SECTION.
      *--------------------------------------*
           DISPLAY '000 ' CTE-PRGM ' - 811000-EXECUTA-ICDS003C'
                                   ' - DB2ICD.CTFD_DGTL'.
      *
           DISPLAY '000 ' CTE-PRGM ' - ANTES DA SUB-ROTINA ICDS003C'.
      *
           DISPLAY '000 ' CTE-PRGM
                   ' - '  CTE-VERS
                   ' - '  ICDS003C.
      *D   PERFORM 811000-DISPLAY-ICDK003W.
           DISPLAY 'CD-ADD-CTFR    (' K003-CD-ADD-CTFR               ')'
           DISPLAY 'CD-NR-SRE-CTFD (' K003-CD-NR-SRE-CTFD            ')'
      *    DISPLAY 'TX-NR-SRE-CTFD (' K003-TX-NR-SRE-CTFD            ')'
      *
           MOVE SPACES      TO GDA-DFHEIBLK.
      *
           CALL ICDS003C USING GDA-DFHEIBLK
                               ICDS003W-DADOS.
           MOVE KRTN-CD-RTN OF ICDS003W-DADOS TO GDA-CD-RTN.
           DISPLAY 'K003-CD-RETN : ' KRTN-CD-RTN OF ICDS003W-DADOS
           DISPLAY 'K003-CD-REA: ' KRTN-CD-REA-AUX OF ICDS003W-DADOS
           DISPLAY 'K003-CD-AUX: ' KRTN-CD-RTN-AUX OF ICDS003W-DADOS

      *
      D    MOVE GDA-CD-RTN  TO KDPY-CD-RTN.
           DISPLAY '000 ' CTE-PRGM ' - VOLTA DA SUB-ROTINA ICDS003C'
                   ' - RETURN-CODE = ' KDPY-CD-RTN.
      *
      D    IF  KRTN-CD-RTN OF ICDS003W-DADOS EQUAL ZEROS
      D        PERFORM 811000-DISPLAY-ICDK003W
      D    END-IF.
      *
       811099-SAI-EXECUTA-ICDS003C.
           EXIT.
      *
      *--------------------------------------*
       811000-EXECUTA-ICDS003I        SECTION.
      *--------------------------------------*
      D    DISPLAY '000 ' CTE-PRGM ' - 811000-EXECUTA-ICDS003I'
      D                            ' - DB2ICD.CTFD_DGTL'.
      *
      D    DISPLAY '000 ' CTE-PRGM ' - Antes da sub-rotina ICDS003I'.
      *
      D    DISPLAY '000 ' CTE-PRGM
      D            ' - '  CTE-VERS
      D            ' - '  ICDS003I.
      *D   PERFORM 811000-DISPLAY-ICDK003W.
      D    DISPLAY 'CD-ADD-CTFR    (' K003-CD-ADD-CTFR               ')'
      D    DISPLAY 'TX-NR-SRE-CTFD (' K003-TX-NR-SRE-CTFD            ')'
      *
           MOVE SPACES      TO GDA-DFHEIBLK.
           CALL ICDS003I USING GDA-DFHEIBLK
                               ICDS003W-DADOS.
           MOVE KRTN-CD-RTN OF ICDS003W-DADOS TO GDA-CD-RTN.
      *
      D    MOVE GDA-CD-RTN  TO KDPY-CD-RTN.
      D    DISPLAY '000 ' CTE-PRGM ' - Volta da sub-rotina ICDS003I'
      D            ' - RETURN-CODE = ' KDPY-CD-RTN.
      *
           IF  KRTN-CD-RTN OF ICDS003W-DADOS NOT EQUAL ZEROS
               MOVE 'ICDS003I' TO S9100-CD-LCZC-ERRO-FON
               GO TO 999000-ERRO-ICDS003I
           END-IF.
      *
      D    PERFORM 811000-DISPLAY-ICDK003W.
      *
       811099-SAI-EXECUTA-ICDS003I.
           EXIT.
      *
      *--------------------------------------*
      D811000-DISPLAY-ICDK003W        SECTION.
      *--------------------------------------*
      *
      D    DISPLAY 'CD-ADD-CTFR    (' K003-CD-ADD-CTFR               ')'
      D    DISPLAY 'CD-NR-SRE-CTFD (' K003-CD-NR-SRE-CTFD            ')'
      D    DISPLAY 'TX-NR-SRE-CTFD (' K003-TX-NR-SRE-CTFD            ')'
      D    DISPLAY 'IDFR-SEQ-CTF   (' K003-CD-IDFR-SEQ-CTFC          ')'
      D    DISPLAY 'FNLD-CTFD-DG   (' K003-CD-FNLD-CTFD-DGTL         ')'
      D    DISPLAY 'NTZ-TITR-CTF   (' K003-CD-NTZ-TITR-CTFD          ')'
      D    DISPLAY 'URL-LS-CNCD    (' K003-CD-URL-LS-CNCD            ')'
      D    DISPLAY 'TS-EMS-CTFD    (' K003-TS-EMS-CTFD-DGTL          ')'
      D    DISPLAY 'TS-EXPC-CTFD   (' K003-TS-EXPC-CTFD-DGTL         ')'
      D    DISPLAY 'TS-CNCT-CTFD   (' K003-TS-CNCT-CTFD-DGTL         ')'
      D    DISPLAY 'CD-EST-CTFD    (' K003-CD-EST-CTFD-DGTL          ')'
      D    DISPLAY 'TS-EST-CTFD    (' K003-TS-EST-CTFD-DGTL          ')'
      D    DISPLAY 'CD-USU-RSP-EST (' K003-CD-USU-RSP-EST            ')'
      D    DISPLAY 'NM-PGM-RSP-EST (' K003-NM-PGM-RSP-EST            ')'
      D    DISPLAY 'NM-DTTR-CTFD   (' K003-NM-DTTR-CTFD-DGTL         ')'
      D    DISPLAY 'MDU-CHV-PBCO   (' K003-TX-MDU-CHV-PBCO-SIZE      ')'
      D                          '-(' K003-TX-MDU-CHV-PBCO-TEXT
      D                            (1:K003-TX-MDU-CHV-PBCO-SIZE    ) ')'
      D    DISPLAY 'EXPT-CHV-PBCO  (' K003-TX-EXPT-CHV-PBCO-SIZE     ')'
      D                          '-(' K003-TX-EXPT-CHV-PBCO-TEXT
      D                            (1:K003-TX-EXPT-CHV-PBCO-SIZE   ) ')'
      D    DISPLAY 'TX-CTFD-DGTL   (' K003-TX-CTFD-DGTL-SIZE         ')'
      D                          '-(' K003-TX-CTFD-DGTL-TEXT
      D                            (1:K003-TX-CTFD-DGTL-SIZE       ) ')'
      D    .
      *
      D811099-SAI-DISPLAY-ICDK003W.
      D    EXIT.
      *
      *--------------------------------------*
       811000-EXECUTA-ICDS004I        SECTION.
      *--------------------------------------*
      D    DISPLAY '000 ' CTE-PRGM ' - 811000-EXECUTA-ICDS004I       '.
      *
      D    DISPLAY '000 ' CTE-PRGM ' - Antes da sub-rotina ICDS004I'.
      *
      D    DISPLAY '000 ' CTE-PRGM
      D            ' - '  CTE-VERS
      D            ' - '  ICDS004I.
      *D   PERFORM 811000-DISPLAY-ICDK004W.
      D    DISPLAY 'CD-ADD-CTFR    (' K004-CD-ADD-CTFR               ')'
      D    DISPLAY 'TX-NR-SRE-CTFD (' K004-TX-NR-SRE-CTFD            ')'
      *
           MOVE SPACES      TO GDA-DFHEIBLK.
           CALL ICDS004I USING GDA-DFHEIBLK
                               ICDS004W-DADOS.
           MOVE KRTN-CD-RTN OF ICDS004W-DADOS TO GDA-CD-RTN.
      *
      D    MOVE GDA-CD-RTN  TO KDPY-CD-RTN.
      D    DISPLAY '000 ' CTE-PRGM ' - Volta da sub-rotina ICDS004I'
      D            ' - RETURN-CODE = ' KDPY-CD-RTN.
      *
           IF  KRTN-CD-RTN OF ICDS004W-DADOS NOT EQUAL ZEROS
               MOVE 'ICDS004I' TO S9100-CD-LCZC-ERRO-FON
               GO TO 999000-ERRO-ICDS004I
           END-IF.
      *
      D    PERFORM 811000-DISPLAY-ICDK004W.
      *
       811099-SAI-EXECUTA-ICDS004I.
           EXIT.
      *
      *--------------------------------------*
      D811000-DISPLAY-ICDK004W        SECTION.
      *--------------------------------------*
      *
      D    DISPLAY 'CD-ADD-CTFR    (' K004-CD-ADD-CTFR               ')'
      D    DISPLAY 'CD-NR-SRE-CTFD (' K004-CD-NR-SRE-CTFD            ')'
      D    DISPLAY 'TX-NR-SRE-CTFD (' K004-TX-NR-SRE-CTFD            ')'
      D    DISPLAY 'TS-ALT-TAB     (' K004-TS-ALT-TAB                ')'
      D    DISPLAY 'CD-ALT-TAB     (' K004-CD-ALT-TAB                ')'
      D    DISPLAY 'CD-RSP-ALT-TAB (' K004-CD-RSP-ALT-TAB            ')'
      D    DISPLAY 'IDFR-SEQ-CTF   (' K004-CD-IDFR-SEQ-CTFC          ')'
      D    DISPLAY 'FNLD-CTFD-DG   (' K004-CD-FNLD-CTFD-DGTL         ')'
      D    DISPLAY 'NTZ-TITR-CTF   (' K004-CD-NTZ-TITR-CTFD          ')'
      D    DISPLAY 'URL-LS-CNCD    (' K004-CD-URL-LS-CNCD            ')'
      D    DISPLAY 'TS-EMS-CTFD    (' K004-TS-EMS-CTFD-DGTL          ')'
      D    DISPLAY 'TS-EXPC-CTFD   (' K004-TS-EXPC-CTFD-DGTL         ')'
      D    DISPLAY 'TS-CNCT-CTFD   (' K004-TS-CNCT-CTFD-DGTL         ')'
      D    DISPLAY 'CD-EST-CTFD    (' K004-CD-EST-CTFD-DGTL          ')'
      D    DISPLAY 'TS-EST-CTFD    (' K004-TS-EST-CTFD-DGTL          ')'
      D    DISPLAY 'CD-USU-RSP-EST (' K004-CD-USU-RSP-EST            ')'
      D    DISPLAY 'NM-PGM-RSP-EST (' K004-NM-PGM-RSP-EST            ')'
      D    DISPLAY 'NM-DTTR-CTFD   (' K004-NM-DTTR-CTFD-DGTL         ')'
      D    DISPLAY 'MDU-CHV-PBCO   (' K004-TX-MDU-CHV-PBCO-SIZE      ')'
      D                          '-(' K004-TX-MDU-CHV-PBCO-TEXT
      D                            (1:K004-TX-MDU-CHV-PBCO-SIZE    ) ')'
      D    DISPLAY 'EXPT-CHV-PBCO  (' K004-TX-EXPT-CHV-PBCO-SIZE     ')'
      D                          '-(' K004-TX-EXPT-CHV-PBCO-TEXT
      D                            (1:K004-TX-EXPT-CHV-PBCO-SIZE   ) ')'
      D    DISPLAY 'TX-CTFD-DGTL   (' K004-TX-CTFD-DGTL-SIZE         ')'
      D                          '-(' K004-TX-CTFD-DGTL-TEXT
      D                            (1:K004-TX-CTFD-DGTL-SIZE       ) ')'
      D    .
      *
      D811099-SAI-DISPLAY-ICDK004W.
      D    EXIT.
      *
      *--------------------------------------*
       811000-EXECUTA-ICDS006C        SECTION.
      *--------------------------------------*
      D    DISPLAY '000 ' CTE-PRGM ' - 811000-EXECUTA-ICDS006C'
      D                            ' - DB2ICD.URL_LS_CTFD_CNCD'.
      *
      D    DISPLAY '000 ' CTE-PRGM ' - Antes da sub-rotina ICDS006C'.
      *
      D    DISPLAY '000 ' CTE-PRGM
      D            ' - '  CTE-VERS
      D            ' - '  ICDS006C.
      *D   PERFORM 811000-DISPLAY-ICDK006W.
      D    DISPLAY 'ADD-CTFR       (' K006-CD-ADD-CTFR              ')'.
      D    DISPLAY 'CD-URL-LCR     (' K006-CD-URL-LS-CNCD           ')'.
      *
           MOVE SPACES      TO GDA-DFHEIBLK.
           CALL ICDS006C USING GDA-DFHEIBLK
                               ICDS006W-DADOS.
           MOVE KRTN-CD-RTN OF ICDS006W-DADOS TO GDA-CD-RTN.
      *
      D    MOVE GDA-CD-RTN  TO KDPY-CD-RTN.
      D    DISPLAY '000 ' CTE-PRGM ' - Volta da sub-rotina ICDS006C'
      D            ' - RETURN-CODE = ' KDPY-CD-RTN.
      *
           IF  KRTN-CD-RTN OF ICDS006W-DADOS NOT EQUAL ZEROS
               MOVE 'ICDS006C' TO S9100-CD-LCZC-ERRO-FON
               GO TO 999000-ERRO-ICDS006C
           END-IF.
      *
      D    PERFORM 811000-DISPLAY-ICDK006W.
      *
       811099-SAI-EXECUTA-ICDS006C.
           EXIT.
      *
      *--------------------------------------*
       811000-EXECUTA-ICDS006I        SECTION.
      *--------------------------------------*
      D    DISPLAY '000 ' CTE-PRGM ' - 811000-EXECUTA-ICDS006I'
      D                            ' - DB2ICD.URL_LS_CTFD_CNCD'.
      *
      D    DISPLAY '000 ' CTE-PRGM ' - Antes da sub-rotina ICDS006I'.
      *
           MOVE 'M'                          TO K006-CD-EST-LS-CNCD.    
           PERFORM 907000-EXECUTA-ICDSUTC0.                             
           MOVE UTCW-DB2-TIME                TO K006-TS-EST-LS-CNCD.    
           MOVE '0001-01-01-00.00.00.000000' TO K006-TS-EMS-LS-CNCD.
           MOVE '0001-01-01-00.00.00.000000' TO K006-TS-VLD-LS-CNCD.
           MOVE '0001-01-01-00.00.00.000000' TO K006-TS-PRX-ATL-LS-CNCD.
      *
      D    DISPLAY '000 ' CTE-PRGM
      D            ' - '  CTE-VERS
      D            ' - '  ICDS006I.
      D    PERFORM 811000-DISPLAY-ICDK006W.
      *
           MOVE SPACES      TO GDA-DFHEIBLK.
           CALL ICDS006I USING GDA-DFHEIBLK
                               ICDS006W-DADOS.
           MOVE KRTN-CD-RTN OF ICDS006W-DADOS TO GDA-CD-RTN.
      *
      D    MOVE GDA-CD-RTN  TO KDPY-CD-RTN.
      D    DISPLAY '000 ' CTE-PRGM ' - Volta da sub-rotina ICDS006I'
      D            ' - RETURN-CODE = ' KDPY-CD-RTN.
      D    DISPLAY 'CD-URL-LCR-out (' K006-CD-URL-LS-CNCD           ')'.
      *
           IF  KRTN-CD-RTN OF ICDS006W-DADOS NOT EQUAL ZEROS
               MOVE 'ICDS006I' TO S9100-CD-LCZC-ERRO-FON
               GO TO 999000-ERRO-ICDS006I
           END-IF.
      *
      D    PERFORM 811000-DISPLAY-ICDK006W.
      *
       811099-SAI-EXECUTA-ICDS006I.
           EXIT.
      *
      *--------------------------------------*
      D811000-DISPLAY-ICDK006W        SECTION.
      *--------------------------------------*
      *
      D    DISPLAY 'ADD-CTFR       (' K006-CD-ADD-CTFR              ')'.
      D    DISPLAY 'CD-URL-LCR     (' K006-CD-URL-LS-CNCD           ')'.
      D    DISPLAY 'TX-URL-LCR     (' K006-TX-URL-LS-CNCD-TEXT
      D                            (1:K006-TX-URL-LS-CNCD-SIZE    ) ')'.
      D    DISPLAY 'CD-EST-LCR     (' K006-CD-EST-LS-CNCD           ')'.
      D    DISPLAY 'TS-EST-LCR     (' K006-TS-EST-LS-CNCD           ')'.
      D    DISPLAY 'ITVL-MAX-ATL   (' K006-HH-ITVL-MAX-ATL-LS       ')'.
      D    DISPLAY 'TS-PRX-ATL-LCR (' K006-TS-PRX-ATL-LS-CNCD       ')'.
      D    DISPLAY 'TS-EMS-LCR     (' K006-TS-EMS-LS-CNCD           ')'.
      D    DISPLAY 'TS-VLD-LCR     (' K006-TS-VLD-LS-CNCD           ')'.
      D    DISPLAY 'NR-SEQL-LCR    (' K006-NR-SEQL-LS-CNCD          ')'.
      *
      D811099-SAI-DISPLAY-ICDS006W.
      D    EXIT.
      *
      *--------------------------------------*
       811000-EXECUTA-ICDS006P        SECTION.
      *--------------------------------------*
      D    DISPLAY '000 ' CTE-PRGM ' - 811000-EXECUTA-ICDS006P'
      D                            ' - DB2ICD.URL_LS_CTFD_CNCD'.
      *
      D    DISPLAY '000 ' CTE-PRGM ' - Antes da sub-rotina ICDS006P'.
      *
      D    DISPLAY '000 ' CTE-PRGM
      D            ' - '  CTE-VERS
      D            ' - '  ICDS006P.
      *D   PERFORM 811000-DISPLAY-ICDK006W.
      D    DISPLAY 'ADD-CTFR       (' K006-CD-ADD-CTFR              ')'.
      D    DISPLAY 'TX-URL-LCR     (' K006-TX-URL-LS-CNCD-TEXT
      D                            (1:K006-TX-URL-LS-CNCD-SIZE    ) ')'.
      *
      *    Open cursor tabela DB2ICD.URL_LS_CTFD_CNCD
      D    DISPLAY '000 ' CTE-PRGM ' - Open cursor URL_LS_CTFD_CNCD'
           EXEC SQL
                OPEN LS-CTFD-CNCD
           END-EXEC.
      *
           MOVE ZERO TO P006-QT-REG.
           MOVE ZERO TO P006-IN-FIM.
      *
           PERFORM UNTIL IN-FIM-006P
      *
      *        Pesquisa tabela DB2ICD.URL_LS_CTFD_CNCD
      D        DISPLAY '000 ' CTE-PRGM ' - Pesquisa URL_LS_CTFD_CNCD'
               EXEC SQL
                    FETCH  LS-CTFD-CNCD
                     INTO  :K006-CD-ADD-CTFR
                        ,  :K006-CD-URL-LS-CNCD
                        ,  :K006-CD-EST-LS-CNCD
                        ,  :K006-TS-EST-LS-CNCD
                        ,  :K006-TX-URL-LS-CNCD
               END-EXEC
      *
      D        DISPLAY '000 ' CTE-PRGM ' - SQLCODE = ' SQLCODE
      *
               EVALUATE SQLCODE
      *
      *        Registro localizado
               WHEN +0
      D             DISPLAY '000 ' CTE-PRGM ' - Registro localizado    '
                    PERFORM 811000-RETORNO-ICDS006P
                    MOVE P006-CD-URL-LS-CNCD (1) TO K006-CD-URL-LS-CNCD
                    MOVE 1 TO GDA-IN-FIM-006P
                    GO TO 811088-CLOSE-CURSOR
      *
      *        Registro nao localizado
               WHEN +100
      D             DISPLAY '000 ' CTE-PRGM ' - Registro nao localizado'
                    MOVE 1 TO GDA-IN-FIM-006P
                    GO TO 811088-CLOSE-CURSOR
      *
      *        Outro valor de retorno
               WHEN OTHER
      D             DISPLAY '000 ' CTE-PRGM ' - Outro valor de retorno '
                    MOVE 'ICDS006P' TO S9100-CD-LCZC-ERRO-FON
                    GO TO 999000-ERRO-ICDS006P
      *
               END-EVALUATE
      *
           END-PERFORM.
      *
      *    Close cursor DB2ICD.URL_LS_CTFD_CNCD
       811088-CLOSE-CURSOR.
      D    DISPLAY '000 ' CTE-PRGM ' - Close cursor URL_LS_CTFD_CNCD'
           EXEC SQL
                CLOSE LS-CTFD-CNCD
           END-EXEC.
      *
      D    PERFORM 811000-DISPLAY-ICDK006W.
      *
       811099-SAI-EXECUTA-ICDS006P.
           EXIT.
      *
      *--------------------------------------*
       811000-RETORNO-ICDS006P        SECTION.
      *--------------------------------------*
      D    DISPLAY '000 ' CTE-PRGM ' - 811000-RETORNO-ICDS006P'
      D                            ' - DB2ICD.URL_LS_CTFD_CNCD'.
      *
           INITIALIZE     ICDS006P-DADOS
               REPLACING  NUMERIC BY  ZEROS
                     ALPHANUMERIC BY SPACES.
      *
            IF  P006-QT-REG GREATER 70
                MOVE  1  TO  P006-IN-FIM
      *         MOVE  1  TO  GDA-IN-FIM-006P
            ELSE
                ADD  +1  TO  P006-QT-REG
      *
                MOVE         K006-CD-ADD-CTFR
                         TO  P006-CD-ADD-CTFR      ( P006-QT-REG )
                MOVE         K006-CD-URL-LS-CNCD
                         TO  P006-CD-URL-LS-CNCD   ( P006-QT-REG )
                MOVE         K006-CD-EST-LS-CNCD
                         TO  P006-CD-EST-LS-CNCD   ( P006-QT-REG )
                MOVE         K006-TS-EST-LS-CNCD
                         TO  P006-TS-EST-LS-CNCD   ( P006-QT-REG )
                MOVE         K006-TX-URL-LS-CNCD
                         TO  P006-TX-URL-LS-CNCD   ( P006-QT-REG )
            END-IF.
      *
       811099-SAI-RETORNO-ICDS006P.
           EXIT.
      *
      *--------------------------------------*
      *D811000-DISPLAY-ICDK006P        SECTION.                         
      *--------------------------------------*
      *
      *D   MOVE +1 TO NDX.                                              
      *D   PERFORM UNTIL NDX NOT LESS P006-QT-REG                       
      *D       DISPLAY 'ADD-CTFR   (' P006-CD-ADD-CTFR    (NDX)      ')'
      *D       DISPLAY 'CD-URL-LCR (' P006-CD-URL-LS-CNCD (NDX)      ')'
      **D      DISPLAY 'TX-URL-LCR (' P006-TX-URL-LS-TEXT (NDX)         
      **D                          (1:P006-TX-URL-LS-SIZE (NDX)    ) ')'
      *D       DISPLAY 'CD-EST-LCR (' P006-CD-EST-LS-CNCD (NDX)      ')'
      *D       DISPLAY 'TS-EST-LCR (' P006-TS-EST-LS-CNCD (NDX)      ')'
      *D       ADD +1 TO NDX                                            
      *D   END-PERFORM.                                                 
      *
      *D811099-SAI-DISPLAY-ICDS006P.                                    
      *D    EXIT.                                                       
      *
      *--------------------------------------*
      *812000-TABELAS-LOGICAS         SECTION.
      *--------------------------------------*
      *
      *--------------------------------------*
       812000-EXECUTA-ICDSL03A        SECTION.
      *--------------------------------------*
      D    DISPLAY '000 ' CTE-PRGM ' - 812000-EXECUTA-ICDSL03A'
      D                            ' - EST_CTFD_DGTL '.
      *
           MOVE 'F01'  TO KL03-FUC.
      *
      D    DISPLAY '000 ' CTE-PRGM
      D             ' - ' CTE-VERS
      D             ' - ' ICDSL03A
      D               '(' KL03-FUC
      D             ') (' KL03-CD-EST-CTFD-DGTL (1)
      D             ')'.
      *
      D    DISPLAY '000 ' CTE-PRGM ' - Antes da sub-rotina ICDSL03A'.
      *
           MOVE SPACES      TO GDA-DFHEIBLK.
           CALL ICDSL03A USING GDA-DFHEIBLK
                               ICDSL03A-DADOS.
           MOVE KRTN-CD-RTN OF ICDSL03A-DADOS TO GDA-CD-RTN.
      *
      D    MOVE GDA-CD-RTN  TO KDPY-CD-RTN.
      D    DISPLAY '000 ' CTE-PRGM ' - Volta da sub-rotina ICDSL03A'
      D            ' - RETURN-CODE = ' KDPY-CD-RTN.
      *
           IF  KRTN-CD-RTN OF ICDSL03A-DADOS NOT EQUAL ZEROS
               MOVE 'ICDSL03A' TO S9100-CD-LCZC-ERRO-FON
               GO TO 999000-ERRO-ICDSL03A
           END-IF.
      *
      D    DISPLAY 'EST-CTFD-DGTL  (' KL03-CD-EST-CTFD-DGTL (1)     ') '
      D                        ' - (' KL03-TX-EST-CTFD-DGTL (1)     ')'.
      *
       812099-SAI-EXECUTA-ICDSL03A.
           EXIT.
      *                                                                 
      *--------------------------------------*
       820000-CONV-ICSF-RET-RES-2-HEX SECTION.
      *--------------------------------------*
      D    DISPLAY '000 ' CTE-PRGM ' - 820000-CONV-ICSF-RET-RES-2-HEX'.
      *
           SET  CNVHEX-FUC-BIN-2-HEX       TO TRUE.
           MOVE +4                         TO CNVHEX-ENTD-SIZE.
           MOVE ICSF-RETURN-CODE-X (3:2)   TO CNVHEX-ENTD-TEXT (1:2).
           MOVE ICSF-REASON-CODE-X (3:2)   TO CNVHEX-ENTD-TEXT (3:2).
           PERFORM 903000-SBCNVHEX.
      *
      D    DISPLAY ' ICSF-RETURN-CODE: '      CNVHEX-SAID-TEXT(1:4).
      D    DISPLAY ' ICSF-REASON-CODE: '      CNVHEX-SAID-TEXT(5:4).
      *
       820099-SAI.
           EXIT.
      *
      *--------------------------------------*
      *900000-INCLUDE-SUBROTINAS      SECTION.
      *--------------------------------------*
      *
      *--------------------------------------*
      *901000-SBCNVB64                SECTION.
      *901100-VALIDA-BASE64           SECTION.
      *902000-SBCNVCOD                SECTION.
      *903000-SBCNVHEX                SECTION.
      *903100-VALIDA-HEXADECIMAL      SECTION.
      *904000-SBCNVOID                SECTION.
      *905000-SBCNVUNI                SECTION.
      *--------------------------------------*
      *-INC   ICDKCNVS                                                  
      *
      ******************************************************************
      * ICDKCNVS - Fonte para sub-rotinas de conversao:                 
      *            - SBCNVB64;                                          
      *            - SBCNVCOD;                                          
      *            - SBCNVHEX;                                          
      *            - SBCNVOID;                                          
      *            - SBCNVUNI.                                          
      ******************************************************************
      * VRS001 08.09.2008 F5127685 - IMPLANTACAO.                       
      ******************************************************************
      *                                                                 
      ******************************************************************
      * SBCNVB64                                                        
      ******************************************************************
      *    Conversao Base64 / binario.                                  
      *    Converter STRING, em formato binario, para o formato Base64  
      *         em EBCDIC/ASCII ou vice-versa.                          
      ******************************************************************
      *                                                                 
      *--------------------------------------*                          
      *901000-SBCNVB64                SECTION.                          
      *--------------------------------------*                          
      *                                                                 
      *    SET  CNVB64-FIM-NML      TO TRUE.                            
      *    MOVE ZEROS               TO CNVB64-SAID-SIZE.                
      *    MOVE SPACES              TO CNVB64-SAID-TEXT.                
      *                                                                 
      *    IF  CNVB64-ENTD-SIZE GREATER LENGTH OF CNVB64-ENTD-TEXT      
      *        SET CNVB64-TAM-INVD  TO TRUE                             
      *        GO TO 901099-SAI                                         
      *    END-IF.                                                      
      *                                                                 
      *    IF  NOT CNVB64-FUC-BIN-2-B64-EBCD                            
      *    AND NOT CNVB64-FUC-B64-EBCD-2-BIN                            
      *    AND NOT CNVB64-FUC-BIN-2-B64-ASCI                            
      *    AND NOT CNVB64-FUC-B64-ASCI-2-BIN                            
      *        SET CNVB64-FUC-INVD  TO TRUE                             
      *        GO TO 901099-SAI                                         
      *    END-IF.                                                      
      *                                                                 
      *    CALL SBCNVB64 USING         CNVB64-FUC                       
      *                                CNVB64-ENTD-SIZE                 
      *                                CNVB64-ENTD-TEXT                 
      *                                CNVB64-SAID-SIZE                 
      *                                CNVB64-SAID-TEXT.                
      *    MOVE RETURN-CODE         TO CNVB64-CD-RTN.                   
      *                                                                 
      *    IF  NOT  CNVB64-FIM-NML                                      
      *        MOVE CNVB64-ENTD     TO CNVB64-SAID                      
      *    END-IF.                                                      
      *                                                                 
      *901099-SAI.                                                      
      *    EXIT.                                                        
      *                                                                 
      *--------------------------------------*                          
       901100-VALIDA-BASE64           SECTION.                          
      *--------------------------------------*                          
      *                                                                 
           IF  NOT WCNV-TAM-CMP-TAB-IS-VALID                            
               SET WCNV-ENCODE-IS-NOT-VALID TO TRUE                     
               GO TO 901199-SAI                                         
           END-IF.                                                      
      *                                                                 
           SET WCNV-ENCODE-IS-VALID TO TRUE.                            
           MOVE +1 TO WCNV-NDX-CMP-TAB.                                 
           PERFORM UNTIL WCNV-ENCODE-IS-NOT-VALID                       
                      OR WCNV-NDX-CMP-TAB GREATER WCNV-TAM-CMP-TAB      
               MOVE WCNV-BYTE(WCNV-NDX-CMP-TAB) TO WCNV-CMP-TX-BYTE     
               IF  NOT WCNV-IS-VALID-BASE64                             
                   SET WCNV-ENCODE-IS-NOT-VALID TO TRUE                 
      *            MOVE WCNV-TAM-CMP-TAB TO WCNV-NDX-CMP-TAB            
               ELSE                                                     
                   ADD +1 TO WCNV-NDX-CMP-TAB                           
               END-IF                                                   
           END-PERFORM.                                                 
      *                                                                 
       901199-SAI.                                                      
           EXIT.                                                        
      *                                                                 
      ******************************************************************
      * SBCNVCOD                                                        
      ******************************************************************
      *    Converter STRING de ASCII para EBCDIC ou EBCDIC para ASCII.  
      ******************************************************************
      *                                                                 
      *--------------------------------------*                          
      *902000-SBCNVCOD                SECTION.                          
      *--------------------------------------*                          
      *                                                                 
      *    SET  CNVCOD-FIM-NML TO TRUE.                                 
      *                                                                 
      *    CALL SBCNVCOD USING    CNVCOD-FUC                            
      *                           CNVCOD-TAB-CNV                        
      *                           CNVCOD-DADO-SIZE                      
      *                           CNVCOD-DADO-TEXT.                     
      *    MOVE RETURN-CODE    TO CNVCOD-CD-RTN.                        
      *                                                                 
      *902099-SAI.                                                      
      *    EXIT.                                                        
      *                                                                 
      ******************************************************************
      * SBCNVHEX                                                        
      ******************************************************************
      *    Converter STRING de binario para hexadecimal zonado ou       
      *    vice-versa.                                                  
      ******************************************************************
      *                                                                 
      *--------------------------------------*                          
       903000-SBCNVHEX                SECTION.                          
      *--------------------------------------*                          
      *                                                                 
           SET  CNVHEX-FIM-NML     TO TRUE.                             
           MOVE ZEROS              TO CNVHEX-SAID-SIZE.                 
           MOVE SPACES             TO CNVHEX-SAID-TEXT.                 
      *                                                                 
           CALL SBCNVHEX USING        CNVHEX-FUC                        
                                      CNVHEX-ENTD-TEXT                  
                                      CNVHEX-ENTD-SIZE                  
                                      CNVHEX-SAID-TEXT.                 
           MOVE RETURN-CODE        TO CNVHEX-CD-RTN.                    
      *                                                                 
           IF  CNVHEX-FIM-NML                                           
               EVALUATE TRUE                                            
                   WHEN CNVHEX-FUC-BIN-2-HEX                            
                        MULTIPLY       CNVHEX-ENTD-SIZE BY 2            
                          GIVING       CNVHEX-SAID-SIZE ROUNDED         
                        END-MULTIPLY                                    
                   WHEN CNVHEX-FUC-HEX-2-BIN                            
                        DIVIDE         CNVHEX-ENTD-SIZE BY 2            
                          GIVING       CNVHEX-SAID-SIZE ROUNDED         
                        END-DIVIDE                                      
                   WHEN OTHER                                           
                        SET CNVHEX-FUC-INVD TO TRUE                     
                        MOVE 0      TO CNVHEX-SAID-SIZE                 
               END-EVALUATE                                             
           ELSE                                                         
               MOVE CNVHEX-ENTD     TO CNVHEX-SAID                      
           END-IF.                                                      
      *                                                                 
       903099-SAI.                                                      
           EXIT.                                                        
      *                                                                 
      *--------------------------------------*                          
      *903100-VALIDA-HEXADECIMAL      SECTION.                          
      *--------------------------------------*                          
      *                                                                 
      *    IF  NOT WCNV-TAM-CMP-TAB-IS-VALID                            
      *        SET WCNV-ENCODE-IS-NOT-VALID TO TRUE                     
      *        GO TO 903199-SAI                                         
      *    END-IF.                                                      
      *                                                                 
      *    SET WCNV-ENCODE-IS-VALID TO TRUE.                            
      *    MOVE +1 TO WCNV-NDX-CMP-TAB.                                 
      *    PERFORM UNTIL WCNV-ENCODE-IS-NOT-VALID                       
      *               OR WCNV-NDX-CMP-TAB GREATER WCNV-TAM-CMP-TAB      
      *        MOVE WCNV-BYTE(WCNV-NDX-CMP-TAB) TO WCNV-CMP-TX-BYTE     
      *        IF  NOT WCNV-IS-VALID-HEXA                               
      *            SET WCNV-ENCODE-IS-NOT-VALID TO TRUE                 
      **           MOVE WCNV-TAM-CMP-TAB TO WCNV-NDX-CMP-TAB            
      *        ELSE                                                     
      *            ADD +1 TO WCNV-NDX-CMP-TAB                           
      *        END-IF                                                   
      *    END-PERFORM.                                                 
      *                                                                 
      *903199-SAI.                                                      
      *    EXIT.                                                        
      *                                                                 
      ******************************************************************
      * SBCNVOID                                                        
      ******************************************************************
      *    Converter OID (Object IDentifier) de hexadecimal zonado para 
      *    leitura humana (dotted number) ou vice-versa.                
      ******************************************************************
      *                                                                 
      *--------------------------------------*                          
      *904000-SBCNVOID                SECTION.                          
      *--------------------------------------*                          
      *                                                                 
      *    SET  CNVOID-FIM-NML      TO TRUE.                            
      *                                                                 
      *    IF  NOT CNVOID-FUC-OID-2-DOT                                 
      *    AND NOT CNVOID-FUC-DOT-2-OID                                 
      *        SET CNVOID-FUC-INVD  TO TRUE                             
      *        GO TO 904099-SAI                                         
      *    END-IF.                                                      
      *                                                                 
      *    CALL SBCNVOID USING         CNVOID-FUC                       
      *                                CNVOID-WRK-AREA                  
      *                                CNVOID-TXT-DOT-OID.              
      *    MOVE RETURN-CODE         TO CNVOID-CD-RTN.                   
      *                                                                 
      *904099-SAI.                                                      
      *    EXIT.                                                        
      *                                                                 
      ******************************************************************
      * SBCNVUNI                                                        
      ******************************************************************
      *    Converter STRING de ASCII/EBCDIC para UNICODE ou vice-versa. 
      ******************************************************************
      *                                                                 
      *--------------------------------------*                          
      *905000-SBCNVUNI                SECTION.                          
      *--------------------------------------*                          
      *                                                                 
      *    SET  CNVUNI-FIM-NML      TO TRUE.                            
      *    MOVE ZEROS               TO CNVUNI-SAID-SIZE.                
      *    MOVE SPACES              TO CNVUNI-SAID-TEXT.                
      *                                                                 
      *    IF  CNVUNI-ENTD-SIZE GREATER LENGTH OF CNVUNI-ENTD-TEXT      
      *        SET CNVUNI-TAM-INVD  TO TRUE                             
      *        GO TO 905099-SAI                                         
      *    END-IF.                                                      
      *                                                                 
      *    IF  NOT CNVUNI-FUC-ASCI-2-UNIC                               
      *    AND NOT CNVUNI-FUC-EBCD-2-UNIC                               
      *    AND NOT CNVUNI-FUC-UNIC-2-ASCI                               
      *    AND NOT CNVUNI-FUC-UNIC-2-EBCD                               
      *        SET CNVUNI-FUC-INVD  TO TRUE                             
      *        GO TO 905099-SAI                                         
      *    END-IF.                                                      
      *                                                                 
      *    CALL SBCNVUNI USING         CNVUNI-FUC                       
      *                                CNVUNI-ENTD-SIZE                 
      *                                CNVUNI-ENTD-TEXT                 
      *                                CNVUNI-SAID-TEXT.                
      *    MOVE RETURN-CODE         TO CNVUNI-CD-RTN.                   
      *                                                                 
      *    IF  CNVUNI-FIM-NML                                           
      *        EVALUATE TRUE                                            
      *                                                                 
      *            WHEN CNVUNI-FUC-ASCI-2-UNIC                          
      *              OR CNVUNI-FUC-EBCD-2-UNIC                          
      *                 MULTIPLY       CNVUNI-ENTD-SIZE BY 2            
      *                   GIVING       CNVUNI-SAID-SIZE ROUNDED         
      *                 END-MULTIPLY                                    
      *                                                                 
      *            WHEN CNVUNI-FUC-UNIC-2-ASCI                          
      *              OR CNVUNI-FUC-UNIC-2-EBCD                          
      *                 DIVIDE         CNVUNI-ENTD-SIZE BY 2            
      *                   GIVING       CNVUNI-SAID-SIZE ROUNDED         
      *                 END-DIVIDE                                      
      *                                                                 
      *            WHEN OTHER                                           
      *                 SET CNVUNI-FUC-INVD TO TRUE                     
      *                 MOVE 0      TO CNVUNI-SAID-SIZE                 
      *                                                                 
      *        END-EVALUATE                                             
      *    ELSE                                                         
      *        MOVE CNVUNI-ENTD     TO CNVUNI-SAID                      
      *    END-IF.                                                      
      *                                                                 
      *905099-SAI.                                                      
      *    EXIT.                                                        
      *                                                                 
      ******************************************************************
      * FIM DO BOOK ICDKCNVS                                            
      ******************************************************************
      *
      *--------------------------------------*
      *907000-EXECUTA-ICDSUTC0        SECTION.                          
      *907100-CNV-UTC-TS              SECTION.
      *907200-CNV-TS-UTC              SECTION.
      *--------------------------------------*
      *-INC   ICDKUTCS                                                  
      *                                                                 
      ******************************************************************
      * ICDKUTCS - Fonte para uso de UTC (Universal Time, Coordinated)  
      ******************************************************************
      * VRS002 12.02.2009 F5127685 - Passa a usar a ICDSUTC0.           
      * VRS001 01.12.2008 F5127685 - IMPLANTACAO.                       
      ******************************************************************
      *                                                                 
      *--------------------------------------*                          
       907000-EXECUTA-ICDSUTC0        SECTION.                          
      *--------------------------------------*                          
      D    DISPLAY '000 ' CTE-PRGM ' - 907000-EXECUTA-ICDSUTC0       '. 
      *                                                                 
      D    DISPLAY '000 ' CTE-PRGM                                      
      D             ' - ' CTE-VERS                                      
      D             ' - ' ICDSUTC0                                      
      *                                                                 
      D    DISPLAY '000 ' CTE-PRGM ' - Antes da sub-rotina ICDSUTC0'.   
      *                                                                 
           MOVE SPACES      TO GDA-DFHEIBLK.                            
           CALL ICDSUTC0 USING GDA-DFHEIBLK                             
                               ICDSUTCW-DADOS.                          
           MOVE KRTN-CD-RTN OF ICDSUTCW-DADOS TO GDA-CD-RTN.            
      *                                                                 
      D    MOVE GDA-CD-RTN  TO KDPY-CD-RTN.                             
      D    DISPLAY '000 ' CTE-PRGM ' - Volta da sub-rotina ICDSUTC0'    
      D            ' - RETURN-CODE = ' KDPY-CD-RTN.                     
      *                                                                 
           IF  KRTN-CD-RTN OF ICDSUTCW-DADOS NOT EQUAL ZEROS            
               MOVE 'ICDSUTC0' TO S9100-CD-LCZC-ERRO-FON                
               GO TO 999000-ERRO-ICDSUTC0                               
           END-IF.                                                      
      *                                                                 
      D    DISPLAY 'UTCW-GEN-TIME  (' UTCW-GEN-TIME                 ') '
      D                        ' - (' UTCW-DB2-TIME                 ')'.
      *                                                                 
       907099-SAI.                                                      
           EXIT.                                                        
      *                                                                 
      *--------------------------------------*                          
       907100-CNV-UTC-TS              SECTION.                          
      *--------------------------------------*                          
      D    DISPLAY '000 ' CTE-PRGM ' - 907100-CNV-UTC-TS             '. 
      *                                                                 
           SET       UTCW-TS-DFL   TO TRUE.                             
           MOVE CORR UTCW-GEN-TIME TO UTCW-DB2-TIME.                    
      *                                                                 
       907199-SAI.                                                      
           EXIT.                                                        
      *                                                                 
      *--------------------------------------*                          
      *907200-CNV-TS-UTC              SECTION.                          
      *--------------------------------------*                          
      *D   DISPLAY '000 ' CTE-PRGM ' - 907200-CNV-TS-UTC             '. 
      *                                                                 
      *    MOVE CORR UTCW-DB2-TIME TO UTCW-GEN-TIME.                    
      *    SET  UTCW-TIME-ZONE-DFL TO TRUE.                             
      *                                                                 
      *907299-SAI.                                                      
      *    EXIT.                                                        
      *
      ******************************************************************
      * Fim do book ICDKUTCS                                            
      ******************************************************************
      *
      *--------------------------------------*
      *906000-SBCALLER                SECTION.
      *906100-SBCALLER-F01            SECTION.
      *906200-SBCALLER-F02            SECTION.
      *906300-SBCALLER-F03            SECTION.
      *--------------------------------------*
      *-INC   ICDKPGMS                                                  
      *                                                                 
      ******************************************************************
      * ICDKPGMS - Fonte para chamada da sub-rotina SBCALLER.           
      ******************************************************************
      * VRS001 08.09.2008 F5127685 - IMPLANTACAO.                       
      ******************************************************************
      *                                                                 
      ******************************************************************
      * SBCALLER                                                        
      *    Identificar programa ou cadeia de programas chamadores da    
      *    sub-rotina.                                                  
      ******************************************************************
      *    F01: Identifica o primeiro programa da cadeia de chamadores. 
      *    F02: Identifica a cadeia de programas chamadores.            
      *    F03: Identifica o programa que chamou o chamador da SBCALLER.
      ******************************************************************
      *                                                                 
      *----------------------------------------------------------------*
      *    Prepara variaveis para armazenamento da cadeia de chamada no 
      *    log de erro e no log de operacao:                            
      *    - Sigla do Sistema Responsavel;                              
      *    - Nome do Programa Responsavel;                              
      *    - Nome do Programa Chamador;                                 
      *    - Lista de Programas Chamadores.                             
      *----------------------------------------------------------------*
      *                                                                 
      *--------------------------------------*                          
       906000-SBCALLER                SECTION.                          
      *--------------------------------------*                          
      *                                                                 
      *    IF  CALLER-SRTN-LOG                                          
      *                                                                 
      *        PERFORM 906100-SBCALLER-F01                              
      *        MOVE CALLER-NM-PGM      TO CALLER-NM-PGM-RSP             
      *                                                                 
      *        IF  NOT CALLER-FIM-NML                                   
      *            GO TO 906099-SAI                                     
      *        END-IF                                                   
      *                                                                 
      *        PERFORM 906200-SBCALLER-F02                              
      *        MOVE CALLER-ARG02       TO CALLER-LS-PGM-CHMR            
      *                                                                 
      *    ELSE                                                         
      *                                                                 
               PERFORM 906300-SBCALLER-F03                              
               MOVE CALLER-NM-PGM      TO CALLER-NM-PGM-CHMR            
      *                                                                 
      *    END-IF.                                                      
      *                                                                 
           PERFORM 906900-SBCALLER-TX-RTN.                              
      *                                                                 
           IF  NOT CALLER-FIM-NML                                       
               GO TO 906099-SAI                                         
           END-IF.                                                      
      *                                                                 
       906099-SAI.                                                      
           EXIT.                                                        
      *                                                                 
      *--------------------------------------*                          
       906100-SBCALLER-F01            SECTION.                          
      *--------------------------------------*                          
      *                                                                 
           SET  CALLER-FUC-IDFR-PRMO-PGM-SEQ TO TRUE.                   
      *                                                                 
           MOVE SPACES      TO CALLER-AREA.                             
      *                                                                 
           CALL SBCALLER USING CALLER-FUC                               
                               CALLER-AREA.                             
           MOVE RETURN-CODE TO CALLER-CD-RTN.                           
      *                                                                 
           IF  NOT CALLER-FIM-NML                                       
               GO TO 906199-SAI                                         
           END-IF.                                                      
      *                                                                 
       906199-SAI.                                                      
           EXIT.                                                        
      *                                                                 
      *--------------------------------------*                          
      *906200-SBCALLER-F02            SECTION.                          
      *--------------------------------------*                          
      *                                                                 
      *    SET  CALLER-FUC-IDFR-SEQ-PGM-CHMR TO TRUE.                   
      *                                                                 
      *    MOVE LENGTH      OF CALLER-ARG02                             
      *                     TO CALLER-ARG01.                            
      *    MOVE SPACES      TO CALLER-ARG02.                            
      *                                                                 
      *    CALL SBCALLER USING CALLER-FUC                               
      *                        CALLER-ARG01                             
      *                        CALLER-ARG02.                            
      *    MOVE RETURN-CODE TO CALLER-CD-RTN.                           
      *                                                                 
      *    IF  NOT CALLER-FIM-NML                                       
      *        GO TO 906299-SAI                                         
      *    END-IF.                                                      
      *                                                                 
      *906299-SAI.                                                      
      *    EXIT.                                                        
      *                                                                 
      *--------------------------------------*                          
       906300-SBCALLER-F03            SECTION.                          
      *--------------------------------------*                          
      *                                                                 
           SET  CALLER-FUC-IDFR-ULT-PGM-SEQ TO TRUE.                    
      *                                                                 
           MOVE SPACES      TO CALLER-AREA.                             
      *                                                                 
           CALL SBCALLER USING CALLER-FUC                               
                               CALLER-AREA.                             
           MOVE RETURN-CODE TO CALLER-CD-RTN.                           
      *                                                                 
           IF  NOT CALLER-FIM-NML                                       
               GO TO 906399-SAI                                         
           END-IF.                                                      
      *                                                                 
       906399-SAI.                                                      
           EXIT.                                                        
      *                                                                 
      *--------------------------------------*                          
       906900-SBCALLER-TX-RTN         SECTION.                          
      *--------------------------------------*                          
      *                                                                 
           MOVE ZEROS      TO CALLER-TX-RTN-SIZE.                       
           MOVE LOW-VALUES TO CALLER-TX-RTN-TEXT.                       
      *                                                                 
           EVALUATE TRUE                                                
      *                                                                 
              WHEN CALLER-FIM-NML                                       
                   STRING  'Fim Normal'                                 
                           DELIMITED BY SIZE INTO CALLER-TX-RTN-TEXT    
                   END-STRING                                           
      *                                                                 
              WHEN CALLER-ERRO-ALOC-AREA-DNMC-TRB                       
                   STRING  'Erro na execucao da macro GETMAIN'          
                           DELIMITED BY SIZE INTO CALLER-TX-RTN-TEXT    
                   END-STRING                                           
      *                                                                 
              WHEN CALLER-NR-ARG-INVD                                   
                   STRING  'Numero de argumentos invalido'              
                           DELIMITED BY SIZE INTO CALLER-TX-RTN-TEXT    
                   END-STRING                                           
      *                                                                 
              WHEN CALLER-FUC-N-DSPN                                    
                   STRING  'Funcao nao disponivel'                      
                           DELIMITED BY SIZE INTO CALLER-TX-RTN-TEXT    
                   END-STRING                                           
      *                                                                 
              WHEN CALLER-ERRO-LIB-AREA-DNMC-TRB                        
                   STRING  'Erro na execucao da macro FREEMAIN'         
                           DELIMITED BY SIZE INTO CALLER-TX-RTN-TEXT    
                   END-STRING                                           
      *                                                                 
              WHEN CALLER-SEQ-COBOL-CICS-INVD                           
                   STRING  'Cadeia COBOL/CICS invalida'                 
                           DELIMITED BY SIZE INTO CALLER-TX-RTN-TEXT    
                   END-STRING                                           
      *                                                                 
              WHEN CALLER-ERRO-IDFC-SEQ-AMB-NTRL                        
                   STRING  'Erro na identificacao da cadeia de '        
                           'programas Natural (Online ou Batch)'        
                           DELIMITED BY SIZE INTO CALLER-TX-RTN-TEXT    
                   END-STRING                                           
      *                                                                 
              WHEN CALLER-ERRO-IDFC-SEQ-AMB-GRI                         
                   STRING  'Erro na identificacao da cadeia de '        
                           'programas em ambiente GRI'                  
                           DELIMITED BY SIZE INTO CALLER-TX-RTN-TEXT    
                   END-STRING                                           
      *                                                                 
              WHEN CALLER-PGM-CHMR-PRMO-SEQ                             
                   STRING  'Programa chamador eh o primeiro da cadeia ' 
                           '(funcao F03)'                               
                           DELIMITED BY SIZE INTO CALLER-TX-RTN-TEXT    
                   END-STRING                                           
      *                                                                 
              WHEN CALLER-N-PGM-IDFD-SEQ-CHMR                           
                   STRING  'Nenhum programa identificado na cadeia de ' 
                           'chamadores'                                 
                           DELIMITED BY SIZE INTO CALLER-TX-RTN-TEXT    
                   END-STRING                                           
      *                                                                 
              WHEN CALLER-TAM-SEQ-PGM-MOR-ARG01                         
                   STRING  'Tamanho da cadeia de programas eh maior '   
                           'que ARG01 (funcao F02)'                     
                           DELIMITED BY SIZE INTO CALLER-TX-RTN-TEXT    
                   END-STRING                                           
      *                                                                 
      *--     WHEN CALLER-ERRO-IDFC-SEQ-NTRL                            
      *--          STRING  'Erro na identificacao da cadeia de '        
      *--                  'programas Natural Online ou Batch '         
      *--                  '(vide NATXXXX para erro 1XXXX)'             
      *--                  DELIMITED BY SIZE INTO CALLER-TX-RTN-TEXT    
      *--          END-STRING                                           
      *                                                                 
           END-EVALUATE.                                                
      *                                                                 
           INSPECT CALLER-TX-RTN-TEXT TALLYING                          
                   CALLER-TX-RTN-SIZE FOR CHARACTERS BEFORE LOW-VALUES. 
      *                                                                 
       906999-SAI.                                                      
           EXIT.                                                        
      *
      ******************************************************************
      *  Fim do book ICDKPGMS                                           
      ******************************************************************
      *
      *--------------------------------------*
       999000-ERROS                   SECTION.
      *--------------------------------------*
      *
       999000-ERRO-001.
           DISPLAY '000 ' CTE-PRGM ' - 999000-ERRO-001.              '.
           MOVE +0001                      TO S9100-CD-OCR
                                              S9100-CD-RTN.
           MOVE GDA-CD-USU                 TO S9100-CD-USU-RSP-OCR.
           MOVE CTE-PRGM                   TO S9100-CD-PGM-RTN.
           MOVE S9100-CD-LCZC-ERRO-FON     TO S9100-CD-ABEND.
           MOVE EIBCALEN                   TO S9100-CD-RTN-AUX.
           MOVE LENGTH OF DFHCOMMAREA      TO S9100-CD-REA-AUX.
           MOVE LOW-VALUES                 TO S9100-OTR-INF-CMPR.
           MOVE EIBCALEN                   TO KDPY-SMALLINT.
           MOVE LENGTH OF DFHCOMMAREA      TO KDPY-AUX-SIZE.
           STRING 'ERRO - PARM - Tamanho COMMAREA invalido'
                              ' - EIBCALEN: ' KDPY-SMALLINT
                              ' - COMMAREA: ' KDPY-AUX-SIZE
                       DELIMITED BY SIZE INTO S9100-OTR-INF-CMPR
           END-STRING.
           GO TO 999999-RETURN.
      *
       999000-ERRO-002.
      D    DISPLAY '000 ' CTE-PRGM ' - 999000-ERRO-002.              '.
           MOVE +0002                      TO S9100-CD-OCR
                                              S9100-CD-RTN.
           MOVE GDA-CD-USU                 TO S9100-CD-USU-RSP-OCR.
           MOVE CTE-PRGM                   TO S9100-CD-PGM-RTN.
           MOVE S9100-CD-LCZC-ERRO-FON     TO S9100-CD-ABEND.
           MOVE +0                         TO S9100-CD-RTN-AUX.
           MOVE +0                         TO S9100-CD-REA-AUX.
           MOVE LOW-VALUES                 TO S9100-OTR-INF-CMPR.
           STRING 'ERRO - PARM - versao invalida: '
                                              S3200-ENTD-IDFR-VRS-PRM
                       DELIMITED BY SIZE INTO S9100-OTR-INF-CMPR
           END-STRING.
           GO TO 999999-RETURN.
      *
       999000-ERRO-003.
      D    DISPLAY '000 ' CTE-PRGM ' - 999000-ERRO-003.              '.
           MOVE +0003                      TO S9100-CD-OCR
                                              S9100-CD-RTN.
           MOVE GDA-CD-USU                 TO S9100-CD-USU-RSP-OCR.
           MOVE CTE-PRGM                   TO S9100-CD-PGM-RTN.
           MOVE S9100-CD-LCZC-ERRO-FON     TO S9100-CD-ABEND.
           MOVE S3200-TX-CTFD-DGTL-SIZE    TO S9100-CD-RTN-AUX.
           MOVE LENGTH OF
                S3200-TX-CTFD-DGTL-TEXT    TO S9100-CD-REA-AUX.
           MOVE LOW-VALUES                 TO S9100-OTR-INF-CMPR.
           MOVE S3200-TX-CTFD-DGTL-SIZE    TO KDPY-SMALLINT.
           MOVE LENGTH OF
                S3200-TX-CTFD-DGTL-TEXT    TO KDPY-AUX-SIZE.
           STRING 'ERRO - PARM - Certificado - Tamanho Base-64 invalido'
                                           ' - informado ' KDPY-SMALLINT
                                           ' - calculado ' KDPY-AUX-SIZE
                       DELIMITED BY SIZE INTO S9100-OTR-INF-CMPR
           END-STRING.
           GO TO 999999-RETURN.
      *
       999000-ERRO-004.
      D    DISPLAY '000 ' CTE-PRGM ' - 999000-ERRO-004.              '.
           MOVE +0004                      TO S9100-CD-OCR
                                              S9100-CD-RTN.
           MOVE GDA-CD-USU                 TO S9100-CD-USU-RSP-OCR.
           MOVE CTE-PRGM                   TO S9100-CD-PGM-RTN.
           MOVE S9100-CD-LCZC-ERRO-FON     TO S9100-CD-ABEND.
           MOVE WCNV-NDX-CMP-TAB           TO S9100-CD-RTN-AUX.
           MOVE +0                         TO S9100-CD-REA-AUX.
           MOVE LOW-VALUES                 TO S9100-OTR-INF-CMPR.
           MOVE WCNV-NDX-CMP-TAB           TO KDPY-SMALLINT.
           STRING 'ERRO - PARM - Codificacao Base-64 invalida: '
                  '('                         WCNV-CMP-TX-BYTE
                  ') na posicao: '            KDPY-SMALLINT
                       DELIMITED BY SIZE INTO S9100-OTR-INF-CMPR
           END-STRING.
           GO TO 999999-RETURN.
      *
       999000-ERRO-005.
      D    DISPLAY '000 ' CTE-PRGM ' - 999000-ERRO-005.              '.
           MOVE +0005                      TO S9100-CD-OCR
                                              S9100-CD-RTN.
           MOVE GDA-CD-USU                 TO S9100-CD-USU-RSP-OCR.
           MOVE CTE-PRGM                   TO S9100-CD-PGM-RTN.
           MOVE S9100-CD-LCZC-ERRO-FON     TO S9100-CD-ABEND.
           MOVE +0                         TO S9100-CD-RTN-AUX.
           MOVE +0                         TO S9100-CD-REA-AUX.
           MOVE LOW-VALUES                 TO S9100-OTR-INF-CMPR.
           STRING 'ERRO - CERTIFICADO - O certificado fornecido eh de'
                        ' AC - Autoridade Certificadora'
                       DELIMITED BY SIZE INTO S9100-OTR-INF-CMPR
           END-STRING.
           GO TO 999999-RETURN.
      *
       999000-ERRO-006.
      D    DISPLAY '000 ' CTE-PRGM ' - 999000-ERRO-006.              '.
           MOVE +0006                      TO S9100-CD-OCR
                                              S9100-CD-RTN.
           MOVE GDA-CD-USU                 TO S9100-CD-USU-RSP-OCR.
           MOVE CTE-PRGM                   TO S9100-CD-PGM-RTN.
           MOVE S9100-CD-LCZC-ERRO-FON     TO S9100-CD-ABEND.
           MOVE +0                         TO S9100-CD-RTN-AUX.
           MOVE +0                         TO S9100-CD-REA-AUX.
           MOVE LOW-VALUES                 TO S9100-OTR-INF-CMPR.
           STRING 'ERRO - CERTIFICADO - OID (algoritmo da assinatura)'
                                      ' diferente OID (algoritmo TBS)'
                       DELIMITED BY SIZE INTO S9100-OTR-INF-CMPR
           END-STRING.
           GO TO 999999-RETURN.
      *
       999000-ERRO-007.
      D    DISPLAY '000 ' CTE-PRGM ' - 999000-ERRO-007.              '.
           MOVE +0007                      TO S9100-CD-OCR
                                              S9100-CD-RTN.
           MOVE GDA-CD-USU                 TO S9100-CD-USU-RSP-OCR.
           MOVE CTE-PRGM                   TO S9100-CD-PGM-RTN.
           MOVE S9100-CD-LCZC-ERRO-FON     TO S9100-CD-ABEND.
           MOVE +0                         TO S9100-CD-RTN-AUX.
           MOVE +0                         TO S9100-CD-REA-AUX.
           MOVE LOW-VALUES                 TO S9100-OTR-INF-CMPR.
           STRING 'ERRO - CERTIFICADO - Tamanho key-Id invalido'
                      ' - Incompativel para Usuario Final'
                       DELIMITED BY SIZE INTO S9100-OTR-INF-CMPR
           END-STRING.
           GO TO 999999-RETURN.
      *
       999000-ERRO-008.
      D    DISPLAY '000 ' CTE-PRGM ' - 999000-ERRO-008.              '.
           MOVE +0008                      TO S9100-CD-OCR
                                              S9100-CD-RTN.
           MOVE GDA-CD-USU                 TO S9100-CD-USU-RSP-OCR.
           MOVE CTE-PRGM                   TO S9100-CD-PGM-RTN.
           MOVE S9100-CD-LCZC-ERRO-FON     TO S9100-CD-ABEND.
           MOVE +0                         TO S9100-CD-RTN-AUX.
           MOVE +0                         TO S9100-CD-REA-AUX.
           MOVE LOW-VALUES                 TO S9100-OTR-INF-CMPR.
           MOVE S2100-NR-VRS-X509          TO KDPY-SMALLINT.
           STRING 'ERRO - CERTIFICADO - Versao X.509 invalida'
                      ' - versao = ' KDPY-SMALLINT ' (diferente de 3)'
                       DELIMITED BY SIZE INTO S9100-OTR-INF-CMPR
           END-STRING.
           GO TO 999999-RETURN.
      *
       999000-ERRO-009.
      D    DISPLAY '000 ' CTE-PRGM ' - 999000-ERRO-009.              '.
           MOVE +0009                      TO S9100-CD-OCR
                                              S9100-CD-RTN.
           MOVE GDA-CD-USU                 TO S9100-CD-USU-RSP-OCR.
           MOVE CTE-PRGM                   TO S9100-CD-PGM-RTN.
           MOVE S9100-CD-LCZC-ERRO-FON     TO S9100-CD-ABEND.
           MOVE +0                         TO S9100-CD-RTN-AUX.
           MOVE +0                         TO S9100-CD-REA-AUX.
           MOVE LOW-VALUES                 TO S9100-OTR-INF-CMPR.
           STRING 'ERRO - CERTIFICADO - Emissao invalida (futura)'
                      ' - emissao = '''       S2100-TS-EMS-CTFD-DGTL
                      ' - proces. = '''       UTCW-GEN-TIME     ''''    
                       DELIMITED BY SIZE INTO S9100-OTR-INF-CMPR
           END-STRING.
           GO TO 999999-RETURN.
      *
       999000-ERRO-010.
      D    DISPLAY '000 ' CTE-PRGM ' - 999000-ERRO-010.              '.
           MOVE +0010                      TO S9100-CD-OCR
                                              S9100-CD-RTN.
           MOVE GDA-CD-USU                 TO S9100-CD-USU-RSP-OCR.
           MOVE CTE-PRGM                   TO S9100-CD-PGM-RTN.
           MOVE S9100-CD-LCZC-ERRO-FON     TO S9100-CD-ABEND.
           MOVE +0                         TO S9100-CD-RTN-AUX.
           MOVE +0                         TO S9100-CD-REA-AUX.
           MOVE LOW-VALUES                 TO S9100-OTR-INF-CMPR.
           STRING 'ERRO - CADEIA DE CERTIFICACAO'
                      ' - Certificado do emissor nao encontrado'
                       DELIMITED BY SIZE INTO S9100-OTR-INF-CMPR
           END-STRING.
           GO TO 999999-RETURN.
      *
       999000-ERRO-011.
      D    DISPLAY '000 ' CTE-PRGM ' - 999000-ERRO-011.              '.
           MOVE +0011                      TO S9100-CD-OCR
                                              S9100-CD-RTN.
           MOVE GDA-CD-USU                 TO S9100-CD-USU-RSP-OCR.
           MOVE CTE-PRGM                   TO S9100-CD-PGM-RTN.
           MOVE S9100-CD-LCZC-ERRO-FON     TO S9100-CD-ABEND.
           MOVE KRTN-CD-RTN                OF ICDS0100-DADOS
                                           TO S9100-CD-RTN-AUX.
           MOVE KRTN-CD-RTN-AUX            OF ICDS0100-DADOS
                                           TO S9100-CD-REA-AUX
                                              ICSF-RETURN-CODE.
           MOVE KRTN-CD-REA-AUX            OF ICDS0100-DADOS
                                           TO ICSF-REASON-CODE.
           PERFORM 820000-CONV-ICSF-RET-RES-2-HEX.
           MOVE LOW-VALUES                 TO S9100-OTR-INF-CMPR.
           STRING 'ERRO - CERTIFICADO INVALIDO'
                      ' - Assinatura do emissor nao confere'
                      ' - ICSF-RETURN: '      CNVHEX-SAID-TEXT(1:4)
                      ' - ICSF-REASON: '      CNVHEX-SAID-TEXT(5:4)
                        DELIMITED BY SIZE INTO S9100-OTR-INF-CMPR
           END-STRING.
           GO TO 999999-RETURN.
      *
       999000-ERRO-012.
      D    DISPLAY '000 ' CTE-PRGM ' - 999000-ERRO-012.              '.
           MOVE +0012                      TO S9100-CD-OCR
                                              S9100-CD-RTN.
           MOVE GDA-CD-USU                 TO S9100-CD-USU-RSP-OCR.
           MOVE CTE-PRGM                   TO S9100-CD-PGM-RTN.
           MOVE S9100-CD-LCZC-ERRO-FON     TO S9100-CD-ABEND.
           MOVE +0                         TO S9100-CD-RTN-AUX.
           MOVE +0                         TO S9100-CD-REA-AUX.
           MOVE LOW-VALUES                 TO S9100-OTR-INF-CMPR.
           STRING 'ERRO - CADEIA DE CERTIFICACAO'
                      ' - authorityKeyId - nao encontrado'
                       DELIMITED BY SIZE INTO S9100-OTR-INF-CMPR
           END-STRING.
           GO TO 999999-RETURN.
      *
       999000-ERRO-013.
      D    DISPLAY '000 ' CTE-PRGM ' - 999000-ERRO-013.              '.
           MOVE +0013                      TO S9100-CD-OCR
                                              S9100-CD-RTN.
           MOVE GDA-CD-USU                 TO S9100-CD-USU-RSP-OCR.
           MOVE CTE-PRGM                   TO S9100-CD-PGM-RTN.
           MOVE S9100-CD-LCZC-ERRO-FON     TO S9100-CD-ABEND.
           MOVE +0                         TO S9100-CD-RTN-AUX.
           MOVE +0                         TO S9100-CD-REA-AUX.
           MOVE LOW-VALUES                 TO S9100-OTR-INF-CMPR.
           STRING 'ERRO - CADEIA DE CERTIFICACAO -'
                      ' - Certificado AC Raiz pendente'
                        DELIMITED BY SIZE INTO S9100-OTR-INF-CMPR
           END-STRING.
           GO TO 999999-RETURN.
      *
       999000-ERRO-014.
      D    DISPLAY '000 ' CTE-PRGM ' - 999000-ERRO-014.              '.
           MOVE +0014                      TO S9100-CD-OCR
                                              S9100-CD-RTN.
           MOVE GDA-CD-USU                 TO S9100-CD-USU-RSP-OCR.
           MOVE CTE-PRGM                   TO S9100-CD-PGM-RTN.
           MOVE S9100-CD-LCZC-ERRO-FON     TO S9100-CD-ABEND.
           MOVE +0                         TO S9100-CD-RTN-AUX.
           MOVE +0                         TO S9100-CD-REA-AUX.
           MOVE LOW-VALUES                 TO S9100-OTR-INF-CMPR.
           STRING 'ERRO - CERTIFICADO - URL da LCR nao encontrada no'
                                      ' certificado fornecido'
                       DELIMITED BY SIZE INTO S9100-OTR-INF-CMPR
           END-STRING.
           GO TO 999999-RETURN.
      *                                                                 
       999000-ERRO-015.                                                 
      D    DISPLAY '000 ' CTE-PRGM ' - 999000-ERRO-015.              '. 
           MOVE +0015                      TO S9100-CD-OCR              
                                              S9100-CD-RTN.             
           MOVE GDA-CD-USU                 TO S9100-CD-USU-RSP-OCR.     
           MOVE ICDS7200                   TO S9100-CD-PGM-RTN.         
           MOVE S9100-CD-LCZC-ERRO-FON     TO S9100-CD-ABEND.           
           MOVE GDA-RESP                   TO S9100-CD-RTN-AUX.         
           MOVE GDA-RESP2                  TO S9100-CD-REA-AUX.         
           MOVE LOW-VALUES                 TO S9100-OTR-INF-CMPR.       
           MOVE GDA-RESP                   TO KDPY-CD-RTN.              
           MOVE GDA-RESP2                  TO KDPY-INTEGER.             
           STRING 'ERRO - CICS - ICDS7100 '                             
                             ' - RESP'  ' = ' KDPY-CD-RTN               
                             ' - RESP2' ' = ' KDPY-INTEGER              
                       DELIMITED BY SIZE INTO S9100-OTR-INF-CMPR        
           END-STRING.                                                  
      **** GO TO 999999-RETURN.                                         
           MOVE SPACES      TO GDA-DFHEIBLK.                            
           CALL ICDS9100 USING GDA-DFHEIBLK                             
                               ICDS9100-DADOS.                          
           EXIT.                                                        
      *
       999000-ERRO-016.
      D    DISPLAY '000 ' CTE-PRGM ' - 999000-ERRO-016.              '.
           MOVE +0016                      TO S9100-CD-OCR
                                              S9100-CD-RTN.
           MOVE GDA-CD-USU                 TO S9100-CD-USU-RSP-OCR.
           MOVE CTE-PRGM                   TO S9100-CD-PGM-RTN.
           MOVE S9100-CD-LCZC-ERRO-FON     TO S9100-CD-ABEND.
           MOVE +0                         TO S9100-CD-RTN-AUX.
           MOVE +0                         TO S9100-CD-REA-AUX.
           MOVE LOW-VALUES                 TO S9100-OTR-INF-CMPR.
           STRING 'ERRO - CERTIFICADO'
                      ' - Formato do Certificado Inválido.'
                       DELIMITED BY SIZE INTO S9100-OTR-INF-CMPR
           END-STRING.
           GO TO 999999-RETURN.
      *
       999000-ERRO-017.
      D    DISPLAY '000 ' CTE-PRGM ' - 999000-ERRO-017.              '.
           MOVE +0017                      TO S9100-CD-OCR
                                              S9100-CD-RTN.
           MOVE GDA-CD-USU                 TO S9100-CD-USU-RSP-OCR.
           MOVE CTE-PRGM                   TO S9100-CD-PGM-RTN.
           MOVE S9100-CD-LCZC-ERRO-FON     TO S9100-CD-ABEND.
           MOVE +0                         TO S9100-CD-RTN-AUX.
           MOVE +0                         TO S9100-CD-REA-AUX.
           MOVE LOW-VALUES                 TO S9100-OTR-INF-CMPR.
           STRING 'ERRO - CERTIFICADO - Erro na Conversão do CTFD de'
                                      ' Binário para B64'
                       DELIMITED BY SIZE INTO S9100-OTR-INF-CMPR
           END-STRING.
           GO TO 999999-RETURN.
      *
       999000-ERRO-018.
      D    DISPLAY '000 ' CTE-PRGM ' - 999000-ERRO-018.              '.
           MOVE +0018                      TO S9100-CD-OCR
                                              S9100-CD-RTN.
           MOVE GDA-CD-USU                 TO S9100-CD-USU-RSP-OCR.
           MOVE CTE-PRGM                   TO S9100-CD-PGM-RTN.
           MOVE S9100-CD-LCZC-ERRO-FON     TO S9100-CD-ABEND.
           MOVE +0                         TO S9100-CD-RTN-AUX.
           MOVE +0                         TO S9100-CD-REA-AUX.
           MOVE LOW-VALUES                 TO S9100-OTR-INF-CMPR.
           STRING 'ERRO - CERTIFICADO - Erro na conversão do CTFD de'
                                      ' ASCII para EBCDIC'
                       DELIMITED BY SIZE INTO S9100-OTR-INF-CMPR
           END-STRING.
           GO TO 999999-RETURN.
      *
       999000-ERRO-ICDS0100.
      D    DISPLAY '000 ' CTE-PRGM ' - 999000-ERRO-ICDS0100.         '.
           MOVE +0100                      TO S9100-CD-OCR
                                              S9100-CD-RTN.
           MOVE GDA-CD-USU                 TO S9100-CD-USU-RSP-OCR.
           MOVE ICDS0100                   TO S9100-CD-PGM-RTN.
           MOVE S9100-CD-LCZC-ERRO-FON     TO S9100-CD-ABEND.
           MOVE KRTN-CD-RTN                OF ICDS0100-DADOS
                                           TO S9100-CD-RTN-AUX.
           MOVE KRTN-CD-RTN-AUX            OF ICDS0100-DADOS
                                           TO S9100-CD-REA-AUX.
           MOVE KRTN-TX-LVRE               OF ICDS0100-DADOS
                                           TO S9100-OTR-INF-CMPR.
           IF  S0100-ERRO-SBICSF12
               MOVE KRTN-CD-RTN-AUX        OF ICDS0100-DADOS
                                           TO ICSF-RETURN-CODE
               MOVE KRTN-CD-REA-AUX        OF ICDS0100-DADOS
                                           TO ICSF-REASON-CODE
               PERFORM 820000-CONV-ICSF-RET-RES-2-HEX
               MOVE LOW-VALUES             TO S9100-OTR-INF-CMPR
               STRING 'ERRO - ICDS0100 - SBICSF12'
                      ' - ICSF-RETURN: '      CNVHEX-SAID-TEXT(1:4)
                      ' - ICSF-REASON: '      CNVHEX-SAID-TEXT(5:4)
                       DELIMITED BY SIZE INTO S9100-OTR-INF-CMPR
               END-STRING
           END-IF.
           GO TO 999999-RETURN.
      *
       999000-ERRO-ICDS2100.
      D    DISPLAY '000 ' CTE-PRGM ' - 999000-ERRO-ICDS2100.         '.
           MOVE +2100                      TO S9100-CD-OCR
                                              S9100-CD-RTN.
           MOVE GDA-CD-USU                 TO S9100-CD-USU-RSP-OCR.
           MOVE ICDS2100                   TO S9100-CD-PGM-RTN.
           MOVE S9100-CD-LCZC-ERRO-FON     TO S9100-CD-ABEND.
           MOVE KRTN-CD-RTN                OF ICDS2100-DADOS
                                           TO S9100-CD-RTN-AUX.
           MOVE KRTN-CD-RTN-AUX            OF ICDS2100-DADOS
                                           TO S9100-CD-REA-AUX.
           MOVE KRTN-TX-LVRE               OF ICDS2100-DADOS
                                           TO S9100-OTR-INF-CMPR.
           GO TO 999999-RETURN.
      *
       999000-ERRO-ICDS2200.
      D    DISPLAY '000 ' CTE-PRGM ' - 999000-ERRO-ICDS2200.         '.
           MOVE +2200                      TO S9100-CD-OCR
                                              S9100-CD-RTN.
           MOVE GDA-CD-USU                 TO S9100-CD-USU-RSP-OCR.
           MOVE ICDS2200                   TO S9100-CD-PGM-RTN.
           MOVE S9100-CD-LCZC-ERRO-FON     TO S9100-CD-ABEND.
           MOVE KRTN-CD-RTN                OF ICDS2200-DADOS
                                           TO S9100-CD-RTN-AUX.
           MOVE KRTN-CD-RTN-AUX            OF ICDS2200-DADOS
                                           TO S9100-CD-REA-AUX.
           MOVE KRTN-TX-LVRE               OF ICDS2200-DADOS
                                           TO S9100-OTR-INF-CMPR.       
      **** GO TO 999999-RETURN.                                         
           MOVE SPACES      TO GDA-DFHEIBLK.                            
           CALL ICDS9100 USING GDA-DFHEIBLK                             
                               ICDS9100-DADOS.                          
           EXIT.                                                        
      *                                                                 
       999000-ERRO-ICDS7100.                                            
      D    DISPLAY '000 ' CTE-PRGM ' - 999000-ERRO-ICDS7100.         '. 
           MOVE +7100                      TO S9100-CD-OCR              
                                              S9100-CD-RTN.             
           MOVE GDA-CD-USU                 TO S9100-CD-USU-RSP-OCR.     
           MOVE ICDS7200                   TO S9100-CD-PGM-RTN.         
           MOVE S9100-CD-LCZC-ERRO-FON     TO S9100-CD-ABEND.           
           MOVE KRTN-CD-RTN                OF ICDS7100-DADOS            
                                           TO S9100-CD-RTN-AUX.         
           MOVE KRTN-CD-RTN-AUX            OF ICDS7100-DADOS            
                                           TO S9100-CD-REA-AUX.         
           MOVE KRTN-TX-LVRE               OF ICDS7100-DADOS            
                                           TO S9100-OTR-INF-CMPR.
      **** GO TO 999999-RETURN.
           MOVE SPACES      TO GDA-DFHEIBLK.
           CALL ICDS9100 USING GDA-DFHEIBLK
                               ICDS9100-DADOS.
           EXIT.
      *
       999000-ERRO-ICDS7200.
      D    DISPLAY '000 ' CTE-PRGM ' - 999000-ERRO-ICDS7200.         '.
           MOVE +7200                      TO S9100-CD-OCR
                                              S9100-CD-RTN.
           MOVE GDA-CD-USU                 TO S9100-CD-USU-RSP-OCR.
           MOVE ICDS7200                   TO S9100-CD-PGM-RTN.
           MOVE S9100-CD-LCZC-ERRO-FON     TO S9100-CD-ABEND.
           MOVE KRTN-CD-RTN                OF ICDS7200-DADOS
                                           TO S9100-CD-RTN-AUX.
           MOVE KRTN-CD-RTN-AUX            OF ICDS7200-DADOS
                                           TO S9100-CD-REA-AUX.
           MOVE KRTN-TX-LVRE               OF ICDS7200-DADOS
                                           TO S9100-OTR-INF-CMPR.
      **** GO TO 999999-RETURN.
           MOVE SPACES      TO GDA-DFHEIBLK.
           CALL ICDS9100 USING GDA-DFHEIBLK
                               ICDS9100-DADOS.
           EXIT.
      *
       999000-ERRO-ICDS002P.
      D    DISPLAY '000 ' CTE-PRGM ' - 999000-ERRO-ICDS002P.         '.
           MOVE +0402                      TO S9100-CD-OCR
                                              S9100-CD-RTN.
           MOVE GDA-CD-USU                 TO S9100-CD-USU-RSP-OCR.
           MOVE ICDS002P                   TO S9100-CD-PGM-RTN.
           MOVE S9100-CD-LCZC-ERRO-FON     TO S9100-CD-ABEND.
           MOVE SQLCODE                    TO S9100-CD-RTN-AUX.
           MOVE P002-QT-REG                TO S9100-CD-REA-AUX.
           MOVE LOW-VALUES                 TO S9100-OTR-INF-CMPR.
           STRING 'ERRO - DB2ICD.IDFR_CHV_ADD_CTFR - '
                                              SQLERRMC
                       DELIMITED BY SIZE INTO S9100-OTR-INF-CMPR
           END-STRING.
           GO TO 999999-RETURN.
      *
       999000-ERRO-ICDS003C.
      D    DISPLAY '000 ' CTE-PRGM ' - 999000-ERRO-ICDS003C.         '.
           MOVE +0403                      TO S9100-CD-OCR
                                              S9100-CD-RTN.
           MOVE GDA-CD-USU                 TO S9100-CD-USU-RSP-OCR.
           MOVE ICDS003C                   TO S9100-CD-PGM-RTN.
           MOVE S9100-CD-LCZC-ERRO-FON     TO S9100-CD-ABEND.
           MOVE KRTN-CD-RTN                OF ICDS003W-DADOS
                                           TO S9100-CD-RTN-AUX.
           MOVE KRTN-CD-RTN-AUX            OF ICDS003W-DADOS
                                           TO S9100-CD-REA-AUX.
           MOVE KRTN-TX-LVRE               OF ICDS003W-DADOS
                                           TO S9100-OTR-INF-CMPR.
           GO TO 999999-RETURN.
      *
       999000-ERRO-ICDS003I.
      D    DISPLAY '000 ' CTE-PRGM ' - 999000-ERRO-ICDS003I.         '.
           MOVE +0403                      TO S9100-CD-OCR
                                              S9100-CD-RTN.
           MOVE GDA-CD-USU                 TO S9100-CD-USU-RSP-OCR.
           MOVE ICDS003I                   TO S9100-CD-PGM-RTN.
           MOVE S9100-CD-LCZC-ERRO-FON     TO S9100-CD-ABEND.
           MOVE KRTN-CD-RTN                OF ICDS003W-DADOS
                                           TO S9100-CD-RTN-AUX.
           MOVE KRTN-CD-RTN-AUX            OF ICDS003W-DADOS
                                           TO S9100-CD-REA-AUX.
           MOVE KRTN-TX-LVRE               OF ICDS003W-DADOS
                                           TO S9100-OTR-INF-CMPR.
           GO TO 999999-RETURN.
      *
       999000-ERRO-ICDS004I.
      D    DISPLAY '000 ' CTE-PRGM ' - 999000-ERRO-ICDS004I.         '.
           MOVE +0404                      TO S9100-CD-OCR
                                              S9100-CD-RTN.
           MOVE GDA-CD-USU                 TO S9100-CD-USU-RSP-OCR.
           MOVE ICDS004I                   TO S9100-CD-PGM-RTN.
           MOVE S9100-CD-LCZC-ERRO-FON     TO S9100-CD-ABEND.
           MOVE KRTN-CD-RTN                OF ICDS004W-DADOS
                                           TO S9100-CD-RTN-AUX.
           MOVE KRTN-CD-RTN-AUX            OF ICDS004W-DADOS
                                           TO S9100-CD-REA-AUX.
           MOVE KRTN-TX-LVRE               OF ICDS004W-DADOS
                                           TO S9100-OTR-INF-CMPR.
           GO TO 999999-RETURN.
      *
       999000-ERRO-ICDS006C.
      D    DISPLAY '000 ' CTE-PRGM ' - 999000-ERRO-ICDS006C.         '.
           MOVE +0406                      TO S9100-CD-OCR
                                              S9100-CD-RTN.
           MOVE GDA-CD-USU                 TO S9100-CD-USU-RSP-OCR.
           MOVE ICDS006C                   TO S9100-CD-PGM-RTN.
           MOVE S9100-CD-LCZC-ERRO-FON     TO S9100-CD-ABEND.
           MOVE KRTN-CD-RTN                OF ICDS006W-DADOS
                                           TO S9100-CD-RTN-AUX.
           MOVE KRTN-CD-RTN-AUX            OF ICDS006W-DADOS
                                           TO S9100-CD-REA-AUX.
           MOVE KRTN-TX-LVRE               OF ICDS006W-DADOS
                                           TO S9100-OTR-INF-CMPR.
           GO TO 999999-RETURN.
      *
       999000-ERRO-ICDS006I.
      D    DISPLAY '000 ' CTE-PRGM ' - 999000-ERRO-ICDS006I.         '.
           MOVE +0406                      TO S9100-CD-OCR
                                              S9100-CD-RTN.
           MOVE GDA-CD-USU                 TO S9100-CD-USU-RSP-OCR.
           MOVE ICDS006I                   TO S9100-CD-PGM-RTN.
           MOVE S9100-CD-LCZC-ERRO-FON     TO S9100-CD-ABEND.
           MOVE KRTN-CD-RTN                OF ICDS006W-DADOS
                                           TO S9100-CD-RTN-AUX.
           MOVE KRTN-CD-RTN-AUX            OF ICDS006W-DADOS
                                           TO S9100-CD-REA-AUX.
           MOVE KRTN-TX-LVRE               OF ICDS006W-DADOS
                                           TO S9100-OTR-INF-CMPR.
           GO TO 999999-RETURN.
      *
       999000-ERRO-ICDS006P.
      D    DISPLAY '000 ' CTE-PRGM ' - 999000-ERRO-ICDS006P.         '.
           MOVE +0406                      TO S9100-CD-OCR
                                              S9100-CD-RTN.
           MOVE GDA-CD-USU                 TO S9100-CD-USU-RSP-OCR.
           MOVE ICDS006P                   TO S9100-CD-PGM-RTN.
           MOVE S9100-CD-LCZC-ERRO-FON     TO S9100-CD-ABEND.
           MOVE SQLCODE                    TO S9100-CD-RTN-AUX.
           MOVE P006-QT-REG                TO S9100-CD-REA-AUX.
           MOVE LOW-VALUES                 TO S9100-OTR-INF-CMPR.
           STRING 'ERRO - DB2ICD.URL_LS_CTFD_CNCD - '
                                              SQLERRMC
                       DELIMITED BY SIZE INTO S9100-OTR-INF-CMPR
           END-STRING.
           GO TO 999999-RETURN.
      *
       999000-ERRO-ICDSL03A.
      D    DISPLAY '000 ' CTE-PRGM ' - 999000-ERRO-ICDSL03A.         '.
           MOVE +1031                      TO S9100-CD-OCR
                                              S9100-CD-RTN.
           MOVE GDA-CD-USU                 TO S9100-CD-USU-RSP-OCR.
           MOVE ICDSL03A                   TO S9100-CD-PGM-RTN.
           MOVE S9100-CD-LCZC-ERRO-FON     TO S9100-CD-ABEND.
           MOVE KRTN-CD-RTN                OF ICDSL03A-DADOS
                                           TO S9100-CD-RTN-AUX.
           MOVE KRTN-CD-RTN-AUX            OF ICDSL03A-DADOS
                                           TO S9100-CD-REA-AUX.
           MOVE KRTN-TX-LVRE               OF ICDSL03A-DADOS
                                           TO S9100-OTR-INF-CMPR.
           GO TO 999999-RETURN.
      *
       999000-ERRO-ICDSUTC0.                                            
      D    DISPLAY '000 ' CTE-PRGM ' - 999000-ERRO-ICDSUTC0.         '. 
           MOVE +8140                      TO S9100-CD-OCR
                                              S9100-CD-RTN.
           MOVE GDA-CD-USU                 TO S9100-CD-USU-RSP-OCR.
           MOVE CTE-PRGM                   TO S9100-CD-PGM-RTN.
           MOVE S9100-CD-LCZC-ERRO-FON     TO S9100-CD-ABEND.
           MOVE KRTN-CD-RTN                OF ICDSUTCW-DADOS            
                                           TO S9100-CD-RTN-AUX.         
           MOVE KRTN-CD-RTN-AUX            OF ICDSUTCW-DADOS            
                                           TO S9100-CD-REA-AUX.         
           MOVE KRTN-TX-LVRE               OF ICDSUTCW-DADOS            
                                           TO S9100-OTR-INF-CMPR.       
           GO TO 999999-RETURN.
      *
      *--------------------------------------*
       999999-RETURN.
      *--------------------------------------*
      *
           IF  S3200-SAID-CD-EST-ACLT EQUAL SPACES
               MOVE 'Z'                  TO S3200-SAID-CD-EST-ACLT
               MOVE S9100-CD-RTN         TO KDPY-SMALLINT
               STRING                       S9100-CD-PGM-RTN
                                       ' (' KDPY-SMALLINT          ')'
                     DELIMITED BY SIZE INTO S3200-SAID-TX-EST-ACLT
               END-STRING
           END-IF.
      *
      D    DISPLAY '999 ' CTE-PRGM ' - EST-ACLT: '
      D                                '('  S3200-SAID-CD-EST-ACLT ')'
      D                                '('  S3200-SAID-TX-EST-ACLT ')'.
      *
      D    DISPLAY '999 ' CTE-PRGM ' - 999999-RETURN.                '.
      D    DISPLAY '999 ' CTE-PRGM ' - CD-OCR..: ' S9100-CD-OCR.
      D    MOVE S9100-CD-RTN                    TO KDPY-INTEGER.
      D    DISPLAY '999 ' CTE-PRGM ' - CD-RTN..: ' KDPY-INTEGER.
      D    DISPLAY '999 ' CTE-PRGM ' - USU-RSP.: ' S9100-CD-USU-RSP-OCR.
      D    DISPLAY '999 ' CTE-PRGM ' - PGM-RTN.: ' S9100-CD-PGM-RTN.
      D    DISPLAY '999 ' CTE-PRGM ' - CD-ABEND: ' S9100-CD-ABEND.
      D    MOVE S9100-CD-RTN-AUX                TO KDPY-INTEGER.
      D    DISPLAY '999 ' CTE-PRGM ' - RTN-AUX.: ' KDPY-INTEGER.
      D    MOVE S9100-CD-REA-AUX                TO KDPY-INTEGER.
      D    DISPLAY '999 ' CTE-PRGM ' - REA-AUX.: ' KDPY-INTEGER.
      D    DISPLAY '999 ' CTE-PRGM ' - INF-CMPR: ' S9100-OTR-INF-CMPR.
      *
           MOVE S9100-CD-PGM-RTN   TO KRTN-CD-PGM-RTN OF GDA-LKS-ENTD.
           MOVE S9100-CD-ABEND     TO KRTN-CD-ABEND   OF GDA-LKS-ENTD.
           MOVE S9100-CD-RTN       TO KRTN-CD-RTN     OF GDA-LKS-ENTD.
           MOVE S9100-CD-RTN-AUX   TO KRTN-CD-RTN-AUX OF GDA-LKS-ENTD.
           MOVE S9100-CD-REA-AUX   TO KRTN-CD-REA-AUX OF GDA-LKS-ENTD.
           MOVE S9100-OTR-INF-CMPR TO KRTN-TX-LVRE    OF GDA-LKS-ENTD.
      *
      D    DISPLAY '999 ' CTE-PRGM ' - KRTN (OF DFHCOMMAREA).       '.
      D    MOVE KRTN-CD-RTN     OF GDA-LKS-ENTD TO KDPY-INTEGER.
      D    DISPLAY '999 ' CTE-PRGM ' - CD-RTN..: ' KDPY-INTEGER.
      D    DISPLAY '999 ' CTE-PRGM ' - PGM-RTN.: ' KRTN-CD-PGM-RTN
      D                                            OF GDA-LKS-ENTD.
      D    DISPLAY '999 ' CTE-PRGM ' - CD-ABEND: ' KRTN-CD-ABEND
      D                                            OF GDA-LKS-ENTD.
      D    MOVE KRTN-CD-RTN-AUX OF GDA-LKS-ENTD TO KDPY-INTEGER.
      D    DISPLAY '999 ' CTE-PRGM ' - RTN-AUX.: ' KDPY-INTEGER.
      D    MOVE KRTN-CD-REA-AUX OF GDA-LKS-ENTD TO KDPY-INTEGER.
      D    DISPLAY '999 ' CTE-PRGM ' - REA-AUX.: ' KDPY-INTEGER.
      D    DISPLAY '999 ' CTE-PRGM ' - TX-LVRE.: ' KRTN-TX-LVRE
      D                                            OF GDA-LKS-ENTD.
      *
           MOVE SPACES      TO GDA-DFHEIBLK.
      *
           CALL ICDS9100 USING GDA-DFHEIBLK
                               ICDS9100-DADOS.
           MOVE KRTN-CD-RTN OF ICDS9100-DADOS TO GDA-CD-RTN.
      *
           IF  CD-RTN-FIM-NML
               MOVE    KRTN-TX-LVRE OF ICDS9100-DADOS                   
                    TO KRTN-TX-LVRE OF GDA-LKS-ENTD                     
      D        DISPLAY '999 ' CTE-PRGM ' - TX-LVRE.: ' KRTN-TX-LVRE
      D                                             OF GDA-LKS-ENTD
           END-IF.
      *
       999999-SAIDA.
           MOVE GDA-LKS-ENTD TO LKS-BOOK-ENTD.
           MOVE KRTN-CD-RTN  OF GDA-LKS-ENTD  TO RETURN-CODE.
           GOBACK.
      *
      *----------------------------------------------------------------*
      *    fixme [begin]
      *----------------------------------------------------------------*
      *
      *--------------------------------------*
      *D666000-FORCE-GOBACK            SECTION.                         
      *--------------------------------------*
      *D   MOVE 'FORCE-GOBACK'                  TO KRTN-TX-LVRE         
      *D                                           OF GDA-LKS-ENTD.     
      *D   DISPLAY '000 ' CTE-PRGM ' - TX-LVRE.: ' KRTN-TX-LVRE         
      *D                                           OF GDA-LKS-ENTD.     
      *D   DISPLAY '000 ' CTE-PRGM ' - S3200-SAID  ('                   
      *D                               S3200-SAID-CD-EST-ACLT       '-' 
      *D                               S3200-SAID-TX-EST-ACLT       ')'.
      *D   MOVE +888                            TO GDA-CD-RTN.          
      *D   DISPLAY '000 ' CTE-PRGM ' - RETURN-CODE (' GDA-CD-RTN    ')'.
      *
      *D   MOVE GDA-CD-RTN   TO KRTN-CD-RTN   OF GDA-LKS-ENTD.          
      *D   MOVE GDA-LKS-ENTD TO LKS-BOOK-ENTD.                          
      *D   MOVE KRTN-CD-RTN  OF GDA-LKS-ENTD  TO RETURN-CODE.           
      *
      *D666099-FINAL.                                                   
      *D   GOBACK.                                                      
      *
      *----------------------------------------------------------------*
      *    fixme [end]
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
