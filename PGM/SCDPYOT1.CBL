      *************************                                         
       IDENTIFICATION DIVISION.                                         
      *************************                                         
       PROGRAM-ID.   SCDPYOT1.                                          
       DATE-WRITTEN. 2017/07/28.                                        
       DATE-COMPILED.                                                   
      *REMARKS.                                                         
      ******************************************************************
      *    OBJECTIF    *                                               *
      ******************                                               *
      *                                                                *
      *    REF FONCTIONNELLE :                                         *
      *                                                                *
      *    BUT DU  PROGRAMME : ONE SHOT POUR EXTRAIRE INFORMATIONS DES *
      *                        FICHIERS EN ENTREE   , ET GENERER DES   *
      *                         FICHIERS  AVEC LES INFOS DE COMPTE DES *
      *                        ADHERENTS QUI APPARTIENENT AUX SINDICATS*
      *                        CIBLES DANS LA JOB.                     *
      *                                                                *
      *              APPELLE : CBLTDLI  PGM D'ACCES AUX DONNEES IMS    *
      *                                                                *
      *                        MXA2YJMA OBTENTION DE AL DATE DU JOUR   *
      *                                                                *
      *                                                                *
      *              ENTREE : FICHIERS SCDFSGIP (DONNEES SAGIP)        *
      *                                SCDFCNST (DONNEES CNESST)       *
      *                                SCDFCATG (CATEGORIES)           *
      *                                SCDFMNST (MINISTERES)           *
      *                                SCDFSYND (SYNDICATS)            *
      *                                SCAUCNST (NAS CNESST)           *
      *                                                                *
      *              SORTIE : FICHIERS SCDFADHE (LISTE DE MEMBRES)     *
      *                                SCDFABST (ADHERENTS ABSENTS)    *
      *                                SCDFINVD (ADHERENTS EXCLUS)     *
      *                                SCDFEDRG (EDL RESIL GARANTIE)   *
      *                                SCDFEDCC (EDL CHANG GARANTIE)   *
      *                                                                *
      *    AUTEUR DE LA CODIFICATION : JULIO TORRES                    *
      *    ANALYSTE ORGANIQUE        :                                 *
      *                                                                *
      *    POINTS IMPORTANTS  :                                        *
      *                                                                *
      ******************************************************************
      *                   M O D I F I C A T I O N S                    *
      ******************************************************************
      * DATE      AUTEUR   DESCRIPTION                                 *
      * --------- -------- ------------------------------------------- *
      * AAAAMMJJ           #REF:                                       *
      *                                                                *
      *                                                                *
      ******************************************************************
       ENVIRONMENT DIVISION.                                            
      **********************                                            
      *                                                                 
       CONFIGURATION SECTION.                                           

      *SOURCE-COMPUTER. IBM-370 WITH DEBUGGING MODE.
                                                                        
       INPUT-OUTPUT SECTION.                                            
      *                                                                 
       FILE-CONTROL.                                                    
      *                                                                 
      ********************** INPUT **************************           

           SELECT FILE-SAGIP           ASSIGN TO UT-S-SCDFSGIP
                 FILE STATUS IS        VAT-FILE-SAGIP-STATUS.

           SELECT FILE-CNEST           ASSIGN TO UT-S-SCDFCNST
                 FILE STATUS IS        VAT-FILE-CNEST-STATUS.

           SELECT FILE-SYND            ASSIGN TO UT-S-SCDFSYND
                 FILE STATUS IS        VAT-FILE-SYND-STATUS.

           SELECT FILE-CAT             ASSIGN TO UT-S-SCDFCATG
                 FILE STATUS IS        VAT-FILE-CAT-STATUS.

           SELECT FILE-MST             ASSIGN TO UT-S-SCDFMNST          
                 FILE STATUS IS        VAT-FILE-MST-STATUS.

           SELECT FILE-NCNES           ASSIGN TO SCACNST
                 ORGANIZATION          IS INDEXED
                 ACCESS  MODE          IS DYNAMIC
                 RECORD   KEY          IS NCNES-NAS
                 FILE  STATUS          IS VAT-FILE-NCNES-STATUS.

      ********************** OUTPUT *************************
           SELECT FILE-ABST            ASSIGN TO UT-S-SCDFABST
                 FILE STATUS IS        VAT-FILE-ABST-STATUS.

           SELECT FILE-ADHE            ASSIGN TO UT-S-SCDFADHE
                 FILE STATUS IS        VAT-FILE-ADHE-STATUS.

           SELECT FILE-INVD            ASSIGN TO UT-S-SCDFINVD
                 FILE STATUS IS        VAT-FILE-INVD-STATUS.

           SELECT FILE-EDRG            ASSIGN TO UT-S-SCDFEDRG
                 FILE STATUS IS        VAT-FILE-EDRG-STATUS.

           SELECT FILE-EDCC            ASSIGN TO UT-S-SCDFEDCC
                 FILE STATUS IS        VAT-FILE-EDCC-STATUS.

      ***************                                                   
       DATA DIVISION.                                                   
      ***************                                                   
      *                                                                 
       FILE SECTION.                                                    
      *                                                                 
       FD  FILE-SAGIP                                                   
           RECORDING MODE IS F                                          
           BLOCK CONTAINS 0 RECORDS                                     
           LABEL RECORDS ARE STANDARD.                                  
                                                                        
       01  RECORD-SAGIP                PIC  X(250).                     
      *
       FD  FILE-CNEST
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORDS ARE STANDARD.

       01  RECORD-CNEST                PIC  X(200).
      *
       FD  FILE-SYND
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORDS ARE STANDARD.

       01  RECORD-SYND                 PIC  X(20).
      *
       FD  FILE-CAT
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORDS ARE STANDARD.

       01  RECORD-CAT                  PIC  X(30).
      *
       FD  FILE-MST                                                     
           RECORDING MODE IS F                                          
           BLOCK CONTAINS 0 RECORDS                                     
           LABEL RECORDS ARE STANDARD.                                  
                                                                        
       01  RECORD-MST                  PIC  X(150).                     
      *
       FD  FILE-ABST
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORDS ARE STANDARD.

       01  RECORD-ABST                 PIC  X(250).
      *
       FD  FILE-ADHE
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORDS ARE STANDARD.

       01  RECORD-ADHE                 PIC  X(300).
      *
       FD  FILE-INVD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORDS ARE STANDARD.

       01  RECORD-INVD                 PIC  X(350).
      *
       FD  FILE-EDRG
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORDS ARE STANDARD.

       01  RECORD-EDRG                 PIC  X(100).
     *
       FD  FILE-EDCC
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORDS ARE STANDARD.

       01  RECORD-EDCC                 PIC  X(100).
     *
       FD  FILE-NCNES
           BLOCK CONTAINS  0 RECORDS
           RECORD CONTAINS 9 CHARACTERS
           LABEL RECORD IS STANDARD.

       01  RECORD-NCNES.
           05  NCNES-NAS               PIC  X(09).

       EJECT                                                            
      *                                                                 
       WORKING-STORAGE SECTION.                                         
                                                                        
       01  COMMENCEMENT-DU-MODULE.                                      
           05  FILLER                  PIC  X(27) VALUE                 
               '***DEBUT DU WORKING-STORAGE'.                           
           05  VAT-NOM-MOD             PIC  X(08) VALUE                 
               'SCDPYOT1'.                                              
      /
      *    REGROUPER LES TYPES DE VARIABLES                             
       01  FILLER                      PIC  X(14) VALUE                 
               'LES INDICES-->'.                                        

       01  LES-INDICES.
           05  IND                     PIC S9(04) VALUE ZEROES.
           05  IND-X                   PIC S9(04) VALUE ZEROES.
      *                                                                 
      /                                                                 
       01  FILLER                      PIC  X(18) VALUE
               'LES COMPTEURS-->'.

       01  LES-COMPTEURS.
           05  CPT-REC-LU              PIC  9(06) VALUE ZEROES.
           05  CPT-REC-TRAIT           PIC  9(06) VALUE ZEROES.
           05  CPT-REC-ABST            PIC  9(06) VALUE ZEROES.
           05  CPT-REC-INVD            PIC  9(06) VALUE ZEROES.
           05  CPT-REC-BYPASS          PIC  9(06) VALUE ZEROES.
           05  CPT-REC-NEXT            PIC  9(06) VALUE ZEROES.
           05  CPT-REC-BYPASS-NAS      PIC  9(06) VALUE ZEROES.
      *
           05  CPT-GAR-QTE-LU          PIC  9(06) VALUE ZEROES.
           05  CPT-GAR-EXEMP           PIC  9(06) VALUE ZEROES.

           05  CPT-EXO-A               PIC  9(06) VALUE ZEROES.
           05  CPT-EXO-B               PIC  9(06) VALUE ZEROES.
      *
      /
       01  FILLER                      PIC  X(18) VALUE                 
               'LES SENTINELLES-->'.                                    
                                                                        
       01  LES-SENTINELLES.                                             
           05  STL-FIN-CAT             PIC  X     VALUE SPACES.
           05  STL-FIN-MST             PIC  X     VALUE SPACES.
           05  STL-FIN-SYND            PIC  X     VALUE SPACES.
           05  STL-FIN-GAR             PIC  X     VALUE SPACES.
           05  STL-FIN-SAGIP           PIC  X     VALUE SPACES.
           05  STL-FIN-CNEST           PIC  X     VALUE SPACES.
           05  STL-FIN-NCNES           PIC  X     VALUE SPACES.
           05  STL-CNT-TRAIT           PIC  X     VALUE SPACES.
           05  STL-FIN-ADH-NAS         PIC  X     VALUE SPACES.
           05  STL-ADHE-EXEMP          PIC  X     VALUE SPACES.
           05  STL-ADHE-VALIDE         PIC  X     VALUE 'O'.

      *    SENTINELLES DE SEARCH BINAIRE
           05  STL-MST-TROUVE          PIC  X(03) VALUE 'NON'.          
               88  STL-MST-PRESENT                VALUE 'OUI'.          
               88  STL-MST-ABSENT                 VALUE 'NON'.          
      *
           05  STL-SYND-TROUVE         PIC  X(03) VALUE 'NON'.          
               88  STL-SYND-PRESENT               VALUE 'OUI'.          
               88  STL-SYND-ABSENT                VALUE 'NON'.          
      *
           05  STL-CAT-TROUVE          PIC  X(03) VALUE 'NON'.          
               88  STL-CAT-PRESENT                VALUE 'OUI'.          
               88  STL-CAT-ABSENT                 VALUE 'NON'.          
      *
           05  STL-NAS-CNES-TROUVE     PIC  X(03) VALUE 'NON'.
               88  STL-NAS-CNES-PRESENT           VALUE 'OUI'.
               88  STL-NAS-CNES-ABSENT            VALUE 'NON'.
      *                                                                 
      /                                                                 
       01  FILLER                      PIC  X(18) VALUE                 
               'LES CONSTANTES-->'.                                     
                                                                        
       01  LES-CONSTANTES.                                              
           05  CTL-OUI                 PIC  X(03) VALUE 'OUI'.          
           05  CTL-NON                 PIC  X(03) VALUE 'NON'.          
      *                                                                 
      /                                                                 
       01  FILLER                      PIC  X(27) VALUE                 
               'LES VARIABLES DE TRAVAIL-->'.                           
                                                                        
       01  LES-VARIABLES.                                               

      *    VARIABLES DE FILE-STATUS
           05  VAT-FILE-SAGIP-STATUS   PIC  99    VALUE ZERO.           
           05  VAT-FILE-CNEST-STATUS   PIC  99    VALUE ZERO.           
           05  VAT-FILE-NCNES-STATUS   PIC  99    VALUE ZERO.
           05  VAT-FILE-SYND-STATUS    PIC  99    VALUE ZERO.           
           05  VAT-FILE-CAT-STATUS     PIC  99    VALUE ZERO.           
           05  VAT-FILE-MST-STATUS     PIC  99    VALUE ZERO.           
           05  VAT-FILE-ABST-STATUS    PIC  99    VALUE ZERO.           
           05  VAT-FILE-ADHE-STATUS    PIC  99    VALUE ZERO.
           05  VAT-FILE-INVD-STATUS    PIC  99    VALUE ZERO.
           05  VAT-FILE-EDRG-STATUS    PIC  99    VALUE ZERO.
           05  VAT-FILE-EDCC-STATUS    PIC  99    VALUE ZERO.

      * ----------------------------------------------
      *    FICHIER ENTREE MINISTERES
      * ----------------------------------------------
           05  VAT-TB-MST-MAX          PIC S9(04)  COMP VALUE +200.
           05  VAT-TB-MST-CNT          PIC S9(04)  COMP VALUE +0.

           05  VAT-TABL-MST.
               07  VAT-MINISTERE       OCCURS 200  TIMES
                                       INDEXED BY  IND-MST.
                   09  VAT-MST-NUM     PIC  X(04)  VALUE SPACES.
                   09  VAT-MST-IN-VISE PIC  X(01)  VALUE 'N'.
                   88  VAT-MST-VISE                VALUE 'O'.
                   88  VAT-MST-NON-VISE            VALUE 'N'.
                   09  VAT-MST-NOM     PIC  X(100) VALUE SPACES.
                   09  FILLER          PIC  X(45)  VALUE SPACES.

      * ----------------------------------------------
      *    FICHIER ENTREE SYNDICAT
      * ----------------------------------------------
           05  VAT-TB-SYND-MAX             PIC S9(04)  COMP VALUE +9.
           05  VAT-TB-SYND-CNT             PIC S9(04)  COMP VALUE +0.

           05  VAT-TABL-SYND.
               07  VAT-SYND                OCCURS  9   TIMES
                                           INDEXED BY IND-SYND.
                   09  VAT-SYND-SEQ        PIC  9(02)  VALUE ZEROES.
                   09  FILLER              PIC  X      VALUE SPACES.
                   09  VAT-SYND-IN-TRAIT   PIC  X      VALUE SPACES.
                   09  FILLER              PIC  X      VALUE SPACES.
                   09  VAT-SYND-NOM        PIC  X(06)  VALUE SPACES.
                   09  FILLER              PIC  X(09)  VALUE SPACES.

      * ----------------------------------------------
      *    FICHIER ENTREE CATEGORIES
      * ----------------------------------------------
           05  VAT-TB-CAT-MAX          PIC S9(04)  COMP VALUE +50.
           05  VAT-TB-CAT-CNT          PIC S9(04)  COMP VALUE +0.
           05  VAT-AUX-CAT             PIC  X(03)  VALUE SPACES.

           05  VAT-TABL-CATG.
               07  VAT-CATEGORIES      OCCURS  50 TIMES
                                       INDEXED BY IND-CATG.
                   09  VAT-CATG-CD-ANC PIC  X(03)  VALUE SPACES.
                   09  VAT-CATG-NOUV   OCCURS   9 TIMES.
                       11  VAT-CATG-CD-NOUV
                                       PIC  X(03)  VALUE SPACES.

      * ----------------------------------------------
      *    FICHIER ENTREE SAGIP
      * ----------------------------------------------
           05  VAT-REC-SAGIP.
               07  VAT-SAGIP-ORGAN     PIC  X(140) VALUE SPACES.
               07  VAT-SAGIP-NOM       PIC  X(35)  VALUE SPACES.
               07  VAT-SAGIP-PRENOM    PIC  X(35)  VALUE SPACES.
               07  VAT-SAGIP-NAS       PIC  X(09)  VALUE SPACES.
               07  VAT-SAGIP-DT-NAIS   PIC  X(10)  VALUE SPACES.
               07  VAT-SAGIP-STATUT    PIC  X(50)  VALUE SPACES.
               07  VAT-SAGIP-STATUT-AF PIC  X(50)  VALUE SPACES.
               07  VAT-SAGIP-SYST-PAIE PIC  X(20)  VALUE SPACES.
               07  VAT-SAGIP-UNIT-SYND PIC  X(10)  VALUE SPACES.
               07  VAT-SAGIP-ADH-SYND  PIC  X(03)  VALUE SPACES.
               07  VAT-SAGIP-EXO-PLAN  PIC  X(02)  VALUE SPACES.
               07  VAT-SAGIP-DESC-EXO  PIC  X(30)  VALUE SPACES.
               07  FILLER              PIC  X(09)  VALUE SPACES.

      * ----------------------------------------------
      *    FICHIER ENTREE CNESST
      * ----------------------------------------------
           05  VAT-REC-CNEST.
               07  VAT-CNEST-NOM       PIC  X(35)  VALUE SPACES.
               07  VAT-CNEST-PRENOM    PIC  X(35)  VALUE SPACES.
               07  VAT-CNEST-NAS       PIC  X(09)  VALUE SPACES.
               07  VAT-CNEST-DT-NAIS   PIC  X(10)  VALUE SPACES.
               07  VAT-CNEST-STATUT    PIC  X(20)  VALUE SPACES.
               07  VAT-CNEST-SYND      PIC  X(10)  VALUE SPACES.
               07  VAT-CNEST-EXO       PIC  X(03)  VALUE SPACES.
               07  VAT-CNEST-RAISON    PIC  X(28)  VALUE SPACES.

      * ----------------------------------------------
      *    FICHIER SORTIE - SCDFABST
      * ----------------------------------------------
      *   'ADHERENT DONT LE NAS NON TROUVE DANS OSCAR'
      * ----------------------------------------------
           05  VAT-ABSENT.
               07  VAT-ABST-NAS        PIC  X(09)  VALUE SPACES.
               07  FILLER              PIC  X      VALUE ';'.
               07  VAT-ABST-NOM        PIC  X(25)  VALUE SPACES.
               07  FILLER              PIC  X      VALUE ';'.
               07  VAT-ABST-PRENOM     PIC  X(25)  VALUE SPACES.
               07  FILLER              PIC  X      VALUE ';'.
               07  VAT-ABST-DT-NAIS    PIC  X(10)  VALUE SPACES.
               07  FILLER              PIC  X      VALUE ';'.
               07  VAT-ABST-SYND-CODE  PIC  X(05)  VALUE SPACES.
               07  FILLER              PIC  X      VALUE ';'.
               07  VAT-ABST-TRAIT-PAIE PIC  X(10)  VALUE SPACES.
               07  FILLER              PIC  X      VALUE ';'.
               07  VAT-ABST-INFO-ORIG  PIC  X(100) VALUE SPACES.
               07  FILLER              PIC  X(62)  VALUE SPACES.

           05  VAT-ABST-ORIG-SAGIP.
               07  VAT-ABST-SAGIP-STATUT   PIC  X(50) VALUE SPACES.
               07  FILLER                  PIC  X     VALUE ';'.
               07  VAT-ABST-SAGIP-UNIT     PIC  X(10) VALUE SPACES.
               07  FILLER                  PIC  X     VALUE ';'.
               07  VAT-ABST-SAGIP-EXO      PIC  X(02) VALUE SPACES.
               07  FILLER                  PIC  X     VALUE ';'.
               07  VAT-ABST-SAGIP-DESC-EXO PIC  X(30) VALUE SPACES.
               07  FILLER                  PIC  X(04) VALUE SPACES.
               07  FILLER                  PIC  X     VALUE ';'.

           05  VAT-ABST-ORIG-CNEST.
               07  FILLER                  PIC  X      VALUE ';'.
               07  FILLER                  PIC  X      VALUE ';'.
               07  VAT-ABST-CNEST-EXO      PIC  X(03) VALUE SPACES.
               07  FILLER                  PIC  X      VALUE ';'.
               07  VAT-ABST-CNEST-RAISON   PIC  X(30) VALUE SPACES.
               07  FILLER                  PIC  X(65) VALUE SPACES.
               07  FILLER                  PIC  X      VALUE ';'.

      * ----------------------------------------------
      *    FICHIER DE SORTIE - INVALIDES.
      * ----------------------------------------------
      *   'ADHERENTS EXCLUS DE LA LISTE DES MEMBRES AVEC SYNDICAT'
      * ----------------------------------------------
           05  VAT-INVD.
               07  VAT-INVD-NAS        PIC  9(09)  VALUE ZEROES.
               07  FILLER              PIC  X      VALUE ';'.
               07  VAT-INVD-NOM        PIC  X(25)  VALUE SPACES.
               07  FILLER              PIC  X      VALUE ';'.
               07  VAT-INVD-PRENOM     PIC  X(25)  VALUE SPACES.
               07  FILLER              PIC  X      VALUE ';'.
               07  VAT-INVD-DT-NAIS    PIC  X(10)  VALUE SPACES.
               07  FILLER              PIC  X      VALUE ';'.
               07  VAT-INVD-SYND       PIC  X(05)  VALUE SPACES.
               07  FILLER              PIC  X      VALUE ';'.
               07  VAT-INVD-INFO-ORIG  PIC  X(100) VALUE SPACES.
               07  FILLER              PIC  X      VALUE ';'.
               07  VAT-INVD-NO-GR-ASR  PIC  X(10)  VALUE SPACES.
               07  FILLER              PIC  X      VALUE ';'.
               07  VAT-INVD-NO-CO-ASR  PIC  X(10)  VALUE SPACES.
               07  FILLER              PIC  X      VALUE ';'.
               07  VAT-INVD-NO-CAT     PIC  X(03)  VALUE SPACES.
               07  FILLER              PIC  X      VALUE ';'.
               07  VAT-INVD-NO-CERT-ASR PIC X(09)  VALUE SPACES.
               07  FILLER              PIC  X      VALUE ';'.
               07  VAT-INVD-CD-ST-ASR  PIC  X(01)  VALUE SPACES.
               07  FILLER              PIC  X      VALUE ';'.
               07  VAT-INVD-CD-ENVG    PIC  X(10)  VALUE SPACES.
               07  FILLER              PIC  X      VALUE ';'.
               07  VAT-INVD-DT-ENVG    PIC  X(08)  VALUE SPACES.
               07  FILLER              PIC  X      VALUE ';'.

               07  VAT-INVD-COMPL.
                   09  VAT-INVD-CD-USA PIC  X(08)  VALUE SPACES.
                   09  VAT-INVD-DT-EFF PIC  X(06)  VALUE SPACES.
                   09  VAT-INVD-TRAN   PIC  X(01)  VALUE SPACES.
                   09  FILLER          PIC  X(01)  VALUE SPACES.
                   09  VAT-INVD-CD-TRAN-EDL  PIC  X(03) VALUE SPACES.
                   09  VAT-INVD-NO-CARTE     PIC  X(02) VALUE SPACES.
                   09  VAT-INVD-NO-COMPTE    PIC  X(10) VALUE SPACES.
                   09  VAT-INVD-NO-CERT      PIC  X(09) VALUE SPACES.
                   09  VAT-INVD-TYPE-TEXTE   PIC  X(03) VALUE SPACES.
                   09  VAT-INVD-NO-LIGNE     PIC  X(03) VALUE SPACES.
                   09  VAT-INVD-NO-SECT      PIC  X(01) VALUE SPACES.
                   09  VAT-INVD-TEXTE        PIC  X(42) VALUE SPACES.

           05  VAT-INVD-ORIG-SAGIP.
               07  VAT-INVD-SAGIP-STATUT   PIC  X(50) VALUE SPACES.
               07  FILLER                  PIC  X     VALUE ';'.
               07  VAT-INVD-SAGIP-UNIT     PIC  X(10) VALUE SPACES.
               07  FILLER                  PIC  X     VALUE ';'.
               07  VAT-INVD-SAGIP-EXO      PIC  X(02) VALUE SPACES.
               07  FILLER                  PIC  X     VALUE ';'.
               07  VAT-INVD-SAGIP-DESC-EXO PIC  X(30) VALUE SPACES.
               07  FILLER                  PIC  X(05) VALUE SPACES.

           05  VAT-INVD-ORIG-CNEST.
               07  FILLER                  PIC  X     VALUE ';'.
               07  FILLER                  PIC  X     VALUE ';'.
               07  VAT-INVD-CNEST-EXO      PIC  X(03) VALUE SPACES.
               07  FILLER                  PIC  X     VALUE ';'.
               07  VAT-INVD-CNEST-RAISON   PIC  X(28) VALUE SPACES.
               07  FILLER                  PIC  X(66) VALUE SPACES.


      * ---------------------------------------------------------
      *    FICHIER SORTIE - SCDFADHE
      * ---------------------------------------------------------
      *   'LISTE DE MEMBRES AVEC SYNDICAT'
      * ---------------------------------------------------------
           05  VAT-ADHE.
               07  VAT-ADH-NO-ASS-SOC  PIC  X(09)  VALUE SPACES.
               07  FILLER              PIC  X      VALUE ';'.
               07  VAT-ADH-NOM-ASR     PIC  X(25)  VALUE SPACES.
               07  FILLER              PIC  X      VALUE ';'.
               07  VAT-ADH-PRENOM-ASR  PIC  X(25)  VALUE SPACES.
               07  FILLER              PIC  X      VALUE ';'.
               07  VAT-ADH-DT-NAIS     PIC  X(10)  VALUE SPACES.
               07  FILLER              PIC  X      VALUE ';'.
               07  VAT-ADH-SYND        PIC  X(05)  VALUE SPACES.
               07  FILLER              PIC  X      VALUE ';'.
               07  VAT-ADH-TRAIT-PAIE  PIC  X(100) VALUE SPACES.
               07  FILLER              PIC  X      VALUE ';'.
               07  VAT-ADH-NO-GR-ASR   PIC  X(10)  VALUE SPACES.
               07  FILLER              PIC  X      VALUE ';'.
               07  VAT-ADH-NO-CO-ASR   PIC  X(10)  VALUE SPACES.
               07  FILLER              PIC  X      VALUE ';'.
               07  VAT-ADH-CD-ST-ASR   PIC  X(01)  VALUE SPACES.
               07  FILLER              PIC  X      VALUE ';'.
               07  VAT-EXEMPTIONS.
                   09  VAT-ADH-GAR-EXEMP               OCCURS  5 TIMES.
                       11  VAT-ADH-CD-GAR-EXEMP PIC X(05) VALUE SPACES.
                       11  FILLER          PIC  X      VALUE ';'.
      *                11  VAT-ADH-RAISON-EXEMP PIC X(05) VALUE SPACES.
      *                11  FILLER          PIC  X      VALUE ';'.
               07  VAT-ADH-CD-TERM-ASR PIC  X(02)  VALUE SPACES.
               07  FILLER              PIC  X      VALUE ';'.
               07  VAT-ADH-NO-CERT-ASR PIC  X(09)  VALUE SPACES.
               07  FILLER              PIC  X      VALUE ';'.
               07  VAT-ADH-ANC-NO-CAT  PIC  X(03)  VALUE SPACES.
               07  FILLER              PIC  X      VALUE ';'.
               07  VAT-ADH-NOU-NO-CAT  PIC  X(03)  VALUE SPACES.
               07  FILLER              PIC  X      VALUE ';'.
               07  FILLER              PIC  X(48)  VALUE SPACES.

           05  VAT-ADH-SAGIP.
               07  VAT-ADH-SAGIP-STATUT    PIC  X(50)  VALUE SPACES.
               07  FILLER                  PIC  X      VALUE ';'.
               07  VAT-ADH-SAGIP-UNIT      PIC  X(10)  VALUE SPACES.
               07  FILLER                  PIC  X      VALUE ';'.
               07  VAT-ADH-SAGIP-EXO       PIC  X(02)  VALUE SPACES.
               07  FILLER                  PIC  X      VALUE ';'.
               07  VAT-ADH-SAGIP-DESC-EXO  PIC  X(30)  VALUE SPACES.
               07  FILLER                  PIC  X(05)  VALUE SPACES.

           05  VAT-ADH-CNEST.
               07  FILLER                  PIC  X      VALUE ';'.
               07  FILLER                  PIC  X      VALUE ';'.
               07  VAT-ADH-CNEST-EXO       PIC  X(03) VALUE SPACES.
               07  FILLER                  PIC  X      VALUE ';'.
               07  VAT-ADH-CNEST-RAISON    PIC  X(30) VALUE SPACES.
               07  FILLER                  PIC  X(64) VALUE SPACES.

      * ---------------------------------------------------------
      *    FICHIER SORTIE - SCDFEDRG
      * ---------------------------------------------------------
      *   'EDL RESILIATION DE GARANTIE'
      * ---------------------------------------------------------
           05  VAT-EDRG.
               07  VAT-EDRG-CD-USA         PIC  X(08)  VALUE SPACES.
               07  VAT-EDRG-DT-EFF         PIC  X(06)  VALUE SPACES.
               07  VAT-EDRG-TRS-FOR        PIC  X      VALUE SPACES.
               07  FILLER                  PIC  X      VALUE SPACES.
               07  VAT-EDRG-TRS-EDL        PIC  X(03)  VALUE SPACES.
               07  VAT-EDRG-NO-CARTE       PIC  X(02)  VALUE SPACES.
               07  VAT-EDRG-NO-CO-ASR      PIC  X(10)  VALUE SPACES.
               07  VAT-EDRG-NO-CERT-ASR    PIC  X(09)  VALUE SPACES.
               07  VAT-EDRG-CD-NOUV-GAR    PIC  X(05)  VALUE SPACES.
               07  VAT-EDRG-CD-RES         PIC  X(02)  VALUE SPACES.
               07  VAT-EDRG-ID-TRS         PIC  X(01)  VALUE SPACES.
               07  VAT-EDRG-NO-GR-ASR      PIC  X(10)  VALUE SPACES.

      * ---------------------------------------------------------
      *    FICHIER SORTIE - SCDFEDCC
      * ---------------------------------------------------------
      *   'EDL CHANGEMENT DE CATEGORIE'
      * ---------------------------------------------------------
           05  VAT-EDCC.
               07  VAT-EDCC-CD-USA     PIC  X(08)  VALUE SPACES.
               07  VAT-EDCC-DT-EFF     PIC  X(06)  VALUE SPACES.
               07  VAT-EDCC-TRS-FOR    PIC  X      VALUE SPACES.
               07  FILLER              PIC  X      VALUE SPACE.
               07  VAT-EDCC-CD-TRS-EDL PIC  X(03)  VALUE SPACES.
               07  VAT-EDCC-NO-CARTE   PIC  X(02)  VALUE SPACES.
               07  VAT-EDCC-NO-CO-ASR  PIC  X(10)  VALUE SPACES.
               07  VAT-EDCC-NO-CERT    PIC  X(09)  VALUE SPACES.
               07  VAT-EDCC-NOUV-CAT   PIC  X(03)  VALUE SPACES.
               07  VAT-EDCC-CD-RES     PIC  X(02)  VALUE SPACES.
               07  VAT-EDCC-ID-TRS     PIC  X      VALUE SPACES.
               07  VAT-EDCC-NO-GR-ASR  PIC  X(10)  VALUE SPACES.

      * ----------------------------------------------
      *    AUTRES VARIABLES
      * ----------------------------------------------
      *    VARIABLES D'APPEL A LA BD IMS
      * ----------------------------------------------
           05  VAT-FONCTION            PIC  X(04).
           05  VAT-NB-PARAM            PIC S9(08)  COMP.
           05  VAT-NAS                 PIC  X(09)  VALUE SPACES.

           05  VAT-TALLY-CHAMP         PIC  9(02)  VALUE ZEROES.
           05  STR-EVAL                PIC  X(120) VALUE SPACES.

           05  VAT-TRAIT-PAIE          PIC  X(06)  VALUE SPACES.
               88  TRAIT-SAGIP                     VALUE 'SAGIP'.
               88  TRAIT-CNEST                     VALUE 'CNEST'.

           05  VAT-NO-MINISTERE        PIC  X(04)  VALUE SPACES.

      * ----------------------------------------------
      *    VARIABLES LECTURE PARAM EN SYSIN
      * ----------------------------------------------
           05  SYSIN-PARAMETER         PIC  X(80)  VALUE SPACES.

           05  VAT-PARM-USA            PIC  X(09)  VALUE SPACES.
           05  VAT-PARM-DT-EFF         PIC  X(06)  VALUE SPACES.

      * ----------------------------------------------
      *    VARIABLES AUXILIAIRES BROUILLON
      * ----------------------------------------------

      *    VARS AUXILIAIRES POUR DATE COMPLEMENTAIRE
           05  WS-DATE-COMPL-NUM.
               07  WS-DATE-COMPL       PIC  9(06)  VALUE ZEROES.

           05  WS-DT-JOUR-COMPL-X.
               07  WS-DT-JOUR-COMPL    PIC  9(06)  VALUE ZEROES.

      *    VARS AUXILIAIRES POUR CONVERTION DE DATE
           05  WS-DATE-8               PIC  9(08).
           05  WS-DATE-8-RED           REDEFINES   WS-DATE-8.
               07  WS-DATE-X8.
                   09  WS-DATE-X8-AA   PIC  9(04).
                   09  WS-DATE-X8-MJ   PIC  X(04).

           05  WS-DATE-X10.
               07  WS-DATE-X10-AA      PIC  X(04)  VALUE SPACES.
               07  FILLER              PIC  X(01)  VALUE '-'.
               07  WS-DATE-X10-MM      PIC  X(02)  VALUE SPACES.
               07  FILLER              PIC  X(01)  VALUE '-'.
               07  WS-DATE-X10-JJ      PIC  X(02)  VALUE SPACES.

           05  WS-AUX-NOM              PIC  X(35)  VALUE SPACES.
           05  WS-AUX-PRENOM           PIC  X(35)  VALUE SPACES.

           05  WS-AUX-TEXT1            PIC  X(30)  VALUE SPACES.
           05  WS-AUX-TEXT2            PIC  X(30)  VALUE SPACES.


      * VARIABLE AUXILIAIRE DE LA RECHERCHE DE SYNDICAT
       01  WS-SYNDICAT.
           05  WS-SYND-SEQ             PIC  9(02)  VALUE ZEROES.
           05  WS-SYND-IN-TRAIT        PIC  X(01)  VALUE SPACES.
           05  WS-SYND-NOM             PIC  X(06)  VALUE SPACES.

      * VARIABLE AUXILIAIRE DE LA RECHERCHE DE MINISTERE
       01  WS-MST-NUM                  PIC  X(04)  VALUE SPACES.

       EJECT
      *
      /                                                                 
       01  FILLER                      PIC  X(22) VALUE                 
               'ZONES DE COMMUNICATION'.                                
                                                                        
      *    CONTROLE D'APPEL                                             
       COPY CIOMOD.                                                     

      *    VARIABLES DU PROGRAMME UTILITAIRE DE CONVERSION DE NOM DE
      *    GARANTIES.
       COPY SCDTCGAR.

      * VARIABLES PROGRAMME OBTENTION DU JOUR
       COPY SCAQZJMA.

       EJECT                                                            
      *                                                                 
      /
       01  FILLER                      PIC  X(10) VALUE
               'LES SSA-->'.

       01  FILLER                      PIC  X(24) VALUE
               '-----SSAASR-----'.
       COPY SSAASR.
       EJECT

       01  FILLER                      PIC  X(24) VALUE
               '-----SSAASRX3-----'.
       COPY SSAASRX3.
       EJECT

       01  FILLER                      PIC  X(24) VALUE
               '-----SSAASRGA-----'.
       COPY SSAASRGA.
       EJECT

       01  FILLER                      PIC  X(24) VALUE
               '-----SSAASRGD-----'.
       COPY SSAASRGD.
       EJECT

       01  FILLER                      PIC  X(24) VALUE
               '-----SSAASRCH-----'.
       COPY SSAASRCH.
       EJECT

       01  FILLER                      PIC  X(24) VALUE
               '-----SSAGRP-----'.
       COPY SSAGRP.
       EJECT

       01  FILLER                      PIC  X(24) VALUE
               '-----SSAGRPCA-----'.
       COPY SSAGRPCA.
       EJECT

      *    SEGMENTS DE BANQUE

       01  FILLER                      PIC  X(24) VALUE
               '-----SPASR-----'.
       COPY SPASR.
       EJECT

       01  FILLER                      PIC  X(24) VALUE
               '-----SPASRGAR-----'.
       COPY SPASRGAR.
       EJECT

       01  FILLER                      PIC  X(24) VALUE
               '-----SPASRGDT-----'.
       COPY SPASRGDT.
       EJECT

       01  FILLER                      PIC  X(24) VALUE
               '-----SPASRCHG-----'.
       COPY SPASRCHG.
       EJECT

       01  FILLER                      PIC  X(24) VALUE
               '-----SPGRP-----'.
       COPY SPGRP.
       EJECT.

       01  FILLER                      PIC  X(24) VALUE
               '-----SPGRPCAT-----'.
       COPY SPGRPCAT.

       EJECT

       01  FILLER                      PIC  X(14) VALUE
               'LES ENTETES-->'.

       01  VAR-ENTETE-FADHE.
           05  VAR-ENT-ADH             PIC  X(300) VALUE SPACES.

       01  VAR-ENTETE-ABST.
           05  VAR-ENT-ABST            PIC  X(250) VALUE SPACES.

       01  VAR-ENTETE-FINVD.
           05  VAR-ENT-INVD            PIC  X(350) VALUE SPACES.

      /                                                                 
       01  FIN-DU-MODULE               PIC  X(31) VALUE                 
               'FIN DU WORKING-STORAGE SCDPYOT1'.                       
       EJECT                                                            
      *                                                                 
       LINKAGE SECTION.                                                 

       COPY PCB01PC.
       COPY PCB02PC.
       COPY PCB03PC.
       COPY PCB04PC.
       COPY PCB05PC.
       COPY PCB06PC.
       COPY PCB07PC.
       COPY PCB08PC.
       COPY PCB09PC.
       COPY PCB10PC.
       COPY PCB11PC.
       COPY PCB12PC.
       COPY PCB13PC.
       COPY PCB14PC.
       COPY PCB15PC.

      *                                                                 
       EJECT
      /
      *                                                                 
      *********************                                             
       PROCEDURE DIVISION.                                              
      *********************                                             
       0000-SECTION-DIRECTRICE         SECTION.                         

           ENTRY 'DLITCBL'             USING PCB01
                                             PCB02
                                             PCB03
                                             PCB04
                                             PCB05
                                             PCB06
                                             PCB07
                                             PCB08
                                             PCB09
                                             PCB10
                                             PCB11
                                             PCB12
                                             PCB13
                                             PCB14
                                             PCB15.

      D    DISPLAY '0000-SECTION-DIRECTRICE'.

           PERFORM 1000-INITIALISATION.                                 

           PERFORM 2000-LECTURE-FICHIERS.
                                                                        
           PERFORM 3000-TRAITEMENT.                                     
                                                                        
           PERFORM 9000-TRAITEMENT-FIN.                                 
                                                                        
           GOBACK.                                                      
                                                                        
       0000-FIN.                                                        
           EXIT.                                                        
      /                                                                 
      ****************************************************************  
      *    SECTION D'INITIALISATION GENERAL DU PROGRAMME.            *  
      ****************************************************************  
       1000-INITIALISATION             SECTION.                         

      D    DISPLAY '1000-INITIALISATION'.

           PERFORM 1100-OPEN-FILES.

           PERFORM 1200-INIT.

           PERFORM 1300-LECTURE-PARM-SYSIN.

           PERFORM 1400-OBTENIR-DT-JOUR.

           PERFORM 1500-INIT-ENTETES.

       1000-FIN.                                                        
           EXIT.                                                        
      /                                                                 
      ****************************************************************  
      *    OUVERTURE DES FICHIERS.                                   *  
      ****************************************************************  
       1100-OPEN-FILES                 SECTION.                         

      D    DISPLAY '1100-OPEN-FILES'.

           OPEN   INPUT   FILE-SYND
                          FILE-MST
                          FILE-CAT
                          FILE-SAGIP
                          FILE-CNEST
                          FILE-NCNES.

           OPEN   OUTPUT  FILE-ABST
                          FILE-ADHE
                          FILE-INVD
                          FILE-EDRG
                          FILE-EDCC.


           IF VAT-FILE-SYND-STATUS NOT = ZEROES
               MOVE 1101               TO CD-RT
               DISPLAY 'SCDYOT1: ERREUR OUVERTURE FICHIER SYNDICAT'
               DISPLAY 'CD-RT       = ' CD-RT
               DISPLAY 'FILE-STATUS = ' VAT-FILE-SYND-STATUS
               GO TO P9999-GOBACK
           END-IF.

           IF VAT-FILE-MST-STATUS NOT = ZEROES
               MOVE 1102               TO CD-RT
               DISPLAY 'SCDYOT1: ERREUR OUVERTURE FICHIER MINISTERES'
               DISPLAY 'CD-RT       = ' CD-RT
               DISPLAY 'FILE-STATUS = ' VAT-FILE-MST-STATUS
               GO TO P9999-GOBACK
           END-IF.

           IF VAT-FILE-ABST-STATUS NOT = ZEROES
               MOVE 1103               TO CD-RT
               DISPLAY 'SCDYOT1: ERREUR OUVERTURE FICHIER ABSENTS'
               DISPLAY 'CD-RT       = ' CD-RT
               DISPLAY 'FILE-STATUS = ' VAT-FILE-ABST-STATUS
               GO TO P9999-GOBACK
           END-IF.

           IF VAT-FILE-ADHE-STATUS NOT = ZEROES
               MOVE 1104               TO CD-RT
               DISPLAY 'SCDYOT1: ERREUR OUVERTURE FICHIER FADHE'
               DISPLAY 'CD-RT       = ' CD-RT
               DISPLAY 'FILE-STATUS = ' VAT-FILE-ADHE-STATUS
               GO TO P9999-GOBACK
           END-IF.

           IF VAT-FILE-CAT-STATUS NOT = ZEROES
               MOVE 1105               TO CD-RT
               DISPLAY 'SCDPYOT1: ERREUR OUVERTURE FICHIER FCAT'
               DISPLAY 'CD-RT       = ' CD-RT
               DISPLAY 'FILE-STATUS = ' VAT-FILE-CAT-STATUS
               GO TO P9999-GOBACK
           END-IF.

           IF VAT-FILE-SAGIP-STATUS NOT = ZEROES
               MOVE 1106               TO CD-RT
               DISPLAY 'SCDPYOT1: ERREUR OUVERTURE FICHIER SAGIP'
               DISPLAY 'CD-RT       = ' CD-RT
               DISPLAY 'FILE-STATUS = ' VAT-FILE-SAGIP-STATUS
               GO TO P9999-GOBACK
           END-IF.

           IF VAT-FILE-CNEST-STATUS NOT = ZEROES
               MOVE 1107               TO CD-RT
               DISPLAY 'SCDPYOT1: ERREUR OUVERTURE FICHIER CNEST'
               DISPLAY 'CD-RT       = ' CD-RT
               DISPLAY 'FILE-STATUS = ' VAT-FILE-CNEST-STATUS
               GO TO P9999-GOBACK
           END-IF.

           IF VAT-FILE-NCNES-STATUS NOT = ZEROES
               MOVE 1108               TO CD-RT
               DISPLAY 'SCDPYOT1: ERREUR OUVERTURE FICHIER NCNES'
               DISPLAY 'CD-RT       = ' CD-RT
               DISPLAY 'FILE-STATUS = ' VAT-FILE-NCNES-STATUS
               GO TO P9999-GOBACK
           END-IF.

           IF VAT-FILE-INVD-STATUS NOT = ZEROES
               MOVE 1109               TO CD-RT
               DISPLAY 'SCDPYOT1: ERREUR OUVERTURE FICHIER INVALIDES'
               DISPLAY 'CD-RT       = ' CD-RT
               DISPLAY 'FILE-STATUS = ' VAT-FILE-INVD-STATUS
               GO TO P9999-GOBACK
           END-IF.

           IF VAT-FILE-EDRG-STATUS NOT = ZEROES
               MOVE 1110               TO CD-RT
               DISPLAY 'SCDPYOT1: ERREUR OUVERTURE FICHIER FEDRG'
               DISPLAY 'CD-RT       = ' CD-RT
               DISPLAY 'FILE-STATUS = ' VAT-FILE-EDRG-STATUS
               GO TO P9999-GOBACK
           END-IF.

           IF VAT-FILE-EDCC-STATUS NOT = ZEROES
               MOVE 1111               TO CD-RT
               DISPLAY 'SCDPYOT1: ERREUR OUVERTURE FICHIER FEDCC'
               DISPLAY 'CD-RT       = ' CD-RT
               DISPLAY 'FILE-STATUS = ' VAT-FILE-EDCC-STATUS
               GO TO P9999-GOBACK
           END-IF.

       1100-FIN.
           EXIT.
      /
      ****************************************************************  
      *    INITIALISATION DES VARIABLES.                             *  
      ****************************************************************  
       1200-INIT                       SECTION.                         

      D    DISPLAY '1200-INIT'.

           INITIALIZE                     RECORD-MST
                                          VAT-TABL-MST
                                          RECORD-CAT
                                          RECORD-SAGIP
                                          VAT-REC-SAGIP
                                          RECORD-CNEST
                                          VAT-REC-CNEST
                                          RECORD-ADHE
                                          RECORD-INVD
                                          RECORD-ABST.


           MOVE 'N'                    TO STL-FIN-MST
                                          STL-FIN-CAT
                                          STL-FIN-SAGIP
                                          STL-FIN-CNEST
                                          STL-FIN-ADH-NAS.

           MOVE 'O'                    TO STL-CNT-TRAIT.

       1200-FIN.
           EXIT.
      /
      ****************************************************************  
      *    LECTURE INITIALE DES FICHIERS                             *  
      ****************************************************************  
       1300-LECTURE-PARM-SYSIN         SECTION.                         

      D    DISPLAY '1300-LECTURE-PARM-SYSIN'.

           ACCEPT SYSIN-PARAMETER      FROM SYSIN.

           UNSTRING SYSIN-PARAMETER    DELIMITED BY ';'
           INTO                        WS-AUX-TEXT1
                                       WS-AUX-TEXT2
           TALLYING IN VAT-TALLY-CHAMP.

           MOVE WS-AUX-TEXT1(8:9)      TO VAT-PARM-USA.
           MOVE WS-AUX-TEXT2(10:6)     TO VAT-PARM-DT-EFF.

           INITIALIZE                  WS-AUX-TEXT1
                                       WS-AUX-TEXT2.

       1300-FIN.
           EXIT.
      /
      ****************************************************************  
      *    INITIALIZE LES ENTETES DES FICHIERS CSV EN SORTIE         *  
      ****************************************************************  
       1400-OBTENIR-DT-JOUR            SECTION.                         

      D    DISPLAY '1400-OBTENIR-DT-JOUR'.

      * ----------------------------------------------
      *    OBTENIR LA DATE DU JOUR
      * ----------------------------------------------

           PERFORM P999-A2K-APPEL-MXA2YJMA.

           MOVE WS-JMA-DATE-RET     TO WS-DT-JOUR-COMPL

      * ----------------------------------------------
      *    FAIRE LE CALCUL DE LA DATE COMPLEMENTAIRE
      * ----------------------------------------------

           COMPUTE
             WS-DATE-COMPL          = 1000000 - WS-DT-JOUR-COMPL.

       1400-FIN.
           EXIT.
      /
      ****************************************************************  
      *    INITIALIZE LES ENTETES DES FICHIERS CSV EN SORTIE         *  
      ****************************************************************  
       1500-INIT-ENTETES               SECTION.                         

      D    DISPLAY '1500-INIT-ENTETES'.

      * ----------------------------------------------
      * --- INITIALISER L'ENTETE DU FICHIER SCDFADHE
      * ----------------------------------------------

           STRING
           'NAS;NOM;PRENOM;DT NAISSANCE;SYNDICAT;STATUT;UNITE SYNDICA'
           'LE;CODE EXONERATION;DESC EXONERATION;CONTRAT;COMPTE;STATU'
           'S ADHERENT;GARANTIE EXEMPT 1;GARANTIE EXEMPT 2;GARANTIE E'
           'XEMPT 3;GARANTIE EXEMPT 4;GARANTIE EXEMPT 5;CODE TERMINAI'
           'SON;NO CERTIFICAT;ANCIEN NO CATEGORIE;NOUVEAU NO CATEGORIE;'
               DELIMITED BY SIZE
                   INTO VAR-ENT-ADH.

           MOVE VAR-ENT-ADH            TO RECORD-ADHE.
           WRITE                          RECORD-ADHE.
           INITIALIZE                     RECORD-ADHE.

      * ----------------------------------------------
      * --- INITIALISER L'ENTETE DU FICHIER SCDFABST
      * ----------------------------------------------

           STRING
           'NAS;NOM;PRENOM;DATE NAISSANCE;SYNDICAT;TRAITEUR PAIE;STAT'
           'UT;UNIT SYNDICALE;EXO PLAN;DESC EXO;'
               DELIMITED BY SIZE
                   INTO VAR-ENT-ABST.

           MOVE VAR-ENT-ABST           TO RECORD-ABST.
           WRITE                          RECORD-ABST.
           INITIALIZE                     RECORD-ABST.

      * ----------------------------------------------
      * --- INITIALISER L'ENTETE DU FICHIER SCDFINVD
      * ----------------------------------------------

           STRING
           'NAS; NOM;PRENOM; DATE NAISSANCE;SYNDICAT;STATUT;UNIT SYND'
           'ICALE;EXO PLAN;DESC EXO;CONTRAT;COMPTE;CATEGORIE;NO CERTI'
           'FICAT;STATUT;CODE EN VIGUEUR;DATE EN VIGUEUR;QUEL NOM CEL'
           'ULLE?;'
               DELIMITED BY SIZE
                   INTO VAR-ENT-INVD.

           MOVE VAR-ENT-INVD           TO RECORD-INVD.
           WRITE                          RECORD-INVD.
           INITIALIZE                     RECORD-INVD.

       1500-FIN.
           EXIT.
      /
      ****************************************************************  
      *    LECTURE INITIALE DES FICHIERS                             *  
      ****************************************************************  
       2000-LECTURE-FICHIERS           SECTION.                         

      D    DISPLAY '2000-LECTURE-FICHIERS'.

           PERFORM 2100-LECTURE-SAGIP.

           PERFORM 2200-LECTURE-CNEST.

           PERFORM 2300-LECTURE-MST.

           PERFORM 2400-LECTURE-CAT.

           PERFORM 2500-LECTURE-SYND.

       2000-FIN.
           EXIT.
      /
      ****************************************************************  
      *    LECTURE DES RECORDS DU FICHIER SAGIP                      *  
      ****************************************************************  
       2100-LECTURE-SAGIP              SECTION.                         

      D    DISPLAY '2100-LECTURE-SAGIP.'

           READ FILE-SAGIP             AT END
               DISPLAY '*********************************************'
               DISPLAY '*** FICHIER SAGIP VIDE                      *'
               DISPLAY '*********************************************'
               MOVE HIGH-VALUES        TO RECORD-SAGIP
               MOVE 2100               TO CD-RT
               MOVE 'O'                TO STL-FIN-SAGIP.
      *        GO TO P9999-GOBACK.

       2100-FIN.
           EXIT.
      /
      ****************************************************************  
      *    LECTURE DES RECORDS DU FICHIER CNEST                      *  
      ****************************************************************  
       2200-LECTURE-CNEST              SECTION.                         

      D    DISPLAY '2200-LECTURE-CNEST.'

           READ FILE-CNEST             AT END
               DISPLAY '*********************************************'
               DISPLAY '*** FICHIER CNEST VIDE                      *'
               DISPLAY '*********************************************'
               MOVE HIGH-VALUES        TO RECORD-CNEST
               MOVE 2200               TO CD-RT
               MOVE 'O'                TO STL-FIN-CNEST.
      *        GO TO P9999-GOBACK.

       2200-FIN.
           EXIT.
      /
      ****************************************************************  
      *    LECTURE DES RECORDS DU FICHIER SCAUNCST                   *  
      ****************************************************************  
       2250-LECTURE-SCAUNCST           SECTION.                         

      D    DISPLAY '2200-LECTURE-CNEST.'

               MOVE 'N'                TO STL-FIN-NCNES.

               START FILE-NCNES
                     KEY IS NOT LESS THAN NCNES-NAS
                     INVALID KEY
                         MOVE 'O'      TO STL-FIN-NCNES.

               IF STL-FIN-NCNES        = 'N'
                   READ FILE-NCNES
               END-IF.

       2250-FIN.
           EXIT.
      /
      ****************************************************************  
      *    LECTURE DES RECORDS DE LA TABLE DE MINISTERES             *  
      ****************************************************************  
       2300-LECTURE-MST                SECTION.                         

      D    DISPLAY '2300-LECTURE-MST'.

           READ FILE-MST  AT END

           DISPLAY ' ************************************************'
           DISPLAY ' *** FICHIER MST VIDE                           *'
           DISPLAY ' ************************************************'
           MOVE HIGH-VALUES            TO RECORD-MST
           MOVE 2300                   TO CD-RT
           MOVE 'O'                    TO STL-FIN-MST
           GO TO   P9999-GOBACK.

           PERFORM 2310-BOUCLE-MST
               UNTIL STL-FIN-MST = 'O'.

      *    PERFORM 9202-DISPLAY-MST.

       2300-FIN.
           EXIT.
      /
      /
      ****************************************************************  
      *    LECTURE DES RECORDS DE LA TABLE DE MINISTERES             *  
      ****************************************************************  
       2310-BOUCLE-MST                 SECTION.                         

           ADD +1                      TO VAT-TB-MST-CNT.

           IF VAT-TB-MST-CNT > VAT-TB-MST-MAX
               DISPLAY '***********************************************'
               DISPLAY '*** A B E N D *********************************'
               DISPLAY '*** PLUS DE 200 MINISTERES DANS FICHIER SCAFMST'
               DISPLAY '*** JOB : SCDCYOT4  PRG: SCDPYOT1 *************'
               DISPLAY '***********************************************'
               MOVE 2310               TO CD-RT
               GO TO P9999-GOBACK
           END-IF.

           MOVE RECORD-MST             TO VAT-MINISTERE(VAT-TB-MST-CNT).

           READ FILE-MST               AT END
               MOVE 'O'                TO STL-FIN-MST.

       2310-FIN.
           EXIT.
      /
      ****************************************************************  
      *    LECTURE DES RECORDS DE LA TABLE DE CATEGORIES             *  
      ****************************************************************  
       2400-LECTURE-CAT                SECTION.                         

      D    DISPLAY '2400-LECTURE-CAT'.

           READ FILE-CAT  AT END

           DISPLAY ' ************************************************'
           DISPLAY ' *** FICHIER CAT VIDE                           *'
           DISPLAY ' ************************************************'
           MOVE HIGH-VALUES            TO RECORD-CAT
           MOVE 2400                   TO CD-RT
           MOVE 'O'                    TO STL-FIN-CAT
           GO TO   P9999-GOBACK.

           MOVE +0                     TO IND.

           PERFORM 2410-BOUCLE-CAT
               UNTIL STL-FIN-CAT = 'O'.

      *    PERFORM 9203-DISPLAY-CAT.

       2400-FIN.
           EXIT.
      *
      ****************************************************************  
      *    LECTURE DES RECORDS DE LA TABLE DE CATEGORIES             *  
      ****************************************************************  
       2410-BOUCLE-CAT                 SECTION.                         

           ADD +1                      TO IND
                                          VAT-TB-CAT-CNT.

           IF VAT-TB-CAT-CNT > VAT-TB-CAT-MAX
               DISPLAY '***********************************************'
               DISPLAY '*** A B E N D *********************************'
               DISPLAY '*** PLUS DE 50 CATEGORIES  DANS FICHIER SCAFCAT'
               DISPLAY '*** JOB : SCDCYOT4  PRG: SCDPYOT1 *************'
               DISPLAY '***********************************************'
               MOVE 2410               TO CD-RT
               GO TO P9999-GOBACK
           END-IF.

           MOVE RECORD-CAT            TO VAT-CATEGORIES(VAT-TB-CAT-CNT).
           READ FILE-CAT               AT END
               MOVE 'O'                TO STL-FIN-CAT.

       2410-FIN.
           EXIT.
      *
      ****************************************************************  
      *    LECTURE DES RECORDS DE LA TABLE DE SYNDICATS              *  
      ****************************************************************  
       2500-LECTURE-SYND               SECTION.                         

      D    DISPLAY '2500-LECTURE-SYND'.

           READ FILE-SYND              AT END

           DISPLAY ' ************************************************'
           DISPLAY ' *** FICHIER SYND VIDE                          *'
           DISPLAY ' ************************************************'
           MOVE HIGH-VALUES            TO RECORD-SYND
           MOVE 2500                   TO CD-RT
           MOVE 'O'                    TO STL-FIN-SYND
           GO TO   P9999-GOBACK.

           MOVE +0                     TO IND.

           PERFORM 2510-BOUCLE-SYND
               UNTIL STL-FIN-SYND = 'O'.

      *    PERFORM 9201-DISPLAY-SYND.

       2500-FIN.
           EXIT.
      /
      ****************************************************************  
      *    LECTURE DES RECORDS DE LA TABLE DE SYNDICATS              *  
      ****************************************************************  
       2510-BOUCLE-SYND                SECTION.                         

           ADD +1                      TO IND
                                          VAT-TB-SYND-CNT.

           IF VAT-TB-SYND-CNT > VAT-TB-SYND-MAX
               DISPLAY '***********************************************'
               DISPLAY '*** A B E N D *********************************'
               DISPLAY '*** PLUS DE 09 SYNDICATS  DANS FICHIER SCAFSYND'
               DISPLAY '*** JOB : SCDCYOT4  PRG: SCDPYOT1 *************'
               DISPLAY '***********************************************'
               MOVE 2510               TO CD-RT
               GO TO P9999-GOBACK
           END-IF.

           MOVE RECORD-SYND            TO VAT-SYND(IND).

           READ FILE-SYND              AT END
               MOVE 'O'                TO STL-FIN-SYND.

       2510-FIN.
           EXIT.
      /
      ****************************************************************  
      *    TRAITEMENT PRINCIPAL.                                     *  
      ****************************************************************  
       3000-TRAITEMENT                 SECTION.                         

      D    DISPLAY '3000-TRAITEMENT'.

           MOVE 'O'                   TO STL-CNT-TRAIT.

           PERFORM 3100-TRAITER-RECORDS-SAGIP.

           PERFORM 3200-TRAITER-RECORDS-CNEST.

                                                                        
       3000-FIN.                                                        
           EXIT.                                                        
      /                                                                 
      ****************************************************************  
      *    TRAITER CHACUN DES ENREGISTREMENTS DANS LE FICHIER SAGIP  *  
      ****************************************************************  
       3100-TRAITER-RECORDS-SAGIP      SECTION.                         

      D    DISPLAY '**************************************************'.
      D    DISPLAY '3100-TRAITER-RECORDS-SAGIP'.
      D    DISPLAY '**************************************************'.

           MOVE 'SAGIP'                TO  VAT-TRAIT-PAIE.

           IF STL-FIN-SAGIP = 'N'
               ADD +1                  TO CPT-REC-LU
               MOVE RECORD-SAGIP       TO VAT-REC-SAGIP

               PERFORM 3110-BOUCLER-RECORDS-SAGIP
                   UNTIL STL-FIN-SAGIP = 'O'

           END-IF.

       3100-FIN.
           EXIT.
      /
      ****************************************************************  
      *    TRAITER CHACUN DES ENREGISTREMENTS DANS LE FICHIER SAGIP  *  
      ****************************************************************  
       3110-BOUCLER-RECORDS-SAGIP      SECTION.                         

           MOVE 'O'                    TO STL-CNT-TRAIT.
           MOVE 'O'                    TO STL-ADHE-VALIDE.

      * --------------------------------------------------------------
      *    FAIRE LA DECOMPOSITION DU FICHIER CSV EN ENTREE DANS CHAMPS
      * --------------------------------------------------------------
           PERFORM 3900-DECOMPOSER-RECORD-SAGIP.

      * ----------------------------------------------
      *     VERIFIE LE TRAITEMENT DE SYNDICAT
      * ----------------------------------------------
           IF VAT-SAGIP-UNIT-SYND(1:4)  = 'SFPQ'
               MOVE 'SFPQ'              TO WS-SYND-NOM
           ELSE
               MOVE VAT-SAGIP-UNIT-SYND TO WS-SYND-NOM
           END-IF.

           PERFORM 9301-CHERCHE-TABL-SYNDICAT.

           IF WS-SYND-IN-TRAIT         = 'N'
               MOVE 'N'                TO STL-CNT-TRAIT
               ADD +1                  TO CPT-REC-BYPASS
           END-IF.

      * ----------------------------------------------
      *  VERIFIER SI LE MINISTERE EST CIBLÉ.
      * ----------------------------------------------
           MOVE VAT-SAGIP-ORGAN(1:4)   TO VAT-NO-MINISTERE.
      *
           IF STL-CNT-TRAIT            = 'O'
               PERFORM 9302-CHERCHE-MINISTERE

               IF STL-MST-TROUVE       = 'N'
                   MOVE 'N'            TO STL-CNT-TRAIT
                   ADD +1              TO CPT-REC-BYPASS
               END-IF
           END-IF.

      * --------------------------------------------------------------
      *    RECHERCHER DANS LA BASE DE DONNEES D'OSCAR SI L'ADHERANT Y
      *    EST PRESENT.
      * --------------------------------------------------------------
           IF STL-CNT-TRAIT            = 'O'

               MOVE 'N'                TO STL-FIN-ADH-NAS

               MOVE VAT-SAGIP-NAS      TO VAT-NAS

               PERFORM 4100-OBTENIR-ADH-PAR-NAS

               IF PCB07-CD-RT          = '  '
                   PERFORM 3500-TRAITER-ADHERENT
                       UNTIL STL-FIN-ADH-NAS = 'O'
               ELSE
                   MOVE 'N'            TO STL-CNT-TRAIT
                   PERFORM 7100-ECRIRE-FABST
               END-IF
           END-IF.

      * ----------------------------------------------
      *    VALIDER SI L'ADHERENT EST PRESENT DANS LA
      *    LISTE CNEST. SI ON LE TROUVE, BYPASSER LE
      *    RECORD DE SAGIP.
      *    === LECTURE VSAM ====
      * ----------------------------------------------
           IF STL-CNT-TRAIT            = 'O'

               MOVE VAT-NAS            TO NCNES-NAS
               PERFORM 2250-LECTURE-SCAUNCST

               IF VAT-FILE-NCNES-STATUS = 00
                   MOVE 'N'            TO STL-CNT-TRAIT
                   ADD +1              TO CPT-REC-BYPASS-NAS
      *            DISPLAY 'NAS PRESENT DANS FICHIER CNESST. BYPASS'    
      *            DISPLAY 'NAS : ' VAT-NAS
               ELSE
                   IF VAT-FILE-NCNES-STATUS = 23
                       CONTINUE
                   ELSE
                       DISPLAY '*************************************'
                       DISPLAY 'ERREUR DE LECTURE DU FICHIER SCAUCNST'
                       DISPLAY 'VAT-FILE-NCNES-STATUS : '
                                VAT-FILE-NCNES-STATUS
                       DISPLAY '*************************************'
                       GO TO P9999-GOBACK
                   END-IF
               END-IF
           END-IF.

      * ----------------------------------------------
      *    LIRE LE PROCHAIN RECORD FICHIER ENTRE SAGIP
      * ----------------------------------------------
           PERFORM 3115-LIRE-PROCHAIN-REC-SAGIP.

       3110-FIN.
           EXIT.
      /
      ****************************************************************  
      *    LIRE PROCHAIN ENREGISTREMENT DANS LE FICHIER SAGIP.       *  
      ****************************************************************  
       3115-LIRE-PROCHAIN-REC-SAGIP    SECTION.                         

           READ FILE-SAGIP             AT END
               MOVE 'O'                TO STL-FIN-SAGIP.

           IF STL-FIN-SAGIP = 'O'
               NEXT SENTENCE
           ELSE
               ADD +1                  TO CPT-REC-LU
               MOVE RECORD-SAGIP       TO VAT-REC-SAGIP
           END-IF.

       3115-FIN.
           EXIT.
      /
      ****************************************************************  
      * TRAITE LES RECORDS RECUS PAR LE FICHIER D'ENTREE CNEST       *  
      ****************************************************************  
       3200-TRAITER-RECORDS-CNEST      SECTION.                         

      D    DISPLAY '**************************************************'.
      D    DISPLAY '3200-TRAITER-RECORDS-CNEST'.
      D    DISPLAY '**************************************************'.

           MOVE 'CNEST'                TO VAT-TRAIT-PAIE.

           IF STL-FIN-CNEST            = 'N'
               ADD +1                  TO CPT-REC-LU
               MOVE RECORD-CNEST       TO VAT-REC-CNEST

               PERFORM 3210-BOUCLER-RECORDS-CNEST
                   UNTIL STL-FIN-CNEST = 'O'
           END-IF.

       3200-FIN.
           EXIT.
      /
      ****************************************************************  
      *   TRAITER CHACUN DES ENREGISTREMENTS DANS LE FICHIER CNESST  *  
      ****************************************************************  
       3210-BOUCLER-RECORDS-CNEST      SECTION.                         

           MOVE 'O'                    TO STL-CNT-TRAIT.

      *---------------------------------------------------------------
      * FAIRE LA DECOMPOSITION DU FICHIER CSV EN CHAMPS
      *---------------------------------------------------------------
           PERFORM 3901-DECOMPOSER-RECORD-CNEST.

      * ----------------------------------------------
      *    ATTRIBUER SFPQ COMME SYNDICAT DE CNESST
      * ----------------------------------------------
           MOVE 'SFPQ'                 TO WS-SYND-NOM.

           PERFORM 9301-CHERCHE-TABL-SYNDICAT.

           IF WS-SYND-IN-TRAIT         = 'N'
               MOVE 'N'                TO STL-CNT-TRAIT
               ADD +1                  TO CPT-REC-BYPASS
           END-IF.

      * ----------------------------------------------
      *    OBTENIR LES INFOS DE L'ADHERENT
      * ----------------------------------------------
           IF STL-CNT-TRAIT            = 'O'
               MOVE 'N'                TO STL-FIN-ADH-NAS

               MOVE VAT-CNEST-NAS      TO VAT-NAS

               PERFORM 4100-OBTENIR-ADH-PAR-NAS

               IF PCB07-CD-RT          = '  '
                   PERFORM 3500-TRAITER-ADHERENT
                       UNTIL STL-FIN-ADH-NAS = 'O'
               ELSE
                   MOVE 'N'            TO STL-CNT-TRAIT
                   PERFORM 7100-ECRIRE-FABST
               END-IF
           END-IF.

      * -----------------------------------------------
      *    LIRE PROCHAIN RECORD DU FICHIER ENTREE CNEST
      * -----------------------------------------------
           PERFORM 3215-LIRE-PROCHAIN-CNEST.

       3210-FIN.
           EXIT.
      /
      ****************************************************************  
      * LIRE PROCHAIN ENREGISTREMENT DANS LE FICHIER CNEST           *  
      ****************************************************************  
       3215-LIRE-PROCHAIN-CNEST        SECTION.                         

           READ FILE-CNEST             AT END
               MOVE 'O'                TO STL-FIN-CNEST.

           IF STL-FIN-CNEST            = 'O'
               NEXT SENTENCE
           ELSE
               ADD +1                  TO CPT-REC-LU
               MOVE RECORD-CNEST       TO VAT-REC-CNEST
           END-IF.

       3215-FIN.
           EXIT.
      /
      ****************************************************************  
      *    TRAITE L'ADHERENT PROVENANT DE SAGIP OU CNESST            *  
      ****************************************************************  
       3500-TRAITER-ADHERENT           SECTION.                         

      D    DISPLAY '3500-TRAITER-ADHERENT'.

      * ----------------------------------------------
      * OBTENIR HISTORIQUE ADHERENT
      * ----------------------------------------------
           PERFORM 4500-OBTENIR-SPASRCHG.

      * ----------------------------------------------
      * *** VALIDER L'ADHERENT
      * ----------------------------------------------
           PERFORM 3800-VALIDER-ADHERENT.

      * ----------------------------------------------
      * *** OBTENIR ET TRAITER LES GARANTIES
      * ----------------------------------------------
           IF STL-CNT-TRAIT = 'O'
               MOVE 'N'                TO STL-FIN-GAR
               MOVE 'N'                TO STL-ADHE-EXEMP
               MOVE +0                 TO CPT-GAR-QTE-LU
               MOVE +0                 TO CPT-GAR-EXEMP
               INITIALIZE                 VAT-EXEMPTIONS
               PERFORM 3600-TRAITER-GARANTIES
                   UNTIL STL-FIN-GAR = 'O'
           END-IF.

      * ----------------------------------------------
      * *** OBTENIR LES CATEGORIES
      * ----------------------------------------------
           IF STL-CNT-TRAIT            = 'O'

               PERFORM 4400-OBTENIR-SPGRPCAT

               IF PCB01-CD-RT          = SPACES
                   NEXT SENTENCE
               ELSE
                   MOVE 'N'            TO STL-CNT-TRAIT
               END-IF.

      * ----------------------------------------------
      * *** SI LE TRAITEMENT A EU SUCCES JUSQU'ICI,
      * *** ECRIRE LE RECORD DE SORTIE.
      * ----------------------------------------------
           IF  STL-CNT-TRAIT           = 'O'
           AND CPT-GAR-EXEMP           >  0
               PERFORM 7300-ECRIRE-FADHE
               PERFORM 7400-ECRIRE-EDL-RES-GAR
               PERFORM 7500-ECRIRE-EDL-CHG-CAT
           END-IF.

      * ----------------------------------------------
      * *** OBTENIR LE PROCHAIN ADHERENT DE LA BD
      * *** (OBTAIN NEXT) DU MEME NAS.
      * ----------------------------------------------
           IF STL-CNT-TRAIT            = 'O'

               PERFORM 4150-OBTENIR-NEXT-ADH-PAR-NAS

               IF PCB07-CD-RT          = 'GE' OR 'GB'
                   MOVE 'O'            TO STL-FIN-ADH-NAS
               ELSE
                   ADD +1              TO CPT-REC-NEXT
      D            DISPLAY 'NEXT-REC. NAS: ' VAT-NAS
               END-IF
           END-IF.

       3500-FIN.
           EXIT.
      /
      ****************************************************************  
      *    TRAITE TOUS LES GARANTIES DE CHAQUE ADHERENT.                
      ****************************************************************  
       3600-TRAITER-GARANTIES          SECTION.                         

      D    DISPLAY '3600-TRAITER-GARANTIES'.

           IF CPT-GAR-QTE-LU           = 0
               PERFORM 4200-OBTENIR-SPASRGAR
           ELSE
               PERFORM 4250-OBTENIR-NEXT-SPASRGAR
           END-IF.

           IF PCB03-CD-RT              = '  '
               ADD +1                  TO CPT-GAR-QTE-LU
           ELSE
               IF PCB03-CD-RT          = 'GB' OR 'GE'
                   MOVE 'O'            TO STL-FIN-GAR
                   ADD +1              TO CPT-EXO-A
                   GO TO 3600-FIN
               END-IF
           END-IF.

      * ----------------------------------------------
      * *** VERIFICATION D'EXEMPTION:
      * *** SI LA GARANTIE MN* OU MD* EST EN VIGUEUR
      * ***    L'ADHERENT N'EST PAS EXEMPTE
      * *** SINON SI L'ADHERENT A UNE OU PLUSIEURS MN*
      * *** OU MD* EXEMPTES
      * ***    L'ADHERENT EST EXEMPTE
      * ----------------------------------------------
           IF (EL-CD-GAR-ASR(1:2)       = 'MN' OR 'MD')
                AND EL-CD-ST-DESC-ASR-1 = '1'
               MOVE 'O'            TO STL-FIN-GAR
               ADD +1 TO CPT-EXO-B
               GO TO 3600-FIN
           END-IF.

           IF (EL-CD-GAR-ASR(1:2)       = 'MN' OR 'MD')
               AND EL-CD-ST-DESC-ASR-1  = '3'

      *    -------------------------------------------
      *    CHERCHER LE PREMIER RECORD DU HISTORIQUE DE
      *    GARANTIE. SI LE CODE DE TERMINAISON EGAL A
      *    11 OU 12 OU 13 OU 19 OU 20; L'ADHERENT EST
      *    EXEMPTE.
      *    ADDITIONER +1 AU COMPTEUR, QUI SERA UTILISÉ
      *    POUR INSCRIRE LE RECORD DANS LA LISTE DE
      *    SORTIE.
      *    -------------------------------------------
               INITIALIZE SEG-RPASRGDT

               PERFORM 4300-OBTENIR-SPASRGDT

               IF EL-CD-TERM-GAR-ASR   = '11' OR '12' OR
                                         '13' OR '19' OR '20'

                   ADD +1              TO CPT-GAR-EXEMP
                   IF CPT-GAR-EXEMP    < 5
                       MOVE EL-CD-GAR-ASR
                                       TO VAT-ADH-CD-GAR-EXEMP
                                         (CPT-GAR-EXEMP)
                   END-IF
               END-IF
           END-IF.

       3600-FIN.
           EXIT.
      /
      ****************************************************************  
      *                                                              *  
      ****************************************************************  
       3800-VALIDER-ADHERENT           SECTION.                         

      D    DISPLAY '3800-VALIDER-ADHERENT'.

      *------------------------------------------------------------
      *    VALIDER LE CHAMP EXONERATION                           *
      *------------------------------------------------------------

           IF VAT-TRAIT-PAIE           = 'SAGIP'
               IF VAT-SAGIP-EXO-PLAN       = '00'
                   MOVE 'O'                TO STL-ADHE-VALIDE
               ELSE
                   MOVE 'N'                TO STL-ADHE-VALIDE
               END-IF
           ELSE
               IF VAT-TRAIT-PAIE       = 'CNEST'
                   IF VAT-CNEST-EXO    = 'OUI'
                       MOVE 'N'        TO STL-ADHE-VALIDE
                   ELSE
                       MOVE 'O'        TO STL-ADHE-VALIDE
                   END-IF
               END-IF
           END-IF.

      * ----------------------------------------------
      *    VALIDER LE STATUT, LE CODE ENVIGUEUR, ET
      *    LES DATES D'INVALIDITE ET FIN
      * ----------------------------------------------
      *$??? BONNE INTERPRETATION DE LA REGLE 3 DU D.O. ???
           IF (((EL-CD-ST-DESC-ASR-1    = 1)
                 AND (EL-CD-ENVG-ASR    = '11' OR '31' OR '32'))
      *
           OR ( EL-CD-ST-DESC-ASR-1    = 4)
      *
           OR ((EL-CD-ST-DESC-ASR-1    = 3)
               AND (EL-CD-TERM-GAR-ASR = '05' OR '17' OR '20' OR
                                         '32' OR '52' OR '61')) )
      *
           AND (EL-DT-DEB-INV-ASR  NOT = ZEROES
                AND  EL-DT-FIN-INV-ASR = ZEROES  )

               MOVE 'N'                TO STL-ADHE-VALIDE

           END-IF.

      * -----------------------------------------------
      *     SI L'ADHERENT EST INVALIDE, ECRIRE LE
      *     RECORD DU FICHIER 'ADHERENTS EXCLUS DE LA
      *     LISTE DES MEMBRES AVEC SYNDICAT' - SCDFINVD
      * -----------------------------------------------
           IF STL-ADHE-VALIDE    = 'N'
               MOVE 'O'                TO STL-FIN-ADH-NAS
               MOVE 'N'                TO STL-CNT-TRAIT
               PERFORM 7200-ECRIRE-FINVD
           END-IF.

       3800-FIN.
           EXIT.
      /
      ****************************************************************  
      *    DECOMPOSE LES CHAMPS SEPARES PAR ';' DANS UNE ZONE DE VARS   
      ****************************************************************  
       3900-DECOMPOSER-RECORD-SAGIP    SECTION.                         

           MOVE ZEROES                 TO VAT-TALLY-CHAMP.
           MOVE SPACES                 TO VAT-REC-SAGIP.

      * ----------------------------------------------
      *   UNSTRING RECORD VARIABLE, AVEC 11 COLONNES
      *   DIVISEES PAR ';' A PARTIR D'UN FICHIER CSV
      *   EN ENTREE.
      * ----------------------------------------------
           UNSTRING RECORD-SAGIP       DELIMITED BY ';'
           INTO                        VAT-SAGIP-ORGAN
                                       VAT-SAGIP-NOM
                                       VAT-SAGIP-NAS
                                       VAT-SAGIP-DT-NAIS
                                       VAT-SAGIP-STATUT
                                       VAT-SAGIP-STATUT-AF
                                       VAT-SAGIP-SYST-PAIE
                                       VAT-SAGIP-UNIT-SYND
                                       VAT-SAGIP-ADH-SYND
                                       VAT-SAGIP-EXO-PLAN
                                       VAT-SAGIP-DESC-EXO
           TALLYING IN VAT-TALLY-CHAMP.

           MOVE ZEROES                 TO VAT-TALLY-CHAMP.
           INITIALIZE                     WS-AUX-NOM
                                          WS-AUX-PRENOM
                                          VAT-SAGIP-PRENOM.

           UNSTRING  VAT-SAGIP-NOM     DELIMITED BY ','
           INTO                        WS-AUX-NOM
                                       WS-AUX-PRENOM
           TALLYING IN VAT-TALLY-CHAMP.

      *    MOVE DE LA POSITION 2, POUR ENLEVER LE SPACE EN
      *    POSITION 01
           MOVE WS-AUX-NOM             TO VAT-SAGIP-NOM.
           MOVE WS-AUX-PRENOM(2:34)    TO VAT-SAGIP-PRENOM.

       3900-FIN.
           EXIT.
      /
      ****************************************************************  
      *    DECOMPOSE LES CHAMPS SEPARES PAR ';' DANS UNE ZONE DE VARS   
      ****************************************************************  
      * NOM;PRENOM;NAS;DATE-NAIS;STATUT;SYNDICAT;EXO;RAISON;
      * VAT-CNEST-NOM;VAT-CNEST-PRENOM;VAT-CNEST-DT-NAIS;
      * VAT-CNEST-STATUT
      ****************************************************************  
       3901-DECOMPOSER-RECORD-CNEST    SECTION.                         

           MOVE ZEROES                 TO VAT-TALLY-CHAMP.
           MOVE SPACES                 TO VAT-REC-CNEST.

           UNSTRING RECORD-CNEST       DELIMITED BY ';'
           INTO                        VAT-CNEST-NOM
                                       VAT-CNEST-PRENOM
                                       VAT-CNEST-NAS
                                       VAT-CNEST-DT-NAIS
                                       VAT-CNEST-STATUT
                                       VAT-CNEST-SYND
                                       VAT-CNEST-EXO
                                       VAT-CNEST-RAISON
           TALLYING IN VAT-TALLY-CHAMP.

       3901-FIN.
           EXIT.
      /
      ****************************************************************  
      *    OBTENIR ADHERENT PAR SON NUMERO DE NAS                    *  
      ****************************************************************  
       4100-OBTENIR-ADH-PAR-NAS        SECTION.                         

      D    DISPLAY '4100-OBTENIR-ADH-PAR-NAS'.

           MOVE 'GU  '                 TO VAT-FONCTION.
           MOVE '('                    TO SSA-PARENTH-1-SPASR-X3.
           MOVE '= '                   TO SSA-OP-RECH-SPASR-X3.
           MOVE VAT-NAS                TO SSA-VAL-CLE-NAS-SPASR-X3.
           MOVE ')'                    TO SSA-PARENTH-2-SPASR-X3.

           MOVE 4                      TO VAT-NB-PARAM.

           PERFORM 8100-LIRE-SPASR.

       4100-FIN.
           EXIT.
      /
      ****************************************************************  
      *    OBTENIR NEXT ADHERENT PAR SON NUMERO DE NAS               *  
      ****************************************************************  
       4150-OBTENIR-NEXT-ADH-PAR-NAS   SECTION.                         

      D    DISPLAY '4150-OBTENIR-NEXT-ADH-PAR-NAS'.

           MOVE 'GN  '                 TO VAT-FONCTION.
           MOVE '('                    TO SSA-PARENTH-1-SPASR-X3.
           MOVE '= '                   TO SSA-OP-RECH-SPASR-X3.
           MOVE VAT-NAS                TO SSA-VAL-CLE-NAS-SPASR-X3.
           MOVE ')'                    TO SSA-PARENTH-2-SPASR-X3.

           MOVE 4                      TO VAT-NB-PARAM.

           PERFORM 8100-LIRE-SPASR.

       4150-FIN.
           EXIT.
      /
      ****************************************************************  
      *    OBTENIR GARANTIE D'ADHERENT                                  
      ****************************************************************  
       4200-OBTENIR-SPASRGAR           SECTION.                         

      D    DISPLAY '4200-OBTENIR-SPASRGAR'.

           MOVE 'GU  '                 TO VAT-FONCTION.
           MOVE '('                    TO SSA-PARENTH-1-SPASR.
           MOVE '= '                   TO SSA-OP-RECH-SPASR.

           MOVE EL-NO-GR-ASR           TO SSA-VAL-CLE-GR-SPASR.
           MOVE EL-NO-CO-ASR           TO SSA-VAL-CLE-CO-SPASR.
           MOVE EL-NO-CERT-ASR         TO SSA-VAL-CLE-CERT-SPASR.
           MOVE SPACES                 TO SSA-PARENTH-1-SPASRGAR.

           MOVE 5                      TO VAT-NB-PARAM.

           PERFORM 8200-LIRE-SPASRGAR.

       4200-FIN.
           EXIT.
      /
      ****************************************************************  
      *    OBTENIR GARANTIE D'ADHERENT                                  
      ****************************************************************  
       4250-OBTENIR-NEXT-SPASRGAR      SECTION.                         

      D    DISPLAY '4250-OBTENIR-NEXT-SPASRGAR'.

           MOVE 'GN  '                 TO VAT-FONCTION.
           MOVE '('                    TO SSA-PARENTH-1-SPASR.
           MOVE '= '                   TO SSA-OP-RECH-SPASR.

           MOVE EL-NO-GR-ASR           TO SSA-VAL-CLE-GR-SPASR.
           MOVE EL-NO-CO-ASR           TO SSA-VAL-CLE-CO-SPASR.
           MOVE EL-NO-CERT-ASR         TO SSA-VAL-CLE-CERT-SPASR.
           MOVE SPACES                 TO SSA-PARENTH-1-SPASRGAR.

           MOVE 5                      TO VAT-NB-PARAM.

           PERFORM 8200-LIRE-SPASRGAR.

       4250-FIN.
           EXIT.
      /
      ****************************************************************  
      *    OBTENIR HISTORIQUE DE GARANTIE D'ADHERENT                    
      ****************************************************************  
       4300-OBTENIR-SPASRGDT           SECTION.                         

      D    DISPLAY '4300-OBTENIR-SPASRGDT'.

           INITIALIZE SEG-RPASRGDT.

           MOVE EL-CD-GAR-ASR          TO SSA-VAL-CLE-SPASRGAR.
           MOVE '='                    TO SSA-OP-RECH-SPASRGAR.
           MOVE '('                    TO SSA-PARENTH-1-SPASRGAR.

           MOVE 'GU  '                 TO VAT-FONCTION.
           MOVE '('                    TO SSA-PARENTH-1-SPASRGDT.
           MOVE '>='                   TO SSA-OP-RECH-SPASRGDT.
      *$ ?? OBTENIR LA DATE PLUS RECENTE
           MOVE WS-DATE-COMPL          TO SSA-VAL-CLE-DT-COMP-SPASRGDT.

           PERFORM 8300-LIRE-SPASRGDT.

       4300-FIN.
           EXIT.
      /
      ****************************************************************  
      *    OBTENIR CATEGORIE                                            
      ****************************************************************  
       4400-OBTENIR-SPGRPCAT           SECTION.                         

      D    DISPLAY '4400-OBTENIR-SPGRPCAT'.

           MOVE 'GU  '                 TO VAT-FONCTION.
           MOVE '('                    TO SSA-PARENTH-1-SPGRP
                                          SSA-PARENTH-1-SPGRPCAT.
           MOVE '= '                   TO SSA-OP-RECH-SPGRP
                                          SSA-OP-RECH-SPGRPCAT.

           MOVE EL-NO-GR-ASR           TO SSA-VAL-CLE-SPGRP.
           MOVE EL-NO-CAT-DESC-ASR(1)  TO SSA-VAL-CLE-SPGRPCAT.
           MOVE ')'                    TO SSA-PARENTH-2-SPGRP
                                          SSA-PARENTH-2-SPGRPCAT.

           MOVE 5                      TO VAT-NB-PARAM.

           PERFORM 8400-LIRE-SPGRPCAT.

       4400-FIN.
           EXIT.
      /
      ****************************************************************  
      *    OBTENIR HISTORIQUE ADHERENT                               *  
      ****************************************************************  
       4500-OBTENIR-SPASRCHG           SECTION.                         

      D    DISPLAY '4500-OBTENIR-SPASRCHG'.

           MOVE  '('                   TO SSA-PARENTH-1-SPASR.
           MOVE  '= '                  TO SSA-OP-RECH-SPASR.
           MOVE EL-NO-GR-ASR           TO SSA-VAL-CLE-GR-SPASR.
           MOVE EL-NO-CO-ASR           TO SSA-VAL-CLE-CO-SPASR.
           MOVE EL-NO-CERT-ASR         TO SSA-VAL-CLE-CERT-SPASR.


           MOVE WS-DATE-COMPL          TO SSA-VAL-CLE-SPASRCHG.
           MOVE 'GU'                   TO VAT-FONCTION.
           MOVE '('                    TO SSA-PARENTH-1-SPASRCHG.
           MOVE '>='                   TO SSA-OP-RECH-SPASRCHG.

           PERFORM 8500-LIRE-SPASRCHG.

       4500-FIN.
           EXIT.
      /
      ****************************************************************  
      *    ECRIRE LE RECORD ABSENT : ADHERENT DONT LE NAS EST ABSENT *  
      *    DANS OSCAR.                                               *
      ****************************************************************  
       7100-ECRIRE-FABST               SECTION.                         

      D    DISPLAY '7100-ECRIRE-FABST'.

           ADD +1                      TO CPT-REC-ABST
           INITIALIZE                  RECORD-ABST.

           MOVE VAT-NAS                TO VAT-ABST-NAS

           IF VAT-TRAIT-PAIE            = 'SAGIP'
               MOVE 'SAGIP'             TO VAT-ABST-TRAIT-PAIE
               MOVE VAT-SAGIP-ORGAN(1:4)TO VAT-ABST-SYND-CODE
               MOVE VAT-SAGIP-NOM       TO VAT-ABST-NOM
               MOVE VAT-SAGIP-PRENOM    TO VAT-ABST-PRENOM
               MOVE VAT-SAGIP-DT-NAIS   TO VAT-ABST-DT-NAIS
               MOVE VAT-SAGIP-STATUT    TO VAT-ABST-SAGIP-STATUT
               MOVE VAT-SAGIP-UNIT-SYND TO VAT-ABST-SAGIP-UNIT
               MOVE VAT-SAGIP-EXO-PLAN  TO VAT-ABST-SAGIP-EXO
               MOVE VAT-SAGIP-DESC-EXO  TO VAT-ABST-SAGIP-DESC-EXO
               MOVE VAT-ABST-ORIG-SAGIP TO VAT-ABST-INFO-ORIG
           ELSE
               IF VAT-TRAIT-PAIE        = 'CNEST'
                   MOVE 'CNESST'        TO VAT-ABST-TRAIT-PAIE

                   MOVE '30703'         TO VAT-ABST-SYND-CODE

                   MOVE VAT-CNEST-NOM       TO VAT-ABST-NOM
                   MOVE VAT-CNEST-PRENOM    TO VAT-ABST-PRENOM
                   MOVE VAT-CNEST-DT-NAIS   TO VAT-ABST-DT-NAIS
                   MOVE VAT-CNEST-EXO       TO VAT-ABST-CNEST-EXO
                   MOVE VAT-CNEST-RAISON    TO VAT-ABST-CNEST-RAISON
                   MOVE VAT-ABST-ORIG-CNEST TO VAT-ABST-INFO-ORIG
               END-IF
           END-IF.

      *    ECRITURE PHYSIQUE
           MOVE VAT-ABSENT             TO RECORD-ABST.
           WRITE                          RECORD-ABST.
           INITIALIZE                     VAT-ABSENT
                                          RECORD-ABST.

       7100-FIN.
           EXIT.
      /
      ****************************************************************  
      *   'ADHERENTS EXCLUS DE LA LISTE DES MEMBRES AVEC SYNDICAT'   *
      * ECRIRE  LES RECORDS DES ADHERENTS  INVALIDES;  ILS SONT DANS *  
      * OSCAR, MAIS ILS REMPLISSENT UNE DES CONDITIONS D'INVALIDITE. *
      *    FICHIER: SCDFINVD                                         *
      *                                                              *
      ****************************************************************  
       7200-ECRIRE-FINVD               SECTION.                         

      D    DISPLAY '7200-ECRIRE-FINVD'.

           ADD +1                      TO CPT-REC-INVD.
           INITIALIZE                     RECORD-INVD
                                          VAT-INVD.

      *    REMPLIR LES CHAMPS VAT-INVD-NOM ET VAT-INVD-PRENOM. RECULER
      *    LE CHAMP PRENOM UNE POSITION, POUR ENLEVER LE BLANC.
           PERFORM 7900-DECOMPOSER-NOM.

           MOVE WS-AUX-NOM                 TO VAT-INVD-NOM

           IF  WS-AUX-PRENOM(1:1)          = ' '
               MOVE WS-AUX-PRENOM(2:24)    TO VAT-INVD-PRENOM
           ELSE
               MOVE WS-AUX-NOM             TO VAT-INVD-PRENOM
           END-IF.

      *$ ??FORMATER LE CHAMP DATE, A X(10) ET AVEC SEPARATEUR '-'
           PERFORM 7901-FORMATER-DATE.
           MOVE WS-DATE-X10            TO VAT-INVD-DT-NAIS.

      *    TRAITER SELON LE TRAITEUR DE PAIE : SAGIP OU CNESST
           IF VAT-TRAIT-PAIE           = 'SAGIP'
               MOVE VAT-SAGIP-ORGAN(1:4)     TO VAT-INVD-SYND
               MOVE VAT-SAGIP-STATUT         TO VAT-INVD-SAGIP-STATUT
               MOVE VAT-SAGIP-UNIT-SYND      TO VAT-INVD-SAGIP-UNIT
               MOVE VAT-SAGIP-EXO-PLAN       TO VAT-INVD-SAGIP-EXO
               MOVE VAT-SAGIP-DESC-EXO       TO VAT-INVD-SAGIP-DESC-EXO
               MOVE 'AU RAT, CHANGER CATEGORIE SFPQ'
                                             TO VAT-INVD-TEXTE
               MOVE VAT-INVD-ORIG-SAGIP      TO VAT-INVD-INFO-ORIG
           ELSE
               IF VAT-TRAIT-PAIE       = 'CNEST'
                   MOVE '30703'              TO VAT-INVD-SYND
                   MOVE VAT-CNEST-EXO        TO VAT-INVD-CNEST-EXO
                   MOVE VAT-CNEST-RAISON     TO VAT-INVD-CNEST-RAISON
                   MOVE 'AU RAT, CHANGER CATEGORIE CNESST'
                                             TO VAT-INVD-TEXTE
                   MOVE VAT-INVD-ORIG-CNEST  TO VAT-INVD-INFO-ORIG
               END-IF
           END-IF.

      *    LES AURTRES VARIABLES
           MOVE EL-NO-ASS-SOC-ASR      TO VAT-INVD-NAS.
           MOVE EL-NO-GR-ASR           TO VAT-INVD-NO-GR-ASR.
           MOVE EL-NO-CO-ASR           TO VAT-INVD-NO-CO-ASR
                                          VAT-INVD-NO-COMPTE.
           MOVE EL-NO-CAT-DESC-ASR(1)  TO VAT-INVD-NO-CAT.
           MOVE EL-NO-CERT-ASR         TO VAT-INVD-NO-CERT-ASR
                                          VAT-INVD-NO-CERT.
           MOVE EL-CD-ST-DESC-ASR-1    TO VAT-INVD-CD-ST-ASR.
           MOVE EL-CD-ENVG-ASR         TO VAT-INVD-CD-ENVG.
           MOVE EL-DT-EFF-CD-ENVG-ASR  TO VAT-INVD-DT-ENVG.

           MOVE VAT-PARM-USA(1:9)      TO VAT-INVD-CD-USA.
           MOVE VAT-PARM-DT-EFF(1:6)   TO VAT-INVD-DT-EFF.

      *    LES VALEURS CONSTANTS
           MOVE 'X'                    TO VAT-INVD-TRAN.
           MOVE 'M55'                  TO VAT-INVD-CD-TRAN-EDL.
           MOVE '01'                   TO VAT-INVD-NO-CARTE.
           MOVE 'GEN'                  TO VAT-INVD-TYPE-TEXTE.
           MOVE '998'                  TO VAT-INVD-NO-LIGNE.
           MOVE '1'                    TO VAT-INVD-NO-SECT.

           STRING 'AU RAT, CHANGER CATEGORIE ' WS-SYND-NOM
           DELIMITED BY SIZE         INTO VAT-INVD-TEXTE.

      *    ECRITURE PHYSIQUE
           MOVE VAT-INVD               TO RECORD-INVD
           WRITE                          RECORD-INVD.
           INITIALIZE                     VAT-INVD
                                          RECORD-INVD.

       7200-FIN.
           EXIT.
      /
      ****************************************************************  01311000
      *    ECRIRE LE RECORD DONT L'ADHERENT A ETE TROUVE DANS OSCAR  *  01312000
      ****************************************************************  01313000
       7300-ECRIRE-FADHE               SECTION.                         01320000

      D    DISPLAY '7300-ECRIRE-FADHE'.

           ADD +1                      TO CPT-REC-TRAIT
           INITIALIZE                  RECORD-ADHE.
      *                                VAT-ADHE.

      *    REMPLIR LE CHAMPS VAT-ADH-NOM ET VAT-ADH-PRENOM.
      *    RECULER LE CHAMP VAT-ADH-PRENOM, POUR ENLEVER LE BLANC.
           PERFORM 7900-DECOMPOSER-NOM.

           MOVE WS-AUX-NOM             TO VAT-ADH-NOM-ASR

           IF WS-AUX-PRENOM(1:1)       = ' '
               MOVE WS-AUX-PRENOM(2:24)    TO VAT-ADH-PRENOM-ASR
           ELSE
               MOVE WS-AUX-PRENOM      TO VAT-ADH-PRENOM-ASR
           END-IF.

      *$ ??FORMATER LE CHAMP DATE, A X(10) ET AVEC SEPARATEUR '-'
           PERFORM 7901-FORMATER-DATE.
           MOVE WS-DATE-X10            TO VAT-ADH-DT-NAIS.

           MOVE EL-NO-ASS-SOC-ASR      TO VAT-ADH-NO-ASS-SOC.
           MOVE EL-NO-GR-ASR           TO VAT-ADH-NO-GR-ASR.
           MOVE EL-NO-CO-ASR           TO VAT-ADH-NO-CO-ASR.
           MOVE EL-CD-ST-DESC-ASR-1    TO VAT-ADH-CD-ST-ASR.
           MOVE EL-CD-TERM-ASR         TO VAT-ADH-CD-TERM-ASR.
           MOVE EL-NO-CERT-ASR         TO VAT-ADH-NO-CERT-ASR.

      * ----------------------------------------------
      *    CHERCHER LA NOUVELLE CATEGORIE
      * ----------------------------------------------
           MOVE EL-NO-CAT-DESC-ASR(1)  TO VAT-ADH-ANC-NO-CAT
           MOVE EL-NO-CAT-DESC-ASR(1)  TO VAT-AUX-CAT.
           PERFORM 9303-LIRE-TABL-CATEGORIE.

           IF STL-CAT-TROUVE           = 'N'
               MOVE SPACES             TO VAT-ADH-NOU-NO-CAT
           ELSE
               MOVE VAT-AUX-CAT        TO VAT-ADH-NOU-NO-CAT
           END-IF.

      * ------------------------------------------------------
      *    TRAITER LES INFOS SPECIFIQUES A UN TRAITEUR DE PAIE
      * ------------------------------------------------------
           IF VAT-TRAIT-PAIE           = 'SAGIP'
               MOVE VAT-SAGIP-ORGAN(1:4)  TO VAT-ADH-SYND
               MOVE VAT-SAGIP-STATUT      TO VAT-ADH-SAGIP-STATUT
               MOVE VAT-SAGIP-UNIT-SYND   TO VAT-ADH-SAGIP-UNIT
               MOVE VAT-SAGIP-EXO-PLAN    TO VAT-ADH-SAGIP-EXO
               MOVE VAT-SAGIP-DESC-EXO    TO VAT-ADH-SAGIP-DESC-EXO

               MOVE VAT-ADH-SAGIP         TO VAT-ADH-TRAIT-PAIE
           ELSE
               IF VAT-TRAIT-PAIE       = 'CNEST'
                   MOVE '30703'           TO VAT-ADH-SYND
                   MOVE VAT-CNEST-EXO     TO VAT-ADH-CNEST-EXO
                   MOVE VAT-CNEST-RAISON  TO VAT-ADH-CNEST-RAISON

               MOVE VAT-ADH-CNEST         TO VAT-ADH-TRAIT-PAIE

               END-IF
           END-IF.

      *    ECRITURE PHYSIQUE
           MOVE  VAT-ADHE              TO RECORD-ADHE.
           WRITE                          RECORD-ADHE.
           INITIALIZE                     VAT-ADHE
                                          RECORD-ADHE.

       7300-FIN.
           EXIT.
      /
      ****************************************************************  
      *    DECOMPOSER LE CHAMP NOM DE LA BD EN CHAMPS NOM ET PRENOM  *  
      ****************************************************************  
       7400-ECRIRE-EDL-RES-GAR         SECTION.                         

      D    DISPLAY '7400-ECRIRE-EDL-RES-GAR'.

           INITIALIZE                  RECORD-EDRG.

           MOVE VAT-PARM-USA(1:9)      TO VAT-EDRG-CD-USA.
           MOVE VAT-PARM-DT-EFF(1:6)   TO VAT-EDRG-DT-EFF.
           MOVE EL-NO-CO-ASR           TO VAT-EDRG-NO-CO-ASR.
           MOVE EL-NO-CERT-ASR         TO VAT-EDRG-NO-CERT-ASR.
           MOVE EL-NO-GR-ASR           TO VAT-EDRG-NO-GR-ASR.
           MOVE EL-CD-TERM-GAR-ASR     TO VAT-EDRG-CD-RES.

      * ----------------------------------------------
      *    CONVERTIR LE NOM DE LA NOUVELLE GARANTIE
      * ----------------------------------------------
           MOVE EL-CD-GAR-ASR          TO VAT-CNV-CD-GAR
           MOVE WS-SYND-NOM            TO VAT-CNV-SYND
           PERFORM 9100-CONVERTIR-GAR.

           IF VAT-CNV-CD-GAR           = 'XXXXX'
               MOVE SPACES             TO VAT-EDRG-CD-NOUV-GAR
           ELSE
               MOVE VAT-CNV-CD-GAR     TO VAT-EDRG-CD-NOUV-GAR
           END-IF.

      *    LES VALEURS CONSTANTS
           MOVE 'X'                    TO VAT-EDRG-TRS-FOR.
           MOVE 'M36'                  TO VAT-EDRG-TRS-EDL.
           MOVE '01'                   TO VAT-EDRG-NO-CARTE.
           MOVE '1'                    TO VAT-EDRG-ID-TRS.

      *    ECRITURE PHYSIQUE
           MOVE  VAT-EDRG              TO RECORD-EDRG.
           WRITE                          RECORD-EDRG.
           INITIALIZE                     VAT-EDRG
                                          RECORD-EDRG.

       7400-FIN.
           EXIT.
      /
      ****************************************************************  
      *    ECRIRE LE FICHIER EDL DE CHANGEMENT DE CATEGORIE          *  
      ****************************************************************  
       7500-ECRIRE-EDL-CHG-CAT         SECTION.                         

      D    DISPLAY '7500-ECRIRE-EDL-CHG-CAT'.

           INITIALIZE                  RECORD-EDCC.

           MOVE VAT-PARM-USA(1:9)      TO VAT-EDCC-CD-USA.
           MOVE VAT-PARM-DT-EFF(1:6)   TO VAT-EDCC-DT-EFF.
           MOVE EL-NO-CO-ASR           TO VAT-EDCC-NO-CO-ASR.
           MOVE EL-NO-CERT-ASR         TO VAT-EDCC-NO-CERT.
           MOVE EL-NO-GR-ASR           TO VAT-EDCC-NO-GR-ASR.
           MOVE VAT-AUX-CAT            TO VAT-EDCC-NOUV-CAT.
           MOVE EL-CD-TERM-GAR-ASR     TO VAT-EDCC-CD-RES.
           MOVE SPACES                 TO VAT-EDCC-TRS-FOR.

      *    VALEURS CONSTANTS
           MOVE 'M07'                  TO VAT-EDCC-CD-TRS-EDL.
           MOVE '01'                   TO VAT-EDCC-NO-CARTE.
           MOVE '1'                    TO VAT-EDCC-ID-TRS.

      *    ECRITURE PHYSIQUE
           MOVE VAT-EDCC               TO RECORD-EDCC.
           WRITE                          RECORD-EDCC.
           INITIALIZE                     VAT-EDCC
                                          RECORD-EDCC.

       7500-FIN.
           EXIT.
      /
      ****************************************************************  
      *    DECOMPOSER LE CHAMP NOM DE LA BD EN CHAMPS NOM ET PRENOM  *  
      ****************************************************************  
       7900-DECOMPOSER-NOM             SECTION.                         

      D    DISPLAY '7900-DECOMPOSER-NOM'.

           MOVE ZEROES                 TO VAT-TALLY-CHAMP.
           INITIALIZE                     WS-AUX-NOM
                                          WS-AUX-PRENOM.

           UNSTRING EL-NOM-ASR         DELIMITED BY ','
           INTO                           WS-AUX-NOM
                                          WS-AUX-PRENOM
           TALLYING IN                    VAT-TALLY-CHAMP.

       7900-FIN.
           EXIT.
      /
      ****************************************************************  
      *    ECRIRE LE RECORD DONT L'ADHERENT A ETE TROUVE DANS OSCAR  *  
      ****************************************************************  
       7901-FORMATER-DATE              SECTION.                         

      D    DISPLAY '7901-FORMATER-DATE'.

           MOVE EL-DT-NAIS-ASR         TO WS-DATE-8.
           ADD  +28                    TO WS-DATE-X8-AA.
           MOVE WS-DATE-X8(1:4)        TO WS-DATE-X10-AA.
           MOVE WS-DATE-X8(5:2)        TO WS-DATE-X10-MM.
           MOVE WS-DATE-X8(7:2)        TO WS-DATE-X10-JJ.

       7901-FIN.
           EXIT.
      /
      ****************************************************************  
      *    LECTURE DE LA BD D'ADHERENTS                              *  
      ****************************************************************  
       8100-LIRE-SPASR                 SECTION.                         

      D    DISPLAY '8100-LIRE-SPASR'.

           INITIALIZE                  SEG-RPASR.

           CALL 'CBLTDLI' USING        VAT-NB-PARAM
                                       VAT-FONCTION
                                       PCB07
                                       SEG-RPASR
                                       SSA-SPASR-X3.

           IF PCB07-CD-RT = '  ' OR 'GE' OR 'GB'
               NEXT SENTENCE
           ELSE
               MOVE 8100               TO CD-RT
               DISPLAY 'CD-RT         = ' CD-RT
               DISPLAY 'PCB07-CD-RT   = ' PCB07-CD-RT
               DISPLAY 'FONCTION      = ' VAT-FONCTION
               DISPLAY 'SSA-SPASRX3   = ' SSA-SPASR-X3
               GO TO P9999-GOBACK
           END-IF.

       8100-FIN.
           EXIT.
      /
      ****************************************************************  
      *    LECTURE DE LA BD DE GARANTIES DE L'ADHERENT               *  
      ****************************************************************  
       8200-LIRE-SPASRGAR              SECTION.                         

      D    DISPLAY '8200-LIRE-SPASRGAR'.

           INITIALIZE                  SEG-RPASRGAR.

           CALL 'CBLTDLI' USING        VAT-FONCTION
                                       PCB03
                                       SEG-RPASRGAR
                                       SSA-SPASR
                                       SSA-SPASRGAR.

           IF PCB03-CD-RT = '  ' OR 'GE' OR 'GB'
               NEXT SENTENCE
           ELSE
               MOVE 8200               TO CD-RT
               DISPLAY 'CD-RT         = ' CD-RT
               DISPLAY 'PCB03-CD-RT   = ' PCB03-CD-RT
               DISPLAY 'FONCTION      = ' VAT-FONCTION
               DISPLAY 'SEG-RPASRGAR  = ' SEG-RPASRGAR
               DISPLAY 'SSA-SPASRGAR  = ' SSA-SPASRGAR
               DISPLAY 'SSA-SPASR     = ' SSA-SPASR
               GO TO P9999-GOBACK
           END-IF.

       8200-FIN.
           EXIT.
      /
      ****************************************************************  
      *    LECTURE DE LA BD D'HISTORIQUE DES GARANTIES               *  
      ****************************************************************  
       8300-LIRE-SPASRGDT              SECTION.                         

      D    DISPLAY '8300-LIRE-SPASRGDT'.

           INITIALIZE                  SEG-RPASRGDT

           CALL 'CBLTDLI' USING        VAT-FONCTION
                                       PCB03
                                       SEG-RPASRGDT
                                       SSA-SPASR
                                       SSA-SPASRGAR
                                       SSA-SPASRGDT.

           IF PCB03-CD-RT = '  ' OR 'GB' OR 'GE'
               NEXT SENTENCE
           ELSE
               MOVE 8300               TO CD-RT
               DISPLAY 'CD-RT         = ' CD-RT
               DISPLAY 'PCB03-CD-RT   = ' PCB03-CD-RT
               DISPLAY 'FONCTION      = ' VAT-FONCTION
               DISPLAY 'SEG-RPASRGDT  = ' SEG-RPASRGDT
               DISPLAY 'SSA-SPASRGDT  = ' SSA-SPASRGDT
               DISPLAY 'SSA-SPASRGAR  = ' SSA-SPASRGAR
               DISPLAY 'SSA-SPASR     = ' SSA-SPASR
               GO TO P9999-GOBACK.

       8300-FIN.
           EXIT.
      /
      ****************************************************************  
      *    LECTURE DE LA BD DE CATEGORIES                            *  
      ****************************************************************  
       8400-LIRE-SPGRPCAT              SECTION.                         

      D    DISPLAY '8400-LIRE-SPGRPCAT'.

           INITIALIZE                  SEG-RPGRPCAT
           CALL 'CBLTDLI' USING        VAT-FONCTION
                                       PCB01
                                       SEG-RPGRPCAT
                                       SSA-SPGRP
                                       SSA-SPGRPCAT.

           IF PCB01-CD-RT = '  ' OR 'GE' OR 'GB'
               NEXT SENTENCE
           ELSE
               MOVE 8400               TO CD-RT
               DISPLAY 'CD-RT         = ' CD-RT
               DISPLAY 'PCB01-CD-RT   = ' PCB01-CD-RT
               DISPLAY 'FONCTION      = ' VAT-FONCTION
               DISPLAY 'SEG-RPGRPCAT  = ' SEG-RPGRPCAT
               DISPLAY 'SSA-SPGRPCAT  = ' SSA-SPGRPCAT
               DISPLAY 'SSA-SPGRP     = ' SSA-SPGRP
               DISPLAY 'PCB01         = ' PCB01
               GO TO P9999-GOBACK
           END-IF.

       8400-FIN.
           EXIT.
      /
      ****************************************************************  
      * LECTURE DE L'HISTORIQUE DE L'ADHERENT                        *  
      ****************************************************************  
       8500-LIRE-SPASRCHG              SECTION.                         

      D    DISPLAY '8500-LIRE-SPASRCHG'.

           INITIALIZE SEG-SPASRCHG.

           MOVE 5                      TO VAT-NB-PARAM.

           CALL 'CBLTDLI'        USING VAT-NB-PARAM
                                       VAT-FONCTION
                                       PCB03
      *                                SEG-SPASRCHG
                                       SEG-RPASRCHG
                                       SSA-SPASR
                                       SSA-SPASRCHG.

           IF PCB03-CD-RT = '  ' OR 'GE' OR 'GB'
               NEXT SENTENCE
           ELSE
               MOVE 8500               TO CD-RT
               DISPLAY 'CD-RT         = ' CD-RT
               DISPLAY 'PCB03-CD-RT   = ' PCB03-CD-RT
               DISPLAY 'FONCTION      = ' VAT-FONCTION
               DISPLAY 'SEG-RPASRCHG  = ' SEG-RPASRCHG
               DISPLAY 'SSA-SPASR     = ' SSA-SPASR
               DISPLAY 'SSA-SPASRCHG  = ' SSA-SPASRCHG
               DISPLAY 'PCB03         = ' PCB03
               GO TO P9999-GOBACK
           END-IF.

       8500-FIN.
           EXIT.
      /
      ****************************************************************  
      *    TRAITEMENTS DE FIN DE PROGRAMME                           *  
      ****************************************************************  
       9000-TRAITEMENT-FIN             SECTION.                         

      D    DISPLAY '9000-TRAITEMENT-FIN'.

           PERFORM 9100-CLOSE-FILES.

           PERFORM 9200-DISPLAY-EXEC.
                                                                        
       9000-FIN.                                                        
           EXIT.                                                        
      /                                                                 
      ****************************************************************
      * PROGRAMME UTILITAIRE DE CONVERSION DE NOM DE GARANTIE.       *
      * APPEL: PERFORM 9100-CONVERTIR-GAR                            *
      ****************************************************************
       COPY SCDPCGAR.
      *
      ****************************************************************  
      *    FERMETURE DES FICHIERS.                                   *  
      ****************************************************************  
       9100-CLOSE-FILES                SECTION.                         

      D    DISPLAY '9100-CLOSE-FILES'.

           CLOSE   FILE-SYND
                   FILE-MST
                   FILE-CAT
                   FILE-SAGIP
                   FILE-CNEST
                   FILE-NCNES
                   FILE-ABST
                   FILE-ADHE
                   FILE-INVD
                   FILE-EDRG
                   FILE-EDCC.

           IF VAT-FILE-SYND-STATUS NOT = ZEROS
               MOVE 9101               TO CD-RT
               DISPLAY 'SCDPYOT1: ERREUR DE FERMETURE DU FICHIER SYND'
               DISPLAY 'CD-RT       = ' CD-RT
               DISPLAY 'FILE-STATUS = ' VAT-FILE-SYND-STATUS
               GO TO P9999-GOBACK
           END-IF.

           IF VAT-FILE-MST-STATUS NOT = ZEROS
               MOVE 9102               TO CD-RT
               DISPLAY 'SCDPYOT1: ERREUR DE FERMETURE DU FICHIER FMST'
               DISPLAY 'CD-RT       = ' CD-RT
               DISPLAY 'FILE-STATUS = ' VAT-FILE-MST-STATUS
               GO TO P9999-GOBACK
           END-IF.

           IF VAT-FILE-CAT-STATUS NOT = ZEROS
               MOVE 9103               TO CD-RT
               DISPLAY 'SCDPYOT1: ERREUR DE FERMETURE DU FICHIER FCAT'
               DISPLAY 'CD-RT       = ' CD-RT
               DISPLAY 'FILE-STATUS = ' VAT-FILE-CAT-STATUS
               GO TO P9999-GOBACK
           END-IF.

           IF VAT-FILE-SAGIP-STATUS NOT = ZEROS
               MOVE 9104               TO CD-RT
               DISPLAY 'SCDPYOT1: ERREUR DE FERMETURE DU FICHIER SAGIP'
               DISPLAY 'CD-RT       = ' CD-RT
               DISPLAY 'FILE-STATUS = ' VAT-FILE-SAGIP-STATUS
               GO TO P9999-GOBACK
           END-IF.

           IF VAT-FILE-CNEST-STATUS NOT = ZEROS
               MOVE 9105               TO CD-RT
               DISPLAY 'SCDPYOT1: ERREUR DE FERMETURE DU FICHIER CNEST'
               DISPLAY 'CD-RT       = ' CD-RT
               DISPLAY 'FILE-STATUS = ' VAT-FILE-CNEST-STATUS
               GO TO P9999-GOBACK
           END-IF.

           IF VAT-FILE-NCNES-STATUS NOT = ZEROS
               MOVE 9106               TO CD-RT
               DISPLAY 'SCDPYOT1: ERREUR DE FERMETURE DU FICHIER NCNES'
               DISPLAY 'CD-RT       = ' CD-RT
               DISPLAY 'FILE-STATUS = ' VAT-FILE-NCNES-STATUS
               GO TO P9999-GOBACK
           END-IF.

           IF VAT-FILE-ABST-STATUS NOT = ZEROS
               MOVE 9107               TO CD-RT
               DISPLAY 'SCDPYOT1: ERREUR DE FERMETURE DU FICHIER FABST'
               DISPLAY 'CD-RT       = ' CD-RT
               DISPLAY 'FILE-STATUS = ' VAT-FILE-ABST-STATUS
               GO TO P9999-GOBACK
           END-IF.

           IF VAT-FILE-ADHE-STATUS NOT = ZEROS
               MOVE 9108               TO CD-RT
               DISPLAY 'SCDPYOT1: ERREUR DE FERMETURE DU FICHIER FADHE'
               DISPLAY 'CD-RT       = ' CD-RT
               DISPLAY 'FILE-STATUS = ' VAT-FILE-ADHE-STATUS
               GO TO P9999-GOBACK
           END-IF.

           IF VAT-FILE-INVD-STATUS NOT = ZEROS
               MOVE 9109               TO CD-RT
               DISPLAY 'SCDPYOT1: ERREUR DE FERMETURE DU FICHIER FINVD'
               DISPLAY 'CD-RT       = ' CD-RT
               DISPLAY 'FILE-STATUS = ' VAT-FILE-INVD-STATUS
               GO TO P9999-GOBACK
           END-IF.

           IF VAT-FILE-EDRG-STATUS NOT = ZEROS
               MOVE 9110               TO CD-RT
               DISPLAY 'SCDPYOT1: ERREUR DE FERMETURE DU FICHIER FEDRG'
               DISPLAY 'CD-RT       = ' CD-RT
               DISPLAY 'FILE-STATUS = ' VAT-FILE-EDRG-STATUS
               GO TO P9999-GOBACK
           END-IF.

           IF VAT-FILE-EDCC-STATUS NOT = ZEROS
               MOVE 9111               TO CD-RT
               DISPLAY 'SCDPYOT1: ERREUR DE FERMETURE DU FICHIER FEDCC'
               DISPLAY 'CD-RT       = ' CD-RT
               DISPLAY 'FILE-STATUS = ' VAT-FILE-EDCC-STATUS
               GO TO P9999-GOBACK
           END-IF.

       9100-FIN.
           EXIT.
      /
      ****************************************************************  
      *    DISPLAY VARS CONTROLE DE L'EXECUTION                      *  
      ****************************************************************  
       9200-DISPLAY-EXEC               SECTION.                         

           DISPLAY ' '.
           DISPLAY '##########################################'.
           DISPLAY '#                                         '.
           DISPLAY '# ### FIN DU PROGRAMME S C D P Y O T 1 ###'.
           DISPLAY '# (A) RECS LUS           : ' CPT-REC-LU.
           DISPLAY '# (B) RECS SUPPL         : ' CPT-REC-NEXT.
           DISPLAY '# (C) RECS ABSENTS OSCAR : ' CPT-REC-ABST.
           DISPLAY '# (D) RECS INVALIDES     : ' CPT-REC-INVD.
           DISPLAY '# (E) RECS TRAITES       : ' CPT-REC-TRAIT.
           DISPLAY '# (F) CPT-REC-BYPASS-NAS : ' CPT-REC-BYPASS-NAS.
           DISPLAY '# (G) CPT-REC-BYPASS     : ' CPT-REC-BYPASS.
           DISPLAY '# (H) CPT-REC-EXO-A      : ' CPT-EXO-A.
           DISPLAY '#     CPT-EXO-B          : ' CPT-EXO-B.
           DISPLAY '#                                         '.
           DISPLAY '#     EQUATION DE BALANCEMENT:            '.
           DISPLAY '#                                         '.
           DISPLAY '#     (A + B) = (C + D + E + F + G + H)   '.
           DISPLAY '#                                         '.
           DISPLAY '##########################################'.
           DISPLAY ' '.

       9200-FIN.
           EXIT.
      /
      ****************************************************************  
      *    FAIRE DISPLAY DES SYNDICATS  LUS DU FICHIER SCDFSYND      *  
      ****************************************************************  
       9201-DISPLAY-SYND               SECTION.                         

           DISPLAY '9201-DISPLAY-SYND'.

           DISPLAY 'QUANTITE SYND LUS: ' VAT-TB-SYND-CNT.
           DISPLAY 'VAT-TB-SYND-CNT : ' VAT-TB-SYND-CNT

           PERFORM VARYING IND FROM 1 BY 1
                UNTIL IND > VAT-TB-SYND-CNT

               DISPLAY '# '       VAT-SYND-SEQ(IND)
                       ' TRAIT? ' VAT-SYND-IN-TRAIT(IND)
                       ' NOM : '  VAT-SYND-NOM(IND)
           END-PERFORM.

       9201-FIN.
           EXIT.
      /
      ****************************************************************  
      *    FAIRE DISPLAY DES MINISTERES LUS DU FICHIER SCDFMST       *  
      ****************************************************************  
       9202-DISPLAY-MST                SECTION.                         

           DISPLAY 'QUANTITE MST LUE: ' VAT-TB-MST-CNT.

           PERFORM VARYING IND FROM 1 BY 1
                UNTIL IND > VAT-TB-MST-CNT

               DISPLAY 'MST NUM   : ' VAT-MST-NUM(IND)
               DISPLAY 'MINISTERE : ' VAT-MST-NOM(IND)
               DISPLAY 'VISE      : ' VAT-MST-IN-VISE(IND)
           END-PERFORM.

       9202-FIN.
           EXIT.
      ****************************************************************  
      *    FAIRE DISPLAY DES CATEGORIES LUS DU FICHIER SCDFCAT       *  
      ****************************************************************  
       9203-DISPLAY-CAT                SECTION.                         

           DISPLAY 'QUANTITE CAT LUE: ' VAT-TB-CAT-CNT.

           MOVE +0                     TO IND.

           PERFORM VARYING IND FROM 1 BY 1
               UNTIL IND > VAT-TB-CAT-CNT
               DISPLAY 'CAT-ACTU : ' VAT-CATG-CD-ANC(IND)

               PERFORM VARYING IND-X FROM 1 BY 1
                   UNTIL IND-X > 9

                   DISPLAY VAT-CATG-CD-NOUV(IND IND-X)

               END-PERFORM
           END-PERFORM.

       9203-FIN.
           EXIT.
      /
      ****************************************************************  
      *    LIRE LA TABLE DE SYNDICATS                                *  
      ****************************************************************  
       9301-CHERCHE-TABL-SYNDICAT      SECTION.                         

      D    DISPLAY '9301-CHERCHE-TABL-SYNDICAT'.

           SET IND-SYND                TO 1.

           SEARCH VAT-SYND
           AT END
               MOVE 'NON'              TO STL-SYND-TROUVE
           WHEN VAT-SYND-NOM(IND-SYND) = WS-SYND-NOM
               MOVE 'OUI'              TO STL-SYND-TROUVE
               MOVE VAT-SYND-SEQ(IND-SYND)
                                       TO WS-SYND-SEQ
               MOVE VAT-SYND-IN-TRAIT(IND-SYND)
                                       TO WS-SYND-IN-TRAIT.

       9301-FIN.
           EXIT.
      /
      ****************************************************************  
      *    LIRE LA TABLE DE MINISTERES.                              *
      *    VERIFIE SI LE RECORD FAIT PARTIE DE LA LISTE DE MINISTERES*  
      *    CIBLES. FAIRE UNE RECHERCHE BINAIRE. SI MST TROUVE, CONTI-*
      *    NUER TRAITEMENT; SINON, BYPASSER RECORD ET LIRE PROCHAIN. *
      ****************************************************************  
       9302-CHERCHE-MINISTERE          SECTION.                         
                                                                        
           SET IND-MST                     TO 1.

           SEARCH VAT-MINISTERE
           AT END
               MOVE 'NON'              TO STL-MST-TROUVE

           WHEN VAT-MST-NUM(IND-MST)     = VAT-NO-MINISTERE
            AND VAT-MST-IN-VISE(IND-MST) = 'O'
               MOVE 'OUI'              TO STL-MST-TROUVE
               MOVE VAT-MST-NUM(IND-MST) TO WS-MST-NUM.

       9302-FIN.
           EXIT.
      /
      ****************************************************************  
      *    LIRE LA TABLE DE CATEGORIES                               *  
      ****************************************************************  
       9303-LIRE-TABL-CATEGORIE        SECTION.                         

      D    DISPLAY '9303-LIRE-TABL-CATEGORIE'.

           SET IND-CATG                TO 1.

           SEARCH VAT-CATEGORIES
           AT END
               MOVE 'NON'              TO STL-CAT-TROUVE

           WHEN VAT-CATG-CD-ANC(IND-CATG) =  VAT-AUX-CAT
               MOVE 'OUI'              TO STL-CAT-TROUVE
               MOVE VAT-CATG-CD-NOUV(IND-CATG WS-SYND-SEQ)
                                       TO VAT-AUX-CAT.

       9303-FIN.
           EXIT.
      /
      ****************************************************************  
      *    OBTENIR DATE DU JOUR                                      *  
      ****************************************************************  
       COPY SQA2KJMA.
      *
      /
      *                                                                 
      ****************************************************************  
      *    GERER LES FINS ANORMALES.                                 *  
      ****************************************************************  
       P9999-GOBACK                    SECTION.                         
                                                                        
           COPY ASGOBACK.                                               

       EJECT
      /                                                                 
      ****************************************************************  
      *        F I N   D U   P R O G R A M M E   S C D P Y O T 1     *  
      ****************************************************************  
