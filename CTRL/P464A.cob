                                                                                
!***********************************************************************        
!       COPYRIGHT, SOCIETE DE L'ASSURANCE AUTOMOBILE DU QUEBEC, 1997            
!                                                                               
!                                                                               
!  "TOUS DROITS RESERVES. IL EST INTERDIT ENTRE AUTRES, DE REPRODUIRE           
!   OU DE COMMUNIQUER EN TOUT OU EN PARTIE L'OEUVRE SOUS QUELQUE FORME          
!   OU PAR QUELQUE PROCEDE QUE CE SOIT, SANS AVOIR OBTENU AU PREALABLE          
!   L'AUTORISATION ECRITE DE LA SOCIETE"                                        
!                                                                               
!***********************************************************************        
                                                                                
                                                                                
!  MODULE:     GFPP466A  PREMAP DU DIALOGUE GFPTECP1                            
!  PROGRAMMEUR:SYLVAIN TURCOTTE,  FIRME: IST                                    
!  ANALYSTE:   YVES LAPOINTE                                                    
!  DATE    :  SEPTEMBRE  1984                                                   
!  REMARQUE:   DESCRIPTION,                                                     
!                                                                               
!                  - FORMATTER L'AFFICHAGE GFPA2131 POUR L'AFFICHAGE DE         
!                    LA PAGE 1 ( USAGER REGULIER )                              
!                  - FORMATTER L'AFFICHAGE GFPA2131 POUR L'AFFICHAGE DE         
!                    LA PAGE 1 ( VERIFICATEURS )                                
!                  - DETECTER LA PRESENCE DES MESSAGES                          
!                    'GFPM201S', 'GFPM303S', 'GFPP364S'                         
!                  - POSSIBILITE D'AFFICHAGE DES MESSAGES GENERES PAR           
!                    LE GUT0003D                                                
!                                                                               
!******************************************************************             
!*    M O D I F I C A T I O N S                                                 
!******************************************************************             
!* 1999-11-05    FAIT PAR HUGO DEMERS (MSI)                                     
!*               PROJET CARTE DE DEBIT                                          
!*                                                                              
!*       -AJOUT D'UN NOUVEAU CODE D'ECART (9) POUR EXPLIQUER LES                
!*        ECARTS LIES AUX DEPOTS DE LA CARTE DE DEBIT OU DE CREDIT.             
!*       -EXCLURE DU PANORAMA ET DES CALCULS LES DONNEES QUI SONT               
!*        RELIEES A L'AGENT FICTIF CREE AVEC LA TRANSACTION GFPL.               
!*        (LE CODE DE CET AGENT CORRESPOND AU # DU CSER.)                       
!*       -SI UN TYP-DEPOT-INTRN = 'D' OU 'K' AFFICHER LE MESSAGE                
!*        GFPM915S SINON AFFICHER LE MESSAGE GFPM303S.                          
!*       - MAI 2007 : CONDITION GFPM915 MODIFIEE                                
!******************************************************************             
!* 2001-02-23    FAIT PAR BENOIT DUPUIS                                         
!*               DEMANDE 46118                                                  
!*                                                                              
!*       -S'ASSURER QU'IL N'Y A PAS DE CYCLE DE PERCEPTION EN COURS.            
!******************************************************************             
!*                                                                *             
!* OCTOBRE 2002                                                   *             
!* DDS #44193                                                     *             
!*  - EN MODE RECOUVREMENT, LORSQUE LE CENTRE DE SERVICE EST DEJA *             
!*    FERME ET QUE LA TX A ETE INITIE PAR LE CODE DE TACHE 'GFPP' *             
!*    GENERER LE MESSAGE GFPM364I, AFIN D'AVISER QUE LE CENTRE DE *             
!*    SERVICE EST DEJA FERME.                                     *             
!*            GERALDA GUILLEMETTE  &  MICHEL LAMONTAGNE           *             
!******************************************************************             
!* 2004-06-10 - SERGE LEBEL                                       *             
!* POUR LE PROJET CYLINDREE VEHICULE                              *             
!* - RECOMPILE POUR CYCPER                                        *             
!******************************************************************             
!* 2011-09-26 - SERGE LEBEL                                       *             
!* POUR LA DDS 62390                                              *             
!* - CESSER DE D'UTILISER GFPD POUR LES CARTES DE DEBIT           *             
!******************************************************************             
! 05 MARS  2014    *** BOUCHER LUC         ***                   *              
! DDS # 208026   ( TROUBLE DE PROD )                             *              
! TROUBLE DE PRODUCTION : NE PLUS S'OCCUPER DE L'HEURE DE FIN    *              
!  PERIODE,  AFIN DE GERER SI LA PERIODE EST OUVERTE OU FERMEE   *              
!            ON DOIT PASSER PAR LE STATUT DE LA PERIODE          *              
!*****************************************************************              
!******************************************************************             
!   MODULE  PRINCIPAL                                             *             
!******************************************************************             
!0000-DIRECTEUR.                                                                
!                                                                               
   CALL  INITZONE.                                                              
!                                                                               
   CALL  INITSM02.                                                              
!                                                                               
   CALL  PREPAFFI.                                                              
!                                                                               
   CALL  INIT-MES.                                                              
!                                                                               
   IF  GZC001-NB-MESS > ZEROES                                                  
   DO.                                                                          
       CALL MESSAGE.                                                            
   END.                                                                         
!                                                                               
!  AFFICHE LE PANORAMA GFPN2131                                                 
!                                                                               
   DISPLAY.                                                                     
!                                                                               
!0000-FIN.                                                                      
!                                                                               
!******************************************************************             
!   INITIALISATION DE LA PARTIE VARIABLE DE GFPZV408              *             
!******************************************************************             
!0500-INITIALISE LA PARTIE VARIABLE DE LA ZONE GFPZV408                         
!                                                                               
DEFINE  SUBROUTINE INITZONE.                                                    
!                                                                               
   MOVE 1     TO FZV408-NB-AGENT-SESSN.                                         
!                                                                               
   WHILE FZV408-NB-AGENT-SESSN LE 80                                            
   REPEAT.                                                                      
      MOVE SPACES   TO  FZV408-NO-AGENT-1                                       
                        (FZV408-NB-AGENT-SESSN).                                
      MOVE ZEROES   TO  FZV408-IDENT-AGENT-SESSN                                
                        (FZV408-NB-AGENT-SESSN).                                
      MOVE SPACES   TO  FZV408-PRES-CYCL-PERCP                                  
                        (FZV408-NB-AGENT-SESSN).                                
      MOVE ZEROES   TO  FZV408-GRP-SESSN-CAISS                                  
                        (FZV408-NB-AGENT-SESSN).                                
      MOVE SPACES   TO  FZV408-INDIC-ENCAI-RECPR                                
                        (FZV408-NB-AGENT-SESSN).                                
      ADD 1         TO  FZV408-NB-AGENT-SESSN.                                  
   END.                                                                         
!                                                                               
   MOVE 1     TO FZV408-INDIC-TEXTE-ECART.                                      
!                                                                               
   WHILE FZV408-INDIC-TEXTE-ECART LE 45                                         
   REPEAT.                                                                      
      MOVE SPACES   TO  FZV408-TEXTE-EXPLI-ECART                                
                        (FZV408-INDIC-TEXTE-ECART).                             
      ADD 1         TO  FZV408-INDIC-TEXTE-ECART.                               
   END.                                                                         
!                                                                               
!* OBTENIR LE SCRATCH INTER-TX POUR AVOIR LA DATE DU JOUR                       
   LINK PROGRAM 'GUT0046D' USING (GGGZC000 GUTZC046 GGGST001 GGGST002).         
!                                                                               
   MOVE ZEROES             TO  FZV408-NB-AGENT-SESSN.                           
   MOVE ZEROES             TO  FZV408-IND-AGNDA.                                
   MOVE SPACES             TO  FZV408-COD-REPON.                                
   MOVE SPACES             TO  FZV408-DAS-DEB-PERIO.                            
   MOVE ZEROES             TO  FZV408-NO-PERIO.                                 
   MOVE SPACES             TO  FZV408-COD-ECART-CSER.                           
   MOVE SPACES             TO  FZV408-INDIC-CONFR.                              
   MOVE SPACES             TO  FZV408-INDIC-PRES-TEXTE-ECART.                   
   MOVE 1                  TO  FZV408-INDIC-TEXTE-ECART.                        
   MOVE 1                  TO  FZV408-INDIC-TEXTE-ECART-MAP.                    
   MOVE ZEROES             TO  FZV408-TOTAL-ECART-CSER.                         
   MOVE ZEROES             TO  FZV408-INDIC-AGENT-SESSN.                        
   MOVE ZEROES             TO  FZV408-INDIC-1.                                  
   MOVE ZEROES             TO  FZV408-IND-TRAIT.                                
   MOVE SPACES             TO  FZV408-SEN-1.                                    
   MOVE SPACES             TO  FZV408-SEN-2.                                    
   MOVE 1                  TO  FZV408-INDIC-AGENT-SESSN-MAP.                    
   MOVE ZEROES             TO  FZV408-INDIC-PRES-CYCL-PERCP.                    
   MOVE ZEROES             TO  FE213A-TOTAL-CRCRE-DBT.                          
   MOVE ZEROES             TO  FE213A-TOTAL-BD-CRCRE-DBT.                       
   MOVE ZEROES             TO  FE213A-MNT-CALC.                                 
!                                                                               
   WHILE FZV408-INDIC-AGENT-SESSN-MAP LE 8                                      
   REPEAT.                                                                      
      MODIFY MAP TEMPORARY                                                      
      FOR (FE213A-NO-AGENT-1    (FZV408-INDIC-AGENT-SESSN-MAP)                  
           FE213A-IDENT-AGENT-SESSN                                             
                                (FZV408-INDIC-AGENT-SESSN-MAP)                  
           FE213A-PRES-CYCL-PERCP                                               
                                (FZV408-INDIC-AGENT-SESSN-MAP)                  
           FE213A-NET-PERCU     (FZV408-INDIC-AGENT-SESSN-MAP)                  
           FE213A-CUMUL-DEPOT-INTRN                                             
                                (FZV408-INDIC-AGENT-SESSN-MAP)                  
           FE213A-TOTAL-ECART-CAISS                                             
                                (FZV408-INDIC-AGENT-SESSN-MAP)                  
           FE213A-TOTAL-ENCAI-RECPR                                             
                                (FZV408-INDIC-AGENT-SESSN-MAP)                  
           FE213A-TOTAL-ENCAI-INTRB                                             
                                (FZV408-INDIC-AGENT-SESSN-MAP))                 
           OUTPUT DATA IS ERASE.                                                
      ADD  1                 TO  FZV408-INDIC-AGENT-SESSN-MAP.                  
   END.                                                                         
!                                                                               
   MOVE 1                    TO  FZV408-INDIC-AGENT-SESSN-MAP.                  
!                                                                               
   GOBACK.                                                                      
!                                                                               
!0500-FIN.                                                                      
!                                                                               
!******************************************************************             
!   INITIALISATION DU SCRATCH AREA DE MESSAGES                    *             
!******************************************************************             
!0600-INITIALISE-SA-MESSAGES                                                    
!                                                                               
DEFINE SUBROUTINE INITSM02.                                                     
!---------------------------                                                    
!                                                                               
   MOVE 100  TO  GSM002-NB-MESS.                                                
!                                                                               
   WHILE GSM002-NB-MESS > 0                                                     
   REPEAT.                                                                      
      MOVE SPACES    TO  GSM002-GRP-MESS-AFICH(GSM002-NB-MESS).                 
      ADD -1         TO  GSM002-NB-MESS.                                        
   END.                                                                         
!                                                                               
   MOVE 1         TO  GSM002-NO-MESS-AFICH.                                     
   MOVE ZEROES    TO  GSM002-NB-MESS.                                           
!                                                                               
   PUT SCRATCH AREA ID     GZV002-GRP-SCR-AREA-MESS                             
               FROM        GGGSM002                                             
               TO          GSM002-FIN-SCR                                       
               RECORD ID   0002  REPLACE.                                       
!                                                                               
   MOVE 1         TO GZV001-DML-SEQ.                                            
!                                                                               
   IF  ERROR-STATUS NE '4317'                                                   
       CALL IDMSSTAT.                                                           
!                                                                               
   GOBACK.                                                                      
!                                                                               
!0600-FIN.                                                                      
!                                                                               
!******************************************************************             
!   PREPARATION DE L'AFFICHAGE GFPN2131                           *             
!******************************************************************             
!1000-PREPARER AFFICHAGE(GFPN2131)                                              
!                                                                               
DEFINE SUBROUTINE PREPAFFI.                                                     
!                                                                               
   INIT RECORDS (GGGZC000,GGGZC001,                                             
                 GGGE000I,GFPE213A,                                             
                 GGGE000M,GGGZV001).                                            
!                                                                               
   MOVE 'GFPP466A'          TO  GZV001-NOM-PGM.                                 
   MOVE 'GFPP466A'          TO  GZC000-NOM-PGM-APELE.                           
   MOVE FZV408-GRP-DAT      TO  GE000I-DAT-AFICH.                               
   MOVE 88                  TO  FZV408-IND-TRAIT.                               
   MOVE FZV408-NO-USAGR     TO  GE000I-NO-USAGR.                                
   MOVE 01                  TO  GE000I-NO-PAGE-COUR.                            
   MOVE 01                  TO  GE000I-NO-PAGE-MAX.                             
!                                                                               
   IF  FZV408-INDIC-RECUP-ACTIF = 'O'                                           
   DO.                                                                          
       MOVE 'RECOUVREMENT'  TO  GE000I-MODE-TRAIT.                              
       MODIFY MAP PERMANENT                                                     
              FOR (GE000I-MODE-TRAIT) OUTPUT DATA IS YES.                       
   END.                                                                         
!                                                                               
   MODIFY MAP PERMANENT                                                         
          FOR (GE000I-DAT-AFICH) OUTPUT DATA IS YES.                            
!                                                                               
   MOVE FZV408-NO-CSER  TO  CSER-NO-CSER.                                       
   MOVE 'N'                TO FZV408-SEN-1.                                     
!                                                                               
   IF FZV408-INDIC-SUIV-PEROPE = ZEROES                                         
   DO.                                                                          
   CALL FIRSTPER.                                                            
      ACCEPT DB-KEY INTO FZV408-DB-KEY-PEROPE FROM CSER-PEROPE                  
                    NEXT CURRENCY.                                              
      IF  PEROPE-NO-PERIO > 01                                                  
      DO.                                                                       
         MOVE 1              TO FZV408-INDIC-SUIV-PEROPE.                       
      END.                                                                      
   END.                                                                         
   ELSE                                                                         
      DO.                                                                       
         IF FZV408-INDIC-NEXT-PEROPE = 1                                        
         DO.                                                                    
            MOVE 4          TO  GZV001-DML-SEQ.                                 
            OBTAIN PEROPE DB-KEY IS FZV408-DB-KEY-PEROPE.                       
            CALL IDMSSTAT.                                                      
            MOVE 5         TO  GZV001-DML-SEQ.                                  
            ACCEPT DB-KEY INTO FZV408-DB-KEY-PEROPE FROM CSER-PEROPE            
                          NEXT CURRENCY.                                        
            CALL IDMSSTAT.                                                      
         END.                                                                   
         ELSE                                                                   
            DO.                                                                 
               IF  FZV408-INDIC-FIRST-PEROPE = ZEROES                           
               DO.                                                              
                  CALL FIRSTPER.                                                
                  ACCEPT DB-KEY INTO FZV408-DB-KEY-PEROPE FROM                  
                                     CSER-PEROPE NEXT CURRENCY.                 
               END.                                                             
            END.                                                                
      END.                                                                      
!                                                                               
   IF  FZV408-INDIC-RECUP-ACTIF = 'O'     AND                                   
       FZV408-NOM-TX-FONC   = 'GFPP    '  AND                                   
       ( PEROPE-STA-ETAT-CTB  = 'F' OR                                          
         PEROPE-STA-ETAT-CTB  = 'C'  )                                          
   DO.                                                                          
      ADD 1           TO GZC001-NB-MESS.                                        
      MOVE 'GFPM364I' TO GZC001-GRP-NO-MESS(GZC001-NB-MESS).                    
   END.                                                                         
!                                                                               
   IF FZV408-NOM-TX-FONC   = 'GFPP    '                                         
   AND PEROPE-STA-ETAT-CTB  NE  'O'                                             
      MOVE 'GFPVP   '  TO  FZV408-NOM-TX-FONC.                                  
   ELSE                                                                         
      DO.                                                                       
      IF FZV408-NOM-TX-FONC    = 'GFPVP   '                                     
      AND PEROPE-STA-ETAT-CTB  =  'O'                                           
         MOVE 'GFPP    '  TO  FZV408-NOM-TX-FONC.                               
      END.                                                                      
                                                                                
!                                                                               
   MOVE PEROPE-NO-PERIO        TO  FE213A-NO-PERIO.                             
   MOVE PEROPE-GRP-IDENT-PERIO TO  FZV408-GRP-IDENT-PERIO.                      
   MOVE PEROPE-MNT-LOT-MANUEL-IMMAT TO                                          
                               FZV408-MNT-LOT-MANUEL-IMMAT.                     
   MOVE PEROPE-NB-TX-LOT-MANUEL-IMMAT TO                                        
                               FZV408-NB-TX-LOT-MANUEL-IMMAT.                   
   MOVE PEROPE-MNT-LOT-MANUEL-PERMI TO                                          
                               FZV408-MNT-LOT-MANUEL-PERMI.                     
   MOVE PEROPE-NB-TX-LOT-MANUEL-PERMI TO                                        
                               FZV408-NB-TX-LOT-MANUEL-PERMI.                   
   MOVE PEROPE-MNT-LOT-MANUEL-PRP   TO                                          
                               FZV408-MNT-LOT-MANUEL-PRP.                       
   MOVE PEROPE-NB-TX-LOT-MANUEL-PRP   TO                                        
                               FZV408-NB-TX-LOT-MANUEL-PRP.                     
!                                                                               
!     ACCES A EXPLICATION ECART                                                 
!                                                                               
   IF  PEROPE-COD-ECART-CSER    NE   SPACES                                     
   AND PEROPE-COD-ECART-CSER    NE   ZEROES                                     
   AND PEROPE-COD-ECART-CSER    NE   '1'                                        
   AND PEROPE-COD-ECART-CSER    NE   '2'                                        
   AND PEROPE-COD-ECART-CSER    NE   '3'                                        
   AND PEROPE-COD-ECART-CSER    NE   '4'                                        
   AND PEROPE-COD-ECART-CSER    NE   '5'                                        
   AND PEROPE-COD-ECART-CSER    NE   '6'                                        
   AND PEROPE-COD-ECART-CSER    NE   '7'                                        
   AND PEROPE-COD-ECART-CSER    NE   '8'                                        
   AND PEROPE-COD-ECART-CSER    NE   '9'                                        
   DO.                                                                          
      MOVE SPACES         TO  PEROPE-COD-ECART-CSER.                            
   END.                                                                         
!                                                                               
   IF  PEROPE-COD-ECART-CSER    NE   SPACES                                     
   DO.                                                                          
      MOVE PEROPE-COD-ECART-CSER                                                
                          TO  FZV408-COD-ECART-CSER.                            
      CALL  EXP-ECA.                                                            
   END.                                                                         
!                                                                               
!     ACCES A CSER                                                              
!                                                                               
   IF  FZV408-INDIC-SUIV-PEROPE NE ZEROES                                       
   DO.                                                                          
      MOVE FZV408-NO-CSER TO  CSER-NO-CSER.                                     
      MOVE 6              TO  GZV001-DML-SEQ.                                   
      FIND CALC CSER.                                                           
      CALL IDMSSTAT.                                                            
   END.                                                                         
!                                                                               
! ACCES A CAISSE                                                                
!                                                                               
   MOVE 7                      TO  GZV001-DML-SEQ.                              
   OBTAIN FIRST CAISS WITHIN CSER-CAISS.                                        
   CALL IDMSSTAT.                                                               
!                                                                               
   MOVE 'N'                    TO  FZV408-SEN-2.                                
!                                                                               
   WHILE ERROR-STATUS = '0000' AND  FZV408-SEN-2 = 'N'                          
   REPEAT.                                                                      
!     SI L'AGENT FICTIF CREE DANS GFPL N'EST PAS PRESENT                        
!     ONT PEUT CONTINUER LE TRAITEMENT.                                         
      IF CAISS-NO-USAGR NE CSER-NO-CSER                                         
      CALL REM-MAT.                                                             
   MOVE 8                      TO  GZV001-DML-SEQ.                              
   MOVE 'N'                    TO  FZV408-SEN-2.                                
!                                                                               
   OBTAIN NEXT CAISS  WITHIN CSER-CAISS.                                        
   IF ERROR-STATUS NE '0307'                                                    
      CALL IDMSSTAT.                                                            
   END.                                                                         
!                                                                               
! TROUVER LE NOMBRE DE PAGE.                                                    
!                                                                               
   CALL  TROU-PAG.                                                              
!                                                                               
! REMPLIR LE PANORAMA GFPN2131                                                  
!                                                                               
   CALL  REM-PANO.                                                              
!                                                                               
   GOBACK.                                                                      
!                                                                               
!1000-FIN.                                                                      
!                                                                               
!******************************************************************             
!   OBTENIR L'INFORMATION CONTENUE DANS PEROPE                    *             
!******************************************************************             
!1050-OBTENIR PEROPE                                                            
!                                                                               
DEFINE SUBROUTINE FIRSTPER.                                                     
!-------------------------                                                      
!                                                                               
! ACCES A CSER                                                                  
!                                                                               
   MOVE 9               TO  GZV001-DML-SEQ.                                     
   FIND CALC CSER.                                                              
   CALL IDMSSTAT.                                                               
!                                                                               
! ACCES A PEROPE                                                                
!                                                                               
   OBTAIN FIRST PEROPE WITHIN CSER-PEROPE.                                      
   MOVE 10                 TO  GZV001-DML-SEQ.                                  
   CALL IDMSSTAT.                                                               
!                                                                               
   IF FZV408-INDIC-RECUP-ACTIF = 'O'                                            
      WHILE FZV408-SEN-1 = 'N'                                                  
      REPEAT.                                                                   
         IF FZV408-GRP-DAT = PEROPE-DAS-DEB-PERIO                               
         DO.                                                                    
            MOVE 'O'    TO FZV408-SEN-1.                                        
         END.                                                                   
         ELSE                                                                   
            DO.                                                                 
               MOVE 11         TO  GZV001-DML-SEQ.                              
               OBTAIN NEXT PEROPE WITHIN CSER-PEROPE.                           
               IF  ERROR-STATUS = '0307'                                        
               DO.                                                              
                  MOVE 'O'     TO FZV408-SEN-1.                                 
                  ADD 1        TO   GZC001-NB-MESS.                             
                  MOVE 'GFPM340S'                                               
                               TO GZC001-GRP-NO-MESS(GZC001-NB-MESS).           
                  MOVE 99      TO FZV408-IND-TRAIT.                             
                  MODIFY MAP TEMPORARY                                          
                         CURSOR AT FIELD FE213A-INDIC-CONSL-INTRB.              
                  IF  FZV408-INDIC-PRES-TEXTE-ECART  =  'X'                     
                  DO.                                                           
                     MODIFY MAP PERMANENT FOR ALL BUT                           
                        (FE213A-INDIC-CONSL-INTRB                               
                         FE213A-INDIC-CONSL-EXPLI)                              
                         ATTRIBUTES PROTECT.                                    
                  END.                                                          
                  ELSE                                                          
                  DO.                                                           
                     MODIFY MAP PERMANENT FOR ALL BUT                           
                        (FE213A-INDIC-CONSL-INTRB)                              
                         ATTRIBUTES PROTECT.                                    
                  END.                                                          
                  CALL MESSAGE.                                                 
                  DISPLAY.                                                      
               END.                                                             
               ELSE                                                             
                  CALL IDMSSTAT.                                                
            END.                                                                
      END.                                                                      
!                                                                               
   MOVE 1    TO FZV408-INDIC-FIRST-PEROPE.                                      
!                                                                               
   GOBACK.                                                                      
!                                                                               
!1050-FIN.                                                                      
!                                                                               
!                                                                               
!******************************************************************             
!   OBTENIR  EXPLICATION ECART                                    *             
!******************************************************************             
!1060-REMPLIR-LA-MATRICE                                                        
!                                                                               
DEFINE SUBROUTINE EXP-ECA.                                                      
!-------------------------                                                      
!                                                                               
!                                                                               
! ** DEBUT MOD DEC '89 *********************************************            
!                                                                               
   IF PEROPE-COD-ECART-CSER   NE   SPACES                                       
   THEN                                                                         
      MOVE PEROPE-COD-ECART-CSER      TO                                        
                               FZV408-COD-ECART-CSER.                           
   ELSE                                                                         
      DO.                                                                       
      MOVE SPACES     TO       FZV408-COD-ECART-CSER.                           
      GOBACK.                                                                   
      END.                                                                      
!                                                                               
   IF PEROPE-COD-ECART-CSER  NE   SPACES                                        
   THEN                                                                         
      DO.                                                                       
      MOVE CSER-NO-CSER  TO    EXPECA-NO-CSER.                                  
      MOVE PEROPE-DAS-DEB-PERIO                                                 
                         TO    EXPECA-DAS-DEB-PERIO.                            
      MOVE PEROPE-NO-PERIO TO  EXPECA-NO-PERIO.                                 
      END.                                                                      
                                                                                
   OBTAIN CALC EXPECA.                                                          
!                                                                               
   IF ERROR-STATUS  =  '0326'                                                   
   THEN                                                                         
      DO.                                                                       
      MOVE SPACES     TO       FZV408-INDIC-PRES-TEXTE-ECART.                   
      GOBACK.                                                                   
      END.                                                                      
   ELSE                                                                         
      DO.                                                                       
      MOVE 'X'        TO       FZV408-INDIC-PRES-TEXTE-ECART.                   
      END.                                                                      
!                                                                               
! **** FIN MOD DEC '89 *********************************************            
!                                                                               
!                                                                               
   GOBACK.                                                                      
!                                                                               
!1060-FIN.                                                                      
!                                                                               
!                                                                               
!******************************************************************             
!   REMPLIR LA MATRICE                                            *             
!******************************************************************             
!1100-REMPLIR-LA-MATRICE                                                        
!                                                                               
DEFINE SUBROUTINE REM-MAT.                                                      
!-------------------------                                                      
!                                                                               
   MOVE 12                    TO  GZV001-DML-SEQ.                               
   OBTAIN FIRST SESCAI WITHIN CAISS-SESCAI.                                     
   CALL IDMSSTAT.                                                               
!                                                                               
   MOVE 'N'                   TO  FZV408-SEN-1.                                 
   MOVE 'N'                   TO  FZV408-SEN-2.                                 
!                                                                               
   WHILE   ERROR-STATUS = '0000'                                                
   REPEAT.                                                                      
      IF  SESCAI-GRP-IDENT-PERIO < PEROPE-GRP-IDENT-PERIO                       
      DO.                                                                       
          MOVE 'O'             TO  FZV408-SEN-2.                                
          EXIT.                                                                 
      END.                                                                      
      IF  SESCAI-GRP-IDENT-PERIO = PEROPE-GRP-IDENT-PERIO                       
      DO.                                                                       
          ADD  1               TO  FZV408-INDIC-AGENT-SESSN.                    
          ADD  1               TO  FZV408-NB-AGENT-SESSN.                       
          CALL CHA-AGEN.                                                        
      END.                                                                      
      MOVE 13                  TO  GZV001-DML-SEQ.                              
      OBTAIN NEXT SESCAI WITHIN CAISS-SESCAI.                                   
      IF ERROR-STATUS NE '0307'                                                 
         CALL IDMSSTAT.                                                         
   END.                                                                         
!                                                                               
   GOBACK.                                                                      
!                                                                               
!1100-FIN.                                                                      
!                                                                               
!******************************************************************             
!   CHARGEMENT DE LA SESSION DE L'AGENT                           *             
!******************************************************************             
!1110-CHARGEMENT-DE-GFP-AGENT-SESSN                                             
!                                                                               
DEFINE SUBROUTINE CHA-AGEN.                                                     
!--------------------------                                                     
!                                                                               
   IF  FZV408-INDIC-AGENT-SESSN > 80                                            
   DO.                                                                          
      MOVE 99                TO  GZC000-COD-RTR.                                
      MOVE 'GGG512S'         TO  GZC000-GRP-NO-MESS(1).                         
      MOVE 'GFPP466A'        TO  GZC000-NOM-PGM-APLAN.                          
      MOVE 'LE NOMBRE D''AGENT DEPASSE LE MAXIMUN REQUIS(80)'                   
                             TO  GZC000-GRP-TEXTE-MESS(1).                      
      CALL COD-RET.                                                             
   END.                                                                         
                                                                                
!                                                                               
   ADD 1  TO FZV408-INDIC-1.                                                    
   IF  FZV408-INDIC-1 > 8                                                       
      MOVE 1                 TO  FZV408-INDIC-1.                                
!                                                                               
   IF  FZV408-SEN-1 = 'N' OR  FZV408-INDIC-1 = 1                                
   DO.                                                                          
      MOVE 'O'               TO  FZV408-SEN-1.                                  
      MOVE CAISS-NO-USAGR    TO                                                 
                   FZV408-NO-AGENT-1(FZV408-INDIC-AGENT-SESSN).                 
!                                                                               
      MOVE CAISS-NO-USAGR    TO  CYCPER-NO-USAGR.                               
      OBTAIN CALC CYCPER.                                                       
!                                                                               
      IF ERROR-STATUS  =  '0326'                                                
      THEN                                                                      
         MOVE 'N'            TO  FZV408-PRES-CYCL-PERCP                         
                                        (FZV408-INDIC-AGENT-SESSN).             
      ELSE                                                                      
         IF CYCPER-DAS-TX LE GE000I-DAT-AFICH                                   
         AND CYCPER-COD-CONTR-ENCAI = '0'                                       
         THEN                                                                   
            DO.                                                                 
            MOVE  1          TO  FZV408-INDIC-PRES-CYCL-PERCP.                  
            MOVE 'O'         TO  FZV408-PRES-CYCL-PERCP                         
                                        (FZV408-INDIC-AGENT-SESSN).             
            END.                                                                
         ELSE                                                                   
            MOVE 'N'         TO  FZV408-PRES-CYCL-PERCP                         
                                        (FZV408-INDIC-AGENT-SESSN).             
   END.                                                                         
!                                                                               
!* PROCEDURE AJOUTEE POUR LIRE LE MONTANT DE CARTE DE CREDIT ET DEBIT           
!* DES DEPINT RELIES AU SESCAI                                                  
!* LA VARIABLE FZV408-INDIC-CONFR SERT A SAVOIR QUAND LES DEPINT SONT           
!* TOUS LUS. UTILISE DANS TROP DE DIALOGUES POUR AJOUTER UN AUTRE CHAMP.        
   MOVE 'O'                            TO FZV408-INDIC-CONFR.                   
!                                                                               
   OBTAIN FIRST DEPINT WITHIN SESCAI-DEPINT.                                    
   IF ERROR-STATUS EQ '0307'                                                    
   DO.                                                                          
      MOVE 'N'                         TO FZV408-INDIC-CONFR.                   
   END.                                                                         
!                                                                               
   WHILE FZV408-INDIC-CONFR         = 'O'                                       
   REPEAT.                                                                      
      IF DEPINT-TYP-DEPOT-INTRN     = 'D'                                       
      OR DEPINT-TYP-DEPOT-INTRN     = 'K'                                       
      DO.                                                                       
         ADD DEPINT-TOTAL-DEPOT-INTRN    TO FE213A-TOTAL-CRCRE-DBT.             
      END.                                                                      
      OBTAIN NEXT DEPINT WITHIN SESCAI-DEPINT.                                  
      IF ERROR-STATUS EQ '0307'                                                 
      DO.                                                                       
         MOVE 'N'                   TO FZV408-INDIC-CONFR.                      
      END.                                                                      
      ELSE CALL IDMSSTAT.                                                       
   END.                                                                         
   MOVE SPACES             TO  FZV408-INDIC-CONFR.                              
!* FIN AJOUT                                                                    
!                                                                               
!                                                                               
   MOVE SESCAI-NO-SESSN-CAISS  TO                                               
                    FZV408-NO-SESSN-CAISS(FZV408-INDIC-AGENT-SESSN).            
!                                                                               
   COMPUTE FZV408-NET-PERCU(FZV408-INDIC-AGENT-SESSN) =                         
           SESCAI-CUMUL-ENCAI + SESCAI-CUMUL-ENCAI-RECPR -                      
                                SESCAI-CUMUL-DECAI.                             
!                                                                               
   MOVE SESCAI-CUMUL-DEPOT-INTRN TO                                             
                    FZV408-CUMUL-DEPOT-INTRN(FZV408-INDIC-AGENT-SESSN).         
   ADD SESCAI-TOTAL-PMDIR-CAISS TO                                              
                 FZV408-CUMUL-DEPOT-INTRN(FZV408-INDIC-AGENT-SESSN).            
   ADD SESCAI-TOTAL-CRCRE-CAISS TO                                              
                 FZV408-CUMUL-DEPOT-INTRN(FZV408-INDIC-AGENT-SESSN).            
   ADD SESCAI-TOTAL-PMDIR-CAISS    TO FE213A-TOTAL-CRCRE-DBT.                   
   ADD SESCAI-TOTAL-CRCRE-CAISS    TO FE213A-TOTAL-CRCRE-DBT.                   
!                                                                               
   COMPUTE FZV408-TOTAL-ECART-CAISS(FZV408-INDIC-AGENT-SESSN) =                 
           FZV408-NET-PERCU(FZV408-INDIC-AGENT-SESSN) -                         
           FZV408-CUMUL-DEPOT-INTRN(FZV408-INDIC-AGENT-SESSN).                  
!                                                                               
!   LE BUT DE CE TRAITEMENT EST D'IDENTIFIER S'IL Y A DES ECARTS POUR           
! LE CENTRE DE SERVICE.                                                         
!   FAIRE LE CUMUL DES ECARTS POUR CHAQUE SESSION-CAISSE EN VALEUR              
! ABSOLUE, POUR EVITER QU'IL Y AIT DES ECARTS QUI S'ANNULENT, (+/-)             
! DIMINUANT OU ANNULANT AINSI L'ECART TOTAL DU CENTRE DE SERVICE.               
!   L'ELEMENT "FZV408-TOTAL-ECART-CSER" EST UTILISE TEMPORAIREMENT              
! POUR CE TRAITEMENT. APRES AVOIR INITIALISE L'ELEMENT                          
! "FZV408-IND-AGNDA" (SECTION "REM-PANO") IL EST REMIS A ZERO.                  
!                                                                               
   ADD ABS-VAL(FZV408-TOTAL-ECART-CAISS(FZV408-INDIC-AGENT-SESSN)) TO           
               FZV408-TOTAL-ECART-CSER.                                         
!                                                                               
   MOVE    SESCAI-CUMUL-ENCAI-RECPR   TO FZV408-TOTAL-ENCAI-RECPR               
                                        (FZV408-INDIC-AGENT-SESSN).             
   MOVE    SESCAI-MNT-TX-INTRB        TO FZV408-TOTAL-ENCAI-INTRB               
                                        (FZV408-INDIC-AGENT-SESSN).             
!                                                                               
   IF  SESCAI-HEURE-FERME-CAISS = ZEROES AND                                    
       FZV408-DAS-DEB-PERIO = FZV408-GRP-DAT                                    
   DO.                                                                          
      MOVE 'O'               TO                                                 
                    FZV408-STA-CAISS(FZV408-INDIC-AGENT-SESSN).                 
      ADD 1                  TO  FE213A-NB-CAISS-OUVERTE.                       
   END.                                                                         
   ELSE                                                                         
   DO.                                                                          
      MOVE 'F'               TO                                                 
                   FZV408-STA-CAISS(FZV408-INDIC-AGENT-SESSN).                  
   END.                                                                         
!                                                                               
! CUMULER LES OCCURENCES DU GROUPE DONT LE STA-CAISS = 'F' OU  'O'              
!                                                                               
   ADD 1                      TO  FE213A-NB-CAISS-TOTAL.                        
   ADD FZV408-NET-PERCU(FZV408-INDIC-AGENT-SESSN)                  TO           
       FE213A-GR-NET-PERCU.                                                     
   ADD FZV408-CUMUL-DEPOT-INTRN(FZV408-INDIC-AGENT-SESSN)          TO           
       FE213A-GR-CUMUL-DEPOT-INTRN.                                             
   ADD FZV408-TOTAL-ECART-CAISS(FZV408-INDIC-AGENT-SESSN)          TO           
       FE213A-GR-TOTAL-ECART-CAISS.                                             
   ADD FZV408-TOTAL-ENCAI-RECPR(FZV408-INDIC-AGENT-SESSN)          TO           
       FE213A-GR-TOTAL-ENCAI-RECPR.                                             
   ADD FZV408-TOTAL-ENCAI-INTRB(FZV408-INDIC-AGENT-SESSN)          TO           
       FE213A-GR-TOTAL-ENCAI-INTRB.                                             
!                                                                               
   GOBACK.                                                                      
!                                                                               
!1110-FIN.                                                                      
!                                                                               
!******************************************************************             
!   DETERMINER LE NOMBRE DE PAGE MAXIMUM D'AFFICHAGE              *             
!******************************************************************             
!1200-TROUVER-LE-NO-DE-PAGE-MAXIMUN                                             
!                                                                               
DEFINE SUBROUTINE TROU-PAG.                                                     
!--------------------------                                                     
!                                                                               
   IF FZV408-INDIC-AGENT-SESSN > 72                                             
      MOVE 10 TO GE000I-NO-PAGE-MAX.                                            
   ELSE                                                                         
      IF FZV408-INDIC-AGENT-SESSN > 64                                          
         MOVE 9 TO GE000I-NO-PAGE-MAX.                                          
      ELSE                                                                      
         IF FZV408-INDIC-AGENT-SESSN > 56                                       
            MOVE 8 TO GE000I-NO-PAGE-MAX.                                       
         ELSE                                                                   
            IF FZV408-INDIC-AGENT-SESSN > 48                                    
               MOVE 7 TO GE000I-NO-PAGE-MAX.                                    
            ELSE                                                                
               IF FZV408-INDIC-AGENT-SESSN > 40                                 
                  MOVE 6 TO GE000I-NO-PAGE-MAX.                                 
               ELSE                                                             
                  IF FZV408-INDIC-AGENT-SESSN > 32                              
                     MOVE 5 TO GE000I-NO-PAGE-MAX.                              
                  ELSE                                                          
                     IF FZV408-INDIC-AGENT-SESSN > 24                           
                        MOVE 4 TO GE000I-NO-PAGE-MAX.                           
                     ELSE                                                       
                        IF FZV408-INDIC-AGENT-SESSN > 16                        
                           MOVE 3 TO GE000I-NO-PAGE-MAX.                        
                        ELSE                                                    
                           IF FZV408-INDIC-AGENT-SESSN > 8                      
                              MOVE 2 TO GE000I-NO-PAGE-MAX.                     
!                                                                               
   GOBACK.                                                                      
                                                                                
!1200-FIN.                                                                      
!                                                                               
!******************************************************************             
!   DETERMINER LE CONTENU DU PANORAMA GFPN2131                    *             
!******************************************************************             
!1300-REMPLIR-LE-PANORAMA                                                       
!                                                                               
DEFINE SUBROUTINE REM-PANO.                                                     
!--------------------------                                                     
!                                                                               
   MOVE 1                 TO FZV408-INDIC-1.                                    
!                                                                               
! CHARGER LA MATRICE FE213A-GRP-AGENT-SESSN(1 A 8)                              
!                                                                               
   WHILE FZV408-INDIC-1   LE 8 AND                                              
         FZV408-INDIC-1   LE FZV408-INDIC-AGENT-SESSN                           
   REPEAT.                                                                      
      MOVE FZV408-GRP-AGENT-SESSN(FZV408-INDIC-1)  TO                           
           FE213A-GRP-AGENT-SESSN(FZV408-INDIC-1).                              
      MODIFY MAP TEMPORARY                                                      
             FOR (FE213A-NO-AGENT-1         (FZV408-INDIC-1)                    
                  FE213A-IDENT-AGENT-SESSN  (FZV408-INDIC-1)                    
                  FE213A-PRES-CYCL-PERCP    (FZV408-INDIC-1)                    
                  FE213A-NET-PERCU          (FZV408-INDIC-1)                    
                  FE213A-CUMUL-DEPOT-INTRN  (FZV408-INDIC-1)                    
                  FE213A-TOTAL-ECART-CAISS  (FZV408-INDIC-1)                    
                  FE213A-TOTAL-ENCAI-RECPR  (FZV408-INDIC-1)                    
                  FE213A-TOTAL-ENCAI-INTRB  (FZV408-INDIC-1))                   
                  OUTPUT DATA IS YES.                                           
      ADD  1  TO  FZV408-INDIC-1.                                               
   END.                                                                         
!                                                                               
   MOVE PEROPE-MNT-TOTAL-BD-PERIO    TO                                         
                             FE213A-MNT-TOTAL-BD-PERIO.                         
!                                                                               
   COMPUTE FE213A-TOTAL-BD = FE213A-GR-CUMUL-DEPOT-INTRN -                      
                             FE213A-MNT-TOTAL-BD-PERIO.                         
!                                                                               
!* NOUVEAUX CHAMPS, MAI 2007                                                    
   COMPUTE FE213A-TOTAL-BD-CRCRE-DBT = FE213A-MNT-TOTAL-BD-PERIO                
                                     + FE213A-TOTAL-CRCRE-DBT.                  
   COMPUTE FE213A-MNT-CALC           = FE213A-GR-CUMUL-DEPOT-INTRN              
                                     - FE213A-TOTAL-BD-CRCRE-DBT.               
!                                                                               
!* FORMULES MODIFIEES, MAI 2007                                                 
!  COMPUTE FE213A-TOTAL-ECART-CSER = FE213A-GR-TOTAL-ECART-CAISS +              
!                            FE213A-TOTAL-BD.                                   
   COMPUTE FE213A-TOTAL-ECART-CSER = FE213A-GR-TOTAL-ECART-CAISS                
                                   + FE213A-MNT-CALC.                           
!                                                                               
!* INITIALISATION MODIFIEE, MAI 2007                                            
!  ADD ABS-VAL(FE213A-TOTAL-BD) TO FZV408-TOTAL-ECART-CSER.                     
   ADD ABS-VAL(FE213A-MNT-CALC) TO FZV408-TOTAL-ECART-CSER.                     
!                                                                               
! AJUSTER L'ELEMENT QUI INDIQUE LA PRESENCE D'ECART POUR LE "CSER".             
!                                                                               
   IF  FZV408-TOTAL-ECART-CSER > FZV408-SEUIL-ECART-PERCP-CSER                  
      DO.                                                                       
            MOVE 1          TO FZV408-IND-AGNDA.                                
      END.                                                                      
!                                                                               
! REMISE A ZERO DE L'ELEMENT UTILISE DE FACON TEMPORAIRE.                       
!                                                                               
   MOVE ZEROES      TO  FZV408-TOTAL-ECART-CSER.                                
!                                                                               
   IF  PEROPE-HEURE-FIN-PERIO NE ZEROES  AND                                    
       PEROPE-HEURE-FIN-PERIO NE 8888 AND                                       
       PEROPE-HEURE-FIN-PERIO NE 9999                                           
   DO.                                                                          
      MOVE 'HEURE DE FERMETURE:'                                                
                             TO FE213A-LIBEL-HEURE-FIN-PERIO.                   
      MOVE PEROPE-HEURE-FIN-PERIO                                               
                             TO FE213A-HEURE-FIN-PERIO.                         
      BRIGHT (FE213A-LIBEL-HEURE-FIN-PERIO                                      
              FE213A-HEURE-FIN-PERIO).                                          
      MODIFY MAP PERMANENT                                                      
             FOR (FE213A-LIBEL-HEURE-FIN-PERIO                                  
                  FE213A-HEURE-FIN-PERIO)                                       
                  OUTPUT DATA IS YES.                                           
   END.                                                                         
   ELSE                                                                         
      DO.                                                                       
         MOVE SPACES         TO FE213A-LIBEL-HEURE-FIN-PERIO.                   
         MOVE ZEROES         TO FE213A-HEURE-FIN-PERIO.                         
         MODIFY MAP PERMANENT                                                   
                FOR (FE213A-LIBEL-HEURE-FIN-PERIO                               
                     FE213A-HEURE-FIN-PERIO)                                    
                     OUTPUT DATA IS ERASE.                                      
      END.                                                                      
!                                                                               
   MODIFY MAP  PERMANENT                                                        
          FOR (FE213A-MNT-TOTAL-BD-PERIO)                                       
               OUTPUT DATA IS YES.                                              
!                                                                               
   MODIFY MAP PERMANENT                                                         
          FOR (FE213A-NB-CAISS-TOTAL                                            
               FE213A-GR-NET-PERCU                                              
               FE213A-GR-CUMUL-DEPOT-INTRN                                      
               FE213A-GR-TOTAL-ECART-CAISS                                      
               FE213A-GR-TOTAL-ENCAI-RECPR                                      
               FE213A-GR-TOTAL-ENCAI-INTRB)                                     
          OUTPUT DATA IS YES.                                                   
!                                                                               
   IF FZV408-NOM-TX-FONC NE 'GFPVP   '                                          
   DO.                                                                          
      MOVE 'POUR FERMER(X):' TO FE213A-LIBEL-FERMER.                            
      MODIFY MAP PERMANENT                                                      
             FOR (FE213A-LIBEL-FERMER)                                          
                  OUTPUT DATA IS YES.                                           
      IF FZV408-COD-ECART-CSER NE SPACE                                         
      DO.                                                                       
         MOVE 'EXPLICATION ECART ==>CODE:' TO                                   
                           FE213A-LIBEL-EXPLI-ECART-1.                          
         MOVE FZV408-COD-ECART-CSER TO                                          
                           FE213A-COD-ECART-CSER.                               
         MODIFY MAP TEMPORARY                                                   
                FOR (FE213A-LIBEL-EXPLI-ECART-1                                 
                     FE213A-COD-ECART-CSER)                                     
                     OUTPUT DATA IS YES.                                        
      END.                                                                      
      IF FZV408-INDIC-PRES-TEXTE-ECART NE SPACE                                 
      DO.                                                                       
         MOVE 'CONSL. EXPLI. ECART(X):' TO                                      
                           FE213A-LIBEL-CONSL-EXPLI.                            
         MODIFY MAP TEMPORARY                                                   
                FOR (FE213A-LIBEL-CONSL-EXPLI)                                  
                     OUTPUT DATA IS YES.                                        
         MODIFY MAP PERMANENT                                                   
                FOR (FE213A-INDIC-CONSL-EXPLI)                                  
                     ATTRIBUTES UNPROTECTED.                                    
         MOVE 'PRESENCE TEXTE EXPLI.(X):' TO                                    
                           FE213A-LIBEL-PRES-EXPLI.                             
         MOVE 'X'  TO      FE213A-INDIC-PRES-EXPLI.                             
         MODIFY MAP TEMPORARY                                                   
                FOR (FE213A-LIBEL-PRES-EXPLI                                    
                     FE213A-INDIC-PRES-EXPLI)                                   
                     OUTPUT DATA IS YES.                                        
      END.                                                                      
      IF  PEROPE-STA-ETAT-CTB  = 'O'                                            
      DO.                                                                       
         UNPROTECT  (FE213A-COD-REPON).                                         
         MODIFY MAP PERMANENT                                                   
                CURSOR AT FIELD FE213A-COD-REPON.                               
      END.                                                                      
      ELSE                                                                      
      DO.                                                                       
         IF FZV408-INDIC-PRES-TEXTE-ECART NE SPACE                              
         DO.                                                                    
            MOVE 'CONSL. EXPLI. ECART(X):' TO                                   
                           FE213A-LIBEL-CONSL-EXPLI.                            
            MOVE 'X' TO    FE213A-INDIC-PRES-EXPLI.                             
            MODIFY MAP TEMPORARY                                                
                   FOR (FE213A-INDIC-PRES-EXPLI                                 
                        FE213A-LIBEL-CONSL-EXPLI)                               
                        OUTPUT DATA IS YES.                                     
            UNPROTECT  (FE213A-INDIC-CONSL-EXPLI).                              
         END.                                                                   
         MODIFY MAP PERMANENT                                                   
                FOR ALL BUT (FE213A-INDIC-CONSL-INTRB)                          
                             ATTRIBUTES PROTECTED.                              
         MODIFY MAP TEMPORARY                                                   
                CURSOR AT FIELD FE213A-INDIC-CONSL-INTRB.                       
      END.                                                                      
   END.                                                                         
!                                                                               
   IF FZV408-NOM-TX-FONC  =  'GFPVP   '                                         
   DO.                                                                          
      MODIFY MAP PERMANENT                                                      
             FOR (FE213A-COD-REPON)                                             
                  ATTRIBUTE PROTECTED.                                          
      IF FZV408-COD-ECART-CSER NE SPACE                                         
      DO.                                                                       
         MOVE 'EXPLICATION ECART ==>CODE:' TO                                   
                                 FE213A-LIBEL-EXPLI-ECART-1.                    
         MOVE FZV408-COD-ECART-CSER TO                                          
                                 FE213A-COD-ECART-CSER.                         
         MODIFY MAP PERMANENT                                                   
                FOR (FE213A-LIBEL-EXPLI-ECART-1                                 
                     FE213A-COD-ECART-CSER)                                     
                     OUTPUT DATA IS YES.                                        
      END.                                                                      
      IF FZV408-INDIC-PRES-TEXTE-ECART NE SPACE                                 
      DO.                                                                       
         MOVE 'CONSL. EXPLI. ECART(X):' TO                                      
                              FE213A-LIBEL-CONSL-EXPLI.                         
         MODIFY MAP PERMANENT                                                   
                FOR (FE213A-LIBEL-CONSL-EXPLI)                                  
                     OUTPUT DATA IS YES.                                        
         MODIFY MAP PERMANENT                                                   
                FOR (FE213A-INDIC-CONSL-EXPLI)                                  
                     ATTRIBUTES UNPROTECTED.                                    
!        MOVE 'PRESENCE TEXTE EXPLI.(X):' TO                                    
!                             FE213A-LIBEL-PRES-EXPLI.                          
!        MOVE 'X'         TO  FE213A-INDIC-PRES-EXPLI.                          
!        MODIFY MAP PERMANENT                                                   
!               FOR (FE213A-LIBEL-PRES-EXPLI                                    
!                    FE213A-INDIC-PRES-EXPLI)                                   
!                    OUTPUT DATA IS YES.                                        
         PROTECT    (FE213A-INDIC-PRES-EXPLI).                                  
      END.                                                                      
   END.                                                                         
!                                                                               
   GOBACK.                                                                      
!                                                                               
!1300-FIN.                                                                      
!                                                                               
!******************************************************************             
!   INITIALISER LA ZONE DE MESSAGES GGGE000M                      *             
!******************************************************************             
!2000-INITIALISER-LA-ZONE-DE-MESSAGE-GGGE000M                                   
!                                                                               
DEFINE SUBROUTINE INIT-MES.                                                     
!--------------------------                                                     
!                                                                               
   IF FZV408-INDIC-RECUP-ACTIF = 'O'      AND                                   
      FZV408-NOM-TX-FONC       = 'GFPVP   '                                     
   DO.                                                                          
      ADD 1       TO   GZC001-NB-MESS.                                          
      MOVE 'GFPM914I' TO GZC001-GRP-NO-MESS(GZC001-NB-MESS).                    
                                                                                
   END.                                                                         
!                                                                               
   IF (FZV408-INDIC-RECUP-ACTIF         = 'O'                                   
   AND FZV408-DAS-DEB-PERIO             = GST002-GRP-DAT-CALEN                  
   AND FZV408-NOM-TX-FONC               NE 'GFPVP   ')                          
   DO.                                                                          
      ADD 1                          TO GZC001-NB-MESS.                         
      MOVE 'GFPM413S'             TO GZC001-GRP-NO-MESS(GZC001-NB-MESS).        
      MOVE FZV408-DAS-DEB-PERIO      TO GZC001-TEXTE-MESS-03                    
                                       (GZC001-NB-MESS).                        
      MOVE 99                        TO FZV408-IND-TRAIT.                       
      IF  FZV408-INDIC-PRES-TEXTE-ECART  =  'X'                                 
      DO.                                                                       
          MODIFY MAP PERMANENT FOR ALL BUT                                      
                    (FE213A-INDIC-CONSL-INTRB                                   
                     FE213A-INDIC-CONSL-EXPLI)                                  
                     ATTRIBUTES PROTECT.                                        
      END.                                                                      
      ELSE                                                                      
      DO.                                                                       
         MODIFY MAP PERMANENT FOR ALL BUT                                       
                   (FE213A-INDIC-CONSL-INTRB)                                   
                    ATTRIBUTES PROTECT.                                         
      END.                                                                      
      MODIFY MAP TEMPORARY                                                      
             CURSOR AT FIELD FE213A-INDIC-CONSL-INTRB.                          
   END.                                                                         
!                                                                               
   IF  FZV408-INDIC-SUIV-PEROPE  =  1         AND                               
       FZV408-INDIC-RECUP-ACTIF  = 'O'                                          
   DO.                                                                          
      ADD 1           TO   GZC001-NB-MESS.                                      
      MOVE 'GFPM433I' TO   GZC001-GRP-NO-MESS(GZC001-NB-MESS).                  
   END.                                                                         
!                                                                               
   IF  FZV408-DAS-DEB-PERIO NE FZV408-GRP-DAT  AND                              
       FZV408-NOM-TX-FONC  NE  'GFPVP   '                                       
   DO.                                                                          
      ADD 1           TO   GZC001-NB-MESS.                                      
      MOVE 'GFPM371S' TO   GZC001-GRP-NO-MESS(GZC001-NB-MESS).                  
      MOVE 99                        TO FZV408-IND-TRAIT.                       
      IF  FZV408-INDIC-PRES-TEXTE-ECART  =  'X'                                 
      DO.                                                                       
          MODIFY MAP PERMANENT FOR ALL BUT                                      
                    (FE213A-INDIC-CONSL-INTRB                                   
                     FE213A-INDIC-CONSL-EXPLI)                                  
                     ATTRIBUTES PROTECT.                                        
      END.                                                                      
      ELSE                                                                      
      DO.                                                                       
         MODIFY MAP PERMANENT FOR ALL BUT                                       
                   (FE213A-INDIC-CONSL-INTRB)                                   
                    ATTRIBUTES PROTECT.                                         
      END.                                                                      
      MODIFY MAP TEMPORARY                                                      
             CURSOR AT FIELD FE213A-INDIC-CONSL-INTRB.                          
      MOVE FZV408-DAS-DEB-PERIO                                                 
                      TO   GZC001-TEXTE-MESS-03(GZC001-NB-MESS).                
   END.                                                                         
   ELSE                                                                         
      DO.                                                                       
         IF FZV408-NOM-TX-FONC NE 'GFPVP   '                                    
         DO.                                                                    
            CALL INITMES2.                                                      
         END.                                                                   
         ELSE                                                                   
           DO.                                                                  
              MODIFY MAP TEMPORARY                                              
                 CURSOR AT FIELD FE213A-INDIC-CONSL-INTRB.                      
           END.                                                                 
      END.                                                                      
!                                                                               
   GOBACK.                                                                      
!                                                                               
!2000-FIN.                                                                      
!                                                                               
!******************************************************************             
!   INITIALISER LA ZONE DE MESSAGES GGGE000M ( SUITE )            *             
!******************************************************************             
!2100 SUITE DE L'INITIALISATION DE LA ZONE DE MESSAGE GGGE000M                  
!                                                                               
DEFINE SUBROUTINE INITMES2.                                                     
!--------------------------                                                     
!                                                                               
   IF  PEROPE-STA-ETAT-CTB = 'F'                                                
   DO.                                                                          
      IF  FZV408-INDIC-PRES-TEXTE-ECART  =  'X'                                 
      DO.                                                                       
          MODIFY MAP PERMANENT FOR ALL BUT                                      
                    (FE213A-INDIC-CONSL-INTRB                                   
                     FE213A-INDIC-CONSL-EXPLI)                                  
                     ATTRIBUTES PROTECT.                                        
      END.                                                                      
      ELSE                                                                      
      DO.                                                                       
         MODIFY MAP PERMANENT FOR ALL BUT                                       
                   (FE213A-INDIC-CONSL-INTRB)                                   
                    ATTRIBUTES PROTECT.                                         
      END.                                                                      
      MODIFY MAP TEMPORARY                                                      
             CURSOR AT FIELD FE213A-INDIC-CONSL-INTRB.                          
   END.                                                                         
!                                                                               
   IF  FE213A-NB-CAISS-OUVERTE > ZEROES                                         
   DO.                                                                          
      ADD 1           TO GZC001-NB-MESS.                                        
      MOVE 'GFPM201S' TO GZC001-GRP-NO-MESS(GZC001-NB-MESS).                    
      MOVE 99         TO FZV408-IND-TRAIT.                                      
      IF  FZV408-INDIC-PRES-TEXTE-ECART  =  'X'                                 
      DO.                                                                       
          MODIFY MAP PERMANENT FOR ALL BUT                                      
                    (FE213A-INDIC-CONSL-INTRB                                   
                     FE213A-INDIC-CONSL-EXPLI)                                  
                     ATTRIBUTES PROTECT.                                        
      END.                                                                      
      ELSE                                                                      
      DO.                                                                       
         MODIFY MAP PERMANENT FOR ALL BUT                                       
                   (FE213A-INDIC-CONSL-INTRB)                                   
                    ATTRIBUTES PROTECT.                                         
      END.                                                                      
      MODIFY MAP TEMPORARY                                                      
             CURSOR AT FIELD FE213A-INDIC-CONSL-INTRB.                          
      MODIFY MAP TEMPORARY                                                      
             FOR FE213A-TOTAL-ECART-CSER                                        
                          OUTPUT DATA IS ERASE.                                 
   END.                                                                         
!                                                                               
! OBTENIR LE PREMIER DEPOT INTERNE RELIE AU CENTRE DE SERVICE                   
   OBTAIN FIRST DEPINT WITHIN CSER-DEPINT.                                      
                                                                                
   IF  ERROR-STATUS = '0000' AND FZV408-INDIC-RECUP-ACTIF NE 'O'                
   DO.                                                                          
      ADD 1                    TO GZC001-NB-MESS.                               
      MOVE 'GFPM303S'       TO GZC001-GRP-NO-MESS(GZC001-NB-MESS).              
      MOVE 99                  TO FZV408-IND-TRAIT.                             
      IF  FZV408-INDIC-PRES-TEXTE-ECART  =  'X'                                 
      DO.                                                                       
           MODIFY MAP PERMANENT FOR ALL BUT                                     
                 (FE213A-INDIC-CONSL-INTRB                                      
                  FE213A-INDIC-CONSL-EXPLI)                                     
                  ATTRIBUTES PROTECT.                                           
      END.                                                                      
      ELSE                                                                      
        DO.                                                                     
           MODIFY MAP PERMANENT FOR ALL BUT                                     
                 (FE213A-INDIC-CONSL-INTRB)                                     
                 ATTRIBUTES PROTECT.                                            
       END.                                                                     
      MODIFY MAP TEMPORARY                                                      
      CURSOR AT FIELD FE213A-INDIC-CONSL-INTRB.                                 
      MODIFY MAP TEMPORARY                                                      
      FOR FE213A-TOTAL-ECART-CSER                                               
             OUTPUT DATA IS ERASE.                                              
   END.                                                                         
   ELSE                                                                         
     DO.                                                                        
        IF FE213A-NB-CAISS-OUVERTE LE ZEROES                                    
           MODIFY MAP TEMPORARY                                                 
                  FOR FE213A-TOTAL-ECART-CSER                                   
                      OUTPUT DATA IS YES.                                       
   END.                                                                         
                                                                                
   IF  ERROR-STATUS NE '0307'                                                   
      DO.                                                                       
         CALL IDMSSTAT.                                                         
      END.                                                                      
                                                                                
   IF  FZV408-INDIC-RECUP-ACTIF = 'O'                                           
      MODIFY MAP TEMPORARY                                                      
             FOR FE213A-TOTAL-ECART-CSER                                        
                 OUTPUT DATA IS YES.                                            
                                                                                
   IF  FE213A-GR-TOTAL-ENCAI-RECPR NE ZEROES                                    
   DO.                                                                          
      ADD 1           TO GZC001-NB-MESS.                                        
      MOVE 'GFPM913S' TO GZC001-GRP-NO-MESS(GZC001-NB-MESS).                    
      MOVE 99         TO FZV408-IND-TRAIT.                                      
      IF  FZV408-INDIC-PRES-TEXTE-ECART  =  'X'                                 
      DO.                                                                       
          MODIFY MAP PERMANENT FOR ALL BUT                                      
                    (FE213A-INDIC-CONSL-INTRB                                   
                     FE213A-INDIC-CONSL-EXPLI)                                  
                     ATTRIBUTES PROTECT.                                        
      END.                                                                      
      ELSE                                                                      
      DO.                                                                       
         MODIFY MAP PERMANENT FOR ALL BUT                                       
                   (FE213A-INDIC-CONSL-INTRB)                                   
                    ATTRIBUTES PROTECT.                                         
      END.                                                                      
      MODIFY MAP TEMPORARY                                                      
             CURSOR AT FIELD FE213A-INDIC-CONSL-INTRB.                          
   END.                                                                         
                                                                                
! OBTENIR LE PREMIER DEPOT INTERNE RELIE AU CENTRE DE SERVICE                   
   OBTAIN FIRST DEPINT WITHIN CSER-DEPINT.                                      
                                                                                
   IF  ERROR-STATUS = '0000'                                                    
   AND FE213A-GR-NET-PERCU             GT ZEROES                                
   AND FE213A-TOTAL-BD-CRCRE-DBT       = ZEROES                                 
   DO.                                                                          
       IF GZC001-NB-MESS > 0                                                    
       DO.                                                                      
           IF GZC001-NO-MESS(GZC001-NB-MESS) NE 'GFPM303'                       
           DO.                                                                  
              ADD 1           TO GZC001-NB-MESS.                                
           END.                                                                 
       END.                                                                     
       ELSE                                                                     
       DO.                                                                      
          ADD 1               TO GZC001-NB-MESS.                                
       END.                                                                     
       MOVE 'GFPM303' TO GZC001-NO-MESS(GZC001-NB-MESS).                        
       CALL MESSAGE.                                                            
       IF GE000M-COD-SEVER (GZC001-NB-MESS)  =  'S'                             
       DO.                                                                      
          MOVE 99                  TO FZV408-IND-TRAIT.                         
          IF  FZV408-INDIC-PRES-TEXTE-ECART  =  'X'                             
          DO.                                                                   
            MODIFY MAP PERMANENT FOR ALL BUT                                    
                     (FE213A-INDIC-CONSL-INTRB                                  
                     FE213A-INDIC-CONSL-EXPLI)                                  
                     ATTRIBUTES PROTECT.                                        
          END.                                                                  
          ELSE                                                                  
          DO.                                                                   
            MODIFY MAP PERMANENT FOR ALL BUT                                    
                    (FE213A-INDIC-CONSL-INTRB)                                  
                    ATTRIBUTES PROTECT.                                         
          END.                                                                  
             MODIFY MAP TEMPORARY                                               
                    CURSOR AT FIELD FE213A-INDIC-CONSL-INTRB.                   
             DISPLAY.                                                           
       END.                                                                     
   END.                                                                         
                                                                                
   MOVE 2               TO  GZV001-DML-SEQ.                                     
   OBTAIN FIRST DEPINT WITHIN CSER-DEPINT.                                      
                                                                                
   WHILE ERROR-STATUS = ZEROES                                                  
   REPEAT.                                                                      
      IF  DEPINT-DAS-DEPOT-INTRN = PEROPE-DAS-DEB-PERIO                         
      DO.                                                                       
         IF GZC001-NB-MESS > 0                                                  
         DO.                                                                    
            IF GZC001-NO-MESS(GZC001-NB-MESS) NE 'GFPM303'                      
            DO.                                                                 
               ADD 1           TO GZC001-NB-MESS.                               
            END.                                                                
         END.                                                                   
         ELSE                                                                   
         DO.                                                                    
            ADD 1              TO GZC001-NB-MESS.                               
         END.                                                                   
         MOVE 'GFPM303S'      TO GZC001-GRP-NO-MESS(GZC001-NB-MESS).            
         CALL MESSAGE.                                                          
         IF GE000M-COD-SEVER (GZC001-NB-MESS) = 'S'                             
         DO.                                                                    
            MOVE 99              TO FZV408-IND-TRAIT.                           
            IF  FZV408-INDIC-PRES-TEXTE-ECART  =  'X'                           
            DO.                                                                 
                MODIFY MAP PERMANENT FOR ALL BUT                                
                          (FE213A-INDIC-CONSL-INTRB                             
                           FE213A-INDIC-CONSL-EXPLI)                            
                           ATTRIBUTES PROTECT.                                  
            END.                                                                
            ELSE                                                                
            DO.                                                                 
               MODIFY MAP PERMANENT FOR ALL BUT                                 
                         (FE213A-INDIC-CONSL-INTRB)                             
                          ATTRIBUTES PROTECT.                                   
            END.                                                                
            MODIFY MAP TEMPORARY                                                
                   CURSOR AT FIELD FE213A-INDIC-CONSL-INTRB.                    
            DISPLAY.                                                            
         END.                                                                   
         ELSE                                                                   
            DO.                                                                 
               EXIT.                                                            
            END.                                                                
      END.                                                                      
      ELSE                                                                      
         DO.                                                                    
            OBTAIN NEXT DEPINT WITHIN CSER-DEPINT.                              
         END.                                                                   
   END.                                                                         
                                                                                
   CALL PERPREC.                                                                
!                                                                               
   GOBACK.                                                                      
!                                                                               
!2100-FIN.                                                                      
!                                                                               
!******************************************************************             
!   OBTENIR LA PERIODE D'OPERATION PRECEDENTE                     *             
!******************************************************************             
!2150-PERIODE-PRECEDENTE.                                                       
!                                                                               
DEFINE SUBROUTINE PERPREC.                                                      
!------------------------                                                       
!                                                                               
   MOVE 'N'       TO  FZV408-SEN-1.                                             
!                                                                               
   MOVE 14              TO  GZV001-DML-SEQ.                                     
   OBTAIN PEROPE DB-KEY IS FZV408-DB-KEY-PEROPE.                                
!                                                                               
   IF ERROR-STATUS = '0307' OR                                                  
      ERROR-STATUS = '0326'                                                     
      GOBACK.                                                                   
!                                                                               
   CALL IDMSSTAT.                                                               
   WHILE  PEROPE-STA-ETAT-CTB    = 'O'    OR                                    
                                                                                
         (PEROPE-DAS-DEB-PERIO < '19890101' AND                                 
          PEROPE-STA-COHAB-GFP-CXX = ZEROES)                                    
   REPEAT.                                                                      
      MOVE 15     TO  GZV001-DML-SEQ.                                           
      MOVE 'O'    TO FZV408-SEN-1.                                              
      OBTAIN NEXT PEROPE WITHIN CSER-PEROPE.                                    
      IF ERROR-STATUS NE '0000' AND                                             
         ERROR-STATUS NE '0307' AND                                             
         ERROR-STATUS NE '0326'                                                 
         CALL IDMSSTAT.                                                         
      IF ERROR-STATUS = '0307'  OR                                              
         ERROR-STATUS = '0326'                                                  
         EXIT.                                                                  
   END.                                                                         
!                                                                               
   IF  FZV408-SEN-1 = 'O'                                                       
   DO.                                                                          
      OBTAIN PRIOR PEROPE WITHIN CSER-PEROPE.                                   
      ADD 1           TO   GZC001-NB-MESS.                                      
      MOVE 'GFPM810S' TO GZC001-GRP-NO-MESS(GZC001-NB-MESS).                    
      MOVE PEROPE-DAS-DEB-PERIO        TO GZC001-TEXTE-MESS-03                  
                                         (GZC001-NB-MESS).                      
      MOVE 99                          TO FZV408-IND-TRAIT.                     
      IF  FZV408-INDIC-PRES-TEXTE-ECART  =  'X'                                 
      DO.                                                                       
          MODIFY MAP PERMANENT FOR ALL BUT                                      
                    (FE213A-INDIC-CONSL-INTRB                                   
                     FE213A-INDIC-CONSL-EXPLI)                                  
                     ATTRIBUTES PROTECT.                                        
      END.                                                                      
      ELSE                                                                      
      DO.                                                                       
         MODIFY MAP PERMANENT FOR ALL BUT                                       
                   (FE213A-INDIC-CONSL-INTRB)                                   
                    ATTRIBUTES PROTECT.                                         
      END.                                                                      
      MODIFY MAP TEMPORARY                                                      
             CURSOR AT FIELD FE213A-INDIC-CONSL-INTRB.                          
   END.                                                                         
!                                                                               
   GOBACK.                                                                      
!                                                                               
!2150-FIN                                                                       
!                                                                               
!******************************************************************             
!   FORMATTER LES MESSAGES                                        *             
!******************************************************************             
!8000-FORMATTER LES MESSAGES                                                    
!                                                                               
DEFINE SUBROUTINE MESSAGE.                                                      
!--------------------------                                                     
!                                                                               
   MOVE 16               TO  GZV001-DML-SEQ.                                    
   LINK PROGRAM 'GUT0401D' USING (GGGZC000 GGGZC001).                           
!                                                                               
   CALL COD-RET.                                                                
!                                                                               
   MOVE 1                    TO GZC001-NB-MESS.                                 
!                                                                               
   WHILE GZC000-GRP-NO-MESS(GZC001-NB-MESS) NE SPACES                           
         AND   GZC001-NB-MESS LE 4                                              
   REPEAT.                                                                      
      MOVE GZC000-GRP-NO-MESS(GZC001-NB-MESS) TO                                
                              GE000M-GRP-NO-MESS(GZC001-NB-MESS).               
      MOVE GZC000-GRP-TEXTE-MESS(GZC001-NB-MESS) TO                             
                              GE000M-GRP-TEXTE-MESS(GZC001-NB-MESS).            
      MODIFY MAP TEMPORARY                                                      
             FOR (GE000M-GRP-NO-MESS(GZC001-NB-MESS)                            
                  GE000M-GRP-TEXTE-MESS(GZC001-NB-MESS))                        
                  OUTPUT DATA IS YES.                                           
      ADD 1  TO GZC001-NB-MESS.                                                 
   END.                                                                         
!                                                                               
   MOVE GZC000-NB-MESS       TO GZC001-NB-MESS.                                 
!                                                                               
   IF  GZC001-NB-MESS LE 4                                                      
   DO.                                                                          
      BRIGHT (GE000M-GRP-NO-MESS(GZC001-NB-MESS)                                
              GE000M-GRP-TEXTE-MESS(GZC001-NB-MESS)) TEMP.                      
   END.                                                                         
!                                                                               
   GOBACK.                                                                      
!                                                                               
!8000-FIN.                                                                      
!                                                                               
!********************************************************************           
!    SI LE CODE DE RETOUR EST ANORMAL,  APPELER LA ROUTINE GUTTABEN *           
!********************************************************************           
!8000-TRAITER-LE-RETOUR.                                                        
!                                                                               
DEFINE SUBROUTINE COD-RET.                                                      
!------------------------                                                       
!                                                                               
   IF  GZC000-COD-RTR = 99                                                      
   DO.                                                                          
      MOVE 1                     TO GZC000-NB-MESS.                             
!                                                                               
      PUT  SCRATCH AREA ID GZV002-GRP-SCR-AREA-INTER-TX                         
           FROM GGGZC000                                                        
           RECORD ID 10.                                                        
      LINK TO DIALOG 'GUTTABEN'.                                                
   END.                                                                         
!                                                                               
   MOVE 'GFPP466A'          TO  GZV001-NOM-PGM.                                 
   MOVE 'GFPP466A'          TO  GZC000-NOM-PGM-APELE.                           
!                                                                               
   GOBACK.                                                                      
!                                                                               
!9000-FIN.                                                                      
!                                                                               
!********************************************************************           
!    SI LE CODE DE RETOUR EST ANORMAL,  APPELER LA ROUTINE GUTTABEN *           
!********************************************************************           
!9500-TRAITER-LE-RETOUR-DE BANQUE.                                              
!                                                                               
DEFINE SUBROUTINE IDMSSTAT.                                                     
!-------------------------                                                      
!                                                                               
   IF  ERROR-STATUS NE '0000'                                                   
   DO.                                                                          
      MOVE 1                     TO GZC000-NB-MESS.                             
      MOVE 'GGGM511S'            TO GZC000-GRP-NO-MESS(1).                      
      MOVE 99                    TO GZC000-COD-RTR.                             
      MOVE ERROR-STATUS          TO GZV001-ERROR-STATUS.                        
      MOVE GGGZV001              TO GZC000-GRP-TEXTE-MESS(1).                   
!                                                                               
      PUT SCRATCH AREA ID GZV002-GRP-SCR-AREA-INTER-TX                          
          FROM GGGZC000                                                         
          RECORD ID 10.                                                         
!                                                                               
      LINK TO DIALOG 'GUTTABEN'.                                                
   END.                                                                         
!                                                                               
   MOVE 'GFPP466A'          TO  GZV001-NOM-PGM.                                 
   MOVE 'GFPP466A'          TO  GZC000-NOM-PGM-APELE.                           
!                                                                               
   GOBACK.                                                                      
!                                                                               
