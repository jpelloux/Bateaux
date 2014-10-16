--===========================================================================--
-- ENTÊTE DU PROGRAMME
--===========================================================================--
--
--                    THÈME : Location de bateaux
--
--===========================================================================--

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Sequential_IO;

procedure bateaux is

   ----------------------------------------------------------------------------
   ------------------------LEXIQUE PRINCIPAL-----------------------------------
   -- Definitions de constantes
   ----------------------------
   NBATMAX : constant Integer := 20; -- nombre maximum de bateaux
   H_OUVR  : constant Integer := 8;  -- heure d'ouverture : 8 h 00
   H_FERM  : constant Integer := 19; -- heure de fermeture : 19 h 00

   ----------------------------------------
   -- Définitions de types et de sous-types
   ----------------------------------------
   subtype NATUREL is Integer range 0 .. Integer'Last;

   type HEURE is record
      NH : Integer range H_OUVR .. H_FERM;
      NM : Integer range 0 .. 59;
   end record;

   type BATEAU is record
      NBAT : Integer range 0 .. NBATMAX;
      H    : HEURE;
   end record;

   type DUREE is record
      NBH : NATUREL;
      NBM : Integer range 0 .. 59;
   end record;

   type LOCATION is record
      NBAT : Integer range 0 .. NBATMAX;
      Hd   : HEURE;
      Hr   : HEURE;
   end record;
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   --           MACHINE BATEAUX ET LOCATIONS
   ----------------------------------------------------------------------------
   package FICH_BATEAUX is new Sequential_IO (BATEAU);
   use FICH_BATEAUX;

   subtype RUBAN_DE_BATEAUX is FICH_BATEAUX.File_Type;

   R1, R2 : RUBAN_DE_BATEAUX;

   F1 : constant String := "fich_departs";
   F2 : constant String := "fich_retours";

   BAT_COUR1, BAT_COUR2 : BATEAU;

   MARQUE : constant BATEAU := (0, (H_OUVR, 0));

   package FICH_LOCATIONS is new Sequential_IO (LOCATION);
   use FICH_LOCATIONS;

   subtype RUBAN_DE_LOCATIONS is FICH_LOCATIONS.File_Type;

   RL : RUBAN_DE_LOCATIONS;

   FL : constant String := "fich_location";

   LOC_COUR : LOCATION;

   LMARQUE : constant LOCATION := (0, (H_OUVR, 0), (H_FERM, 0));
   ----------------------------------------------------------------------------
   procedure DEMARRER1 is
   begin
      Open (R1, In_File, Name => F1);
      Read (R1, BAT_COUR1);
   end DEMARRER1;
   ----------------------------------------------------------------------------
   procedure DEMARRER2 is
   begin
      Open (R2, In_File, Name => F2);
      Read (R2, BAT_COUR2);
   end DEMARRER2;
   ----------------------------------------------------------------------------
   procedure DEMARRERL is
   begin
      Open (RL, In_File, Name => FL);
      Read (RL, LOC_COUR);
   end DEMARRERL;
   ----------------------------------------------------------------------------
   procedure AVANCER1 is
   begin
      Read (R1, BAT_COUR1);
   end AVANCER1;
   ----------------------------------------------------------------------------
   procedure AVANCER2 is
   begin
      Read (R2, BAT_COUR2);
   end AVANCER2;
   ----------------------------------------------------------------------------
   procedure AVANCERL is
   begin
      Read (RL, LOC_COUR);
   end AVANCERL;
   ----------------------------------------------------------------------------
   procedure FERMER1 is
   begin
      Close (R1);
   end FERMER1;
   ----------------------------------------------------------------------------
   procedure FERMER2 is
   begin
      Close (R2);
   end FERMER2;
   ----------------------------------------------------------------------------
   procedure FERMERL is
   begin
      Close (RL);
   end FERMERL;
   ----------------------------------------------------------------------------
   procedure AMORCER (R : in out RUBAN_DE_BATEAUX; F : in String) is
   begin
      Create (R, Name => F);
   end AMORCER;
   ----------------------------------------------------------------------------
   procedure AMORCERL (R : in out RUBAN_DE_LOCATIONS; F : in String) is
   begin
      Create (R, Name => F);
   end AMORCERL;
   ----------------------------------------------------------------------------
   procedure ENREGISTRER (R : in RUBAN_DE_BATEAUX; BAT_COUR : in BATEAU) is
   begin
      Write (R, BAT_COUR);
   end ENREGISTRER;
   ----------------------------------------------------------------------------
   procedure ENREGISTRERL (R : in RUBAN_DE_LOCATIONS; LOC : in LOCATION) is
   begin
      Write (R, LOC);
   end ENREGISTRERL;
   ----------------------------------------------------------------------------
   procedure MARQUER (R : in out RUBAN_DE_BATEAUX) is
   begin
      Write (R, MARQUE);
      Close (R);
   end MARQUER;
   ----------------------------------------------------------------------------
   procedure MARQUERL (R : in out RUBAN_DE_LOCATIONS) is
   begin
      Write (R, LMARQUE);
      Close (R);
   end MARQUERL;
   ----------------------------------------------------------------------------
   --         FIN DE LA DESCRIPTION DE LA MACHINE BATEAUX ET LOCATIONS
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure SAISIE (R : in out RUBAN_DE_BATEAUX; F : in String) is
      --{ei: La machine bateaux a été déclarée. R est soit R1 soit R2,
      --     F le nom du fichier où vont être enregistrés les bateaux}
      RE1 : Integer;
      BA  : BATEAU;
   begin
      AMORCER (R, F);
      Put_Line ("La marque est (0,(8,0))");
      loop
         Put_Line ("Entrer un bateau (numero , (heure , minute))");
         loop
            Put ("Numéro : ");
            Get (RE1);
            exit when RE1 >= 0 and RE1 <= NBATMAX;
         end loop;
         BA.NBAT := RE1;
         if BA.NBAT /= 0 then
            loop
               Put ("Heure : ");
               Get (RE1);
               exit when RE1 >= H_OUVR and RE1 <= H_FERM;
            end loop;
            BA.H.NH := RE1;
            if BA.H.NH = H_FERM then
               BA.H.NM := 0;
               Put_Line ("Minutes : 0");
            else
               loop
                  Put ("Minutes : ");
                  Get (RE1);
                  exit when RE1 >= 0 and RE1 <= 59;
               end loop;
               BA.H.NM := RE1;
            end if;
         end if;
         exit when BA.NBAT = 0;
         ENREGISTRER (R, BA);
      end loop;
      MARQUER (R);
      -- *** {ef :  on a composé un ruban de bateaux}
   end SAISIE;
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   procedure SAISIE_RUBANS is
      RE1 : Integer;
      RE2 : Integer range 1 .. 3;
   begin
      Put_Line ("-----------------");
      Put_Line ("Saisie des rubans");
      Put_Line ("_________________");
      loop
      -- *** {MENU SAISIE} ***
         Put_Line ("");
         Put_Line ("");
         Put_Line ("     -- Menu Saisie --");
         Put_Line ("");
         Put_Line ("1 : fichier des départs");
         Put_Line ("2 : fichier des retours");
         Put_Line ("3 : sortie de Menu Saisie");
         -- *** {test de validité du choix} ***
         loop
            Put ("Votre choix : ");
            Get (RE1);
            exit when (RE1 = 1 or RE1 = 2 or RE1 = 3);
         end loop;
         RE2 := RE1;
         case RE2 is
            when 1 =>
               SAISIE (R1, F1);
            when 2 =>
               SAISIE (R2, F2);
            when 3 =>
               null;
         end case;
         exit when RE2 = 3;
      end loop;
   end SAISIE_RUBANS;
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure AFFICHE_BATEAUX is
   --{ei : les rubans de bateaux sont disponibles }
   begin
      Put_Line ("------------------------------------");
      Put_Line ("Affichage des Départs et des Retours");
      Put_Line ("____________________________________");
      Put_Line ("les Départs");
      DEMARRER1;
      while BAT_COUR1 /= MARQUE loop
         Put ("<");
         Put (BAT_COUR1.NBAT, 0);
         Put (",");
         Put ("<");
         Put (BAT_COUR1.H.NH, 0);
         Put (",");
         Put (BAT_COUR1.H.NM, 0);
         Put (">>");
         New_Line;
         AVANCER1;
      end loop;

      Put_Line ("les Retours");
      DEMARRER2;
      while BAT_COUR2 /= MARQUE loop
         Put ("<");
         Put (BAT_COUR2.NBAT, 0);
         Put (",");
         Put ("<");
         Put (BAT_COUR2.H.NH, 0);
         Put (",");
         Put (BAT_COUR2.H.NM, 0);
         Put (">>");
         New_Line;
         AVANCER2;
      end loop;
      FERMER1;
      FERMER2;
      --{ ef : on a affiché la séquence des départs et celle des retours }
   end AFFICHE_BATEAUX;
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   --------           DESCRIPTION DES PROCÉDURES ASSOCIÉES
   --------           AUX AUTRES FONCTIONNALITÉS DEMANDÉES
   ----------------------------------------------------------------------------
   --
   --
   -- À RÉDIGER
   --
   procedure Autocomplete is  --Autocomplete liste  pour test


      BA  : BATEAU;
   begin
      AMORCER (R1, F1); --Depart
         BA.NBAT := 1; --Bat1
         BA.H.NH := 8;
         BA.H.NM := 00;
         ENREGISTRER (R1, BA);
         BA.NBAT := 2; --Bat2
         BA.H.NH := 8;
         BA.H.NM := 00;
         ENREGISTRER (R1, BA);
         BA.NBAT := 3; --Bat3
         BA.H.NH := 8;
         BA.H.NM := 00;
         ENREGISTRER (R1, BA);
         BA.NBAT := 4; --Bat4
         BA.H.NH := 8;
         BA.H.NM := 00;
         ENREGISTRER (R1, BA);
         BA.NBAT := 5; --Bat5
         BA.H.NH := 8;
         BA.H.NM := 00;
         ENREGISTRER (R1, BA);
      MARQUER (R1);
      --------------------------------
      AMORCER (R2, F2); --Retour
         BA.NBAT := 1; --Bat1
         BA.H.NH := 8;
         BA.H.NM := 30;
         ENREGISTRER (R2, BA);
         BA.NBAT := 2; --Bat2
         BA.H.NH := 9;
         BA.H.NM := 30;
         ENREGISTRER (R2, BA);
         BA.NBAT := 3; --Bat3
         BA.H.NH := 10;
         BA.H.NM := 00;
         ENREGISTRER (R2, BA);
         BA.NBAT := 4; --Bat4
         BA.H.NH := 14;
         BA.H.NM := 30;
         ENREGISTRER (R2, BA);
         BA.NBAT := 5; --Bat5
         BA.H.NH := 18;
         BA.H.NM := 30;
         ENREGISTRER (R2, BA);
      MARQUER (R2);

   end Autocomplete;

   procedure INVERSE_ORDRE_DANS_RUBAN (Position: in integer;K: integer ) is --Position : position du bateau dans le ruban, K : 1= Depart :2=Arrive 3:Location

      BaTamponInf: BATEAU;
      BaTamponSup: BATEAU;

      procedure Ouvrir_Ruban_K (K: in integer) is
      begin
          if K=1 then --Ouvrir ruban
         DEMARRER1;
      elsif K=2 then
         DEMARRER2;
      else
         DEMARRERL;
      end if;
      end Ouvrir_Ruban_K;

      procedure Choix_Avancer (K : in integer )is
      begin
         if K=1 then
            AVANCER1;
         elsif K=2 then
            AVANCER2;
         else
            AVANCERL;
         end if;
      end Choix_Avancer;

      procedure Fermer_Ruban_K (K: in integer) is
      begin
         if K=1 then -- Fermer ruban
            FERMER1;
         elsif k=2 then
            FERMER2;
         else
         FERMERL;
          end if;
      end Fermer_Ruban_K;

      procedure Mise_En_Position (Position : in integer; K : in integer)is
      i : integer :=0; --compteur
      begin
         Fermer_Ruban_K(K);
         Ouvrir_Ruban_K(K);
         i:=0;
         While i /= Position loop --On se place sur la i-�me donn�e
            Choix_Avancer(K);
            i := i+1;
         end loop;
      end Mise_En_Position;

   begin
      Ouvrir_Ruban_K(K); --Ouvrir ruban

      Mise_En_Position(Position,K); --On se place sur la i-�me donn�e

      if K=1 then --INVERSION
         BaTamponInf.NBAT := BAT_COUR1.NBAT; --Mise en tampon du premier champ
         BaTamponInf.H.NH := BAT_COUR1.H.NH;
         BaTamponInf.H.NM := BAT_COUR1.H.NM;
         Choix_Avancer(K);
         BaTamponSup.NBAT := BAT_COUR1.NBAT; --Mise en tampon du 2eme champ
         BaTamponSup.H.NH := BAT_COUR1.H.NH;
         BaTamponSup.H.NM := BAT_COUR1.H.NM;

         ENREGISTRER (R1,BaTamponInf);--2eme champ devient 1er champ
         Mise_En_Position(Position,K);
         ENREGISTRER (R1,BaTamponSup);--1eme champ devient 2er champ

      elsif K=2 then
         BaTamponInf.NBAT := BAT_COUR2.NBAT; --Mise en tampon du premier champ
         BaTamponInf.H.NH := BAT_COUR2.H.NH;
         BaTamponInf.H.NM := BAT_COUR2.H.NM;
         Choix_Avancer(K);
         BaTamponSup.NBAT := BAT_COUR2.NBAT; --Mise en tampon du 2eme champ
         BaTamponSup.H.NH := BAT_COUR2.H.NH;
         BaTamponSup.H.NM := BAT_COUR2.H.NM;

         ENREGISTRER (R2,BaTamponInf);--2eme champ devient 1er champ
         Mise_En_Position(Position,K);
         ENREGISTRER (R2,BaTamponSup);--1eme champ devient 2er champ
      else
         Null;
          ------A MODIFIER
--           TamponNbatInf := LOC_COUR.NBAT; --Mise en tampon du premier champ
--           TamponHInf.NH := LOC_COUR.H.NH;
--           TamponHInf.NM := LOC_COUR.h.NM;
--           Choix_Avancer(K);
--           TamponNbatSup := LOC_COUR.NBAT; --Mise en tampon du 2eme champ
--           TamponHSup.NH := LOC_COUR.H.NH;
--           TamponHSup.NM := LOC_COUR.h.NM;
--
--           LOC_COUR.NBAT := TamponNbatInf;--2eme champ devient 1er champ
--           LOC_COUR.H.NH := TamponHInf.NH;
--           LOC_COUR.H.NM := TamponHInf.NM
--           Mise_En_Position(Position,K);
--           LOC_COUR.NBAT := TamponNbatSup;--1eme champ devient 2er champ
--           LOC_COUR.H.NH := TamponHSup.NH;
--           LOC_COUR.H.NM := TamponHSup.NM
      end if;

      Fermer_Ruban_K(K); -- Fermer ruban
   end INVERSE_ORDRE_DANS_RUBAN;
--     procedure TRI_RUBAN_PAR_HEURE (R: in out RUBAN_DE_BATEAUX) is
--        H:HEURE;
--     begin
--
--        if  then
--           null;
--        else
--        end if;
--     end TRI_RUBAN_PAR_HEURE;

   function TIME_PLUS (D: DUREE ;H: HEURE ) return DUREE is

         SomM: Integer range 0..118; -- Somme des minutes
         Res: DUREE;
      begin
         SomM := D.NBM + H.NM;
         if SomM <=59
           then
     		Res := (D.NBH+H.NH,SomM);
               --Res.NBH := D.NBH+H.NH;
               --Res.NBM := SomM;
            else
               Res := (1+D.NBH+H.NH,SomM-60);
               --Res.NBH := 1+D.NBH+H.NH;
               --Res.NBM := SomM-60;
         end if;
         return Res;
   end TIME_PLUS;

   function TIME_MOINS (D1,D2: DUREE) return DUREE is
         DiffM: Integer range -59..59; -- Somme des minutes
         Res: DUREE;
      begin
         DiffM := D1.NBM - D2.NBM;
         if DiffM >= 0
            then
               Res:= (D1.NBH-D2.NBH,DiffM);
               --Res.NBH := D1.NBH-D2.NBH;
               --Res.NBM := DiffM;
            else
               Res:= (D1.NBH-D2.NBH-1,DiffM+60);
               --Res.NBH := D1.NBH-D2.NBH-1;
               --Res.NBM := DiffM+60;
         end if;
         return Res;
     end TIME_MOINS;

   procedure CALCUL_OCCUP is
      SomHd: DUREE; --Resultat,Somme des heures de départs
      SomHr: DUREE; --Resultat,Somme des heures de retours

   begin
      DEMARRER1;DEMARRER2;
      SomHd := (0,0);
      SomHr := (0,0);

      while BAT_COUR1 /= MARQUE loop
         SomHd := TIME_PLUS(SomHd,BAT_COUR1.H);
         SomHr := TIME_PLUS(SomHr,BAT_COUR2.H);
         AVANCER1;
         AVANCER2;
      end loop;
      put("Le temps total d'occupation des bateaux est : ");
      put(TIME_MOINS(SomHr,SomHd).NBH);
      put(",");
      put(TIME_MOINS(SomHr,SomHd).NBM);
      FERMER1;FERMER2;
   end CALCUL_OCCUP;

   procedure AFFICH_NB_LOC_INF_2H is
     Res : integer:=0;
   begin
      DEMARRER1;DEMARRER2;
      While BAT_COUR1 /= MARQUE loop
         if BAT_COUR2.H.NH-BAT_COUR1.H.NH < 2 then
            Res:=Res+1;
         else
            Null;
         end if;
         AVANCER1;
         AVANCER2;
      end loop;
      put_line("Nb de loc strict inf a 2h : ");
      put(Res);
      FERMER1;FERMER2;
   end AFFICH_NB_LOC_INF_2H;

--     procedure CREATION_RUBAN_LOCATIONS is
--
--     begin
--
--     end CREATION_RUBAN_LOCATIONS;
--

   ----------------------------------------------------------------------------
   ------------------------- ALGORITHME PRINCIPAL
   -------------------------
   -- Informations Globales
   ---------------------------
   REP1 : Integer;
   REP2 : Integer range 1 .. 8;
   ----------------------------------------------------------------------------
begin

   loop

   -- *** {MENU PRINCIPAL} ***

      Put_Line ("");
      Put_Line ("");
      Put_Line ("                     -----  M E N U -----");
      Put_Line ("");

      Put_Line ("1 : Saisie des heures de départs ou d'arrivée des bateaux.");

      Put_Line
        ("2 : Affichage du ruban des départs puis de celui des retours.");

      Put_Line ("3 : Temps total de location des bateaux.");

      Put_Line ("4 : Nombre de locations inférieures à deux heures.");

      --- Put_Line ("5 : Création du ruban des locations.");
      Put_Line ("5 : Autocomplete");

      --Put_Line ("6 : Affichage du ruban des locations.");
      Put_Line ("6 : Teste!.");

      Put_Line ("7 : Temps maximum d'attente.");

      Put_Line ("8 : Quitter le programme.");

      -- *** {test de validité du choix} ***

      loop
         Put ("  Votre choix : ");
         Get (REP1);
         exit when REP1 >= 1 and REP1 <= 8;
      end loop;

      REP2 := REP1;

      case REP2 is

         when 1 =>
            SAISIE_RUBANS;

         when 2 =>
            AFFICHE_BATEAUX;

         when 3 =>
            CALCUL_OCCUP; -- à remplacer par l'appel de la procédure associée
                  -- à cette opération

         when 4 =>
            AFFICH_NB_LOC_INF_2H; -- à remplacer

         when 5 =>
            Autocomplete; -- à remplacer

         when 6 =>
            INVERSE_ORDRE_DANS_RUBAN(3,2); -- à remplacer

         when 7 =>
            null; -- à remplacer

         when 8 =>
            null; -- on quitte le programme

      end case;

      exit when REP2 = 8;

   end loop;

end bateaux;

------------------------------------------------------------------------------
--                       FIN DU TP1   BATEAUX                               --
----------ermer_Ruban_K--------------------------------------------------------------------Ouvrir_Ruban_K
