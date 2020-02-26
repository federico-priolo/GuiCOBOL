        IDENTIFICATION DIVISION.
        PROGRAM-ID.    guicobol.
      *---------------------------------------------------------------------
      *
      * Gui builder for OPENCOBOL
      * pre-release agar    26 february  2020  0.1.2.23  fixedcolor      *
      * pre-release agar    22 february  2020  0.1.2.23      
      * pre-release agar     1 August    2019  0.1.2.18      
      * FIRST                1 SEPTEMBER 2011  0.1.0
      *
      * Copyright (C) 2010-2019 Federico Priolo 
      *
      * This program is free software; you can redistribute it and/or modify
      * it under the terms of the GNU General Public License as published by
      * the Free Software Foundation; either version 2, or (at your option)
      * any later version.
      *
      * This program is distributed in the hope that it will be useful,
      * but WITHOUT ANY WARRANTY; without even the implied warranty of
      * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
      * GNU General Public License for more details.
      *
      * You should have received a copy of the GNU General Public License
      * along with this software; see the file COPYING.  If not, write to
      * the Free Software Foundation, 51 Franklin Street, Fifth Floor
      * Boston, MA 02110-1301 USA
      *
      *
      *---------------------------------------------------------------------
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER.        PC-IBM.
        OBJECT-COMPUTER.        PC-IBM.
        SPECIAL-NAMES.

           DECIMAL-POINT IS COMMA.

        INPUT-OUTPUT SECTION.
        FILE-CONTROL.

                SELECT ARK-IN ASSIGN TO  FILE-IN
                 ORGANIZATION IS LINE SEQUENTIAL
                 FILE STATUS  IS STATUS-IN.

                SELECT ARK-DO ASSIGN TO  FILE-DO
                 ORGANIZATION IS LINE SEQUENTIAL
                 FILE STATUS  IS STATUS-DO.

                
                SELECT ARK-OUT ASSIGN TO FILE-OUT
                 ORGANIZATION IS LINE SEQUENTIAL
                 FILE STATUS  IS STATUS-OUT.

                SELECT ARK-WORK ASSIGN TO FILE-WORK
                 ORGANIZATION IS LINE SEQUENTIAL
                 FILE STATUS  IS STATUS-WORK.


        DATA DIVISION.
        FILE SECTION.

        FD ARK-WORK.
        01 REC-WORK.
           02 DATI-WORK               PIC X(80).


        FD ARK-IN.
        01 REC-IN.
           02 DATI-IN                 PIC X(256).

        FD ARK-DO.
        01 REC-DO.
           02 DATI-DO                 PIC X(500).


        

        FD ARK-OUT.
        01 REC-OUT.
           02 DATI-OUT                PIC X(256).

        WORKING-STORAGE SECTION.
        01 COLOR-REQUIRED             PIC X(50) VALUE SPACE.
        01 TAB-FUNZIONE.
           12 MIN-FUNZIONE            PIC 99.
           12 MAX-FUNZIONE            PIC 99.
           12 DUMMY                   PIC X.

        01 PARAMETRI.

         07 IDENTIFICATION-DIVISION   pic is  X(30).
         07 COUNT-LINE                PIC 9(9)   VALUE ZERO.
         07 EXIT-WITH-ERRORS          PIC X      VALUE SPACE.
         07 TAB-PARAMETRI.
           12 METTI-DOT               PIC X      VALUE SPACE.
           12 CONTA-PARAM             PIC 99     VALUE ZEROS.
           12 PARAMETRO               PIC X(128) OCCURS 50 TIMES.
           12 USAGE-PARAM             PIC X(30)  OCCURS 50 TIMES.
         07 REMEMBER-METTI-DOT        PIC X      VALUE SPACE.
         07 PARAMETRO1                PIC X(128) VALUE SPACE.
         07 PARAMETRO2                PIC X(128) VALUE SPACE.
         07 PARAMETRO3                PIC X(128) VALUE SPACE.
         07 PARAMETRO4                PIC X(128) VALUE SPACE.
         07 CONTA-LINE                PIC 9(6)   VALUE ZEROS.
         07 DONE-SOMETHING            PIC X      VALUE SPACE.
         07 TIME-SYS                  PIC 9(6)   VALUE ZEROS.
         07 FILE-SYS                  PIC X(6)   VALUE SPACE.
         07 ISTRUZIONE                PIC X(40)  VALUE SPACE.
         07 VALORE                    PIC X(512) VALUE SPACE.
         07 FINE-FILE                 PIC X      VALUE SPACE.
         07 FILE-IN                   PIC X(200) VALUE SPACE.
         07 FILE-OUT                  PIC X(200) VALUE SPACE.
         07 FILE-WORK                 PIC X(200) VALUE SPACE.
         07 FILE-DO                   PIC X(200) VALUE SPACE.
         07 FILE-COLOR                PIC X(200) VALUE SPACE.
         07 STATUS-IN                 PIC XX     VALUE SPACE.
         07 STATUS-OUT                PIC XX     VALUE SPACE.
         07 STATUS-DO                 PIC XX     VALUE SPACE.
         07 STATUS-COLOR              PIC XX     VALUE SPACE.
         07 STATUS-WORK               PIC XX     VALUE SPACE.
         07 IND                       PIC 9(9)   VALUE ZEROS.
         07 IND1                      PIC 9(9)   VALUE ZEROS.
         07 IND2                      PIC 9(9)   VALUE ZEROS.
         07 END-COBOL                 PIC 9(3)   VALUE ZEROS.
         07 START-COBOL               PIC 9(3)   VALUE ZEROS.
         07 STRINGA                   PIC X(256) VALUE SPACE.
         07 STRINGA1                  PIC X(256) VALUE SPACE.
         07 COMPARA                   PIC X(10)  VALUE SPACE.
         07 LCOMPARA                  PIC 99     VALUE ZEROS.
         07 STRINGA2                  PIC X(256) VALUE SPACE.
         07 LSTRINGA2                 PIC 99     VALUE ZEROS.
         07 CHIUSO                    PIC XX     VALUE SPACE.
         07 COMANDO                   PIC X(1024) VALUE SPACES.
         07 COPY1                     PIC X(100) VALUE SPACE.
         07 COPY2                     PIC X(100) VALUE SPACE.
         07 DO-DLL                    PIC XX     VALUE SPACE.
         07 TAB-OPTIONS.
          09 SW-OPTION                PIC X OCCURS 20 TIMES.
         78 MAXOPZ                    VALUE 20.
         07 SW-NOCOMPILA              PIC X.
         07 SW-SAVE                   PIC X.
         07 SW-DATI                   PIC X.
         07 SW-DATI-OK                PIC X.
         07 SW-HELP                   PIC X.
         07 SW-MANUAL                 PIC X.
         07 SW-CODICE                 PIC X.
         07 SW-FREE                   PIC X.
         07 SW-CONST                  PIC X.
         07 SW-STOP                   PIC X.
         07 SW-EXE                    PIC X.
         07 SW-ANALYSIS               PIC X.
         07 SW-VERBOSE                PIC X.
         07 XX                        PIC XX.
         07 REM-COLUMN                PIC 9.
         07 CAMPO1                    PIC X(30).
         07 CAMPO2.
           09 LIVELLO                 PIC 99.
             88 LIVELLO-OK            VALUE 01 THRU 77
                                            79 THRU 87.

         07 CAMPO3                    PIC X(30).
         07 CAMPO4                    PIC X(30).
         07 CAMPO5                    PIC X(30).
         07 RIC-LIVELLO               PIC 99.
         07 INSIDE-OCCURS             PIC XX.
         07 EOF-DO                    PIC X.
         07 EOF-COLOR                 PIC X.
         07 AREA-DATI                 PIC X.
         07 TYPE-SYSTEM               PIC X(10).
         07 CONTA-STACK               PIC 9(9).
         07 CONTA-RECORD              PIC 9(9).
         07 MIN-RECORD                PIC 9(9).
         07 MAX-RECORD                PIC 9(9).
         07 START-RECORD              PIC 9(9).
         07 POINT-RECORD              PIC 9(9).
         07 ESCI                      PIC XX     VALUE SPACE.
         07 PARENT                    PIC X(30)  VALUE SPACE.
         07 GLOBAL-LEVEL              PIC 99     VALUE ZEROS.
         07 LAST-LABEL                PIC X(30)  VALUE SPACE.
         07 THIS-LABEL                PIC X(30)  VALUE SPACE.
         07 CONTA-WORKING-STORAGE     PIC 99     VALUE ZERO.



        PROCEDURE DIVISION.
        INIZIO SECTION.

                PERFORM INIZIALI      THRU EX-INIZIALI.

                PERFORM LOGO          THRU EX-LOGO.

                ACCEPT FILE-IN FROM COMMAND-LINE.

                IF FILE-IN  = SPACES
                 PERFORM HELP THRU EX-HELP
                  STOP RUN
                END-IF.

                PERFORM OPZIONI  THRU EX-OPZIONI.

                PERFORM APERTURE THRU EX-APERTURE.

                PERFORM ELABORA  THRU EX-ELABORA UNTIL FINE-FILE = "S".

                PERFORM CHIUSURE THRU EX-CHIUSURE.

                GOBACK.

        LETTURA.

                IF FINE-FILE = "S" GO TO EX-LETTURA.

                MOVE SPACES            TO REC-IN REC-OUT.

                IF FINE-FILE NOT = "S"
                 READ ARK-IN NEXT AT END
                  MOVE "S" TO FINE-FILE
                 END-IF.

                IF FINE-FILE = "S" GO TO EX-LETTURA.

                ADD 1 TO COUNT-LINE.


                MOVE ZEROS TO          IND.
                INSPECT FUNCTION UPPER-CASE(REC-IN)
                 TALLYING IND FOR ALL ">>SOURCE FORMAT IS FREE"
      *
      *    before to set true the FREE FORMAT switch
      *    we must decide if it is not another ... for example
      *    if you want to debug the animator with animator itself..
      *    it could find the sentence but with other meaning...
      *
                  IF IND = 1
                  MOVE FUNCTION UPPER-CASE(REC-IN) TO STRINGA
                  MOVE SPACES         TO COMANDO
                  UNSTRING STRINGA
                   DELIMITED BY ">>SOURCE FORMAT IS FREE"
                    INTO COMANDO ISTRUZIONE
                   END-UNSTRING
                   IF ISTRUZIONE = SPACES
                   MOVE "S"             TO SW-FREE
                   MOVE 1               TO REM-COLUMN
                   MOVE REC-IN          TO REC-OUT
                   WRITE REC-OUT
                   GO TO LETTURA.

                 IF REC-IN(7:1) = "*" GO TO LETTURA.

      *
      ** manage the free format option
      *

                IF SW-FREE =  "S"
                AND REC-IN > SPACES
                 PERFORM VARYING IND FROM 1 BY 1 UNTIL IND > 100
                  OR REC-IN(IND:1) NOT = SPACES
                  CONTINUE
                  END-PERFORM

                  MOVE SPACES          TO STRINGA

                   IF REC-IN(IND:1) = "*"

                    MOVE 7              TO IND1

                    IF REC-IN((IND + 1):1) = SPACES
                    MOVE ">" TO REC-IN((IND + 1) :1)
                    END-IF

                   ELSE

                    MOVE 8              TO IND1

                   END-IF

                   MOVE REC-IN(IND: )   TO STRINGA(IND1:)
                   MOVE STRINGA         TO REC-IN

                END-IF.

                IF REC-IN = SPACES GO TO LETTURA.

                if REC-IN(1:6) NUMERIC 
                 MOVE 7        TO IND
                 ELSE
                 MOVE 1        TO IND.

                 PERFORM VARYING IND FROM IND BY  1 UNTIL IND > 100
                 OR REC-IN(IND:1) > SPACES
                  CONTINUE
                 END-PERFORM.
      *
      ****  a line with only a character is skiped (.) or other...
      *
                 IF REC-IN(IND + 1:) =  " " GO TO LETTURA.

                 MOVE REC-IN(IND:)    TO STRINGA.



        EX-LETTURA.
                EXIT.

        ELABORA SECTION.

                IF FINE-FILE NOT = "S"
                  PERFORM LETTURA      THRU EX-LETTURA
                  PERFORM TRATTA       THRU EX-TRATTA.

        EX-ELABORA.
                EXIT.

        UNSTRINGA-AGAIN.

               IF SW-VERBOSE = "S"
                display " loop again for " conta-param " OF  "
                min-funzione
                " " FUNCTION TRIM(STRINGA).

                PERFORM LETTURA        THRU EX-LETTURA.

                IF FINE-FILE = "S" GO TO EX-UNSTRINGA-AGAIN.

                PERFORM UNSTRINGA      THRU EX-UNSTRINGA.

        EX-UNSTRINGA-AGAIN.
                EXIT.

        UNSTRINGA.

                MOVE 1 TO IND.

        CICLO-UNSTRINGA.

                PERFORM VARYING IND FROM IND BY 1 UNTIL IND >  100
                OR STRINGA(IND:1) > SPACES
                CONTINUE
                END-PERFORM.

                IF IND > 100 GO TO FINE-UNSTRINGA.


                MOVE STRINGA           TO STRINGA1

                IF STRINGA(IND:1) = '"'
                 MOVE IND              TO IND2
                 ADD 1                 TO IND2
                 PERFORM VARYING IND2 FROM IND2 BY 1 UNTIL IND2 > 100
                  OR  STRINGA(IND2:1) = '"'
                   IF STRINGA1(IND2:1) = SPACE
                   MOVE "!"  TO STRINGA1(IND2:1)
                   END-IF
                 END-PERFORM
                END-IF.

                MOVE 1 TO IND1.
                ADD 1  TO CONTA-PARAM.

                PERFORM VARYING IND FROM IND BY 1 UNTIL IND > 100
                 OR STRINGA1(IND:1) = SPACES

                 IF IND1 NOT > LENGTH OF PARAMETRO(CONTA-PARAM)
                 MOVE STRINGA(IND:1)  TO PARAMETRO(CONTA-PARAM)(IND1:1)
                 ADD  1               TO IND1
                 END-IF

                END-PERFORM.

                IF FUNCTION UPPER-CASE(PARAMETRO(CONTA-PARAM)) = "SELF"
                 MOVE "agar-Form"       TO PARAMETRO(CONTA-PARAM).

                IF FUNCTION UPPER-CASE(PARAMETRO(CONTA-PARAM)) = "SELF."
                 MOVE "agar-Form."      TO PARAMETRO(CONTA-PARAM).

                IF FUNCTION UPPER-CASE(PARAMETRO(CONTA-PARAM)) = "TRUE "
                 MOVE "1"               TO PARAMETRO(CONTA-PARAM).

               IF FUNCTION UPPER-CASE(PARAMETRO(CONTA-PARAM)) = "FALSE "
                 MOVE "0"               TO PARAMETRO(CONTA-PARAM).

 
                GO TO CICLO-UNSTRINGA.

        FINE-UNSTRINGA.

                 MOVE "N"              TO METTI-DOT.

                 IF PARAMETRO(CONTA-PARAM) = "."
                  MOVE "S"             TO METTI-DOT.

                 PERFORM VARYING IND FROM
                   LENGTH OF PARAMETRO(CONTA-PARAM)
                    BY -1 UNTIL IND = ZEROS
                 OR PARAMETRO(CONTA-PARAM) (IND:1) > SPACES
                 CONTINUE
                 END-PERFORM
                 IF IND > ZEROS
                  AND PARAMETRO(CONTA-PARAM) (IND:1) = "."
                  MOVE SPACES     TO PARAMETRO(CONTA-PARAM)(IND:1)
                  MOVE "S"        TO METTI-DOT.

        EX-UNSTRINGA.
                EXIT.

        TRATTA.

                 MOVE SPACES           TO TAB-PARAMETRI.

                 MOVE ZEROS            TO IND1.
                 MOVE ZEROS            TO CONTA-PARAM.

                 PERFORM UNSTRINGA     THRU EX-UNSTRINGA.


                 EVALUATE FUNCTION UPPER-CASE(PARAMETRO(1))

      *
      *   CASE A:   IF "title" of gtk-form .......
      *

                  WHEN "IF"

                   PERFORM ADDED-LINES     THRU EX-ADDED-LINES

                   MOVE ZEROS                    TO IND1

                   INSPECT PARAMETRO(2) TALLYING IND1 FOR ALL '"'

                   IF IND1 = 2
                   AND FUNCTION UPPER-CASE(PARAMETRO(3)) = "OF"
                   PERFORM DO-IF     THRU EX-DO-IF
                   GO TO EX-TRATTA


                  WHEN "INVOKE"

                   PERFORM ADDED-LINES     THRU EX-ADDED-LINES
                   
                   PERFORM DO-INVOKE THRU EX-DO-INVOKE
                   GO TO EX-TRATTA

                  WHEN "MOVE"

                   PERFORM ADDED-LINES     THRU EX-ADDED-LINES
      *
      *   CASE A:   move "name of the window" to "title" of gtk-form.
      *
                   MOVE ZEROS TO IND1
                   INSPECT PARAMETRO(4) TALLYING IND1 FOR ALL '"'

                   IF IND1 = 2
                   AND FUNCTION UPPER-CASE(PARAMETRO(3)) = "TO"
                    PERFORM FAI-SET    THRU EX-FAI-SET
                    GO TO EX-TRATTA
                   END-IF
      *
      *
      *  CASE --- move function uppercase("gray") to "title" of gtk-form.
      *
                   MOVE ZEROS TO IND1
                   INSPECT PARAMETRO(5) TALLYING IND1 FOR ALL '"'

                   IF IND1 = 2
                   AND (FUNCTION UPPER-CASE(PARAMETRO(2)) = "FUNCTION"
                   AND  FUNCTION UPPER-CASE(PARAMETRO(4)) = "TO"
                   AND  FUNCTION UPPER-CASE(PARAMETRO(6)) = "OF" )

                   PERFORM VARYING IND1 FROM 3 BY 1
                     UNTIL IND1 > CONTA-PARAM

                   MOVE PARAMETRO(IND1)  TO PARAMETRO( IND1 - 1)

                   END-PERFORM

                   MOVE SPACES         TO PARAMETRO(CONTA-PARAM)
                   SUBTRACT 1 FROM CONTA-PARAM

                    PERFORM FAI-SET    THRU EX-FAI-SET
                    GO TO EX-TRATTA
                   END-IF

      ** a token with ---> " OF " ----> MEANS move to get a value
      *  infact you could have coded:
      * 
      *   CASE B:   move "title" of gtk-form to TITLE-OF-THE-FORM.
      *
      *
                   MOVE ZEROS                    TO IND1

                   INSPECT PARAMETRO(2) TALLYING IND1 FOR ALL '"'

                   IF IND1 = 2
                   AND FUNCTION UPPER-CASE(PARAMETRO(3)) = "OF"
                   AND FUNCTION UPPER-CASE(PARAMETRO(7)) NOT = "OF"

                   MOVE PARAMETRO(2) (2:) TO ISTRUZIONE
                   INSPECT ISTRUZIONE REPLACING ALL '"' BY " "
                   PERFORM DO-GET        THRU EX-DO-GET
                   GO TO EX-TRATTA
                   END-IF
      *
      *   CASE C:   move "title" of gtk-form to "Text" of EDIT.
      *

                   MOVE ZEROS                    TO IND1

                   INSPECT PARAMETRO(2) TALLYING IND1 FOR ALL '"'

                   IF IND1 = 2
                   AND FUNCTION UPPER-CASE(PARAMETRO(3)) = "OF"
                   AND FUNCTION UPPER-CASE(PARAMETRO(7)) = "OF"

                   MOVE PARAMETRO(6)             TO PARAMETRO1
                   MOVE PARAMETRO(7)             TO PARAMETRO2
                   MOVE PARAMETRO(8)             TO PARAMETRO3
                   MOVE PARAMETRO(9)             TO PARAMETRO4

                   MOVE "agar-text"              TO PARAMETRO(6)
                   MOVE SPACES                   TO PARAMETRO(7)
                   MOVE SPACES                   TO PARAMETRO(8)
                   MOVE SPACES                   TO PARAMETRO(9)
                   MOVE METTI-DOT                TO REMEMBER-METTI-DOT

                   MOVE PARAMETRO(2) (2:) TO ISTRUZIONE
                   INSPECT ISTRUZIONE REPLACING ALL '"' BY " "

                   PERFORM DO-GET                THRU EX-DO-GET

                   INITIALIZE TAB-PARAMETRI


                   MOVE REMEMBER-METTI-DOT       TO METTI-DOT
                   MOVE "move"                   TO PARAMETRO(1)
                   MOVE "agar-text"              TO PARAMETRO(2)
                   MOVE "to"                     TO PARAMETRO(3)
                   MOVE PARAMETRO1               TO PARAMETRO(4)
                   MOVE PARAMETRO2               TO PARAMETRO(5)
                   MOVE PARAMETRO3               TO PARAMETRO(6)
                   MOVE PARAMETRO4               TO PARAMETRO(7)
                   MOVE SPACES                   TO STRINGA

                   STRING
                          PARAMETRO(1) DELIMITED BY "      "
                          " " DELIMITED BY SIZE
                          PARAMETRO(2) DELIMITED BY "      "
                          " " DELIMITED BY SIZE
                          PARAMETRO(3) DELIMITED BY "      "
                          " " DELIMITED BY SIZE
                          PARAMETRO(4) DELIMITED BY "      "
                          " " DELIMITED BY SIZE
                          PARAMETRO(5) DELIMITED BY "      "
                          " " DELIMITED BY SIZE
                          PARAMETRO(6) DELIMITED BY "      "
                          " " DELIMITED BY SIZE
                          PARAMETRO(7) DELIMITED BY "      "
                          " " DELIMITED BY SIZE
                          INTO STRINGA

                   PERFORM FAI-SET               THRU EX-FAI-SET

                   MOVE SPACES                   TO DATI-IN STRINGA

                   GO TO EX-TRATTA
                   END-IF


                 END-EVALUATE.

                MOVE ZEROS TO IND1.
       
                move function upper-case(DATI-IN) TO DATI-OUT
                
                INSPECT DATI-OUT TALLYING IND1 FOR 
                  ALL "WORKING-STORAGE SECTION".

                IF IND1 = 1                 
                 MOVE DATI-IN         TO DATI-OUT
                 PERFORM SCRITTURA    THRU EX-SCRITTURA
                 MOVE '                 copy "global".' tO DATI-OUT
                 PERFORM SCRITTURA    THRU EX-SCRITTURA

                 ADD 1 TO CONTA-WORKING-STORAGE

                  IF CONTA-WORKING-STORAGE > 1
                   MOVE '                 copy "working".' tO DATI-IN
                  ELSE
                   MOVE SPACES           TO DATI-IN
                 END-IF

                   go to FINE-TRATTA.

                MOVE ZEROS TO IND1.

                INSPECT DATI-OUT TALLYING IND1 FOR ALL 
                '"COPY GLOBAL".'.
                IF IND1 = 1 GO TO END-TRATTA.

                INSPECT DATI-OUT TALLYING IND1 FOR ALL
                '"COPY GLOBAL.CPY".'.

                IF IND1 = 1 GO TO END-TRATTA.
                
        
        FINE-TRATTA.
        
                 MOVE ZEROS TO IND1
                 INSPECT
                  FUNCTION UPPER-CASE(DATI-IN)
                    TALLYING IND1 FOR ALL "END "
                 
                   IF IND1 = 1

                    move zeros to IND1

                   INSPECT
                    FUNCTION UPPER-CASE(DATI-IN)
                    TALLYING IND1 FOR ALL "METHOD."
                        
                    IF IND1 = 1
                    PERFORM ADDED-LINES THRU EX-ADDED-LINES

                    PERFORM FAI-ENDMETHOD THRU EX-FAI-ENDMETHOD
                    GO TO EX-TRATTA.
 
                 MOVE ZEROS TO IND1
                 INSPECT
                  FUNCTION UPPER-CASE(DATI-IN)
                    TALLYING IND1 FOR ALL "METHOD-ID."
                 
                   IF IND1 = 1
                    PERFORM ADDED-LINES THRU EX-ADDED-LINES
                    PERFORM FAI-METHOD THRU EX-FAI-METHOD
                    GO TO EX-TRATTA.
        
                 MOVE DATI-IN         TO DATI-OUT.

                 PERFORM SCRITTURA    THRU EX-SCRITTURA.

        END-TRATTA.

                 MOVE ZEROS TO IND1
                 INSPECT
                  FUNCTION UPPER-CASE(DATI-IN)
                    TALLYING IND1 FOR ALL "EXTERNAL"
                 
                   IF IND1 = 1
                   MOVE DATI-IN        TO REC-WORK
                   WRITE REC-WORK.

        EX-TRATTA.
                EXIT.


        FAI-METHOD.
        
                  MOVE SPACES    TO IDENTIFICATION-DIVISION.
 
                  UNSTRING  DATI-IN DELIMITED BY "." 
                     INTO  dummy  IDENTIFICATION-DIVISION
                     
                   DISPLAY "METHOD:" IDENTIFICATION-DIVISION.
                   
                  move spaces to DATI-OUT
                  MOVE   "        identification division."  TO REC-OUT 
                  PERFORM SCRITTURA THRU EX-SCRITTURA
                  STRING "        program-id. "
                     FUNCTION TRIM(IDENTIFICATION-DIVISION)
                       "."
                     delimited by size into DATI-OUT
                         PERFORM SCRITTURA THRU EX-SCRITTURA
                         
                   MOVE  "        environment division."  TO REC-OUT 
                         PERFORM SCRITTURA THRU EX-SCRITTURA
                           
                   MOVE  "        data  division."  TO REC-OUT 
                         PERFORM SCRITTURA THRU EX-SCRITTURA

                   MOVE  "        working-storage section."  TO REC-OUT 
                          PERFORM SCRITTURA THRU EX-SCRITTURA

                   MOVE  '             copy "global".' tO DATI-OUT
                         PERFORM SCRITTURA    THRU EX-SCRITTURA

                   MOVE  '             copy "working".' TO DATI-OUT
                   PERFORM SCRITTURA    THRU EX-SCRITTURA
                   
                *>    MOVE  "        procedure division."  TO REC-OUT 
                *>          PERFORM SCRITTURA THRU EX-SCRITTURA
                   
                 
                    MOVE SPACES TO DATI-IN.
                        
        EX-FAI-METHOD.
                EXIT.

        FAI-ENDMETHOD.
      *
      **** do not remove: leave this split into separate lines to allows the tp-cobol-debugger to run inside guicobol.cbl itself for testing...        
      *
                    STRING "       end"
                         " program " 
                      FUNCTION TRIM(IDENTIFICATION-DIVISION)
                         "."
                         delimited by size into DATI-OUT
                         PERFORM SCRITTURA THRU EX-SCRITTURA
                         MOVE SPACES TO DATI-IN.
                        
        EX-FAI-ENDMETHOD.
                EXIT.


        FAI-SET.

                   MOVE ZEROS TO IND
                   INSPECT STRINGA TALLYING IND FOR ALL '"'

                   IF IND = ZEROS GO TO EX-FAI-SET.

                   MOVE " TO "     TO COMPARA
                   MOVE 4          TO LCOMPARA
                   PERFORM VARYING IND FROM 1 BY 1 UNTIL IND >
                    ( LENGTH OF STRINGA - 4 )
                    OR FUNCTION UPPER-CASE(STRINGA(IND:LCOMPARA))
                                           = COMPARA(1:LCOMPARA)
                    CONTINUE
                   END-PERFORM.

                   IF IND  NOT > ( LENGTH OF STRINGA - 4 )
                     AND FUNCTION UPPER-CASE(STRINGA(IND:LCOMPARA))
                     = COMPARA(1:LCOMPARA)
                     MOVE IND      TO END-COBOL
                     MOVE STRINGA(1:END-COBOL) TO STRINGA2
                      ADD 3 TO IND
                      PERFORM VARYING IND FROM IND BY 1
                       UNTIL IND > LENGTH OF STRINGA
                      OR STRINGA(IND:1) > SPACES
                      CONTINUE
                      END-PERFORM
                      IF IND NOT > LENGTH OF STRINGA
                       AND STRINGA(IND:1) = '"'
                       ADD 1       TO IND
                       MOVE ZEROS  TO IND1
                       MOVE SPACES TO ISTRUZIONE
                       PERFORM VARYING IND FROM IND BY 1
                        UNTIL IND > LENGTH OF STRINGA
                       OR STRINGA(IND:1) = '"'
                       ADD  1                    TO IND1
                       MOVE STRINGA(IND:1)       TO ISTRUZIONE(IND1:1)
                       END-PERFORM
                       PERFORM DO-SET            THRU EX-DO-SET.

        EX-FAI-SET.
                  EXIT.

        SCRITTURA.

                 IF REC-OUT = SPACES GO TO EX-SCRITTURA.

                 ADD 1                 TO CONTA-LINE.

                 IF SW-FREE = "S"
                  MOVE REC-OUT(7:)     TO STRINGA
                  MOVE STRINGA         TO REC-OUT
                 ELSE
                 MOVE CONTA-LINE       TO REC-OUT(1:6).

                 WRITE REC-OUT.
                 
                 move SPACES TO REC-OUT.

        EX-SCRITTURA.
                EXIT.
 

        APERTURE SECTION.

                IF SW-VERBOSE = "S"
                Display "Processing open file..".

                MOVE ZEROS TO IND.
                INSPECT FILE-IN  TALLYING IND FOR ALL ".gui".
                IF IND = 1
                 DISPLAY "Source cannot contains .gui extension"
                 STOP RUN.

                MOVE ZEROS TO IND
                INSPECT FILE-IN TALLYING IND FOR ALL ".cbl".
                INSPECT FILE-IN TALLYING IND FOR ALL ".src".
                INSPECT FILE-IN TALLYING IND FOR ALL ".cpy".
                INSPECT FILE-IN TALLYING IND FOR ALL ".CBL".
                INSPECT FILE-IN TALLYING IND FOR ALL ".SRC".
                INSPECT FILE-IN TALLYING IND FOR ALL ".CPY".
                INSPECT FILE-IN TALLYING IND FOR ALL ".COB".
                INSPECT FILE-IN TALLYING IND FOR ALL ".cob".
      *
      * ADD THE DEFAULT .CBL ESTENSION....
      *
                IF IND = ZEROS
                PERFORM VARYING IND FROM 100 BY -1 UNTIL IND = ZEROS

                 IF FILE-IN(IND:1) NOT = SPACES
                 ADD 1 TO IND
                 MOVE ".cbl" TO FILE-IN(IND:)
                 MOVE 1      TO IND
                 END-IF
                 END-PERFORM
                END-IF.


                MOVE FILE-IN TO FILE-OUT.

                INSPECT FILE-OUT REPLACING ALL ".CBL" BY ".gui".
                INSPECT FILE-OUT REPLACING ALL ".SRC" BY ".gui".
                INSPECT FILE-OUT REPLACING ALL ".CPY" BY ".gui".
                INSPECT FILE-OUT REPLACING ALL ".cbl" BY ".gui".
                INSPECT FILE-OUT REPLACING ALL ".src" BY ".gui".
                INSPECT FILE-OUT REPLACING ALL ".cpy" BY ".gui".
                INSPECT FILE-OUT REPLACING ALL ".COB" BY ".gui".
                INSPECT FILE-OUT REPLACING ALL ".cob" BY ".gui".
                MOVE ZEROS TO IND
                INSPECT FILE-OUT TALLYING IND FOR ALL ".gui"
                 IF IND NOT = 1
                 DISPLAY "Source must contains cbl/src/cpy/cob " &
                         "(upper/lower case) extension"
                 STOP RUN.

                OPEN INPUT   ARK-IN.

                IF STATUS-IN NOT = 00
                 DISPLAY "The source supplied is not available "
                  STATUS-IN
                 " (file:" function trim(FILE-IN) " )"
                  STOP RUN
                END-IF.


                IF SW-CONST = "S"
                 MOVE "ANDATI"  TO FILE-SYS
                 ELSE
                ACCEPT TIME-SYS FROM TIME
                MOVE TIME-SYS   TO FILE-SYS
                END-IF.

                OPEN OUTPUT  ARK-OUT.

                IF STATUS-OUT NOT = 00
                 DISPLAY "Unable to create:" FILE-OUT
                 STOP RUN
                END-IF.

                MOVE "working.cpy" to FILE-WORK.
                OPEN OUTPUT  ARK-WORK.

                IF STATUS-WORK NOT = 00
                 DISPLAY "Unable to create:" FILE-WORK
                 STOP RUN
                END-IF.

                move SPACES TO REC-WORK.

                MOVE "      *" TO REC-WORK. WRITE REC-WORK.

                MOVE "      * INTERNAL WORKING STORAGE" TO REC-WORK.
                WRITE REC-WORK.

                MOVE "      *" TO REC-WORK. WRITE REC-WORK.

        EX-APERTURE.
                EXIT.


        CHIUSURE SECTION.
        CHIUSUREX.

                MOVE SPACES TO STRINGA.

                IF EXIT-WITH-ERRORS = "Y"
                MOVE   "Found errors, please check them" TO STRINGA
                ELSE
                STRING "Done...please compile the build "
                function trim(FILE-OUT)
                " source program. " DELIMITED BY SIZE INTO STRINGA.

                DISPLAY STRINGA.

                MOVE SPACES TO STRINGA.

                CLOSE ARK-IN ARK-OUT ARK-WORK. 

        EX-CHIUSURE.
                EXIT.

        READ-NEXT-DO.

                MOVE SPACES TO REC-DO EOF-DO.

                READ ARK-DO NEXT RECORD AT END
                 MOVE "S"    TO EOF-DO.


                IF EOF-DO = "S"
                 MOVE SPACES TO REC-DO
                  GO TO EX-READ-NEXT-DO.

                IF DATI-DO = SPACES GO TO READ-NEXT-DO.

                IF DATI-DO(1:1) = "*" GO TO READ-NEXT-DO.


        EX-READ-NEXT-DO.
                EXIT.

        LOGO.
                DISPLAY
                "GuiCOBOL builder for GNUCOBOL "
                "Versione 0.1.2.23 Package 22-02-2020".
                DISPLAY "CopyRight(C) 2011-2020 Federico Priolo "
                DISPLAY "                              ".
                DISPLAY "federico.priolo@tp-one.it ".
                DISPLAY "                              ".
                DISPLAY "                               ".

                IF SW-VERBOSE = "S"
                DISPLAY "Running on: " TYPE-SYSTEM.

                IF SW-VERBOSE = "S"
                DISPLAY "Processing command line..".
        EX-LOGO.
                EXIT.

        INIZIALI.

                INITIALIZE PARAMETRI.
        

                MOVE 7       TO REM-COLUMN.

                ACCEPT TYPE-SYSTEM FROM ENVIRONMENT 'OS' END-ACCEPT.

                IF TYPE-SYSTEM NOT > SPACES
                 ACCEPT TYPE-SYSTEM FROM ENVIRONMENT 'OSTYPE' END-ACCEPT
                 END-IF.


        EX-INIZIALI.
                EXIT.

        HELP.

                DISPLAY "Usage: guicobol projectfile [options]".
                DISPLAY "                              ".
                DISPLAY "File: cobol source.cbl/cpy/src (if no suplied"
                        '".cbl" is added by default....)'.
                DISPLAY "                              ".
                DISPLAY "Options:                      ".
                DISPLAY "-? This support panel         "
                DISPLAY "-F use free format            "
                DISPLAY "-v Turn on verbose            ".
 
        EX-HELP.
                EXIT.

        OPZIONI.

               ACCEPT FILE-DO FROM ENVIRONMENT "guicobol_inf"
                END-ACCEPT.

                IF FILE-DO = SPACES
                MOVE "guicobol.inf"    TO FILE-DO.


      ****************** > here we are sure that the command have something

                PERFORM VARYING IND FROM 1 BY 1 UNTIL IND > 100
                OR FILE-IN(IND:1) NOT = SPACES
                 CONTINUE
                END-PERFORM.

      *
      ****************** > if first is "-" we could have -? or -m option..
      *

                IF FILE-IN(IND:1) NOT = "-"

      ****************** > from here we can check for the option

                 PERFORM VARYING IND FROM IND BY 1 UNTIL IND > 100
                 OR FILE-IN(IND:1) = SPACES
                  CONTINUE
                 END-PERFORM

                END-IF.

                MOVE 1                 TO IND1.

                PERFORM VARYING IND FROM IND BY 1 UNTIL IND > 100
                OR IND1 > MAXOPZ

                 IF FILE-IN(IND:1) = "-"
                 MOVE SPACES TO FILE-IN(IND:1)

                 ADD 1       TO IND
                 MOVE FILE-IN(IND:1)   TO SW-OPTION(IND1)
                 ADD 1       TO IND1

                  IF IND1 > MAXOPZ
                   DISPLAY "Too many command line options"
                   STOP RUN
                  END-IF

                 PERFORM VARYING IND FROM IND BY 1 UNTIL IND > 100
                 OR FILE-IN(IND:1) = SPACES
                  MOVE SPACES TO FILE-IN(IND:1)
                 END-PERFORM

                 END-IF

                END-PERFORM.

                IF TAB-OPTIONS > SPACES
                DISPLAY "Invoked argument(S) for the builder:".

      *
      *  set the appropriate switch
      *
                PERFORM VARYING IND FROM 1 BY 1 UNTIL IND = IND1

                 EVALUATE SW-OPTION(IND)
                  WHEN "?" MOVE "S"   TO SW-HELP
                  DISPLAY "show the help panel............."
                  WHEN "F" MOVE "S"   TO SW-FREE
                  DISPLAY "use the free format reading file"
                  WHEN "v" MOVE "S"   TO SW-VERBOSE
                  DISPLAY "show verbose process            "
                  WHEN OTHER
                  DISPLAY "Bad argument in the command line:"
                    SW-OPTION(IND)
                  STOP RUN
                 END-EVALUATE

                END-PERFORM.



                IF SW-HELP = "S"
                 PERFORM HELP THRU EX-HELP
                 STOP RUN.

        EX-OPZIONI.
                EXIT.

        DO-INVOKE.

                MOVE PARAMETRO(3) (2:) TO ISTRUZIONE.

                PERFORM VARYING IND FROM 1 BY 1 UNTIL IND >
                 LENGTH OF ISTRUZIONE
                 OR ISTRUZIONE(IND:1) = '"'
                 CONTINUE
                END-PERFORM.
                IF IND NOT > 40
                 AND ISTRUZIONE(IND:1) = '"'
                  MOVE SPACES TO ISTRUZIONE(IND:)
                   PERFORM CERCA-DO    THRU EX-CERCA-DO.

        EX-DO-INVOKE.
                EXIT.

        DO-IF.

                MOVE PARAMETRO(2) (2:) TO ISTRUZIONE.

                PERFORM VARYING IND FROM 1 BY 1 UNTIL IND >
                 LENGTH OF ISTRUZIONE
                 OR ISTRUZIONE(IND:1) = '"'
                 CONTINUE
                END-PERFORM.

                IF IND NOT > 40
                 AND ISTRUZIONE(IND:1) = '"'
                  MOVE SPACES TO ISTRUZIONE(IND:).

                PERFORM IF-GET         THRU EX-IF-GET.

                PERFORM VARYING IND FROM 5 BY 1 UNTIL IND > CONTA-PARAM
                MOVE PARAMETRO(IND)    TO   REC-OUT(13:)
                PERFORM SCRITTURA       THRU EX-SCRITTURA
                END-PERFORM.

        EX-DO-IF.
                EXIT.

        DO-GET.

                MOVE SPACES            TO STRINGA
                STRING "GET-" ISTRUZIONE DELIMITED BY SIZE INTO
                 STRINGA
                 MOVE STRINGA          TO ISTRUZIONE.

                PERFORM CERCA-DO       THRU EX-CERCA-DO.

        EX-DO-GET.
                EXIT.

        IF-GET.

                MOVE SPACES            TO STRINGA
                STRING "IF-GET-" ISTRUZIONE DELIMITED BY SIZE INTO
                 STRINGA
                 MOVE STRINGA          TO ISTRUZIONE.

                PERFORM CERCA-DO       THRU EX-CERCA-DO.

        EX-IF-GET.
                EXIT.

        DO-SET.

                MOVE SPACES            TO STRINGA
                STRING "SET-" ISTRUZIONE DELIMITED BY SIZE INTO
                 STRINGA
                 MOVE STRINGA          TO ISTRUZIONE.

                PERFORM CERCA-DO       THRU EX-CERCA-DO.

        EX-DO-SET.
                EXIT.
 
        CERCA-DO.


                MOVE SPACES TO EOF-DO DATI-DO.

                INITIALIZE TAB-FUNZIONE.
      *
      * get size of the INSTRUCTION...
      *

                PERFORM VARYING IND FROM LENGTH OF ISTRUZIONE
                 BY -1 UNTIL IND = ZERO
                   OR ISTRUZIONE(IND:1) > SPACES
                   CONTINUE
                END-PERFORM.
      * look for the single scecific token... hide" ---> hideform   take this for a mistake!
      *                                        hide "     hideform   skip since not match 
                
                ADD  1 to IND.
                
      *
      * istruzione is always checkd into upper-case
      *

                MOVE FUNCTION UPPER-CASE(ISTRUZIONE) TO ISTRUZIONE.

                 IF SW-VERBOSE = "S"
                  Display "Build "
                    FUNCTION TRIM(ISTRUZIONE) " " stringa(1:60).


                OPEN INPUT ARK-DO.
      *
      * arrive to the #property/method declare section
      *

                PERFORM UNTIL EOF-DO = "S"
                 OR FUNCTION UPPER-CASE(DATI-DO(2:IND))
                                      = ISTRUZIONE(1:IND)

                 IF EOF-DO NOT = "S"
                 PERFORM READ-NEXT-DO THRU EX-READ-NEXT-DO
                 END-IF

                END-PERFORM.


                IF EOF-DO = "S"
                  PERFORM TOKEN-ERROR    THRU EX-TOKEN-ERROR.
      *
      *  here is located at #token line.. go ahead at the next valid...
      *
                 IF SW-VERBOSE = "S"
                  Display "found:" DATI-DO(2:78).

                IF EOF-DO NOT = "S"
                  AND FUNCTION UPPER-CASE(DATI-DO(2:IND))
                   = ISTRUZIONE(1:IND)
                   UNSTRING DATI-DO(2:)
                    DELIMITED BY ALL SPACES  INTO
                     USAGE-PARAM(1)
                     USAGE-PARAM(2)
                     USAGE-PARAM(3)
                     USAGE-PARAM(4)
                     USAGE-PARAM(5)
                     PERFORM VARYING IND FROM 1 BY 1 UNTIL IND > 5

                     MOVE FUNCTION UPPER-CASE(USAGE-PARAM(IND))
                                           TO USAGE-PARAM(IND)

                      MOVE ZERO TO IND1
                      INSPECT USAGE-PARAM(IND) TALLYING IND1
                       FOR ALL "MIN="
                       IF IND1 = 1
                        UNSTRING USAGE-PARAM(IND)
                        DELIMITED BY "MIN=" INTO DUMMY MIN-FUNZIONE
                       END-IF

                      MOVE ZERO TO IND1
                      INSPECT USAGE-PARAM(IND) TALLYING IND1
                       FOR ALL "MAX="
                       IF IND1 = 1
                        UNSTRING USAGE-PARAM(IND)
                        DELIMITED BY "MAX=" INTO DUMMY MAX-FUNZIONE
                      END-IF


                     END-PERFORM

                      IF MIN-FUNZIONE = ZEROS
                       MOVE MAX-FUNZIONE TO MIN-FUNZIONE
                      END-IF

                      IF MAX-FUNZIONE = ZEROS
                       MOVE MIN-FUNZIONE TO MAX-FUNZIONE
                      END-IF

                      IF MIN-FUNZIONE = ZERO
                       AND MAX-FUNZIONE = ZERO
                       MOVE 6          TO MIN-FUNZIONE MAX-FUNZIONE
                      END-IF

                      IF FUNCTION UPPER-CASE(PARAMETRO(1)) = "INVOKE"
                       AND ( CONTA-PARAM  < MIN-FUNZIONE )
                       AND MIN-FUNZIONE NOT = ZEROS

      ***** WARNING STEP..  it could become an infinite loop

                       PERFORM UNTIL CONTA-PARAM NOT < MIN-FUNZIONE

                       PERFORM UNSTRINGA-AGAIN THRU EX-UNSTRINGA-AGAIN

                       END-PERFORM

                      END-IF


                      IF FUNCTION UPPER-CASE(PARAMETRO(1)) = "INVOKE"
                       AND ( CONTA-PARAM  < MIN-FUNZIONE
                       OR CONTA-PARAM  > MAX-FUNZIONE )
                        DISPLAY "numero errato di parametri "
                         " per " USAGE-PARAM(1)
                         " " CONTA-PARAM " invece di " MIN-FUNZIONE
                                            " o " MAX-FUNZIONE
                       GO TO FINE-CERCA-DO
                      END-IF


                 PERFORM READ-NEXT-DO THRU EX-READ-NEXT-DO

      *
      *  and then manage the token by adding it inside the cobol source
      *  take care it runs processing something otherwise an error is
      *  occurred to we use the DONE-SOMETHING flag
      *

                 MOVE "N"    TO DONE-SOMETHING

                 PERFORM UNTIL EOF-DO = "S"

                 IF DATI-DO(1:1) = "#"
                  MOVE "S" TO EOF-DO
                  IF DONE-SOMETHING = "N"
                   PERFORM TOKEN-ERROR    THRU EX-TOKEN-ERROR
                  END-IF

                 END-IF

                 IF EOF-DO NOT = "S"
                  PERFORM MANAGE-TEMPLATE THRU EX-MANAGE-TEMPLATE
                 END-IF

                 IF EOF-DO NOT = "S"
                  PERFORM READ-NEXT-DO THRU EX-READ-NEXT-DO
                 END-IF

                END-PERFORM.

        FINE-CERCA-DO.

                CLOSE ARK-DO.

        EX-CERCA-DO.
               EXIT.
         
        ADDED-LINES.
        
                 move rec-in           to REC-OUT.
                 move "*"              to rec-out(7:1).
                 write rec-out.
                 move spaces           to REC-OUT.
                 
        EX-ADDED-LINES.
               EXIT.

        TOKEN-ERROR.

                MOVE "Y"               TO EXIT-WITH-ERRORS.

                DISPLAY 
                " Severe Error: Missing property/method into " &
                   "guicobol.inf:"
                 ISTRUZIONE(1:IND)
                  " at " COUNT-LINE " line ".


        EX-TOKEN-ERROR.
               EXIT.

        MANAGE-TEMPLATE.


               IF DATI-DO = SPACES GO TO EX-MANAGE-TEMPLATE.

               MOVE "Y"   TO DONE-SOMETHING.

               MOVE ZEROS TO IND1.

               INSPECT DATI-DO TALLYING IND1 FOR ALL "$PARAM(1)".

               IF IND1 = 1
                MOVE PARAMETRO(1)                TO REC-OUT(13:)
               PERFORM SCRITTURA                 THRU EX-SCRITTURA
               GO TO EX-MANAGE-TEMPLATE.

               MOVE ZEROS TO IND1.

               INSPECT DATI-DO TALLYING IND1 FOR ALL "$PARAM(2)".

               IF IND1 = 1
                MOVE PARAMETRO(2)                TO REC-OUT(13:)
               PERFORM SCRITTURA                 THRU EX-SCRITTURA
               GO TO EX-MANAGE-TEMPLATE.

               MOVE ZEROS TO IND1.

               INSPECT DATI-DO TALLYING IND1 FOR ALL "$PARAM(3)".

               IF IND1 = 1
                MOVE PARAMETRO(3)                TO REC-OUT(13:)
               PERFORM SCRITTURA                 THRU EX-SCRITTURA
               GO TO EX-MANAGE-TEMPLATE.

               MOVE ZEROS TO IND1.

               INSPECT DATI-DO TALLYING IND1 FOR ALL "$PARAM(4)".

               IF IND1 = 1
                MOVE PARAMETRO(4)                TO REC-OUT(13:)
               PERFORM SCRITTURA                 THRU EX-SCRITTURA
               GO TO EX-MANAGE-TEMPLATE.

               MOVE ZEROS TO IND1.

               INSPECT DATI-DO TALLYING IND1 FOR ALL "$PARAM(5)".

               IF IND1 = 1
                MOVE PARAMETRO(5)                TO REC-OUT(13:)
               PERFORM SCRITTURA                 THRU EX-SCRITTURA
               GO TO EX-MANAGE-TEMPLATE.

               MOVE ZEROS TO IND1.

               INSPECT DATI-DO TALLYING IND1 FOR ALL "$PARAM(6)".

               IF IND1 = 1
                MOVE PARAMETRO(6)                TO REC-OUT(13:)
               PERFORM SCRITTURA                 THRU EX-SCRITTURA
               GO TO EX-MANAGE-TEMPLATE.

               MOVE ZEROS TO IND1.


               INSPECT DATI-DO TALLYING IND1 FOR ALL "$PARAM(7)".

               IF IND1 = 1
                MOVE PARAMETRO(7)                TO REC-OUT(13:)
               PERFORM SCRITTURA                 THRU EX-SCRITTURA
               GO TO EX-MANAGE-TEMPLATE.

               MOVE ZEROS TO IND1.

               INSPECT DATI-DO TALLYING IND1 FOR ALL "$PARAM(8)".

               IF IND1 = 1
                MOVE PARAMETRO(8)                TO REC-OUT(13:)
               PERFORM SCRITTURA                 THRU EX-SCRITTURA
               GO TO EX-MANAGE-TEMPLATE.

               MOVE ZEROS TO IND1.

               INSPECT DATI-DO TALLYING IND1 FOR ALL "$PARAM(9)".

               IF IND1 = 1
                MOVE PARAMETRO(9)                TO REC-OUT(13:)
               PERFORM SCRITTURA                 THRU EX-SCRITTURA
               GO TO EX-MANAGE-TEMPLATE.

               MOVE ZEROS TO IND1.

               INSPECT DATI-DO TALLYING IND1 FOR ALL "$PARAM(10)".

               IF IND1 = 1
                MOVE PARAMETRO(10)               TO REC-OUT(13:)
               PERFORM SCRITTURA                 THRU EX-SCRITTURA
               GO TO EX-MANAGE-TEMPLATE.

               MOVE ZEROS TO IND1.

               INSPECT DATI-DO TALLYING IND1 FOR ALL "$PARAM(11)".

               IF IND1 = 1
                MOVE PARAMETRO(11)               TO REC-OUT(13:)
               PERFORM SCRITTURA                 THRU EX-SCRITTURA
               GO TO EX-MANAGE-TEMPLATE.

               MOVE ZEROS TO IND1.

               INSPECT DATI-DO TALLYING IND1 FOR ALL "$PARAM(12)".

               IF IND1 = 1
                MOVE PARAMETRO(12)               TO REC-OUT(13:)
               PERFORM SCRITTURA                 THRU EX-SCRITTURA
               GO TO EX-MANAGE-TEMPLATE.

               MOVE ZEROS TO IND1.

               INSPECT DATI-DO TALLYING IND1 FOR ALL "$PARAM(13)".

               IF IND1 = 1
                MOVE PARAMETRO(13)               TO REC-OUT(13:)
               PERFORM SCRITTURA                 THRU EX-SCRITTURA
               GO TO EX-MANAGE-TEMPLATE.


               MOVE ZEROS TO IND1.

               INSPECT DATI-DO TALLYING IND1 FOR ALL "$COLOR(2)".

               IF IND1 = 1
                MOVE PARAMETRO(2)                TO REC-OUT(20:)
                PERFORM SCRITTURA                THRU EX-SCRITTURA
                GO TO EX-MANAGE-TEMPLATE.


      *
      *  replace instruction (put a dot yes or no (METTI-DOT=S for yes)
      *

               MOVE ZEROS TO IND1.

               INSPECT DATI-DO TALLYING IND1 FOR ALL "$DOT".

               IF IND1 = 1

                IF  METTI-DOT = "S"
                  MOVE "."             TO REC-OUT(20:1)
                ELSE
                  MOVE SPACES          TO REC-OUT
                END-IF

               PERFORM SCRITTURA       THRU EX-SCRITTURA
               GO TO EX-MANAGE-TEMPLATE.


      *
      * LAST replace instruction
      *

               MOVE ZEROS TO IND1.

               INSPECT DATI-DO TALLYING IND1 FOR ALL "$STATEMENT".

               IF IND1 = 1
                MOVE STRINGA2                    TO REC-OUT(13:)
               ELSE

      *
      * default move instruction
      *

               MOVE DATI-DO                      TO REC-OUT.

               PERFORM SCRITTURA                 THRU EX-SCRITTURA.
        EX-MANAGE-TEMPLATE.
               EXIT.

        end program guicobol.


