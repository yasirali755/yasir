                IDENTIFICATION DIVISION.
       PROGRAM-ID.             CBLYAL04.
       AUTHOR.                 yasir ali.
       DATE-WRITTEN.           18/1/2018.
       DATE-COMPILED.

      *****************************************************************
      * THIS PROGRAM WILL Calculate the amaount of pop sales sold for *
      * a local fundraiser all records must be validated. Records in  *
      * error are to be dumped in an error report and provessing      *
      * bypassed       good job                                                 *
      *****************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT POPSL-FILE
                 ASSIGN TO "C:\Users\Owner\Desktop\cobol\CBLPOPSL.DAT"
                 ORGANIZATION IS LINE SEQUENTIAL.
           SELECT popsl-changes

           SELECT PRTSLOUT
                 ASSIGN TO "C:\Users\Owner\Desktop\cobol\COBPOPERR.PRT"
                 ORGANIZATION IS  RECORD SEQUENTIAL.

           SELECT PRTERROUT
                 ASSIGN TO "C:\Users\Owner\Desktop\cobol\COBPOPERR.PRT"
                 ORGANIZATION IS RECORD SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  POPSL-FILE
             LABEL RECORD IS STANDARD
             RECORD CONTAINS 71 CHARACTERS
             DATA RECORD IS I-REC.


       01  I-REC.
             05  I-LNAME                 PIC X(15).
             05  I-FNAME                 PIC X(15).
             05  I-ADDRESS               PIC X(15).
             05  I-CITY                  PIC X(10).
             05  I-STATE                 PIC XX.
                   88 VAL-STATE
                      VALUE "IA","IL","MI","MO","NE","WI".
             05  I-ZIP.
                 10 I-ZIP-CODE-1         PIC 9(5).
                 10 I-ZIP-CODE-2         PIC 9(4).
                 88 VAL-CODE   VALUE 0 THRU 999999999.
             05  I-POP-TYPE              PIC 99.
                 88 VAL-CASES         VALUE     1 thru 6.
             05  I-NUM-CASES             PIC 99.
             05  I-TEAM                  PIC X.
                 88 VAL-TEAMS            VALUE  'A' thru 'E'.

       FD  PRTSLOUT
             LABEL RECORD IS OMITTED
             RECORD CONTAINS 132 CHARACTERS
             DATA RECORD IS PRTSLLINE
             LINAGE IS 60 WITH FOOTING AT 56.

       01  PRTSLLINE                 PIC X(132).

       FD  PRTERROUT
             LABEL RECORD IS OMITTED
             RECORD CONTAINS 132 CHARACTERS
             DATA RECORD IS PRTERRLINE
             LINAGE IS 60 WITH FOOTING AT 56.

       01  PRTERRLINE                 PIC X(132).

       WORKING-STORAGE SECTION.
        01 CURRENT-DATE-AND-TIME.
           05 I-DATE.
               10 I-YEAR               PIC 9(4).
               10 I-MONTH              PIC 99.
               10 I-DAY                PIC 99.
           05 I-TIME                   PIC X(11).

       01  WORK-AREA.
           05  CPCTR                   PIC 99             VALUE ZERO.
           05  CPCTR-ERRO              PIC 99             VALUE ZERO.
           05  SCTR                    PIC 999            VALUE ZERO.
           05  ERRO-SW                   PIC X(3).
            05 C-DEPOSIT               PIC 9(6)V99        VALUE ZERO.
           05 C-TOTAL-SAL              PIC 9(6)V99        VALUE ZERO.
           05 c-ctr-coke               PIC 9(6)V99        VALUE ZERO.
           05 c-ctr-diet-coke          PIC 9(6)V99        VALUE ZERO.
           05 c-ctr-mello-yello        PIC 9(7)V99        VALUE ZERO.
           05 c-ctr-cherry-coke        PIC 9(7)V99        VALUE ZERO.
           05 c-ctr-diet-cherry        PIC 9(7)V99        VALUE ZERO.
           05 c-ctr-sprite             PIC 99             VALUE ZERO.
           05 C-TOTAL            PIC 9(7)V99        VALUE ZERO.
           05 c-team-total-A             PIC 9(7)V99        VALUE ZERO.
           05 c-team-total-B             PIC 9(7)V99        VALUE ZERO.
           05 c-team-total-C             PIC 9(7)V99        VALUE ZERO.
           05 c-team-total-D             PIC 9(7)V99        VALUE ZERO.
           05 c-team-total-E             PIC 9(7)V99        VALUE ZERO.
           05 TOTAL-ERROR               PIC 9(3)           VALUE ZERO.
           05 C-ERROR-CTR               PIC 9(2)           VALUE ZERO.
           05 GT-AMOUNT-DUE               PIC 999999         VALUE ZERO.
           05 MORE-RECS                   PIC XXX           VALUE 'YES'.


       01  TITLE-LINE.

               05  FILLER                     PIC X(6)            VALUE
                                                           "DATE: ".
             05  O-MONTH              PIC 99.
             05  FILLER               PIC X               VALUE "/".
             05  O-DAY                PIC 99.
             05  FILLER               PIC X               VALUE "/".
             05  O-YEAR               PIC 9(4).
             05  FILLER               PIC X(36)           VALUE SPACES.
             05  FILLER               PIC X(28)           VALUE
                                   'ALBIA SOCCER CLUB FUNDRAISER'.
             05  FILLER               PIC X(44)           VALUE SPACES.
             05  FILLER               PIC X(6)            VALUE
                                                           'PAGE: '.
             05  O-PCTR               PIC Z9.

       01 TITLE-LINE1.
           05 FILLER                  PIC X(60)           VALUE SPACES.
           05 FILLER                  PIC X(12)           VALUE
                                                   'SALE REPORT'.
           05 FILLER                  PIC X(60)           VALUE SPACES.

       01 TITLE-LINE2.
           05 FILLER                  PIC X(8)            VALUE
                                       'COBYAL05'.
           05 FILLER                  PIC X(48)           VALUE SPACES.
           05 FILLER                  PIC X(19)           VALUE
                                                  "YASIR''S DIVISION".
           05 FILLER                  PIC X(57)           VALUE SPACES.



        01 ERRIR-LINE.
           05 FILLER                  PIC X(6)            VALUE
                                                       'DATE:'.
           05 O-MMM                   PIC 99.
           05 FILLER                  PIC X               VALUE '/'.
           05 O-DDD                   PIC 99.
           05 FILLER                  PIC X               VALUE '/'.
           05 O-YY                    PIC 9(4).
           05 FILLER                  PIC X(36)           VALUE SPACES.
           05 FILLER                  PIC X(28)           VALUE
                                        "ALBIA SOCCER CLUB FUNDRAISER" .
           05 FILLER                  PIC X(44)           VALUE SPACES.
           05 FILLER                  PIC X(6)            VALUE 'PAGE:'.
           05 OPCTRR                  PIC Z9.

        01 ERRIR-LINE1.
           05 FILLER                  PIC X(60)           VALUE SPACES.
           05 FILLER                  PIC X(13)           VALUE
                                       "ERROR REPORT ".
           05 FILLER                  PIC X(59)           VALUE SPACES.

        01 ERRIR-LINE2.
           05 FILLER                  PIC X(8)            VALUE
                                           'COBYAL05'.
           05 FILLER                  PIC X(48)           VALUE SPACES.
           05 FILLER                  PIC X(19)           VALUE
                                       "yasir  DIVISION" .
           05 FILLER                  PIC X(57)           VALUE SPACES.


       01  HEADING-LINE.

       05 FILLER                      PIC X(3)            VALUE SPACES.
           05 FILLER                  PIC X(9)            VALUE
                                           'LAST NAME'.
           05 FILLER                  PIC X(8)            VALUE SPACES.
           05 FILLER                  PIC X(10)           VALUE
                                       'FIRST NAME'.
           05 FILLER                  PIC X(7)            VALUE SPACES.
           05 FILLER                  PIC X(4)            VALUE 'CITY'.
           05 FILLER                  PIC X(8)            VALUE SPACES.
           05 FILLER                  PIC X(5)            VALUE
                                                               'STATE'.
           05 FILLER                  PIC X               VALUE SPACES.
           05 FILLER                  PIC X(8)            VALUE
                                                            'ZIP CODE'.
           05 FILLER                  PIC X(4)            VALUE SPACES.
           05 FILLER                  PIC X(8)            VALUE
                                                            'POP TYPE'.
           05 FILLER                  PIC X(13)           VALUE SPACES.
           05 FILLER                  PIC X(8)            VALUE
                                                            'QUANTITY'.
           05 FILLER                  PIC X(6)            VALUE SPACES.
           05 FILLER                  PIC X(11)           VALUE
                                                         'DEPOSIT AMT'.
           05 FILLER                  PIC X(6)            VALUE SPACES.
           05 FILLER                  PIC X(11)           VALUE
                                                         'TOTAL SALES'.
           05 FILLER                  PIC X(2)            VALUE SPACES.


       01  HEADING-ERRO-LINE.

       05 FILLER                      PIC X(12)           VALUE
                                                        'ERROR RECORD'.
           05 FILLER                  PIC X(60)           VALUE SPACES.
           05 FILLER                  PIC X(17)           VALUE
                                                   'ERROR DESCRIPTION'.
           05 FILLER                  PIC X(43)           VALUE SPACES.

       01 DETAIL-LINE.
           05 FILLER            PIC X(3)            VALUE  SPACES.
           05 O-LNAME           PIC X(15).
           05 FILLER            PIC X(2)            VALUE  SPACES.
           05 O-FNAME           PIC X(15).
           05 FILLER            PIC X(2)            VALUE SPACES.
           05 O-CITY            PIC X(10).
           05 FILLER            PIC X(3)            VALUE  SPACES.
           05 O-STATE           PIC XX.
           05 FILLER            PIC X(3)            VALUE SPACES.
           05 O-ZIP-CODE        PIC 9(5).
           05 FILLER            PIC X               VALUE '-'.
           05 O-ZIP-CODE1       PIC 9(4).
           05 FILLER            PIC X(2)            VALUE SPACES.
           05 O-POP-TYPE         PIC X(16).
           05 FILLER            PIC X(8)            VALUE SPACES.
           05 O-QTY             PIC Z9.
           05 FILLER            PIC X(11)            VALUE SPACES.
           05 O-DEPOSIT         PIC $$$$.99.
           05 FILLER            PIC X(9)            VALUE SPACES.
           05 O-TOTAL-SAL       PIC $$,$$$.99.
           05 FILLER            PIC X(3)            VALUE  SPACES.

       01  DETAIL-ERR-LINE.
           05  O-ERR-field      PIC X(71).
           05 FILLER            PIC X            VALUE SPACES.
           05 POP               PIC X(60).



       01  TEAM-TOTAL.
           05 FILLER                  PIC X(10)           VALUE
                                                         'TEAM TOTAL'.
           05 FILLER                  PIC X(122)          VALUE SPACES.





        01 TEAM-A .
           05 FILLER                  PIC X(3)            VALUE SPACES.
           05 FILLER                  PIC X               VALUE'A'.
           05 O-team-total-A          PIC $$$$,$$$,$$$.99.
           05 FILLER                  PIC X(112)          VALUE SPACES.
         01 TEAM-B.
           05 FILLER                  PIC X(3)            VALUE SPACES.
           05 FILLER                  PIC X               VALUE 'B'.
           05 O-team-total-B          PIC $$$$,$$$,$$$.99.
           05 FILLER                  PIC X(112)          VALUE SPACES.
        01 TEAM-C.
           05 FILLER                  PIC X(3)            VALUE SPACES.
           05 FILLER                  PIC X               VALUE 'C'.
           05 O-team-total-C          PIC $$$$,$$$,$$$.99.
           05 FILLER                  PIC X(112)          VALUE SPACES.

         01 TEAM-D.
           05 FILLER                  PIC X(3)           VALUE SPACES.
           05 FILLER                  PIC X              VALUE 'D'.
           05 O-team-total-D          PIC $$$$,$$$,$$$.99.
           05 FILLER                  PIC X(112)         VALUE SPACES.

        01 TEAM-E.
           05 FILLER                  PIC X(3)           VALUE SPACES.
           05 FILLER                  PIC X              VALUE 'E'.
           05 O-team-total-E          PIC $$$$,$$$,$$$.99.
           05 FILLER                  PIC X(112)         VALUE SPACES.


         01 TOTAL-ERRORS.
           05 FILLER                  PIC X(3)           VALUE SPACES.
           05 FILLER                  PIC X(13)          VALUE
                                                       'TOTAL ERRORS '.
           05 O-GT-ERR-CTR            PIC Z,ZZ9.
           05 FILLER                  PIC X(111)          VALUE SPACES.

         01 O-GTOTALS.
             05 FILLER               PIC X(13)    VALUE 'GRAND TOTALS:'.
             05 FILLER               PIC X(119)   VALUE SPACES.


       01 GRAND-POPTYPE.
           05 FILLER                  PIC X(3)            VALUE SPACES.
           05 FILLER                  PIC X(16)           VALUE 'COKE'.
           05 FILLER                  PIC X               VALUE SPACES.
           05 O-ctr-coke              PIC ZZZ,ZZ9         VALUE SPACES.
           05 FILLER                  PIC X(6)            VALUE SPACES.
           05 FILLER                  PIC X(16)           VALUE
                                                           'DIET COKE'.
           05 FILLER                  PIC X               VALUE SPACES.
           05 O-ctr-diet-coke         PIC ZZZ,ZZ9         VALUE SPACES.
           05 FILLER                  PIC X(6)            VALUE SPACES.
           05 FILLER                  PIC X(16)           VALUE
                                                         'MELLO YELLO'.
           05 FILLER                  PIC X               VALUE SPACES.
           05 O-ctr-mello-yello       PIC ZZZ,ZZ9
                                                          VALUE SPACES.
           05 FILLER                  PIC X(28)           VALUE SPACES.




       01 GRAND-POPTYPE1.
           05 FILLER            PIC X(3)            VALUE SPACES.
           05 FILLER            PIC X(16)           VALUE 'CHERRY COKE'.
           05 FILLER            PIC X               VALUE SPACES.
           05 O-ctr-cherry-coke   PIC ZZZ,ZZ9         VALUE SPACES.
           05 FILLER            PIC X(6)            VALUE SPACES.
           05 FILLER            PIC X(16)           VALUE "DIET CHERRY C
      -                                                  "OKE".
           05 FILLER            PIC X               VALUE SPACES.
           05 O-ctr-diet-cherry       PIC ZZZ,ZZ9   VALUE
                                               SPACES.
           05 FILLER            PIC X(6)            VALUE SPACES.
           05 FILLER            PIC X(16)           VALUE 'SPRITE'.
            05 FILLER           PIC X               VALUE SPACES.
           05 O-ctr-sprite      PIC ZZZ,ZZ9         VALUE SPACES.
           05 FILLER            PIC X(28)           VALUE SPACES.



        PROCEDURE DIVISION.
       0000-COBYAL05.
           PERFORM 1000-INIT.
           PERFORM 2000-MAINLINE
               UNTIL MORE-RECS ='NO'.
                   PERFORM 3000-CLOSING.
           STOP RUN.

       1000-INIT.
           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-AND-TIME.
           MOVE I-MONTH        TO O-MONTH.
           MOVE I-DAY          TO O-DAY.
           MOVE I-YEAR         TO O-YEAR.
           MOVE I-MONTH        TO O-MMM.
           MOVE I-DAY          TO O-DDD.
           MOVE I-YEAR         TO O-YY.
           OPEN INPUT POPSL-FILE.
           OPEN OUTPUT PRTSLOUT.
           OPEN OUTPUT PRTERROUT.
           PERFORM 9000-READ.
           PERFORM 9100-HEADING1.
           PERFORM 9200-HEADING.



       2000-MAINLINE.
           PERFORM 2100-VAL thru 2100-EXIT
           IF ERRO-SW = 'YES'
               PERFORM 2200-ERRORS
           ELSE
               PERFORM 2300-CALC
               PERFORM 2400-MOVES.
           PERFORM 9000-READ.


        2100-VAL.
           MOVE 'YES' TO ERRO-SW.
           IF I-LNAME=SPACES
               MOVE 'LAST NAME REGUIRED' TO POP
               GO TO 2100-EXIT.
           IF I-FNAME = SPACES
               MOVE 'FIRST  NAME REGUIRED' TO POP
               GO TO 2100-EXIT.
           IF I-ADDRESS= SPACES
               MOVE 'ADDRESS REGUIRED' TO POP
               GO TO 2100-EXIT.
           IF I-CITY = SPACES
               MOVE 'CITY REGUIRED' TO POP
               GO TO 2100-EXIT.
           IF NOT VAL-STATE
               MOVE 'STATE MUST BE"IA", "IL", "MI", "MO", "NE", "WI"' TO
                                       POP
               GO TO 2100-EXIT.
           IF I-ZIP NOT NUMERIC
               MOVE 'ZIP MUST BE DIGIT ' TO POP
               GO TO 2100-EXIT.
           IF NOT VAL-CODE
               MOVE 'ZIP MUST BE 0 TO 999999999' TO POP
               GO TO 2100-EXIT.
           IF I-NUM-CASES   NOT NUMERIC
               MOVE 'ICASES MUST BE NUMERIC' TO POP
               GO TO 2100-EXIT.
           IF NOT VAL-CASES
               MOVE 'CASES MUST BE 01 TO 06' TO POP
               GO TO 2100-EXIT.
           IF I-TEAM NOT ALPHABETIC
               MOVE 'TEAM MUST BE ALPHABET' TO POP
               GO TO 2100-EXIT.
           IF NOT VAL-TEAMS
               MOVE 'TEAM MUST BE ALPHABETIC' TO POP
               GO TO 2100-EXIT.
           MOVE 'NO' TO ERRO-SW.
           2100-EXIT.
           EXIT.


       2200-ERRORS.
           ADD 1 TO C-ERROR-CTR.
           move I-REC         TO O-ERR-field.
           WRITE PRTERRLINE
              FROM DETAIL-ERR-LINE
                 AFTER ADVANCING 2 LINES
                      AT EOP
                          PERFORM 9200-HEADING.


       2200-ERROR.
           ADD 1 TO C-ERROR-CTR.
           move I-REC         TO O-ERR-field.
           WRITE PRTERRLINE
              FROM DETAIL-ERR-LINE
                 AFTER ADVANCING 2 LINES
                      AT EOP
                          PERFORM 9200-HEADING.


        2400-MOVES.
           move I-LNAME                TO O-LNAME .
           MOVE I-FNAME                TO O-FNAME.
           MOVE I-CITY                TO O-CITY.
           MOVE I-STATE                TO O-STATE .
           MOVE I-ZIP-CODE-1        TO O-ZIP-CODE.
           MOVE I-ZIP-CODE-2        TO O-ZIP-CODE1.
           MOVE I-NUM-CASES         TO O-QTY.
           MOVE C-DEPOSIT           TO O-DEPOSIT.
           MOVE C-TOTAL       TO O-TOTAL-SAL .
           WRITE PRTSLLINE
              FROM DETAIL-LINE
                 AFTER ADVANCING 2 LINES
                      AT EOP
                          PERFORM 9100-HEADING1.



       2300-CALC.
           MOVE 0 TO C-DEPOSIT.
           ADD 1 TO SCTR.
           EVALUATE I-STATE
               WHEN 'IA'
                   MOVE "IOWA" TO O-STATE
                   COMPUTE C-DEPOSIT=0.05*I-NUM-CASES*24
               WHEN 'IL'
                   MOVE "ILLINOIS" TO O-STATE
                   MOVE 0 TO O-DEPOSIT

               WHEN 'MI'
                   MOVE "MICHIGAN" TO O-STATE
                   COMPUTE C-DEPOSIT=0.10*I-NUM-CASES*24
               WHEN 'MO'
                   MOVE "MISSOURI" TO O-STATE
                   MOVE 0 TO O-DEPOSIT
                WHEN 'NE'
                   MOVE "NEBREASKA" TO O-STATE
                   COMPUTE C-DEPOSIT=0.05*I-NUM-CASES*24
               WHEN 'WI'
                   MOVE "WICONSIN" TO O-STATE
                   COMPUTE C-DEPOSIT=0.05*I-NUM-CASES*24
           END-EVALUATE.


       EVALUATE I-POP-TYPE
               WHEN 1
                   MOVE "COKE" TO O-POP-TYPE
                   COMPUTE c-ctr-coke =c-ctr-coke +I-NUM-CASES
               WHEN 2
                   MOVE "DIET COKE" TO O-POP-TYPE
                   COMPUTE c-ctr-diet-coke = c-ctr-diet-coke +
                       I-NUM-CASES
               WHEN 03
                   MOVE "MELLO YELLO" TO O-POP-TYPE
                   COMPUTE c-ctr-mello-yello =
                           c-ctr-mello-yello +I-NUM-CASES
               WHEN 04
                   MOVE "CHERRY COKE" TO O-POP-TYPE
                   COMPUTE c-ctr-cherry-coke =
                                   c-ctr-cherry-coke +I-NUM-CASES
               WHEN 05
                   MOVE "DIET CHERRY COKE" TO O-POP-TYPE
                   COMPUTE c-ctr-diet-cherry =
                               c-ctr-diet-cherry   +I-NUM-CASES
               WHEN 06
                   MOVE "SPRITE" TO O-POP-TYPE
                   COMPUTE c-ctr-sprite  =
                           c-ctr-sprite  + I-NUM-CASES
           END-EVALUATE.
           COMPUTE C-TOTAL=18.71*I-NUM-CASES+ C-DEPOSIT.
           EVALUATE I-TEAM
               WHEN 'A'

                   COMPUTE c-team-total-A =
                           c-team-total-A +C-TOTAL
               WHEN 'B'

                   COMPUTE c-team-total-B =
                           c-team-total-B +C-TOTAL
               WHEN 'C'

                   COMPUTE c-team-total-C =
                           c-team-total-C +C-TOTAL
               WHEN 'D'

                   COMPUTE c-team-total-D =
                           c-team-total-D +C-TOTAL
               WHEN 'E'

                   COMPUTE c-team-total-E =
                           c-team-total-E +C-TOTAL
           END-EVALUATE.



       3000-CLOSING.
           MOVE c-ctr-coke          TO O-ctr-coke.
           MOVE c-ctr-diet-coke     TO O-ctr-diet-coke.
           MOVE c-ctr-mello-yello   TO O-CTR-MELLO-YELLO.
           MOVE c-ctr-cherry-coke   TO O-CTR-CHERRY-COKE.
           MOVE c-ctr-diet-cherry   TO O-ctr-diet-cherry .
           MOVE C-ctr-sprite        TO O-ctr-sprite .
           MOVE C-TEAM-TOTAL-A      TO O-TEAM-TOTAL-A.
           MOVE C-TEAM-TOTAL-B      TO O-TEAM-TOTAL-B.
           MOVE C-TEAM-TOTAL-C      TO O-TEAM-TOTAL-C.
           MOVE C-TEAM-TOTAL-D      TO O-TEAM-TOTAL-D.
           MOVE C-TEAM-TOTAL-E      TO O-TEAM-TOTAL-E.
           MOVE C-ERROR-CTR   TO O-GT-ERR-CTR.
           ADD 1 TO CPCTR.
           MOVE CPCTR TO O-PCTR .
           WRITE PRTSLLINE
             FROM TITLE-LINE
                AFTER ADVANCING PAGE.
           WRITE PRTSLLINE
              FROM TITLE-LINE2
                 AFTER ADVANCING 1 LINES
                    AT EOP
                       PERFORM 9100-HEADING1.
           WRITE PRTSLLINE
              FROM TITLE-LINE1
                 AFTER ADVANCING 1 LINES
                    AT EOP
                        PERFORM 9100-HEADING1.

           WRITE PRTSLLINE
              FROM HEADING-LINE
                 AFTER ADVANCING 2 LINES
                    AT EOP
                       PERFORM 9100-HEADING1.

            write PRTSLLINE
               from O-GTOTALS
                 after advancing 3 lines
                  AT EOP
                       PERFORM 9100-HEADING1.


           WRITE PRTSLLINE
              FROM GRAND-POPTYPE
                 AFTER ADVANCING 3 LINES
                    AT EOP
                        PERFORM 9100-HEADING1.

           WRITE PRTSLLINE
              FROM GRAND-POPTYPE1
                 AFTER ADVANCING 2 LINES
                    AT EOP
                        PERFORM 9100-HEADING1.
           WRITE PRTSLLINE
              FROM TEAM-TOTAL
                 AFTER ADVANCING 3 LINES
                    AT EOP
                        PERFORM 9100-HEADING1.
           WRITE PRTSLLINE
              FROM TEAM-A
                 AFTER ADVANCING 2 LINES
                    AT EOP
                        PERFORM 9100-HEADING1.
           WRITE PRTSLLINE
              FROM TEAM-B
                 AFTER ADVANCING 2 LINES
                    AT EOP
                        PERFORM 9100-HEADING1.
           WRITE PRTSLLINE
              FROM TEAM-C
                 AFTER ADVANCING 2 LINES
                    AT EOP
                        PERFORM 9100-HEADING1.
           WRITE PRTSLLINE
              FROM TEAM-D
                 AFTER ADVANCING 2 LINES
                    AT EOP
                        PERFORM 9100-HEADING1.
           WRITE PRTSLLINE
              FROM TEAM-E
                 AFTER ADVANCING 2 LINES
                    AT EOP
                        PERFORM 9100-HEADING1.

           WRITE PRTERRLINE
              FROM TOTAL-ERRORS
                 AFTER ADVANCING 2 LINES
                    AT EOP
                        PERFORM 9200-HEADING.
           CLOSE PRTSLOUT.
           CLOSE PRTERROUT.
           CLOSE POPSL-FILE.

       9000-READ.
           READ POPSL-FILE
               AT END MOVE 'NO' TO MORE-RECS.



       9100-HEADING1.
           ADD 1 TO CPCTR.
           MOVE CPCTR TO O-PCTR.
           WRITE PRTSLLINE
             FROM TITLE-LINE
                AFTER ADVANCING PAGE.
           WRITE PRTSLLINE
              FROM TITLE-LINE2
                 AFTER ADVANCING 1 LINES
                    AT EOP
                       PERFORM 9100-HEADING1.
           WRITE PRTSLLINE
              FROM TITLE-LINE1
                 AFTER ADVANCING 1 LINES
                    AT EOP
                        PERFORM 9100-HEADING1.
           WRITE PRTSLLINE
              FROM HEADING-LINE
                 AFTER ADVANCING 2 LINES
                    AT EOP
                       PERFORM 9100-HEADING1.

       9200-HEADING.
           ADD 1 TO  CPCTR-ERRO.
           MOVE  CPCTR-ERRO TO OPCTRR.
           WRITE PRTERRLINE
             FROM ERRIR-LINE
                AFTER ADVANCING PAGE
                   AT EOP
                        PERFORM 9200-HEADING.
           WRITE PRTERRLINE
              FROM ERRIR-LINE2
                 AFTER ADVANCING 1 LINES
                    AT EOP
                        PERFORM 9200-HEADING.
           WRITE PRTERRLINE
             FROM ERRIR-LINE1
                AFTER ADVANCING 1 LINES
                    AT EOP
                        PERFORM 9200-HEADING.
