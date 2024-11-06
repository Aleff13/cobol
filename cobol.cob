       IDENTIFICATION DIVISION.
       PROGRAM-ID. REMOVE-SPECIAL-CHARACTERS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 INPUT-STRING        PIC X(50) VALUE "Hello, World! COBOL @ 2023 #Example".
       01 OUTPUT-STRING       PIC X(50) VALUE SPACES.
       01 INDEX-IN            PIC 9(4) COMP VALUE 1.
       01 INDEX-OUT           PIC 9(4) COMP VALUE 1.
       01 CURRENT-CHAR        PIC X.

       PROCEDURE DIVISION.
           PERFORM VARYING INDEX-IN FROM 1 BY 1 UNTIL INDEX-IN > LENGTH OF INPUT-STRING
               MOVE INPUT-STRING(INDEX-IN:1) TO CURRENT-CHAR
               EVALUATE TRUE
                   WHEN CURRENT-CHAR >= "A" AND CURRENT-CHAR <= "Z"
                       MOVE CURRENT-CHAR TO OUTPUT-STRING(INDEX-OUT:1)
                       ADD 1 TO INDEX-OUT
                   WHEN CURRENT-CHAR >= "a" AND CURRENT-CHAR <= "z"
                       MOVE CURRENT-CHAR TO OUTPUT-STRING(INDEX-OUT:1)
                       ADD 1 TO INDEX-OUT
                   WHEN CURRENT-CHAR >= "0" AND CURRENT-CHAR <= "9"
                       MOVE CURRENT-CHAR TO OUTPUT-STRING(INDEX-OUT:1)
                       ADD 1 TO INDEX-OUT
                   WHEN CURRENT-CHAR = SPACE
                       MOVE CURRENT-CHAR TO OUTPUT-STRING(INDEX-OUT:1)
                       ADD 1 TO INDEX-OUT
               END-EVALUATE
           END-PERFORM

           DISPLAY "Original String: " INPUT-STRING
           DISPLAY "Filtered String: " OUTPUT-STRING
           STOP RUN.

