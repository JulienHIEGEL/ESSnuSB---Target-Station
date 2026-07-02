*$ CREATE MGDRAW.FOR
*COPY MGDRAW
*                                                                      *
*=== mgdraw ===========================================================*
*                                                                      *
      SUBROUTINE MGDRAW ( ICODE, MREG )

      INCLUDE 'dblprc.inc'

      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST /.TRUE./

      DOUBLE PRECISION X, Y, Z, PX, PY, PZ

c --- ouvrir le fichier une seule fois
      IF (FIRST) THEN
         OPEN(99, FILE='track.dat', STATUS='UNKNOWN')
         FIRST = .FALSE.
      ENDIF

c --- garder uniquement les pions +
      IF (JTRACK .NE. 8) RETURN

c --- récupérer les infos via TRACK variables globales
      CALL GETTRK (X, Y, Z, PX, PY, PZ)

      WRITE(99,*) X, Y, Z, PX, PY, PZ

      RETURN
      END