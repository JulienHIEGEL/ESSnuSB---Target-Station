*$ CREATE SOURCE.FOR
*COPY SOURCE
*
*=== source ===========================================================*
*
      SUBROUTINE SOURCE ( NOMORE )

      INCLUDE 'dblprc.inc'
      INCLUDE 'dimpar.inc'
      INCLUDE 'iounit.inc'
*
*----------------------------------------------------------------------*
*                                                                      *
*     Copyright (C) 1990-2021      by    Alfredo Ferrari & Paola Sala  *
*     All Rights Reserved.                                             *
*                                                                      *
*                                                                      *
*     New source for FLUKA9x-FLUKA20xy:                                *
*                                                                      *
*     Created on 07 January 1990   by    Alfredo Ferrari & Paola Sala  *
*                                                Infn - Milan          *
*                                                                      *
*     Last change on  03-Apr-21    by             Paola Sala           *
*                                                Infn - Milan          *
*                                                                      *
*  This is just an example of a possible user written source routine.  *
*  note that the beam card still has some meaning - in the scoring the *
*  maximum momentum used in deciding the binning is taken from the     *
*  beam momentum.  Other beam card parameters are obsolete.            *
*                                                                      *
*       Output variables:                                              *
*                                                                      *
*              Nomore = if > 0 the run will be terminated              *
*                                                                      *
*----------------------------------------------------------------------*
*
      INCLUDE 'beamcm.inc'
      INCLUDE 'fheavy.inc'
      INCLUDE 'flkstk.inc'
      INCLUDE 'ioiocm.inc'
      INCLUDE 'ltclcm.inc'
      INCLUDE 'paprop.inc'
      INCLUDE 'sourcm.inc'
      INCLUDE 'sumcou.inc'
*
      LOGICAL LFIRST, LISNUT
      INTEGER SEED
      INTEGER NIN, NN

      PARAMETER (NMAX=10000000)

      CHARACTER*250 LINE

      DIMENSION  XI(NMAX), YI(NMAX), ZI(NMAX)
      DIMENSION  UI(NMAX), VI(NMAX), WI(NMAX)
      DIMENSION  PXI(NMAX), PYI(NMAX), PZI(NMAX), PTOT(NMAX)
      DIMENSION  IJI(NMAX)
*
      SAVE LFIRST
      DATA LFIRST / .TRUE. /
*  Statement function:
      LISNUT (IJ) = INDEX ( PRNAME (IJ), 'NEUTRI' ) .GT. 0

ccc      SAVE SEED, NIN
      SAVE SEED
      SAVE NIN, NN
      SAVE XI, YI, ZI, UI, VI, WI, PXI, PYI, PZI, PTOT, IJI
*======================================================================*
*                                                                      *
*                 BASIC VERSION                                        *
*                                                                      *
*======================================================================*
      NOMORE = 0
*  +-------------------------------------------------------------------*
*  |  First call initializations:
      IF ( LFIRST ) THEN
*  |  *** The following 3 cards are mandatory ***
         TKESUM = ZERZER
         LFIRST = .FALSE.
         LUSSRC = .TRUE.
*  |  *** User initialization ***

         LUNRD = NINT(WHASOU(1))
         TOT = NINT(WHASOU(2))
         SEED = INT(WHASOU(5))
         CALL SRAND(SEED)

         NN=1
ccc         SEED = INT(WHASOU(5))
ccc         CALL SRAND(SEED)

c  |   particle counter
         NIN = 0

c     read the lines from the inputfile until
c     the limit of NMAX is reached or EOF
c         do while(nin.le.tot)
 10      CONTINUE
         do while (nin.lt.tot)
            READ( LUNRD, '(A)', ERR=9999, END=20 ) LINE

            READ (LINE,*,ERR=10)IJ0, X0, Y0, Z0, PX0, PY0, PZ0

            NIN = NIN + 1
            IF (NIN.GT.NMAX) CALL FLABRT('SOURCE','Increase NMAX')

            XI(NIN) = X0
            YI(NIN) = Y0
            ZI(NIN) = Z0
            PXI(NIN) = PX0
            PYI(NIN) = PY0
            PZI(NIN) = PZ0
            PTOT(NIN) = SQRT(PX0**2 + PY0**2 + PZ0**2)
            UI(NIN) = PX0 / PTOT(NIN)
            VI(NIN) = PY0 / PTOT(NIN)
            WI(NIN) = PZ0 / PTOT(NIN)
            IJI(NIN) = IJ0

*      if (NIN.lt.10) then
*      write(82,1002,err=992)xi(nin),yi(nin),zi(nin)
*      endif

* 992    continue
* 1002   format(3(1x,e14.7))
         end do
c         GOTO 10

         IF (NIN.EQ.0) CALL FLABRT('SOURCE','Error reading file')
         WRITE (LUNOUT,*) NIN,' particles loaded'

 20      CONTINUE
 9999    CONTINUE

      END IF
*  |
*  +-------------------------------------------------------------------*
*  Push one source particle to the stack. Note that you could as well
*  push many but this way we reserve a maximum amount of space in the
*  stack for the secondaries to be generated
*  Npflka is the stack counter: of course any time source is called it
*  must be =0
      NPFLKA = NPFLKA + 1
*  Wt is the weight of the particle
      WTFLK  (NPFLKA) = ONEONE
      WEIPRI = WEIPRI + WTFLK (NPFLKA)
      IJBEAM=IJI(NN)
*  Particle type (1=proton.....). Ijbeam is the type set by the BEAM
*  card
*  +-------------------------------------------------------------------*
*  |  (Radioactive) isotope:
      IF ( IJBEAM .EQ. -2 .AND. LRDBEA ) THEN
         IARES  = IPROA
         IZRES  = IPROZ
         IISRES = IPROM
         CALL STISBM ( IARES, IZRES, IISRES )
         IJHION = IPROM  * 100000 + MOD ( IPROZ, 100 ) * 1000 + IPROA
         IJHION = IJHION * 100    + KXHEAV
         IONID  = IJHION
         CALL DCDION ( IONID )
         CALL SETION ( IONID )
         EEXSTK (NPFLKA) = EXENRG (IONID)
         TMNSTK (NPFLKA) = TMNLF  (IONID)
         ILVSTK (NPFLKA) = IEXLVL (IONID)
         LFRPHN (NPFLKA) = .FALSE.
*  |
*  +-------------------------------------------------------------------*
*  |  Heavy ion:
      ELSE IF ( IJBEAM .EQ. -2 ) THEN
         IJHION = IPROM  * 100000 + MOD ( IPROZ, 100 ) * 1000 + IPROA
         IJHION = IJHION * 100    + KXHEAV
         IONID  = IJHION
         CALL DCDION ( IONID )
         CALL SETION ( IONID )
         EEXSTK (NPFLKA) = EXENRG (IONID)
         TMNSTK (NPFLKA) = TMNLF  (IONID)
         ILVSTK (NPFLKA) = IEXLVL (IONID)
         ILOFLK (NPFLKA) = IJHION
*  |  Flag this is prompt radiation
         LRADDC (NPFLKA) = .FALSE.
*  |  Group number for "low" energy neutrons, set to 0 anyway
         IGROUP (NPFLKA) = 0
*  |  Parent radioactive isotope:
         IRDAZM (NPFLKA) = 0
*  |  Particle age (s)
         AGESTK (NPFLKA) = +ZERZER
*  |  Kinetic energy of the particle (GeV)
         TKEFLK (NPFLKA) = SQRT ( PBEAM**2 + AM (IONID)**2 )
     &                   - AM (IONID)
*  |  Particle momentum
         PMOFLK (NPFLKA) = PBEAM
*        PMOFLK (NPFLKA) = SQRT ( TKEFLK (NPFLKA) * ( TKEFLK (NPFLKA)
*    &                          + TWOTWO * AM (IONID) ) )
         LFRPHN (NPFLKA) = .FALSE.
*  |
*  +-------------------------------------------------------------------*
*  |  Normal hadron or lepton:
      ELSE
         IONID = IJBEAM
         EEXSTK (NPFLKA) = EXENRG (IJBEAM)
         TMNSTK (NPFLKA) = TMNLF  (IJBEAM)
         ILVSTK (NPFLKA) = IEXLVL (IJBEAM)
         ILOFLK (NPFLKA) = IJBEAM
*  |  Flag this is prompt radiation
         LRADDC (NPFLKA) = .FALSE.
*  |  Group number for "low" energy neutrons, set to 0 anyway
         IGROUP (NPFLKA) = 0
*  |  Parent radioactive isotope:
         IRDAZM (NPFLKA) = 0
*  |  Particle age (s)
         AGESTK (NPFLKA) = +ZERZER
*  |  Kinetic energy of the particle (GeV)
         TKEFLK (NPFLKA) = SQRT ( PBEAM**2 + AM (IONID)**2 )
     &                   - AM (IONID)
*  |  Particle momentum
         PMOFLK (NPFLKA) = PBEAM
*        PMOFLK (NPFLKA) = SQRT ( TKEFLK (NPFLKA) * ( TKEFLK (NPFLKA)
*    &                          + TWOTWO * AM (IONID) ) )
*  |  +----------------------------------------------------------------*
*  |  |  Check if it is a neutrino, if so force the interaction
*  |  |  (unless the relevant flag has been disabled)
         IF ( LISNUT (IJBEAM) .AND. LNUFIN ) THEN
            LFRPHN (NPFLKA) = .TRUE.
*  |  |
*  |  +----------------------------------------------------------------*
*  |  |  Not a neutrino
         ELSE
            LFRPHN (NPFLKA) = .FALSE.
         END IF
*  |  |
*  |  +----------------------------------------------------------------*
      END IF
*  |
*  +-------------------------------------------------------------------*
*  From this point .....

ccc      IONID = 13
ccc      RNDSIG = RAND()
ccc      N = INT (NIN*RNDSIG) + 1
      n=nn
      IONID=IJI(N)

      RNDSIGX = RAND()

      if((rndsigx.ge.0).and.(rndsigx.lt.0.25))then
        auxx = 1.0
        auxy = 1.0
      else if((rndsigx.ge.0.25).and.(rndsigx.lt.0.5))then
        auxx = -1.0
        auxy = 1.0
      else if((rndsigx.ge.0.5).and.(rndsigx.lt.0.75))then
        auxx = -1.0
        auxy = -1.0
      else if((rndsigx.ge.0.75).and.(rndsigx.le.1.0))then
        auxx = 1.0
        auxy = -1.0
      endif

*  Particle generation (1 for primaries)
      LOFLK  (NPFLKA) = 1
*  User dependent flag:
      LOUSE  (NPFLKA) = 0
*  No channeling:
      KCHFLK (NPFLKA) = 0
      ECRFLK (NPFLKA) = ZERZER
*  Extra infos:
      INFSTK (NPFLKA) = 0
      LNFSTK (NPFLKA) = 0
      ANFSTK (NPFLKA) = ZERZER
*  Parent variables:
      IPRSTK (NPFLKA) = 0
      EKPSTK (NPFLKA) = ZERZER
*  User dependent spare variables:
      DO 100 ISPR = 1, MKBMX1
         SPAREK (ISPR,NPFLKA) = ZERZER
 100  CONTINUE
*  User dependent spare flags:
      DO 200 ISPR = 1, MKBMX2
         ISPARK (ISPR,NPFLKA) = 0
 200  CONTINUE
*  Save the track number of the stack particle:
      ISPARK (MKBMX2,NPFLKA) = NPFLKA
      NPARMA = NPARMA + 1
      NUMPAR (NPFLKA) = NPARMA
      NEVENT (NPFLKA) = 0
      DFNEAR (NPFLKA) = +ZERZER
c      N = NPFLKA
*  ... to this point: don't change anything
      AKNSHR (NPFLKA) = -TWOTWO
*  Kinetic energy of the particle (GeV)
*      TKEFLK (NPFLKA) = SQRT ( PBEAM**2 + AM (IONID)**2 ) - AM (IONID)
      TKEFLK (NPFLKA) = SQRT ( PTOT(N)**2 + AM (IONID)**2 ) - AM (IONID)
*  Particle momentum
*      PMOFLK (NPFLKA) = PBEAM
      PMOFLK (NPFLKA) = PTOT(N)
*     PMOFLK (NPFLKA) = SQRT ( TKEFLK (NPFLKA) * ( TKEFLK (NPFLKA)
*    &                       + TWOTWO * AM (IONID) ) )
*  Cosines (tx,ty,tz)
*      TXFLK  (NPFLKA) = UBEAM
*      TYFLK  (NPFLKA) = VBEAM
*      TZFLK  (NPFLKA) = WBEAM
      TXFLK  (NPFLKA) = UI(N)
      TYFLK  (NPFLKA) = VI(N)
      TZFLK  (NPFLKA) = WI(N)
*     TZFLK  (NPFLKA) = SQRT ( ONEONE - TXFLK (NPFLKA)**2
*    &                       - TYFLK (NPFLKA)**2 )
*  Polarization cosines:
      TXPOL  (NPFLKA) = -TWOTWO
      TYPOL  (NPFLKA) = +ZERZER
      TZPOL  (NPFLKA) = +ZERZER
*  Particle coordinates
      XFLK   (NPFLKA) = XBEAM*auxx + XI(N)
      YFLK   (NPFLKA) = YBEAM*auxy + YI(N)
      ZFLK   (NPFLKA) = ZBEAM + ZI(N)

c      if (N.lt.10) then
c      write(80,1001,err=990)n,ionid,xflk(npflka),yflk(npflka),
c     &zflk(npflka)
c      endif

c 990    continue
c 1001   format(2(i8),3(1x,e14.7))
       nn=nn+1

c       save nn

*  Calculate the total kinetic energy of the primaries: don't change
*  +-------------------------------------------------------------------*
*  |  (Radioactive) isotope:
      IF ( IJBEAM .EQ. -2 .AND. LRDBEA ) THEN
*  |
*  +-------------------------------------------------------------------*
*  |  Heavy ion:
      ELSE IF ( ILOFLK (NPFLKA) .EQ. -2 .OR.
     &          ABS (ILOFLK (NPFLKA)) .GE. KPIOMN ) THEN
         TKESUM = TKESUM + TKEFLK (NPFLKA) * WTFLK (NPFLKA)
*  |
*  +-------------------------------------------------------------------*
*  |  Standard particle:
      ELSE IF ( ILOFLK (NPFLKA) .NE. 0 ) THEN
         TKESUM = TKESUM + ( TKEFLK (NPFLKA) + AMDISC (ILOFLK(NPFLKA)) )
     &          * WTFLK (NPFLKA)
*  |
*  +-------------------------------------------------------------------*
*  |
      ELSE
         TKESUM = TKESUM + TKEFLK (NPFLKA) * WTFLK (NPFLKA)
      END IF
*  |
*  +-------------------------------------------------------------------*
      RADDLY (NPFLKA) = ZERZER
*  Here we ask for the region number of the hitting point.
*     NREG (NPFLKA) = ...
*  The following line makes the starting region search much more
*  robust if particles are starting very close to a boundary:
      CALL GEOCRS ( TXFLK (NPFLKA), TYFLK (NPFLKA), TZFLK (NPFLKA) )
      CALL GEOREG ( XFLK  (NPFLKA), YFLK  (NPFLKA), ZFLK  (NPFLKA),
     &              NRGFLK(NPFLKA), IDISC )
*  Do not change these cards:
      CALL GEOHSM ( NHSPNT (NPFLKA), 1, -11, MLATTC )
      NLATTC (NPFLKA) = MLATTC
      CMPATH (NPFLKA) = ZERZER
      CALL SOEVSV
      RETURN
*=== End of subroutine Source =========================================*
      END

