
*$ CREATE USRMED.FOR
*COPY USRMED
*                                                                      *
*=== usrmed ===========================================================*
*                                                                      *
      SUBROUTINE USRMED ( IJ, EKSCO, PLA, WEE, MREG, NEWREG, XX, YY, ZZ,
     &                    TXX, TYY, TZZ )

      INCLUDE 'dblprc.inc'
      INCLUDE 'dimpar.inc'
      INCLUDE 'iounit.inc'
*
*----------------------------------------------------------------------*
*                                                                      *
*     Copyright (C) 1991-2005      by    Alfredo Ferrari & Paola Sala  *
*     All Rights Reserved.                                             *
*                                                                      *
*                                                                      *
*     USeR MEDium dependent directives:                                *
*                                                                      *
*     Created on  10  may  1996    by    Alfredo Ferrari & Paola Sala  *
*                                                   Infn - Milan       *
*                                                                      *
*     Last change on   29-may-96   by    Alfredo Ferrari               *
*                                                                      *
*     Input variables:                                                 *
*             ij = particle id                                         *
*          Eksco = particle kinetic energy (GeV)                       *
*            Pla = particle momentum (GeV/c)                           *
*            Wee = particle weight                                     *
*           Mreg = (original) region number                            *
*         Newreg = (final)    region number                            *
*       Xx,Yy,Zz = particle position                                   *
*    Txx,Tyy,Tzz = particle direction                                  *
*                                                                      *
*     The user is supposed to change only WEE if MREG = NEWREG and     *
*     WEE, NEWREG, TXX, TYY, TZZ if MREG .NE. NEWREG                   *
*                                                                      *
*----------------------------------------------------------------------*
*

ccc  Pions and Kaons exiting the target
      character*8 mregn, newregn

      call geor2n(mreg,mregn,ierr1)
      call geor2n(newreg,newregn,ierr1)

*     Score All particle coming out of the target frontier
*     --
      if((mregn.eq."TARGET").and.(newregn.ne."TARGET")) then
       write(50,1040) ij,Xx,Yy,Zz,Pla,Txx,Tyy,Tzz
      endif

*     Score All particle at CLMT 
*     --
      if (mregn .eq. "EXPAREA" .and. (newregn .eq. "CLMT" .or. newregn .eq. "CLMTHOLE" .or. newregn .eq. "CLMTW")) then
        write(51,1050) ij,Xx,Yy,Zz,Pla,Txx,Tyy,Tzz
      endif

*     Score All particle at CLMT 
*     --
      if (mregn .eq. "BFIELD" .and. (newregn.ne."BFIELD")) then
        write(52,1050) ij,Xx,Yy,Zz,Pla,Txx,Tyy,Tzz
      endif


*     Score All particle leaving DT (hitting walls, Beam Dump .. Magnet ..)
*     --
      if ((mregn .eq. "DT").and.(newregn .ne. "DT")) then
        write(53,1050) ij,Xx,Yy,Zz,Pla,Txx,Tyy,Tzz
      endif

*     Score All particle leaving DT (hitting walls, Beam Dump .. Magnet ..)
*     --
      if ((mregn .eq. "TTLINE").and.(newregn .ne. "TTLINE")) then
        write(54,1050) ij,Xx,Yy,Zz,Pla,Txx,Tyy,Tzz
      endif


 900    continue
 1040   format(I4,7(1x,e14.7))
 1041   format(2(1x,e14.7))
 1042   format(2(1x,e14.7))
 1050   format(I4,7(1x,e14.7))
 1051   format(2(1x,e14.7))
 1052   format(2(1x,e14.7))

      RETURN
*=== End of subroutine Usrmed =========================================*
      END
