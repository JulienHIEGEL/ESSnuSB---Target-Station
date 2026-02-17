*
*=== usrout ===========================================================*
*
      SUBROUTINE USROUT ( WHAT, SDUM )

      INCLUDE 'dblprc.inc'
      INCLUDE 'dimpar.inc'
      INCLUDE 'iounit.inc'
*
*----------------------------------------------------------------------*
*                                                                      *
*     Copyright (C) 2003-2019:  CERN & INFN                            *
*     All Rights Reserved.                                             *
*                                                                      *
*     USeR OUTput: this routine is called every time the USROCALL card *
*                  is found in the input stream                        *
*                                                                      *
*     Created on 01 January 1991   by    Alfredo Ferrari & Paola Sala  *
*                                                   Infn - Milan       *
*                                                                      *
*----------------------------------------------------------------------*
*
      DIMENSION WHAT (6)
      CHARACTER SDUM*8
*
      close(39)

      close(40)
      close(41)
      close(42)

      close(50)
      close(51)
      close(52)

      close(53)
      close(54)
      close(55)

      close(56)
      close(57)
      close(58)

      close(59)
      close(60)
      close(61)

      close(62)
      close(63)
      close(64)

      close(65)
      close(66)
      close(67)

      close(68)
      close(69)
      close(70)

      close(71)
      close(72)
      close(73)

      close(74)
      close(75)
      close(76)

      close(77)
      close(78)
      close(79)

      close(80)
      close(81)
      close(82)

      RETURN
*=== End of subroutine Usrout =========================================*
      END

