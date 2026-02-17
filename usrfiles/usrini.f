*
*=== usrini ===========================================================*
*
      SUBROUTINE USRINI ( WHAT, SDUM )

      INCLUDE 'dblprc.inc'
      INCLUDE 'dimpar.inc'
      INCLUDE 'iounit.inc'
*
*----------------------------------------------------------------------*
*                                                                      *
*     Copyright (C) 2003-2019:  CERN & INFN                            *
*     All Rights Reserved.                                             *
*                                                                      *
*     USeR INItialization: this routine is called every time the       *
*                          USRICALL card is found in the input stream  *
*                                                                      *
*     Created on 01 January 1991   by    Alfredo Ferrari & Paola Sala  *
*                                                   Infn - Milan       *
*                                                                      *
*----------------------------------------------------------------------*
*
      DIMENSION WHAT (6)
      CHARACTER SDUM*8
*
*  Don't change the following line:
      LUSRIN = .TRUE.
* *** Write from here on *** *

*----------------------------------------------------------------------*
*     
*    Data file definition
*    --
*    40 : Contain all particle existing the target
*         Filled in usermed.f
*----------------------------------------------------------------------*

      open (unit = 9, file = 'message', status = 'UNKNOWN')
      open (unit = 10, file = 'target', status = 'UNKNOWN')
      open (unit = 20, file = 'clmt', status = 'UNKNOWN')
      open (unit = 30, file = 'DT', status = 'UNKNOWN')
      open (unit = 40, file = 'BLKBODY', status = 'UNKNOWN')



      RETURN

*=== End of subroutine Usrini =========================================*
      END

