! ----------------------------------------------------------------------
   subroutine astro_sam &
     &   (logf,swscre,daynr,lat,dayl,daylp,sinld,cosld,dsinb,dsinbe_sam,dso)
! ----------------------------------------------------------------------
! Subroutine astro (daynr,lat,dayl,daylp,sinld,cosld)
! Authors: this routine Astro is based on Sastro (Daniel van Kraalingen)
! Date   : 28-November-2005 
! Purpose: This subroutine calculates solar constant, daily
!          extraterrestrial radiation, daylength and some intermediate
!          variables required by other routines. The routine has been
!          rewritten such that latitudes from pole to pole can be used.
!
! Formal parameters:  (I=input,O=output,C=control,IN=init,T=time)
! name   type meaning                                     units  class
! ----   ---- -------                                     -----  -----
! logf    I4  Internal number of logbook output file *.LOG   -      I
! swscre  I4  Switch of screen display:  0 = no display;     -      I
!             1 = summary water balance; 2 = daynumber
! daynr   I4  Day of year (Jan 1st = 1)                      d      I  
! lat     R8  Latitude of the site                       degrees    I  
! dayl    R8  Astronomical daylength (base = 0 degrees)      h      O  
! daylp   R8  Photoperiodic daylength (base = -4 degrees)    h      O  
! sinld   R8  Intermediate variable for other subroutine     -      O  
! cosld   R8  Intermediate variable for other subroutine     -      O  
! dsinb   R8  Daily total of sine of solar height            s      O  
! dsinbe  R8  Daily integral of sine of solar height         s      O  
!             corrected for lower transmission at low                  
!             elevation                                                
! sc      R8  Solar constant at day=daynr                   W/m2     O  
! dso     R8  Daily extraterrestrial radiation            J/m2/d    O  
!                                                                      
! Fatal error checks (on input): lat > 90, lat < -90
! Warnings          : lat above polar circle, lat within polar circle  
! Subprograms called: Warning
! File usage        : none
!----------------------------------------------------------------------
      implicit none
 
!     formal parameters
      integer logf,swscre,daynr
      real*8  lat,dayl,daylp,sinld,cosld,dsinb,dsinbe_sam,dso

!     local parameters
      real*8  angle,aob,dec,pi,rad,zza,zzcos,zzsin,sc,help1
      character messag*200

      data    pi /3.1415926d0/,angle /-4.0d0/
! ----------------------------------------------------------------------
! --- declination of the sun as a function of daynr
!     (see ref.manual: Radiation term: 23.45*rad=0.409 en (90-10)*rad=1.39)
      rad = pi/180.d0
      dec = -asin(sin(23.45d0*rad)*cos(2.d0*pi*dble(daynr+10)/365.0d0))
! --- some intermediate variables
      sinld = sin(rad*lat)*sin(dec)
      cosld = cos(rad*lat)*cos(dec)
      aob = sinld/cosld
! --- calculation of daylenght and photoperiodic daylength
!     solution for polar circle altutude adopted from 
!     Daniel van Kraalingen (routine Sastro, dd 12-june-1996,version 1.1)
      if (aob.lt.-1.0d0) then
        messag = 'Warning: latitude above polar circle, daylength= 0hrs'
        call warn ('Astro',messag,logf,swscre)
        dayl = 0.0d0
        zzcos =  0.0d0
        zzsin =  1.0d0
      else if (aob.gt.1.0d0) then
        messag = 'Warning: latitude within polar circle,daylength=24hrs'
        call warn ('Astro',messag,logf,swscre)
        dayl = 24.0d0
        zzcos =  0.0d0
        zzsin = -1.0d0
      else
        dayl  = 12.0d0*(1.0d0+2.0d0*asin(aob)/pi)
        help1 = (-sin(angle*rad)+sinld)/cosld
        if (help1.gt.1.0d0) then
          daylp = 24.0d0
        else
          daylp = 12.0d0*(1.0d0+2.0d0*asin(help1)/pi)
        endif
!        write(logf,*) 'help1=',help1,'daylp=',daylp
        zza   = pi*(12.0d0+dayl)/24.0d0
        zzcos = cos (zza)
        zzsin = sin (zza)
      endif

!     Daily integral of sine of solar height (DSINB) with a
!     correction for lower atmospheric transmission at lower solar
!     elevations (DSINBE)
      dsinb  = 2.0d0*3600.0d0*(dayl*0.50d0*sinld-12.0d0*cosld*zzcos/pi)
      dsinbe_sam = 2.0d0*3600.0d0*(dayl*(0.50d0*sinld+0.20d0*sinld**2.0d0+ &
     &      0.10d0*cosld**2.0d0)-(12.0d0*cosld*zzcos+ &
     &      9.6d0*sinld*cosld*zzcos+2.4d0*cosld**2.0d0*zzcos*zzsin)/pi)

!     Solar constant and daily extraterrestrial radiation
      sc = 1370.0d0*(1.0d0+0.033d0*cos (2.0d0*pi*daynr/365.d0))
      dso  = sc*dsinb

      return
      end
