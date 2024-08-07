! File VersionID:
!   $Id: hysteresis.f90 298 2016-07-25 20:07:31Z kroes006 $
! ----------------------------------------------------------------------
      subroutine hysteresis (numnod,layer,h,hm1,indeks,tau,paramvg,     &
     &               cofgen,dimoca,theta,disnod,dt,swsophy,numtab,sptab,&
     &               ientrytab)
! ----------------------------------------------------------------------
!     date               : 23/10/2000    
!     purpose            : check for hysteretic reversal and update      
!                          model parameters            
! ----------------------------------------------------------------------
      implicit none
      include 'arrays.fi'

! --- global
      integer  numnod,layer(macp),indeks(macp)
      real(8)  disnod(macp+1),dt,h(macp),hm1(macp),tau
      real(8) theta(macp),cofgen(12,macp),paramvg(10,maho),dimoca(macp)
      integer  swsophy,numtab(macp)
      real(8)  sptab(5,macp,matab)
      integer   ientrytab(macp,0:matabentries)    ! Soil Physical functions (h,theta,k,dthetadh,dkdtheta) tabulated for each model compartment

! ----------------------------------------------------------------------

! --- local
      integer node,lay,indtem(macp)
      real(8) moiscap,delp,sew,sed,fvalue,prhead
      real(8) thetar(macp),thetas(macp),alfamg(macp)
! ----------------------------------------------------------------------

! --- check for reversal
      do node = 1,numnod
        delp = hm1(node)-h(node)
        if (delp/float(indeks(node)).gt.tau.and.                        &
     &     h(node).lt.-10.0d0 .and. h(node).gt.-1.0d3) then
          indtem(node) = -indeks(node)
        else
          indtem(node) = indeks(node)
        endif
      end do

! --- change parameters scanning curves
      do 100 node = 1,numnod
        lay = layer(node)

! ---   no change
        if (indtem(node).eq.indeks(node) .or.                           &
     &     abs(paramvg(4,lay)-paramvg(8,lay)) .lt. 1.d-4) goto 100

! ---   relative saturation
        sew = (1.0d0+(paramvg(8,lay)*(-h(node)))**paramvg(6,lay))       &
     &        **(-paramvg(7,lay)) 
        sed = (1.0d0+(paramvg(4,lay)*(-h(node)))**paramvg(6,lay))       &
     &        **(-paramvg(7,lay))

! ---   change index
        indeks(node) = -1*indeks(node)

! ---   update alfa, thetar and thetas
        if (indeks(node).eq.1) then
! ---     wetting branch
          alfamg(node) = paramvg(8,lay)
          thetas(node) = paramvg(2,lay)
          thetar(node) = (theta(node)-thetas(node)*sew)/(1.0d0-sew)

! ---     check on thetar(node) value, if needed correction of h
          fvalue = thetar(node)
          if(thetar(node).lt.paramvg(1,lay)) thetar(node)=paramvg(1,lay)
          if(thetar(node).gt.paramvg(2,lay)) thetar(node)=paramvg(2,lay)
          cofgen(4,node) = alfamg(node)
          cofgen(1,node) = thetar(node)
          cofgen(2,node) = thetas(node)
          if (abs(fvalue-thetar(node)) .gt. 1.d-10) then
             h(node) = prhead(node,disnod(node),cofgen,theta(node),h,   &
     &                        swsophy,numtab,sptab,ientrytab)
          endif
        else
! ---     drying branch
          alfamg(node) = paramvg(4,lay)
          thetar(node) = paramvg(1,lay)
          thetas(node) = thetar(node)+(theta(node)-thetar(node))/sed

! ---     check on thetas(node) value, if needed correction of h
          fvalue = thetas(node)
          if(thetas(node).lt.paramvg(1,lay)) thetas(node)=paramvg(1,lay)
          if(thetas(node).gt.paramvg(2,lay)) thetas(node)=paramvg(2,lay)
          cofgen(4,node) = alfamg(node)
          cofgen(1,node) = thetar(node)
          cofgen(2,node) = thetas(node)
          if (abs(fvalue-thetas(node)) .gt. 1.d-10) then
             h(node) = prhead(node,disnod(node),cofgen,theta(node),h,   &
     &                        swsophy,numtab,sptab,ientrytab)
          endif
        endif

! ---   update capacity 
        dimoca(node) = moiscap(node,h(node),cofgen,dt,swsophy,          &
     &                         numtab,sptab,ientrytab)


 100  continue

      return
      end

