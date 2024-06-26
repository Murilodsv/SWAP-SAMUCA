! File VersionID:
!   $Id: calcgwl.f90 298 2016-07-25 20:07:31Z kroes006 $
! ----------------------------------------------------------------------
      subroutine calcgwl(logf,swscre,swbotb,flmacropore,numnod,         &
     &            CritUndSatVol,gwlinp,h,z,dz,pond,t1900,Theta,ThetaS,  &
     &            gwl,nodgwl,bpegwl,npegwl,pegwl,nodgwlflcpzo,gwlflcpzo)
! ----------------------------------------------------------------------
!     date               : july 2002, updated april 2008
!     purpose            : search for the watertable and perched
!                          watertable (if existing).
!
!     update             : an unsaturated zone embedded in a saturated soil
!                          column should contain at least a total of 'CritAir' cm 
!                          of air to be recognized as really unsaturated
! ----------------------------------------------------------------------
      implicit none
! --- global
      include 'arrays.fi'

      integer  logf,numnod,swbotb,swscre,nodgwl,bpegwl,npegwl
      integer  nodgwlflcpzo
      logical  flmacropore
      real(8)  CritUndSatVol,gwl,z(macp),dz(macp),pond,pegwl,gwlinp
      real(8)  h(macp),t1900,Theta(macp),ThetaS(macp), gwlflcpzo
! --- local
      integer   i, node, nodhlp
      real(8)   level
      logical   flsat,flunsat
      character(len=200) messag
      character(len=19) datexti

      save
! ----------------------------------------------------------------------

! --- set initial values
      gwl       = 999.0d0
      flsat     = .false.
      nodgwl    = numnod+1
      nodhlp    = numnod

! --- search for groundwater table
      if (h(numnod).ge.0.0d0) flsat  = .true.

      node = numnod
      nodgwlflcpzo = numnod + 1
      gwlflcpzo    = gwl
      do while (flsat .and. node.gt.1)
         node = node - 1 
         if(swbotb.eq.1)then
            if (h(node) .lt. 0.0d0) then
               gwl = z(node+1) + h(node+1) / (h(node+1)-h(node))        &
     &             * 0.5d0 * (dz(node) + dz(node+1))
               flsat   =.false.
               nodgwl  = node
            endif
         else 
! adjusted : MM/JK-20151109            if (h(node) .lt. -1.0d-3) then
            if (h(node) .lt. 0.0d0) then
               if (flmacropore) then
                  if (gwl.gt.990.0d0) then
                     nodgwl = node
                     gwl    = level(node,dz,h,z)
                  endif
!
                  call watertable(node,nodgwlflcpzo,nodhlp,flsat,       &
     &                      CritUndSatVol,dz,h,Theta,ThetaS,z,gwlflcpzo)
               else
                  flsat = .false.
                  nodgwl = node
                  gwl    = level(node,dz,h,z)
               endif
            endif
         endif
      end do

!   - whole profile saturated, then add ponding layer to groundwater level
      if (flsat)then
         if(h(1) .gt. 0.0d0)then
            if (pond .lt. 1.d-8) then
               gwl = min(z(1)+h(1),pond)
            else
               gwl = pond
            endif
         else
            gwl = 0.0d0
         end if
         nodgwl = 1
         if (flmacropore) then
            nodgwlflcpzo = 1
            gwlflcpzo    = gwl
         endif         
      endif         
 
! --- search for perched groundwater table

!   - first, search for first saturated compartment (i) above groundwater level
      i = nodhlp
      flunsat = .true.
      do while (flunsat .and. i.ge.1)
         if (h(i).ge.0.0d0) flunsat = .false.
         i = i - 1 
      enddo
!
!   - if saturated compartment above gwl exists, then find perched groundwater table
      if (i.ne.0) then
         flsat  = .true.
         bpegwl = i
         node   = bpegwl
         do while (flsat .and. node.gt.1)
            node = node - 1 
! adjusted : MM/JK-20151109            if (h(node) .lt. -1.0d-3) then
            if (h(node) .lt. 0.0d0) then
               if (flmacropore) then
                  call watertable(node,npegwl,nodhlp,flsat,             &
     &                          CritUndSatVol,dz,h,Theta,ThetaS,z,pegwl)
               else
                  flsat = .false.
                  npegwl = node
                  pegwl  = level(node,dz,h,z)
               endif
            endif
         end do
!
!   - whole profile saturated, then add ponding layer to perched groundwater level
         if (flsat)then
            if(h(1) .gt. 0.0d0)then
               if (pond .lt. 1.d-8) then
                  pegwl = min(z(1)+h(1),pond)
               else
                  pegwl = pond
               endif
            else
               pegwl = 0.0d0
            end if
            npegwl = 1
         endif 
      else
         bpegwl = -1
         npegwl = -1
      endif

! --- fatal error if gwl below profile and flux has to be calculated
      if ((swbotb.eq.3.or.swbotb.eq.4).and.gwl.gt.998.0d0) then
          messag = 'The groundwater level descends below the lower'     &
     &     //' boundary. This conflicts with bottom boundary'           &
     &     //' condition 3 and 4. Extend soil profile!'
         call fatalerr ('calcgwl',messag)
      endif

! --- warning error if there is inconsistency between defined gwl and soil physics
      if (swbotb.eq.1 .and. gwlinp .ge.z(1) .and. gwl.gt.998.0d0) then
!         determine date and date-time
         call dtdpst('year-month-day,hour:minute:seconds',t1900,datexti)
         write(messag,'(6a)')                                           &
     &         'No groundwater level because unsaturation at bottom ',  &
     &         'compartment ( ', datexti,  ' ). ',                      &
     &         'This is caused by inconsistency between ',              &
     &         'given gwl and soil physical parameters '
         call warn ('Calcgwl',messag,logf,swscre)
      endif


      return
      end

! ----------------------------------------------------------------------
      subroutine watertable(node,nodlev,nodhlp,flsat,CritUndSatVol,dz,h,&
     &                      Theta,ThetaS,z,waterlevel)
! ----------------------------------------------------------------------
!     date               : april 2008
!     purpose            : search for watertable and perched
!                          watertable (if existing).
! ----------------------------------------------------------------------
      implicit none
      include 'arrays.fi'

! --- global                                                          in
      integer node,nodhlp,nodlev
     
      real(8) CritUndSatVol,dz(macp),h(macp),Theta(macp),ThetaS(macp)
      real(8) z(macp)
      logical flsat
!                                                                    out
      real(8) waterlevel
! ----------------------------------------------------------------------
! --- local
      integer i
      real(8) level, TotUndSatVol
      logical flsat2

! ----------------------------------------------------------------------
      TotUndSatVol = 0.0d0
      flsat2 = .false. 
      i = node
      do while (TotUndSatVol.lt.CritUndSatVol .and. .not.flsat2 .and.   &
     &                                                           i.ge.1)
         TotUndSatVol = TotUndSatVol + (ThetaS(i) - Theta(i)) * dz(i)
         if (h(i).gt.-1.d-3) flsat2 = .true.
         i = i - 1
      enddo
!
      if (i.eq.0 .or. TotUndSatVol.gt.CritUndSatVol-1.d-8) then
         flsat = .false.
!!!!!!!! NODGWL is NOT node with GWL, but DEEPEST UNSATURATED NODE !!!!!!!!!!!!!
         nodlev = node
         nodhlp = i
      elseif(flsat2) then
         node   = i + 1
      endif
!
      if (.not.flsat) then        
         waterlevel = level(node,dz,h,z)
      endif
   
      return
      end

!-----------------------------------------------------------------------
      real(8) function level(node,dz,h,z)
! ----------------------------------------------------------------------
!     Date               : april 2008
!     Purpose            : calc. water level from pressure head
! ----------------------------------------------------------------------
      implicit none
      include 'arrays.fi'
! --- global 
      integer node
      real(8) dz(macp), h(macp), z(macp)
! ----------------------------------------------------------------------
      if (h(node+1).ge.0.0d0)then
         level = z(node+1) + h(node+1) / (h(node+1)-h(node))            &
     &           * 0.5d0 * (dz(node) + dz(node+1))
      else   
         level = z(node) - 0.5d0 * dz(node) - h(node)
         level = min(z(node),max(z(node)-0.5d0*dz(node),level))
      end if

      return
      end
