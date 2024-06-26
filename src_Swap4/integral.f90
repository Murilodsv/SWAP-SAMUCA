! File VersionID:
!   $Id: integral.f90 298 2016-07-25 20:07:31Z kroes006 $
! ----------------------------------------------------------------------
      subroutine integral 
! ----------------------------------------------------------------------
!     Date:    November 2004
!     Purpose: calculation of intermediate and cumulative fluxes
! ----------------------------------------------------------------------
      Use Variables
      implicit none

! --- local variables
      integer node,level
      real(8) qrotts,qdrats,ptrats,pevats,revats,qbotts
! ----------------------------------------------------------------------
             
      if (flzerointr) then
        igrai = 0.d0
        inrai = 0.d0
        iprec = 0.d0
        igird = 0.d0
        inird = 0.d0
      endif

! --- potential transpiration of this timestep
      ptrats = ptra * dt

! --- potential soil evaporation of this timestep
      pevats = peva * dt

! --- reduced soil evaporation of this timestep
      revats = reva * dt

! --- flux lower boundary of this timestep
      qbotts = qbot*dt

! --- total root extraction of this timestep
      qrotts = qrosum * dt

! --- total drainage flux of this timestep
      qdrats = qdrtot * dt

! --- determine daily actual transpiration
      if (fldaystart) tra = 0.0d0
      tra = tra + qrotts

! --- add time step fluxes to intermediate totals
      iqrot = iqrot + qrotts
      do 300 node = 1,numnod
        inqrot(node) = inqrot(node) + qrot(node) * dt
  300 continue
      iqredwet = iqredwet + qredwetsum*dt
      iqreddry = iqreddry + qreddrysum*dt
      iqredsol = iqredsol + qredsolsum*dt
      iqredfrs = iqredfrs + qredfrssum*dt
      ies0 = ies0 + 0.1d0*es0*dt
      iet0 = iet0 + 0.1d0*et0*dt
      iew0 = iew0 + 0.1d0*ew0*dt

      iqdra = iqdra + qdrats + QRapDra*dt
      do 410 node = 1,numnod
        qdraincomp(node) = 0.d0
        do 400 level = 1,nrlevs
          inqdra(level,node) = inqdra(level,node)+qdra(level,node)*dt
          qdraincomp(node) = qdra(level,node) + qdraincomp(node)
400     continue
410   continue

      iintc = iintc + (aintcdt+gird-nird)*dt

      iptra = iptra + ptrats
      ipeva = ipeva + pevats
      ievap = ievap + revats
      iruno = iruno + runots
      irunon = irunon + runon*dt
      iprec = iprec + (graidt+gird)*dt
      igrai = igrai + graidt*dt
      igird = igird + gird*dt
      inrai = inrai + nraidt*dt
      inird = inird + nird*dt
      iqbot = iqbot + qbotts
!      iQMaPo = iQMaPo +QMaPo*dt
! --- add time step fluxes to total cumulative values
      cqrot = cqrot + qrotts
      cqdra = cqdra + qdrats
      cptra = cptra + ptrats
      cpeva = cpeva + pevats
      cevap = cevap + revats
      if (runots.lt.0.0d0) then
        cinund = cinund - runots
      else if (runots.gt.0.0d0) then
        crunoff = crunoff + runots
      endif

      caintc = caintc + (aintcdt+gird-nird)*dt

      cgrai = cgrai + graidt*dt
      cnrai = cnrai + nraidt*dt
!      cnrai = cgrai - caintc
      cgird = cgird + gird*dt
      cnird = cnird + nird*dt

      if (qbotts.lt.0.0d0) then
        cqbotdo = cqbotdo - qbotts
      else if (qbotts.gt.0.0d0) then
        cqbotup = cqbotup + qbotts
      endif
      cqbot = cqbot + qbotts
      do level = 1,nrlevs
        ! infiltration
        if (qdrain(level).lt.0.0d0) then
          cqdrainin(level) = cqdrainin(level) - qdrain(level)*dt
        ! drainage
        else if (qdrain(level).gt.0.0d0) then
          cqdrainout(level) = cqdrainout(level) + qdrain(level)*dt
        endif      
        cqdrain(level) = cqdrain(level) + qdrain(level)*dt
      enddo
      ! rain on the ponding surface
      cqprai = cqprai + nraidt*dt    
      crunon = crunon + runon*dt     
      if (q(1).lt.0.0d0) then
        cqtdo = cqtdo - q(1)*dt
      else if (q(1).gt.0.0d0) then
        cqtup = cqtup + q(1)*dt
      endif

! --- compensate water balance error of this time step during remaining day part
! --- cumulative water balance error
      if (swsnow.eq.0) then 
        wbalance = cnrai + cnird + crunon - crunoff - cqrot - cevap     &
     &        - cqdra + cqbot + volini - volact + PondIni -pond 
      else
         wbalance = cqprai + cnird + cmelt + crunon - crunoff           &
     &        - cqrot - cevap - cqdra                                   &
     &        + cqbot + volini - volact + PondIni - pond
      endif

      if (Swmacro.eq.1) wbalance = wbalance - cQMpOutDrRap -            &
     &                  (WaSrDm1 + WaSrDm2 - WaSrDm1Ini - WaSrDm2Ini)

      return
      end




